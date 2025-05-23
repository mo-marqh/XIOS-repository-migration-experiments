#include "p2p_context_server.hpp"
#include "buffer_in.hpp"
#include "type.hpp"
#include "context.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "attribute_template.hpp"
#include "domain.hpp"
#include "field.hpp"
#include "file.hpp"
#include "grid.hpp"
#include "mpi.hpp"
#include "tracer.hpp"
#include "timer.hpp"
#include "cxios.hpp"
#include "event_scheduler.hpp"
#include "server.hpp"
#include "servers_ressource.hpp"
#include "pool_ressource.hpp"
#include "services.hpp"
#include "contexts_manager.hpp"
#include "timeline_events.hpp"

#include <random>
#include <chrono>


namespace xios
{
  using namespace std ;
  extern CLogType logTimers ;
  extern CLogType logProfile ;

  CP2pContextServer::CP2pContextServer(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm)
                         : CContextServer(parent, intraComm, interComm), 
                           isProcessingEvent_(false)
  {
   
    xios::MPI_Comm_dup(intraComm_, &processEventBarrier_) ;
    CXios::getMpiGarbageCollector().registerCommunicator(processEventBarrier_) ;
    
    currentTimeLine_=1;
    scheduled_=false;
    finished_=false;

    xios::MPI_Intercomm_merge(interComm_,true,&interCommMerged_) ;
    CXios::getMpiGarbageCollector().registerCommunicator(interCommMerged_) ;
    xios::MPI_Comm_split(intraComm_, intraCommRank_, intraCommRank_, &commSelf_) ; // for windows
    CXios::getMpiGarbageCollector().registerCommunicator(commSelf_) ;
    
    itLastTimeLine_=lastTimeLine_.begin() ;

    pureOneSided_=CXios::getin<bool>("pure_one_sided",false); // pure one sided communication (for test)
      
  }

  void CP2pContextServer::setPendingEvent(void)
  {
    pendingEvent_=true;
  }

  bool CP2pContextServer::hasPendingEvent(void)
  {
    return ((pendingEvents_.size()!=0)||(completedEvents_.size()!=0));
  }

  bool CP2pContextServer::hasFinished(void)
  {
    return finished_;
  }

  bool CP2pContextServer::eventLoop(bool enableEventsProcessing /*= true*/)
  {
    if (info.isActive(logProfile)) CTimer::get("Recv event loop (p2p)").resume();
    if (info.isActive(logTimers)) CTimer::get("listen request").resume();
    listen();
    if (info.isActive(logTimers)) CTimer::get("listen request").suspend();

    if (info.isActive(logTimers)) CTimer::get("listen pending request").resume();
    listenPendingRequest() ;
    if (info.isActive(logTimers)) CTimer::get("listen pending request").suspend();

    if (info.isActive(logTimers)) CTimer::get("check server Buffers").resume();
    checkBuffers() ;
    if (info.isActive(logTimers)) CTimer::get("check server Buffers").suspend();

    if (info.isActive(logTimers)) CTimer::get("check event process").resume();
    processEvents(enableEventsProcessing);
    if (info.isActive(logTimers)) CTimer::get("check event process").suspend();
    if (info.isActive(logProfile)) CTimer::get("Recv event loop (p2p)").suspend();
    return finished_;

  }

 void CP2pContextServer::listen(void)
  {
    int rank;
    int flag;
    MPI_Status status;
    flag=true ;

    while(flag)
    {
      traceOff();
      MPI_Iprobe(MPI_ANY_SOURCE, 20,interCommMerged_, &flag, &status);

      traceOn();
      if (flag==true)
      {
        int rank=status.MPI_SOURCE ;
        auto& rankRequests = requests_[rank];
        rankRequests.push_back(new CRequest(interCommMerged_, status)) ;
        // Test 1st request of the list, request treatment must be ordered 
        if (rankRequests.front()->test())
        {
          processRequest( *(rankRequests.front()) );
          delete rankRequests.front();
          rankRequests.pop_front() ;
        }
      }
    }
  }

  void CP2pContextServer::listenPendingRequest(void)
  {
    for(auto it_rank=requests_.begin() ; it_rank!=requests_.end() ; ++it_rank)
    {
      int rank = it_rank->first;
      auto& rankRequests = it_rank->second;
      while ( (!rankRequests.empty()) && (rankRequests.front()->test()) )
      {
        processRequest( *(rankRequests.front()) );
        delete rankRequests.front();
        rankRequests.pop_front() ;
      }
    }
  }

  void CP2pContextServer::processRequest(CRequest& request)
  {
    int rank = request.getRank() ;
    auto it=buffers_.find(rank);
    if (it==buffers_.end())
    {
      buffers_[rank] = new CP2pServerBuffer(rank, commSelf_, interCommMerged_, pendingEvents_, completedEvents_, request.getBuffer()) ;
    }
    else
    {
      it->second->receivedRequest(request.getBuffer()) ;
    }
  }

  void CP2pContextServer::checkBuffers(void)
  {
    if (!pendingEvents_.empty())
    {
/*
      SPendingEvent& nextEvent = pendingEvents_.begin()->second ;
      for(auto& buffer : nextEvent.buffers ) buffer->eventLoop() ;
      if (nextEvent.nbSenders==0) pendingEvents_.erase(pendingEvents_.begin()) ;
*/
      for(auto it=pendingEvents_.begin() ;  it!=pendingEvents_.end() ;)
      {
        SPendingEvent& nextEvent = it->second ;
        for(auto& buffer : nextEvent.buffers ) buffer->eventLoop() ;
        if (nextEvent.nbSenders==0) it=pendingEvents_.erase(it) ;
        else ++it ;
      }
    }
  }


  void CP2pContextServer::processEvents(bool enableEventsProcessing)
  {
  
    if (isProcessingEvent_) return ;

    auto it=completedEvents_.find(currentTimeLine_);

    if (it!=completedEvents_.end())
    {
      if (it->second.nbSenders == it->second.currentNbSenders)
      {
        if (!scheduled_) 
        {
          eventScheduler_->registerEvent(currentTimeLine_,hashId_);
          scheduled_=true;
        }
        else if (eventScheduler_->queryEvent(currentTimeLine_,hashId_) )
        {
          //if (!enableEventsProcessing && isCollectiveEvent(event)) return ;

          if (!eventScheduled_) 
          {
            MPI_Ibarrier(processEventBarrier_,&processEventRequest_) ;
            eventScheduled_=true ;
            return ;
          }
          else 
          {
            MPI_Status status ;
            int flag ;
            MPI_Test(&processEventRequest_, &flag, &status) ;
            if (!flag) return ;
            eventScheduled_=false ;
          }

          eventScheduler_->popEvent() ;

          isProcessingEvent_=true ;
          CEventServer event(this) ;
          for(auto& buffer : it->second.buffers) buffer->fillEventServer(currentTimeLine_, event) ;
//          MPI_Barrier(intraComm) ;
          CTimer::get("Process events").resume();
          info(100)<<"Context id "<<context_->getId()<<" : Process Event "<<currentTimeLine_<<" of class "<<event.classId<<" of type "<<event.type<<endl ;
          dispatchEvent(event);
          CTimer::get("Process events").suspend();
          isProcessingEvent_=false ;
//         context_->unsetProcessingEvent() ;
          pendingEvent_=false;
          completedEvents_.erase(it);
          currentTimeLine_++;
          scheduled_ = false;
        }
      }
    }
  }

  CP2pContextServer::~CP2pContextServer()
  {
    for(auto& buffer : buffers_) delete buffer.second;
    buffers_.clear() ;
  }

  void CP2pContextServer::releaseBuffers()
  {
    //for(auto it=buffers.begin();it!=buffers.end();++it) delete it->second ;
    //buffers.clear() ; 
    freeWindows() ;
  }

  void CP2pContextServer::freeWindows()
  {
    //  for(auto& it : winComm_)
    //  {
    //    int rank = it.first ;
    //    MPI_Win_free(&windows_[rank][0]);
    //    MPI_Win_free(&windows_[rank][1]);
    //    xios::MPI_Comm_free(&winComm_[rank]) ;
    //  }
  }

  void CP2pContextServer::notifyClientsFinalize(void)
  {
    for(auto it=buffers_.begin();it!=buffers_.end();++it)
    {
      it->second->notifyClientFinalize() ;
    }
  }

  void CP2pContextServer::dispatchEvent(CEventServer& event)
  {
    string contextName;
    string buff;
    int MsgSize;
    int rank;
    list<CEventServer::SSubEvent>::iterator it;
    StdString ctxId = context_->getId();
    CContext::setCurrent(ctxId);
    StdSize totalBuf = 0;

    if (event.classId==CContext::GetType() && event.type==CContext::EVENT_ID_CONTEXT_FINALIZE)
    {
      CTimer::get("Context finalize").resume();
      finished_=true;
      info(20)<<" CP2pContextServer: Receive context <"<<context_->getId()<<"> finalize."<<endl;
      notifyClientsFinalize() ;
      if (info.isActive(logTimers)) CTimer::get("receiving requests").suspend();
      context_->finalize();
      
      std::map<int, StdSize>::const_iterator itbMap = mapBufferSize_.begin(),
                           iteMap = mapBufferSize_.end(), itMap;
      for (itMap = itbMap; itMap != iteMap; ++itMap)
      {
        rank = itMap->first;
        report(10)<< " Memory report : Context <"<<ctxId<<"> : server side : memory used for buffer of each connection to client" << endl
            << "  +) With client of rank " << rank << " : " << itMap->second << " bytes " << endl;
        totalBuf += itMap->second;
      }
      report(0)<< " Memory report : Context <"<<ctxId<<"> : server side : total memory used for buffer "<<totalBuf<<" bytes"<<endl;
      CTimer::get("Context finalize").suspend();
    }
    else if (event.classId==CContext::GetType()) CContext::dispatchEvent(event);
    else if (event.classId==CContextGroup::GetType()) CContextGroup::dispatchEvent(event);
    else if (event.classId==CCalendarWrapper::GetType()) CCalendarWrapper::dispatchEvent(event);
    else if (event.classId==CDomain::GetType()) CDomain::dispatchEvent(event);
    else if (event.classId==CDomainGroup::GetType()) CDomainGroup::dispatchEvent(event);
    else if (event.classId==CAxis::GetType()) CAxis::dispatchEvent(event);
    else if (event.classId==CAxisGroup::GetType()) CAxisGroup::dispatchEvent(event);
    else if (event.classId==CScalar::GetType()) CScalar::dispatchEvent(event);
    else if (event.classId==CScalarGroup::GetType()) CScalarGroup::dispatchEvent(event);
    else if (event.classId==CGrid::GetType()) CGrid::dispatchEvent(event);
    else if (event.classId==CGridGroup::GetType()) CGridGroup::dispatchEvent(event);
    else if (event.classId==CField::GetType()) 
    {
      if (event.type==CField::EVENT_ID_UPDATE_DATA) CField::dispatchEvent(event);
      else CField::dispatchEvent(event);
    }
    else if (event.classId==CFieldGroup::GetType()) CFieldGroup::dispatchEvent(event);
    else if (event.classId==CFile::GetType()) CFile::dispatchEvent(event);
    else if (event.classId==CFileGroup::GetType()) CFileGroup::dispatchEvent(event);
    else if (event.classId==CVariable::GetType()) CVariable::dispatchEvent(event);
    else
    {
      ERROR("void CP2pContextServer::dispatchEvent(CEventServer& event)",<<" Bad event class Id"<<endl);
    }
  }

  bool CP2pContextServer::isCollectiveEvent(CEventServer& event)
  {
    if (event.type>1000) return false ;
    else return true ;
  }
}
