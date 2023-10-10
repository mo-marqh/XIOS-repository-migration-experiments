#include "one_sided_context_server.hpp"
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

#include <boost/functional/hash.hpp>
#include <random>
#include <chrono>


namespace xios
{
  using namespace std ;

  COneSidedContextServer::COneSidedContextServer(CContext* parent,MPI_Comm intraComm_,MPI_Comm interComm_)
                         : CContextServer(parent, intraComm_, interComm_), 
                           isProcessingEvent_(false)
  {
   
    xios::MPI_Comm_dup(intraComm, &processEventBarrier_) ;
    CXios::getMpiGarbageCollector().registerCommunicator(processEventBarrier_) ;
    
    currentTimeLine=1;
    scheduled=false;
    finished=false;

    xios::MPI_Intercomm_merge(interComm_,true,&interCommMerged_) ;
    CXios::getMpiGarbageCollector().registerCommunicator(interCommMerged_) ;
    xios::MPI_Comm_split(intraComm_, intraCommRank, intraCommRank, &commSelf_) ; // for windows
    CXios::getMpiGarbageCollector().registerCommunicator(commSelf_) ;

    itLastTimeLine=lastTimeLine.begin() ;

    pureOneSided=CXios::getin<bool>("pure_one_sided",false); // pure one sided communication (for test)
      
  }

  void COneSidedContextServer::setPendingEvent(void)
  {
    pendingEvent=true;
  }

  bool COneSidedContextServer::hasPendingEvent(void)
  {
    return pendingEvent;
  }

  bool COneSidedContextServer::hasFinished(void)
  {
    return finished;
  }

  bool COneSidedContextServer::eventLoop(bool enableEventsProcessing /*= true*/)
  {
    CTimer::get("listen request").resume();
    listen();
    CTimer::get("listen request").suspend();

    CTimer::get("listen pending request").resume();
    listenPendingRequest() ;
    CTimer::get("listen pending request").suspend();

    CTimer::get("check server Buffers").resume();
    checkBuffers() ;
    CTimer::get("check server Buffers").suspend();

    CTimer::get("check event process").resume();
    processEvents(enableEventsProcessing);
    CTimer::get("check event process").suspend();
    return finished;

  }

 void COneSidedContextServer::listen(void)
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
          processRequest(*(rankRequests.front())) ;
          delete rankRequests.front();
          rankRequests.pop_front() ;
        }
      }
    }
  }

  void COneSidedContextServer::listenPendingRequest(void)
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

  void COneSidedContextServer::processRequest(CRequest& request)
  {
    int rank = request.getRank() ;
    auto it=buffers_.find(rank);
    if (it==buffers_.end())
    {
      buffers_[rank] = new COneSidedServerBuffer(rank, commSelf_, interCommMerged_, pendingEvents_, completedEvents_, request.getBuffer()) ;
    }
    else it->second->receivedRequest(request.getBuffer()) ;
  }

  void COneSidedContextServer::checkBuffers(void)
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


  void COneSidedContextServer::processEvents(bool enableEventsProcessing)
  {
  
    if (isProcessingEvent_) return ;

    auto it=completedEvents_.find(currentTimeLine);

    if (it!=completedEvents_.end())
    {
      if (it->second.nbSenders == it->second.currentNbSenders)
      {
        if (!scheduled) 
        {
          eventScheduler_->registerEvent(currentTimeLine,hashId);
          scheduled=true;
        }
        else if (eventScheduler_->queryEvent(currentTimeLine,hashId) )
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
          for(auto& buffer : it->second.buffers) buffer->fillEventServer(currentTimeLine, event) ;
//          MPI_Barrier(intraComm) ;
          CTimer::get("Process events").resume();
          info(100)<<"Context id "<<context->getId()<<" : Process Event "<<currentTimeLine<<" of class "<<event.classId<<" of type "<<event.type<<endl ;
          dispatchEvent(event);
          CTimer::get("Process events").suspend();
          isProcessingEvent_=false ;
//         context->unsetProcessingEvent() ;
          pendingEvent=false;
          completedEvents_.erase(it);
          currentTimeLine++;
          scheduled = false;
        }
      }
    }
  }

  COneSidedContextServer::~COneSidedContextServer()
  {
    for(auto& buffer : buffers_) delete buffer.second;
    buffers_.clear() ;
  }

  void COneSidedContextServer::releaseBuffers()
  {
    //for(auto it=buffers.begin();it!=buffers.end();++it) delete it->second ;
    //buffers.clear() ; 
    freeWindows() ;
  }

  void COneSidedContextServer::freeWindows()
  {
    //  for(auto& it : winComm_)
    //  {
    //    int rank = it.first ;
    //    MPI_Win_free(&windows_[rank][0]);
    //    MPI_Win_free(&windows_[rank][1]);
    //    xios::MPI_Comm_free(&winComm_[rank]) ;
    //  }
  }

  void COneSidedContextServer::notifyClientsFinalize(void)
  {
    for(auto it=buffers_.begin();it!=buffers_.end();++it)
    {
      it->second->notifyClientFinalize() ;
    }
  }

  void COneSidedContextServer::dispatchEvent(CEventServer& event)
  {
    string contextName;
    string buff;
    int MsgSize;
    int rank;
    list<CEventServer::SSubEvent>::iterator it;
    StdString ctxId = context->getId();
    CContext::setCurrent(ctxId);
    StdSize totalBuf = 0;

    if (event.classId==CContext::GetType() && event.type==CContext::EVENT_ID_CONTEXT_FINALIZE)
    {
      finished=true;
      info(20)<<" COneSidedContextServer: Receive context <"<<context->getId()<<"> finalize."<<endl;
      notifyClientsFinalize() ;
      CTimer::get("receiving requests").suspend();
      context->finalize();
      
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
      ERROR("void COneSidedContextServer::dispatchEvent(CEventServer& event)",<<" Bad event class Id"<<endl);
    }
  }

  bool COneSidedContextServer::isCollectiveEvent(CEventServer& event)
  {
    if (event.type>1000) return false ;
    else return true ;
  }
}
