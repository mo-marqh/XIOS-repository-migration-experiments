#include "legacy_context_server_v2.hpp"
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

  CLegacyContextServerV2::CLegacyContextServerV2(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm) 
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
    
    int maxAttachedWindows = CXios::getin<int>("max_attached_mem_window", 32)/2; // max number of attached memory to a dynamic window by client
    if (intraCommSize_ <= maxAttachedWindows) numServerByWindow_ = intraCommSize_ ;
    else numServerByWindow_ = maxAttachedWindows ;
    int nbWin =  intraCommSize_ / numServerByWindow_ ;
    if (intraCommSize_ % numServerByWindow_ != 0 ) nbWin++ ;
    winDynamics_.resize(nbWin) ;
    for(auto& win : winDynamics_ ) 
    {
      win = new CWindowDynamic ;
      win->create(interCommMerged_) ;
    }
  
    itLastTimeLine_=lastTimeLine_.begin() ;

    pureOneSided_=CXios::getin<bool>("pure_one_sided",false); // pure one sided communication (for test)
  }
 
  void CLegacyContextServerV2::setPendingEvent(void)
  {
    pendingEvent_=true;
  }

  bool CLegacyContextServerV2::hasPendingEvent(void)
  {
    return (pendingRequest_.size()!=0);
  }

  bool CLegacyContextServerV2::hasFinished(void)
  {
    return finished_;
  }

  bool CLegacyContextServerV2::eventLoop(bool enableEventsProcessing /*= true*/)
  {
    if (info.isActive(logProfile)) CTimer::get("Recv event loop (legacy)").resume();
    if (info.isActive(logTimers)) CTimer::get("listen request").resume();
    listen();
    if (info.isActive(logTimers)) CTimer::get("listen request").suspend();
    if (info.isActive(logTimers)) CTimer::get("check pending request").resume();
    checkPendingRequest();
    checkPendingProbe() ;
    if (info.isActive(logTimers)) CTimer::get("check pending request").suspend();
    if (info.isActive(logTimers)) CTimer::get("check event process").resume();
    processEvents(enableEventsProcessing);
    if (info.isActive(logTimers)) CTimer::get("check event process").suspend();
    if (info.isActive(logProfile)) CTimer::get("Recv event loop (legacy)").suspend();
    return finished_;
  }

 void CLegacyContextServerV2::listen(void)
  {
    int rank;
    int flag;
    int count;
    char * addr;
    MPI_Status status;
    MPI_Message message ;
    map<int,CServerBuffer*>::iterator it;
    bool okLoop;

    traceOff();
    MPI_Improbe(MPI_ANY_SOURCE, 20, interCommMerged_,&flag,&message, &status);
    traceOn();
    if (flag==true) listenPendingRequest(message, status) ;
  }

  bool CLegacyContextServerV2::listenPendingRequest( MPI_Message &message, MPI_Status& status)
  {
    int count;
    char * addr;
    map<int,CServerBuffer*>::iterator it;
    int rank=status.MPI_SOURCE ;

    it=buffers_.find(rank);
    if (it==buffers_.end()) // Receive the buffer size and allocate the buffer
    {
      MPI_Aint recvBuff[4] ;
      MPI_Mrecv(recvBuff, 4, MPI_AINT,  &message, &status);
      remoteHashId_ = recvBuff[0] ;
      StdSize buffSize = recvBuff[1];
      vector<MPI_Aint> winBufferAddress(2) ;
      winBufferAddress[0]=recvBuff[2] ; winBufferAddress[1]=recvBuff[3] ;
      mapBufferSize_.insert(std::make_pair(rank, buffSize));

      if (info.isActive(logTimers)) CTimer::get("create Windows").resume() ;

      windows_[rank].resize(2) ;
      int serverRank, commSize ;
      MPI_Comm_rank(intraComm_, &serverRank) ;
      MPI_Comm_size(intraComm_, &commSize) ;

      int numWindow = serverRank / numServerByWindow_ ;
      
      windows_[rank][0] = new CWindowDynamicView(winDynamics_[numWindow], serverRank) ;
      windows_[rank][1] = new CWindowDynamicView(winDynamics_[numWindow], commSize+serverRank) ;

      windows_[rank][0] -> setWinBufferAddress(winBufferAddress[0],rank) ;
      windows_[rank][1] -> setWinBufferAddress(winBufferAddress[1],rank) ;

      if (info.isActive(logTimers)) CTimer::get("create Windows").suspend() ;

      it=(buffers_.insert(pair<int,CServerBuffer*>(rank, new CServerBuffer(rank, windows_[rank], winBufferAddress, rank, buffSize)))).first;
      lastTimeLine_[rank]=0 ;
      itLastTimeLine_=lastTimeLine_.begin() ;

      return true;
    }
    else
    {
        std::pair<MPI_Message,MPI_Status> mypair(message,status) ;
        pendingProbe_[rank].push_back(mypair) ;
        return false;
    }
  }

  void CLegacyContextServerV2::checkPendingProbe(void)
  {
    
    list<int> recvProbe ;
    list<int>::iterator itRecv ;
    map<int, list<std::pair<MPI_Message,MPI_Status> > >::iterator itProbe;

    for(itProbe=pendingProbe_.begin();itProbe!=pendingProbe_.end();itProbe++)
    {
      int rank=itProbe->first ;
      if (pendingRequest_.count(rank)==0)
      {
        MPI_Message& message = itProbe->second.front().first ;
        MPI_Status& status = itProbe->second.front().second ;
        int count ;
        MPI_Get_count(&status,MPI_CHAR,&count);
        map<int,CServerBuffer*>::iterator it = buffers_.find(rank);
        if ( (it->second->isBufferFree(count) && !it->second->isResizing()) // accept new request if buffer is free
          || (it->second->isResizing() && it->second->isBufferEmpty()) )    // or if resizing wait for buffer is empty
        {
          char * addr;
          addr=(char*)it->second->getBuffer(count);
          MPI_Imrecv(addr,count,MPI_CHAR, &message, &pendingRequest_[rank]);
          bufferRequest_[rank]=addr;
          recvProbe.push_back(rank) ;
          itProbe->second.pop_front() ;
        }
      }
    }

    for(itRecv=recvProbe.begin(); itRecv!=recvProbe.end(); itRecv++) if (pendingProbe_[*itRecv].empty()) pendingProbe_.erase(*itRecv) ;
  }


  void CLegacyContextServerV2::checkPendingRequest(void)
  {
    map<int,MPI_Request>::iterator it;
    list<int> recvRequest;
    list<int>::iterator itRecv;
    int rank;
    int flag;
    int count;
    MPI_Status status;
   
    if (!pendingRequest_.empty())
    {
      if (info.isActive(logTimers)) CTimer::get("receiving requests").resume();
    }
    else if (info.isActive(logTimers)) CTimer::get("receiving requests").suspend();

    for(it=pendingRequest_.begin();it!=pendingRequest_.end();it++)
    {
      rank=it->first;
      traceOff();
      MPI_Test(& it->second, &flag, &status);
      traceOn();
      if (flag==true)
      {
        buffers_[rank]->updateCurrentWindows() ;
        recvRequest.push_back(rank);
        MPI_Get_count(&status,MPI_CHAR,&count);
        processRequest(rank,bufferRequest_[rank],count);
      }
    }

    for(itRecv=recvRequest.begin();itRecv!=recvRequest.end();itRecv++)
    {
      pendingRequest_.erase(*itRecv);
      bufferRequest_.erase(*itRecv);
    }
  }

  void CLegacyContextServerV2::getBufferFromClient(size_t timeLine)
  {
    if (info.isActive(logTimers)) CTimer::get("CLegacyContextServerV2::getBufferFromClient").resume() ;

    int rank ;
    char *buffer ;
    size_t count ; 

    if (itLastTimeLine_==lastTimeLine_.end()) itLastTimeLine_=lastTimeLine_.begin() ;
    for(;itLastTimeLine_!=lastTimeLine_.end();++itLastTimeLine_)
    {
      rank=itLastTimeLine_->first ;
      if (itLastTimeLine_->second < timeLine &&  pendingRequest_.count(rank)==0 && buffers_[rank]->isBufferEmpty())
      {
        if (buffers_[rank]->getBufferFromClient(timeLine, buffer, count)) processRequest(rank, buffer, count);
        if (count >= 0) ++itLastTimeLine_ ;
        break ;
      }
    }
    if (info.isActive(logTimers)) CTimer::get("CLegacyContextServerV2::getBufferFromClient").suspend() ;
  }
         
       
  void CLegacyContextServerV2::processRequest(int rank, char* buff,int count)
  {

    CBufferIn buffer(buff,count);
    char* startBuffer,endBuffer;
    int size, offset;
    size_t timeLine=0;
    map<size_t,CEventServer*>::iterator it;

    
    CTimer::get("Process request").resume();
    while(count>0)
    {
      char* startBuffer=(char*)buffer.ptr();
      CBufferIn newBuffer(startBuffer,buffer.remain());
      newBuffer>>size>>timeLine;

      if (timeLine==timelineEventNotifyChangeBufferSize)
      {
        buffers_[rank]->notifyBufferResizing() ;
        buffers_[rank]->updateCurrentWindows() ;
        buffers_[rank]->popBuffer(count) ;
        info(100)<<"Context id "<<context_->getId()<<" : Receive NotifyChangeBufferSize from client rank "<<rank<<endl
                 <<"isBufferEmpty ? "<<buffers_[rank]->isBufferEmpty()<<"  remaining count : "<<buffers_[rank]->getUsed()<<endl;
      } 
      else if (timeLine==timelineEventChangeBufferSize)
      {
        size_t newSize ;
        vector<MPI_Aint> winBufferAdress(2) ;
        newBuffer>>newSize>>winBufferAdress[0]>>winBufferAdress[1] ;
        buffers_[rank]->freeBuffer(count) ;
        delete buffers_[rank] ;

        int serverRank, commSize ;
        windows_[rank][0] -> setWinBufferAddress(winBufferAdress[0], rank) ;
        windows_[rank][1] -> setWinBufferAddress(winBufferAdress[1], rank) ;
        buffers_[rank] = new CServerBuffer(rank, windows_[rank], winBufferAdress, rank, newSize) ;
        info(100)<<"Context id "<<context_->getId()<<" : Receive ChangeBufferSize from client rank "<<rank
                 <<"  newSize : "<<newSize<<" Address : "<<winBufferAdress[0]<<" & "<<winBufferAdress[1]<<endl ;
      }
      else
      {
        info(100)<<"Context id "<<context_->getId()<<" : Receive standard event from client rank "<<rank<<"  with timeLine : "<<timeLine<<endl ;
        it=events_.find(timeLine);
       
        if (it==events_.end()) it=events_.insert(pair<int,CEventServer*>(timeLine,new CEventServer(this))).first;
        it->second->push(rank,buffers_[rank],startBuffer,size);
        if (timeLine>0) lastTimeLine_[rank]=timeLine ;
      }
      buffer.advance(size);
      count=buffer.remain();
    }
    
    CTimer::get("Process request").suspend();
  }

  void CLegacyContextServerV2::processEvents(bool enableEventsProcessing)
  {
    map<size_t,CEventServer*>::iterator it;
    CEventServer* event;
    
    if (isProcessingEvent_) return ;

    it=events_.find(currentTimeLine_);
    if (it!=events_.end())
    {
      event=it->second;

      if (event->isFull())
      {
        if (!scheduled_)
        {
          eventScheduler_->registerEvent(currentTimeLine_,hashId_);
          info(100)<<"Context id "<<context_->getId()<<"Schedule event : "<< currentTimeLine_ <<"  "<<hashId_<<endl ;
          scheduled_=true;
        }
        else if (eventScheduler_->queryEvent(currentTimeLine_,hashId_) )
        {
          if (!enableEventsProcessing && isCollectiveEvent(*event)) return ;

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
          
          if (CXios::checkEventSync && context_->getServiceType()!=CServicesManager::CLIENT)
          {
            int typeId, classId, typeId_in, classId_in;
            long long timeLine_out;
            long long timeLine_in( currentTimeLine_ );
            typeId_in=event->type ;
            classId_in=event->classId ;
   //        MPI_Allreduce(&timeLine,&timeLine_out, 1, MPI_UINT64_T, MPI_SUM, intraComm_) ; // MPI_UINT64_T standardized by MPI 3
            MPI_Allreduce(&timeLine_in,&timeLine_out, 1, MPI_LONG_LONG_INT, MPI_SUM, intraComm_) ; 
            MPI_Allreduce(&typeId_in,&typeId, 1, MPI_INT, MPI_SUM, intraComm_) ;
            MPI_Allreduce(&classId_in,&classId, 1, MPI_INT, MPI_SUM, intraComm_) ;
            if (typeId/intraCommSize_!=event->type || classId/intraCommSize_!=event->classId || timeLine_out/intraCommSize_!=currentTimeLine_)
            {
               ERROR("void CLegacyContextClient::sendEvent(CEventClient& event)",
                  << "Event are not coherent between client for timeline = "<<currentTimeLine_);
            }
          }

          isProcessingEvent_=true ;
          CTimer::get("Process events").resume();
          info(100)<<"Context id "<<context_->getId()<<" : Process Event "<<currentTimeLine_<<" of class "<<event->classId<<" of type "<<event->type<<endl ;
          eventScheduler_->popEvent() ;
          dispatchEvent(*event);
          CTimer::get("Process events").suspend();
          isProcessingEvent_=false ;
          pendingEvent_=false;
          delete event;
          events_.erase(it);
          currentTimeLine_++;
          scheduled_ = false;
        }
      }
      else if (pendingRequest_.empty()) getBufferFromClient(currentTimeLine_) ;
    }
    else if (pendingRequest_.empty()) getBufferFromClient(currentTimeLine_) ; // if pure one sided check buffer even if no event recorded at current time line
  }

  CLegacyContextServerV2::~CLegacyContextServerV2()
  {
    map<int,CServerBuffer*>::iterator it;
    for(it=buffers_.begin();it!=buffers_.end();++it) delete it->second;
    buffers_.clear() ;
  }

  void CLegacyContextServerV2::releaseBuffers()
  {
    //for(auto it=buffers_.begin();it!=buffers_.end();++it) delete it->second ;
    //buffers_.clear() ; 
    freeWindows() ;
  }

  void CLegacyContextServerV2::freeWindows()
  {
    for(auto& it : windows_)
    {
      delete (it.second)[0] ;
      delete (it.second)[1] ;
    }
    for(auto& win : winDynamics_) delete win ;
  }

  void CLegacyContextServerV2::notifyClientsFinalize(void)
  {
    for(auto it=buffers_.begin();it!=buffers_.end();++it)
    {
      it->second->notifyClientFinalize() ;
    }
  }

  void CLegacyContextServerV2::dispatchEvent(CEventServer& event)
  {
    string contextName;
    string buff;
    int MsgSize;
    int rank;
    list<CEventServer::SSubEvent>::iterator it;
    StdString ctxId = context_->getId();
    StdSize totalBuf = 0;

    if (event.classId==CContext::GetType() && event.type==CContext::EVENT_ID_CONTEXT_FINALIZE)
    {
      if (info.isActive(logProfile)) CTimer::get("Context finalize").resume();
      finished_=true;
      info(20)<<" CLegacyContextServerV2: Receive context <"<<context_->getId()<<"> finalize."<<endl;
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
      if (info.isActive(logProfile)) CTimer::get("Context finalize").suspend();
    }
    else if (event.classId==CContext::GetType()) CContext::dispatchEvent(context_, event);
    else if (event.classId==CContextGroup::GetType()) CContextGroup::dispatchEvent(context_, event);
    else if (event.classId==CCalendarWrapper::GetType()) CCalendarWrapper::dispatchEvent(context_, event);
    else if (event.classId==CDomain::GetType()) CDomain::dispatchEvent(context_, event);
    else if (event.classId==CDomainGroup::GetType()) CDomainGroup::dispatchEvent(context_, event);
    else if (event.classId==CAxis::GetType()) CAxis::dispatchEvent(context_, event);
    else if (event.classId==CAxisGroup::GetType()) CAxisGroup::dispatchEvent(context_, event);
    else if (event.classId==CScalar::GetType()) CScalar::dispatchEvent(context_, event);
    else if (event.classId==CScalarGroup::GetType()) CScalarGroup::dispatchEvent(context_, event);
    else if (event.classId==CGrid::GetType()) CGrid::dispatchEvent(context_, event);
    else if (event.classId==CGridGroup::GetType()) CGridGroup::dispatchEvent(context_, event);
    else if (event.classId==CField::GetType()) CField::dispatchEvent(context_, event);
    else if (event.classId==CFieldGroup::GetType()) CFieldGroup::dispatchEvent(context_, event);
    else if (event.classId==CFile::GetType()) CFile::dispatchEvent(context_, event);
    else if (event.classId==CFileGroup::GetType()) CFileGroup::dispatchEvent(context_, event);
    else if (event.classId==CVariable::GetType()) CVariable::dispatchEvent(context_, event);
    else
    {
      ERROR("void CLegacyContextServerV2::dispatchEvent(CEventServer& event)",<<" Bad event class Id"<<endl);
    }
  }

  bool CLegacyContextServerV2::isCollectiveEvent(CEventServer& event)
  {
    if (event.type>1000) return false ;
    else return true ;
  }
}
