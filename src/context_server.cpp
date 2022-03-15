#include "context_server.hpp"
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

  CContextServer::CContextServer(CContext* parent,MPI_Comm intraComm_,MPI_Comm interComm_) 
    : eventScheduler_(nullptr), isProcessingEvent_(false), associatedClient_(nullptr)
  {
    context=parent;
    intraComm=intraComm_;
    MPI_Comm_size(intraComm,&intraCommSize);
    MPI_Comm_rank(intraComm,&intraCommRank);

    interComm=interComm_;
    int flag;
    MPI_Comm_test_inter(interComm,&flag);

    if (flag) attachedMode=false ;
    else  attachedMode=true ;
    
    if (flag) MPI_Comm_remote_size(interComm,&clientSize_);
    else  MPI_Comm_size(interComm,&clientSize_);

   
    SRegisterContextInfo contextInfo ;
    CXios::getContextsManager()->getContextInfo(context->getId(), contextInfo, intraComm) ;

  //  if (contextInfo.serviceType != CServicesManager::CLIENT) // we must have an event scheduler => to be retrieve from the associated services
  //  {
      //if (!isAttachedModeEnabled()) eventScheduler_=CXios::getPoolRessource()->getService(contextInfo.serviceId,contextInfo.partitionId)->getEventScheduler() ;
    eventScheduler_=CXios::getPoolRessource()->getService(contextInfo.serviceId,contextInfo.partitionId)->getEventScheduler() ;
    MPI_Comm_dup(intraComm, &processEventBarrier_) ;
  //  }


    currentTimeLine=1;
    scheduled=false;
    finished=false;

    // generate unique hash for server
    auto time=chrono::system_clock::now().time_since_epoch().count() ;
    std::default_random_engine rd(time); // not reproducible from a run to another
    std::uniform_int_distribution<size_t> dist;
    hashId=dist(rd) ;
    MPI_Bcast(&hashId,1,MPI_SIZE_T,0,intraComm) ; // Bcast to all server of the context


    if (!isAttachedModeEnabled()) MPI_Intercomm_merge(interComm_,true,&interCommMerged_) ;
    MPI_Comm_split(intraComm_, intraCommRank, intraCommRank, &commSelf_) ; // for windows
    
    itLastTimeLine=lastTimeLine.begin() ;

    pureOneSided=CXios::getin<bool>("pure_one_sided",false); // pure one sided communication (for test)
    if (isAttachedModeEnabled()) pureOneSided=false ; // no one sided in attach mode
      
  }

//! Attached mode is used ?
//! \return true if attached mode is used, false otherwise
  bool CContextServer::isAttachedModeEnabled() const
  {
    return attachedMode ;
  }
  
  void CContextServer::setPendingEvent(void)
  {
    pendingEvent=true;
  }

  bool CContextServer::hasPendingEvent(void)
  {
    return pendingEvent;
  }

  bool CContextServer::hasFinished(void)
  {
    return finished;
  }

  bool CContextServer::eventLoop(bool enableEventsProcessing /*= true*/)
  {
    CTimer::get("listen request").resume();
    listen();
    CTimer::get("listen request").suspend();
    CTimer::get("check pending request").resume();
    checkPendingRequest();
    checkPendingProbe() ;
    CTimer::get("check pending request").suspend();
    CTimer::get("check event process").resume();
    if (enableEventsProcessing)  processEvents();
    CTimer::get("check event process").suspend();
    return finished;
  }

 void CContextServer::listen(void)
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
    MPI_Improbe(MPI_ANY_SOURCE, 20,interComm,&flag,&message, &status);
    traceOn();
    if (flag==true) listenPendingRequest(message, status) ;
  }

  bool CContextServer::listenPendingRequest( MPI_Message &message, MPI_Status& status)
  {
    int count;
    char * addr;
    map<int,CServerBuffer*>::iterator it;
    int rank=status.MPI_SOURCE ;

    it=buffers.find(rank);
    if (it==buffers.end()) // Receive the buffer size and allocate the buffer
    {
      MPI_Aint recvBuff[4] ;
      MPI_Mrecv(recvBuff, 4, MPI_AINT,  &message, &status);
      remoteHashId_ = recvBuff[0] ;
      StdSize buffSize = recvBuff[1];
      vector<MPI_Aint> winAdress(2) ;
      winAdress[0]=recvBuff[2] ; winAdress[1]=recvBuff[3] ;
      mapBufferSize_.insert(std::make_pair(rank, buffSize));

      // create windows dynamically for one-sided
      if (!isAttachedModeEnabled())
      { 
        CTimer::get("create Windows").resume() ;
        MPI_Comm interComm ;
        MPI_Intercomm_create(commSelf_, 0, interCommMerged_, rank, 0 , &interComm) ;
        MPI_Intercomm_merge(interComm, true, &winComm_[rank]) ;
        CXios::getMpiGarbageCollector().registerCommunicator(winComm_[rank]) ;
        MPI_Comm_free(&interComm) ;
        windows_[rank].resize(2) ;
        MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_[rank], &windows_[rank][0]);
        CXios::getMpiGarbageCollector().registerWindow(windows_[rank][0]) ;
        MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_[rank], &windows_[rank][1]);
        CXios::getMpiGarbageCollector().registerWindow(windows_[rank][1]) ;
        CTimer::get("create Windows").suspend() ;
        MPI_Barrier(winComm_[rank]) ;
      }
      else
      {
        winComm_[rank] = MPI_COMM_NULL ;
        windows_[rank].resize(2) ;
        windows_[rank][0] = MPI_WIN_NULL ;
        windows_[rank][1] = MPI_WIN_NULL ;
      }   

      it=(buffers.insert(pair<int,CServerBuffer*>(rank,new CServerBuffer(windows_[rank], winAdress, 0, buffSize)))).first;
      lastTimeLine[rank]=0 ;
      itLastTimeLine=lastTimeLine.begin() ;

      return true;
    }
    else
    {
        std::pair<MPI_Message,MPI_Status> mypair(message,status) ;
        pendingProbe[rank].push_back(mypair) ;
        return false;
    }
  }

  void CContextServer::checkPendingProbe(void)
  {
    
    list<int> recvProbe ;
    list<int>::iterator itRecv ;
    map<int, list<std::pair<MPI_Message,MPI_Status> > >::iterator itProbe;

    for(itProbe=pendingProbe.begin();itProbe!=pendingProbe.end();itProbe++)
    {
      int rank=itProbe->first ;
      if (pendingRequest.count(rank)==0)
      {
        MPI_Message& message = itProbe->second.front().first ;
        MPI_Status& status = itProbe->second.front().second ;
        int count ;
        MPI_Get_count(&status,MPI_CHAR,&count);
        map<int,CServerBuffer*>::iterator it = buffers.find(rank);
        if ( (it->second->isBufferFree(count) && !it->second->isResizing()) // accept new request if buffer is free
          || (it->second->isResizing() && it->second->isBufferEmpty()) )    // or if resizing wait for buffer is empty
        {
          char * addr;
          addr=(char*)it->second->getBuffer(count);
          MPI_Imrecv(addr,count,MPI_CHAR, &message, &pendingRequest[rank]);
          bufferRequest[rank]=addr;
          recvProbe.push_back(rank) ;
          itProbe->second.pop_front() ;
        }
      }
    }

    for(itRecv=recvProbe.begin(); itRecv!=recvProbe.end(); itRecv++) if (pendingProbe[*itRecv].empty()) pendingProbe.erase(*itRecv) ;
  }


  void CContextServer::checkPendingRequest(void)
  {
    map<int,MPI_Request>::iterator it;
    list<int> recvRequest;
    list<int>::iterator itRecv;
    int rank;
    int flag;
    int count;
    MPI_Status status;
   
    if (!pendingRequest.empty()) CTimer::get("receiving requests").resume();
    else CTimer::get("receiving requests").suspend();

    for(it=pendingRequest.begin();it!=pendingRequest.end();it++)
    {
      rank=it->first;
      traceOff();
      MPI_Test(& it->second, &flag, &status);
      traceOn();
      if (flag==true)
      {
        buffers[rank]->updateCurrentWindows() ;
        recvRequest.push_back(rank);
        MPI_Get_count(&status,MPI_CHAR,&count);
        processRequest(rank,bufferRequest[rank],count);
      }
    }

    for(itRecv=recvRequest.begin();itRecv!=recvRequest.end();itRecv++)
    {
      pendingRequest.erase(*itRecv);
      bufferRequest.erase(*itRecv);
    }
  }

  void CContextServer::getBufferFromClient(size_t timeLine)
  {
    CTimer::get("CContextServer::getBufferFromClient").resume() ;
    if (!isAttachedModeEnabled()) // one sided desactivated in attached mode
    {  
      int rank ;
      char *buffer ;
      size_t count ; 

      if (itLastTimeLine==lastTimeLine.end()) itLastTimeLine=lastTimeLine.begin() ;
      for(;itLastTimeLine!=lastTimeLine.end();++itLastTimeLine)
      {
        rank=itLastTimeLine->first ;
        if (itLastTimeLine->second < timeLine &&  pendingRequest.count(rank)==0 && buffers[rank]->isBufferEmpty())
        {
          if (buffers[rank]->getBufferFromClient(timeLine, buffer, count)) processRequest(rank, buffer, count);
          if (count >= 0) ++itLastTimeLine ;
          break ;
        }
      }
    }
    CTimer::get("CContextServer::getBufferFromClient").suspend() ;
  }
         
       
  void CContextServer::processRequest(int rank, char* buff,int count)
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
        buffers[rank]->notifyBufferResizing() ;
        buffers[rank]->updateCurrentWindows() ;
        buffers[rank]->popBuffer(count) ;
        info(100)<<"Context id "<<context->getId()<<" : Receive NotifyChangeBufferSize from client rank "<<rank<<endl
                 <<"isBufferEmpty ? "<<buffers[rank]->isBufferEmpty()<<"  remaining count : "<<buffers[rank]->getUsed()<<endl;
      } 
      else if (timeLine==timelineEventChangeBufferSize)
      {
        size_t newSize ;
        vector<MPI_Aint> winAdress(2) ;
        newBuffer>>newSize>>winAdress[0]>>winAdress[1] ;
        buffers[rank]->freeBuffer(count) ;
        delete buffers[rank] ;
        buffers[rank] = new CServerBuffer(windows_[rank], winAdress, 0, newSize) ;
        info(100)<<"Context id "<<context->getId()<<" : Receive ChangeBufferSize from client rank "<<rank
                 <<"  newSize : "<<newSize<<" Address : "<<winAdress[0]<<" & "<<winAdress[1]<<endl ;
      }
      else
      {
        info(100)<<"Context id "<<context->getId()<<" : Receive standard event from client rank "<<rank<<"  with timeLine : "<<timeLine<<endl ;
        it=events.find(timeLine);
        if (it==events.end()) it=events.insert(pair<int,CEventServer*>(timeLine,new CEventServer(this))).first;
        it->second->push(rank,buffers[rank],startBuffer,size);
        if (timeLine>0) lastTimeLine[rank]=timeLine ;
      }
      buffer.advance(size);
      count=buffer.remain();
    }
    
    CTimer::get("Process request").suspend();
  }

  void CContextServer::processEvents(void)
  {
    map<size_t,CEventServer*>::iterator it;
    CEventServer* event;
    
//    if (context->isProcessingEvent()) return ;
    if (isProcessingEvent_) return ;
    if (isAttachedModeEnabled())
      if (!CXios::getDaemonsManager()->isScheduledContext(remoteHashId_)) return ;

    it=events.find(currentTimeLine);
    if (it!=events.end())
    {
      event=it->second;

      if (event->isFull())
      {
        if (!scheduled && !isAttachedModeEnabled()) // Skip event scheduling for attached mode and reception on client side
        {
          eventScheduler_->registerEvent(currentTimeLine,hashId);
          scheduled=true;
        }
        else if (isAttachedModeEnabled() || eventScheduler_->queryEvent(currentTimeLine,hashId) )
        {

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

          if (!isAttachedModeEnabled()) eventScheduler_->popEvent() ;
          //MPI_Barrier(intraComm) ;
         // When using attached mode, synchronise the processes to avoid that differents event be scheduled by differents processes
         // The best way to properly solve this problem will be to use the event scheduler also in attached mode
         // for now just set up a MPI barrier
//ym to be check later
//         if (!eventScheduler_ && CXios::isServer) MPI_Barrier(intraComm) ;

//         context->setProcessingEvent() ;
         isProcessingEvent_=true ;
         CTimer::get("Process events").resume();
         info(100)<<"Context id "<<context->getId()<<" : Process Event "<<currentTimeLine<<" of class "<<event->classId<<" of type "<<event->type<<endl ;
         dispatchEvent(*event);
         CTimer::get("Process events").suspend();
         isProcessingEvent_=false ;
//         context->unsetProcessingEvent() ;
         pendingEvent=false;
         delete event;
         events.erase(it);
         currentTimeLine++;
         scheduled = false;
         if (isAttachedModeEnabled()) CXios::getDaemonsManager()->unscheduleContext() ;
        }
      }
      else if (pendingRequest.empty()) getBufferFromClient(currentTimeLine) ;
    }
    else if (pendingRequest.empty()) getBufferFromClient(currentTimeLine) ; // if pure one sided check buffer even if no event recorded at current time line
  }

  CContextServer::~CContextServer()
  {
    map<int,CServerBuffer*>::iterator it;
    for(it=buffers.begin();it!=buffers.end();++it) delete it->second;
    buffers.clear() ;
  }

  void CContextServer::releaseBuffers()
  {
    //for(auto it=buffers.begin();it!=buffers.end();++it) delete it->second ;
    //buffers.clear() ; 
    freeWindows() ;
  }

  void CContextServer::freeWindows()
  {
    //if (!isAttachedModeEnabled())
    //{
    //  for(auto& it : winComm_)
    //  {
    //    int rank = it.first ;
    //    MPI_Win_free(&windows_[rank][0]);
    //    MPI_Win_free(&windows_[rank][1]);
    //    MPI_Comm_free(&winComm_[rank]) ;
    //  }
    //}
  }

  void CContextServer::notifyClientsFinalize(void)
  {
    for(auto it=buffers.begin();it!=buffers.end();++it)
    {
      it->second->notifyClientFinalize() ;
    }
  }

  void CContextServer::dispatchEvent(CEventServer& event)
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
      info(20)<<" CContextServer: Receive context <"<<context->getId()<<"> finalize."<<endl;
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
    else if (event.classId==CField::GetType()) CField::dispatchEvent(event);
    else if (event.classId==CFieldGroup::GetType()) CFieldGroup::dispatchEvent(event);
    else if (event.classId==CFile::GetType()) CFile::dispatchEvent(event);
    else if (event.classId==CFileGroup::GetType()) CFileGroup::dispatchEvent(event);
    else if (event.classId==CVariable::GetType()) CVariable::dispatchEvent(event);
    else
    {
      ERROR("void CContextServer::dispatchEvent(CEventServer& event)",<<" Bad event class Id"<<endl);
    }
  }

  bool CContextServer::isCollectiveEvent(CEventServer& event)
  {
    if (event.classId==CField::GetType()) return CField::isCollectiveEvent(event);
    else return true ;
  }
}
