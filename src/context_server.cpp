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
    
    if (flag) MPI_Comm_remote_size(interComm,&commSize);
    else  MPI_Comm_size(interComm,&commSize);

   
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


    if (!isAttachedModeEnabled())
    {
      MPI_Intercomm_merge(interComm_,true,&interCommMerged) ;
// create windows for one sided comm
      int interCommMergedRank;
      MPI_Comm winComm ;
      MPI_Comm_rank(intraComm, &interCommMergedRank);
      windows.resize(2) ;
      for(int rank=commSize; rank<commSize+intraCommSize; rank++)
      {
        if (rank==commSize+interCommMergedRank) 
        {
          MPI_Comm_split(interCommMerged, interCommMergedRank, rank, &winComm);
          int myRank ;
          MPI_Comm_rank(winComm,&myRank);
          MPI_Win_create_dynamic(MPI_INFO_NULL, winComm, &windows[0]);
          MPI_Win_create_dynamic(MPI_INFO_NULL, winComm, &windows[1]);      
        }
        else MPI_Comm_split(interCommMerged, interCommMergedRank, rank, &winComm);
//       ym : Warning : intelMPI doesn't support that communicator of windows be deallocated before the windows deallocation, crash at MPI_Win_lock
//            Bug or not ?          
        // MPI_Comm_free(&winComm) ;
      }
    }
    else 
    {
      windows.resize(2) ;
      windows[0]=MPI_WIN_NULL ;
      windows[1]=MPI_WIN_NULL ;
    }


    
    MPI_Comm_split(intraComm_,intraCommRank,intraCommRank, &commSelf) ;
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
    listen();
    checkPendingRequest();
    if (enableEventsProcessing)  processEvents();
    return finished;
  }

  void CContextServer::listen(void)
  {
    int rank;
    int flag;
    int count;
    char * addr;
    MPI_Status status;
    map<int,CServerBuffer*>::iterator it;
    bool okLoop;

    traceOff();
    // WARNING : with intel MPI, probing crash on an intercommunicator with release library but not with release_mt
    // ==>  source $I_MPI_ROOT/intel64/bin/mpivars.sh release_mt    needed
    MPI_Iprobe(MPI_ANY_SOURCE, 20,interComm,&flag,&status);
    traceOn();

    if (flag==true)
    {
      rank=status.MPI_SOURCE ;
      okLoop = true;
      if (pendingRequest.find(rank)==pendingRequest.end())
        okLoop = !listenPendingRequest(status) ;
      if (okLoop)
      {
        for(rank=0;rank<commSize;rank++)
        {
          if (pendingRequest.find(rank)==pendingRequest.end())
          {

            traceOff();
            MPI_Iprobe(rank, 20,interComm,&flag,&status);
            traceOn();
            if (flag==true) listenPendingRequest(status) ;
          }
        }
      }
    }
  }

  bool CContextServer::listenPendingRequest(MPI_Status& status)
  {
    int count;
    char * addr;
    map<int,CServerBuffer*>::iterator it;
    int rank=status.MPI_SOURCE ;

    it=buffers.find(rank);
    if (it==buffers.end()) // Receive the buffer size and allocate the buffer
    {
       MPI_Aint recvBuff[4] ;
       MPI_Recv(recvBuff, 4, MPI_AINT, rank, 20, interComm, &status);
       remoteHashId_ = recvBuff[0] ;
       StdSize buffSize = recvBuff[1];
       vector<MPI_Aint> winAdress(2) ;
       winAdress[0]=recvBuff[2] ; winAdress[1]=recvBuff[3] ;
       mapBufferSize_.insert(std::make_pair(rank, buffSize));
       it=(buffers.insert(pair<int,CServerBuffer*>(rank,new CServerBuffer(windows, winAdress, rank, buffSize)))).first;
     
       lastTimeLine[rank]=0 ;
       itLastTimeLine=lastTimeLine.begin() ;

       return true;
    }
    else
    {
      MPI_Get_count(&status,MPI_CHAR,&count);
      if (it->second->isBufferFree(count))
      {
         addr=(char*)it->second->getBuffer(count);
         MPI_Irecv(addr,count,MPI_CHAR,rank,20,interComm,&pendingRequest[rank]);
         bufferRequest[rank]=addr;
         return true;
       }
      else
        return false;
    }
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
    if (!isAttachedModeEnabled()) // one sided desactivated in attached mode
    {  
      int rank ;
      char *buffer ;
      size_t count ; 

      if (itLastTimeLine==lastTimeLine.end()) itLastTimeLine=lastTimeLine.begin() ;
      for(;itLastTimeLine!=lastTimeLine.end();++itLastTimeLine)
      {
        rank=itLastTimeLine->first ;
        if (itLastTimeLine->second < timeLine &&  pendingRequest.count(rank)==0)
        {
          if (buffers[rank]->getBufferFromClient(timeLine, buffer, count))
          {
            processRequest(rank, buffer, count);
            break ;
          }
        }
      }
    }
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
      } 
      else if (timeLine==timelineEventChangeBufferSize)
      {
        size_t newSize ;
        vector<MPI_Aint> winAdress(2) ;
        newBuffer>>newSize>>winAdress[0]>>winAdress[1] ;
        buffers.erase(rank) ;
        buffers.insert(pair<int,CServerBuffer*>(rank,new CServerBuffer(windows, winAdress, rank, newSize)));
      }
      else
      {
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
         info(100)<<"Received Event "<<currentTimeLine<<" of class "<<event->classId<<" of type "<<event->type<<endl ;
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
      else getBufferFromClient(currentTimeLine) ;
    }
    else if (pureOneSided) getBufferFromClient(currentTimeLine) ; // if pure one sided check buffer even if no event recorded at current time line
  }

  CContextServer::~CContextServer()
  {
    map<int,CServerBuffer*>::iterator it;
    for(it=buffers.begin();it!=buffers.end();++it) delete it->second;
  }

  void CContextServer::releaseBuffers()
  {
    map<int,CServerBuffer*>::iterator it;
    bool out ;
    do
    {
      out=true ;
      for(it=buffers.begin();it!=buffers.end();++it)
      {
//        out = out && it->second->freeWindows() ;

      }
    } while (! out) ; 
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
//      releaseBuffers() ;
      notifyClientsFinalize() ;
      context->finalize();

/* don't know where release windows
      MPI_Win_free(&windows[0]) ;
      MPI_Win_free(&windows[1]) ;
*/     
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
