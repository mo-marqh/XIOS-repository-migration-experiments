#include "services.hpp"
#include "services_manager.hpp"
#include "mpi.hpp"
#include "cxios.hpp"
#include "server_context.hpp"
#include "event_scheduler.hpp"
#include "thread_manager.hpp"
#include "timer.hpp"

namespace xios
{
  extern CLogType logTimers ;

  CService::CService(MPI_Comm serviceComm, shared_ptr<CEventScheduler> eventScheduler, const std::string& poolId, const std::string& serviceId, const int& partitionId, 
                     int type, int nbPartitions) 
                         : finalizeSignal_(false), eventScheduler_(nullptr), poolId_(poolId), serviceId_(serviceId),
                           partitionId_(partitionId), type_(type), nbPartitions_(nbPartitions), hasNotification_(false)


  {
    info(40)<<"CService::CService  : new service created ; serviceId : "<<serviceId<<endl ;
   
    int localRank, globalRank, commSize ;

    xios::MPI_Comm_dup(serviceComm, &serviceComm_) ;
    CXios::getMpiGarbageCollector().registerCommunicator(serviceComm_) ;
    MPI_Comm globalComm_=CXios::getXiosComm() ;
  
    MPI_Comm_rank(globalComm_,&globalRank) ;
    MPI_Comm_rank(serviceComm_,&localRank) ;
    
    winNotify_ = new CWindowManager(serviceComm_, maxBufferSize_,"CService::winNotify_") ;
    winNotify_->updateToExclusiveWindow(localRank, this, &CService::createContextDumpOut) ;
    MPI_Barrier(serviceComm_) ;
    if (localRank==localLeader_) 
    {
      globalLeader_=globalRank ;
      MPI_Comm_rank(serviceComm_,&commSize) ;
      CXios::getServicesManager()->registerService(poolId, serviceId, partitionId, type, commSize, nbPartitions, globalLeader_) ;
    }
    if (eventScheduler) eventScheduler_ = eventScheduler ;
    else eventScheduler_ = make_shared<CEventScheduler>(serviceComm_) ;

    ostringstream oss;
    oss<<partitionId;
    name_= poolId+"__"+serviceId+"_"+oss.str();
    
    if (CThreadManager::isUsingThreads()) CThreadManager::spawnThread(&CService::threadEventLoop, this) ;
  }

  CService::~CService()
  {
    delete winNotify_ ;
    for(auto& it : contexts_) delete it.second ;
  }


  void CService::createContext( const std::string& poolId, const std::string& serviceId, const int& partitionId, const std::string& contextId)
  {
    int commSize ;
    MPI_Comm_size(serviceComm_, &commSize) ;
    info(40)<<"CService::createContext  : notify CreateContext to all services members ; serviceId : "<<serviceId<<" ; contextId : "<<contextId<<endl ;

    for(int rank=0; rank<commSize; rank++) 
    {
      notifyOutType_=NOTIFY_CREATE_CONTEXT ;
      notifyOutCreateContext_ = make_tuple(poolId, serviceId, partitionId, contextId) ;
      sendNotification(rank) ;
    }
    info(40)<<"CService::createContext  : notify CreateContext to all services members : DONE "<<endl ;
  }
/*
  void CService::createContext(const std::string& contextId)
  {
    contexts_[contextId] = new CServerContext(this, serviceComm_, poolId_, serviceId_, partitionId_, contextId) ; 
  }
*/
  void CService::createContextNotify(int rank, const std::string& poolId, const std::string& serviceId, const int& partitionId, const std::string& contextId)
  {
    winNotify_->lockWindowExclusive(rank) ;
    winNotify_->updateFromLockedWindow(rank, this, &CService::createContextDumpIn) ;
    notifications_.push_back(std::make_tuple(poolId, serviceId, partitionId, contextId)) ;
    winNotify_->updateToLockedWindow(rank, this, &CService::createContextDumpOut) ;  
    winNotify_->unlockWindowExclusive(rank) ;   
  }


  void CService::createContextDumpOut(CBufferOut& buffer)
  {
    buffer.realloc(maxBufferSize_) ;
   
    buffer << (int) (notifications_.size());
    
    for(auto it=notifications_.begin();it!=notifications_.end(); ++it) 
      buffer << std::get<0>(*it) << std::get<1>(*it) << std::get<2>(*it) << std::get<3>(*it)  ;
  }


  void CService::createContextDumpIn(CBufferIn& buffer)
  {
    std::string poolId ;
    std::string serviceId ;
    int partitionId ;
    std::string contextId ;
    
    notifications_.clear() ;
    int nbNotifications ;
    buffer>>nbNotifications ;
    for(int i=0;i<nbNotifications;i++) 
    {
      buffer>>poolId>>serviceId>>partitionId>>contextId ;
      notifications_.push_back(std::make_tuple(poolId, serviceId, partitionId, contextId)) ;
    }
  }

  bool CService::eventLoop(bool serviceOnly)
  {
    //checkCreateContextNotification() ;
    if (info.isActive(logTimers)) CTimer::get("CService::eventLoop").resume();
    int flag ;
    MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
   
//    double time=MPI_Wtime() ;
//    if (time-lastEventLoop_ > eventLoopLatency_) 
//    {
      checkNotifications() ;
//      lastEventLoop_=time ;
//    }


    eventScheduler_->checkEvent() ;
   
    for(auto it=contexts_.begin();it!=contexts_.end();++it) 
    {
      if (it->second->eventLoop(serviceOnly))
      {
        delete it->second ; 
        contexts_.erase(it) ;
        // destroy server_context -> to do later
        break ;
      } ;
    }
  
    if (info.isActive(logTimers)) CTimer::get("CService::eventLoop").suspend();
    if (contexts_.empty() && finalizeSignal_) return true ;
    else return false ;
  }

  void CService::threadEventLoop(void)
  {
    if (info.isActive(logTimers)) CTimer::get("CService::eventLoop").resume();
    info(100)<<"Launch Thread for  CService::threadEventLoop, service id = "<<name_<<endl ;
    CThreadManager::threadInitialize() ; 
    
    do
    {
      int flag ;
      MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
   
//    double time=MPI_Wtime() ;
//    if (time-lastEventLoop_ > eventLoopLatency_) 
//    {
      checkNotifications() ;
//      lastEventLoop_=time ;
//    }


      eventScheduler_->checkEvent() ;
   
      for(auto it=contexts_.begin();it!=contexts_.end();++it) 
      {
        if (it->second->isFinished())
        {
          delete it->second ; 
          contexts_.erase(it) ;
          // destroy server_context -> to do later
          break ;
        } ;
      }

      if (contexts_.empty() && finalizeSignal_) finished_=true ;
      if (!finished_) CThreadManager::yield() ;
    } while (!finished_) ;
    
    CThreadManager::threadFinalize() ;
    info(100)<<"Close thread for  CService::threadEventLoop, service id = "<<name_<<endl ;
    if (info.isActive(logTimers)) CTimer::get("CService::eventLoop").suspend();
  }


  void CService::sendNotification(int rank)
  {
    winNotify_->pushToExclusiveWindow(rank, this, &CService::notificationsDumpOut) ;
  }

  
  void CService::notificationsDumpOut(CBufferOut& buffer)
  {
    
    buffer.realloc(maxBufferSize_) ;
    
    if (notifyOutType_==NOTIFY_CREATE_CONTEXT)
    {
      auto& arg=notifyOutCreateContext_ ;
      buffer << notifyOutType_ << std::get<0>(arg)<<std::get<1>(arg) << std::get<2>(arg)<<std::get<3>(arg) ;
    }
  }

  void CService::notificationsDumpIn(CBufferIn& buffer)
  {
    if (buffer.bufferSize() == 0) notifyInType_= NOTIFY_NOTHING ;
    else
    {
      buffer>>notifyInType_;
      if (notifyInType_==NOTIFY_CREATE_CONTEXT)
      {
        auto& arg=notifyInCreateContext_ ;
        buffer >> std::get<0>(arg)>> std::get<1>(arg) >> std::get<2>(arg)>> std::get<3>(arg);
      }
    }
  }




  void CService::checkNotifications(void)
  {
    if (!hasNotification_)
    {
      double time=MPI_Wtime() ;
      if (time-lastEventLoop_ > eventLoopLatency_) 
      {
        int commRank ;
        MPI_Comm_rank(serviceComm_, &commRank) ;
        winNotify_->popFromExclusiveWindow(commRank, this, &CService::notificationsDumpIn) ;
        
        if (notifyInType_!= NOTIFY_NOTHING)
        {
          hasNotification_=true ;
          std::hash<string> hashString ;
          size_t hashId = hashString(name_) ;
          size_t currentTimeLine=0 ;
          info(40)<<"CService::checkNotifications(void) : receive notification => event scheduler : timeLine : "<<currentTimeLine<<"  hashId : "<<hashId<<endl ;
          eventScheduler_->registerEvent(currentTimeLine,hashId); 
        }
        lastEventLoop_=time ;
      }
    }
    
    if (hasNotification_)
    {
      std::hash<string> hashString ;
      size_t hashId = hashString(name_) ;
      size_t currentTimeLine=0 ;
//      info(40)<<"CService::checkNotifications(void) : receive notification => event scheduler : eventIsReceived ?"<<endl ;
      if (eventScheduler_->queryEvent(currentTimeLine,hashId))
      {
        eventScheduler_->popEvent() ;
        info(40)<<"CService::checkNotifications(void) : receive notification => event scheduler : RECEIVED"<<endl ;
        if (notifyInType_==NOTIFY_CREATE_CONTEXT) createContext() ;
        hasNotification_=false ;
      }
    }
  }



//ym not use any more
  void CService::checkCreateContextNotification(void)
  {
    int commRank ;
    MPI_Comm_rank(serviceComm_, &commRank) ;
    winNotify_->lockWindowExclusive(commRank) ;
    winNotify_->updateFromLockedWindow(commRank, this, &CService::createContextDumpIn) ;
    
    if (!notifications_.empty())
    {
      auto info = notifications_.front() ;
      createNewContext(get<0>(info), get<1>(info), get<2>(info), get<3>(info)) ;
      notifications_.pop_front() ;
      winNotify_->updateToLockedWindow(commRank, this, &CService::createContextDumpOut) ;     
    }
    winNotify_->unlockWindowExclusive(commRank) ;
  }

  void CService::createContext(void)
   {
     info(40)<<"CService::createContext(void)  : receive createContext notification"<<endl ;
     auto& arg=notifyInCreateContext_ ;
     string poolId = get<0>(arg) ;
     string serviceId = get<1>(arg) ;
     int partitionId = get<2>(arg) ;
     string contextId = get<3>(arg) ;
     contexts_[contextId] = new CServerContext(this, serviceComm_, poolId, serviceId, partitionId, contextId) ;
   }

   //to remove, not used anymore
   void CService::createNewContext(const std::string& poolId, const std::string& serviceId, const int& partitionId, const std::string& contextId)
   {
     contexts_[contextId] = new CServerContext(this, serviceComm_, poolId, serviceId, partitionId, contextId) ; 
   }

  void CService::finalizeSignal(void)
  {
    finalizeSignal_=true ;
    for(auto it=contexts_.begin();it!=contexts_.end();++it) it->second->finalizeSignal() ;
  }

  shared_ptr<CEventScheduler> CService::getEventScheduler(void)
  {
    return eventScheduler_ ;
  }
}
