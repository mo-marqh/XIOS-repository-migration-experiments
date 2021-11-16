#include "services.hpp"
#include "services_manager.hpp"
#include "mpi.hpp"
#include "cxios.hpp"
#include "server_context.hpp"
#include "event_scheduler.hpp"
#include "timer.hpp"

namespace xios
{
  CService::CService(MPI_Comm serviceComm, const std::string& poolId, const std::string& serviceId, const int& partitionId, 
                     int type, int nbPartitions) : finalizeSignal_(false), eventScheduler_(nullptr), poolId_(poolId), serviceId_(serviceId),
                                                   partitionId_(partitionId), type_(type), nbPartitions_(nbPartitions), hasNotification_(false)


  {
    info(40)<<"CService::CService  : new service created ; serviceId : "<<serviceId<<endl ;
   
    int localRank, globalRank, commSize ;

    MPI_Comm_dup(serviceComm, &serviceComm_) ;
    MPI_Comm globalComm_=CXios::getXiosComm() ;
  
    MPI_Comm_rank(globalComm_,&globalRank) ;
    MPI_Comm_rank(serviceComm_,&localRank) ;
    
    winNotify_ = new CWindowManager(serviceComm_, maxBufferSize_) ;
    winNotify_->lockWindow(localRank,0) ;
    winNotify_->updateToWindow(localRank, this, &CService::createContextDumpOut) ;
    winNotify_->unlockWindow(localRank,0) ;
    MPI_Barrier(serviceComm_) ;
    if (localRank==localLeader_) 
    {
      globalLeader_=globalRank ;
      MPI_Comm_rank(serviceComm_,&commSize) ;
      CXios::getServicesManager()->registerService(poolId, serviceId, partitionId, type, commSize, nbPartitions, globalLeader_) ;
    }
    eventScheduler_ = new CEventScheduler(serviceComm_) ;

    ostringstream oss;
    oss<<partitionId;
    name_= poolId+"::"+serviceId+"_"+oss.str();
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
    winNotify_->lockWindow(rank,0) ;
    winNotify_->updateFromWindow(rank, this, &CService::createContextDumpIn) ;
    notifications_.push_back(std::make_tuple(poolId, serviceId, partitionId, contextId)) ;
    winNotify_->updateToWindow(rank, this, &CService::createContextDumpOut) ;  
    winNotify_->unlockWindow(rank,0) ;   
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
    CTimer::get("CService::eventLoop").resume();
    
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
        contexts_.erase(it) ;
        // destroy server_context -> to do later
        break ;
      } ;
    }
    CTimer::get("CService::eventLoop").suspend();
    if (contexts_.empty() && finalizeSignal_) return true ;
    else return false ;
  }

  void CService::sendNotification(int rank)
  {
    winNotify_->lockWindowExclusive(rank) ;
    winNotify_->pushToLockedWindow(rank, this, &CService::notificationsDumpOut) ;
    winNotify_->unlockWindow(rank) ;
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
        winNotify_->lockWindowExclusive(commRank) ;
        winNotify_->popFromLockedWindow(commRank, this, &CService::notificationsDumpIn) ;
        winNotify_->unlockWindow(commRank) ;
      
        if (notifyInType_!= NOTIFY_NOTHING)
        {
          hasNotification_=true ;
          std::hash<string> hashString ;
          size_t hashId = hashString(name_) ;
          size_t currentTimeLine=0 ;
          info(40)<<"CService::checkNotifications(void) : receive notification => event scheduler"<<endl ;
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
      info(40)<<"CService::checkNotifications(void) : receive notification => event scheduler : eventIsReceived ?"<<endl ;
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
    winNotify_->lockWindow(commRank,0) ;
    winNotify_->updateFromWindow(commRank, this, &CService::createContextDumpIn) ;
    
    if (!notifications_.empty())
    {
      auto info = notifications_.front() ;
      createNewContext(get<0>(info), get<1>(info), get<2>(info), get<3>(info)) ;
      notifications_.pop_front() ;
      winNotify_->updateToWindow(commRank, this, &CService::createContextDumpOut) ;     
    }
    winNotify_->unlockWindow(commRank,0) ;
  }

  void CService::createContext(void)
   {
     info(40)<<"CService::createContext(void)  : receive createContext notification"<<endl ;
     auto& arg=notifyInCreateContext_ ;
     string poolId = get<0>(arg) ;
     string& serviceId = get<1>(arg) ;
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

  CEventScheduler* CService::getEventScheduler(void)
  {
    return eventScheduler_ ;
  }
}