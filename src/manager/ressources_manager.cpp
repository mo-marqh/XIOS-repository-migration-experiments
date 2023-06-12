#include "ressources_manager.hpp"
#include "server.hpp"
#include "servers_ressource.hpp"
#include "token_manager.hpp"
#include "timer.hpp"





namespace xios
{
  using namespace std;

  CRessourcesManager::CRessourcesManager(bool isXiosServer) 
  {
   
    xiosComm_ = CXios::getXiosComm()  ;
    
    int commRank ;  
    MPI_Comm_rank(xiosComm_, &commRank) ;
    if (commRank==0 && isXiosServer) MPI_Comm_rank(xiosComm_, &commRank) ; 
    else commRank=0 ;
    tokenManager_ = new CTokenManager(xiosComm_,commRank) ;

    MPI_Allreduce(&commRank, &managerGlobalLeader_, 1, MPI_INT, MPI_SUM, xiosComm_) ;

    MPI_Comm_rank(xiosComm_, &commRank) ;
    winNotify_ = new CWindowManager(xiosComm_, maxBufferSize_) ;
   

    winRessources_ = new CWindowManager(xiosComm_, maxBufferSize_) ;
    winRessources_->lockWindow(commRank,0) ;
    serverLeader_=-1 ;
    winRessources_->updateToWindow(commRank, this, &CRessourcesManager::ressourcesDumpOut) ;
    winRessources_->unlockWindow(commRank,0) ;

    MPI_Barrier(xiosComm_)  ;    
  }
  
  CRessourcesManager::~CRessourcesManager()
  {
    delete winNotify_ ;
    delete winRessources_ ;
  } 

  void CRessourcesManager::createPool(const string& poolId, int size)
  {
    info(40)<<"CRessourcesManager::createPool : calling createPool : "<<poolId<<"  of size"<<size<<endl ;
    info(40)<<"send notification to leader : "<<serverLeader_<<endl ;
    winRessources_->updateFromExclusiveWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
    
    notifyType_=NOTIFY_CREATE_POOL ;
    notifyCreatePool_=make_tuple(poolId, size) ;
    info(40)<<"CRessourcesManager::createPool : send notification creating pool to server leader "<<serverLeader_<<endl ;
    sendNotification(serverLeader_) ; 
  }
  
  void CRessourcesManager::finalize(void)
  {
    winRessources_->updateFromExclusiveWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
   
    if (serverLeader_!=-1)
    {
      notifyType_=NOTIFY_FINALIZE ;
      info(40)<<"CRessourcesManager::finalize : send notification finalize to server leader "<<serverLeader_<<endl ;
      sendNotification(serverLeader_) ;
    } 
  }

  void CRessourcesManager::sendNotification(int rank)
  {
    winNotify_->lockWindowExclusive(rank) ;
    winNotify_->pushToLockedWindow(rank, this, &CRessourcesManager::notificationsDumpOut) ;
    winNotify_->unlockWindowExclusive(rank) ;
  }

  
  void CRessourcesManager::notificationsDumpOut(CBufferOut& buffer)
  {
    
    buffer.realloc(maxBufferSize_) ;
    
    if (notifyType_==NOTIFY_CREATE_POOL)
    {
      auto& arg=notifyCreatePool_ ;
      buffer << notifyType_<< get<0>(arg) << get<1>(arg) ;
    }
    else if (notifyType_==NOTIFY_FINALIZE)
    {
      buffer << notifyType_ ;
    }
  }

  void CRessourcesManager::notificationsDumpIn(CBufferIn& buffer)
  {
    if (buffer.bufferSize() == 0) notifyType_= NOTIFY_NOTHING ;
    else
    {
      buffer>>notifyType_;
      if (notifyType_==NOTIFY_CREATE_POOL)
      {
        auto& arg=notifyCreatePool_ ;
        buffer >> get<0>(arg) >> get<1>(arg)  ;
      }
      else if (notifyType_==NOTIFY_FINALIZE) { /*nothing to do*/ }
    }

  }

  void CRessourcesManager::eventLoop(void)
  {
    CTimer::get("CRessourcesManager::eventLoop").resume();
    int flag ;
    MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
    double time=MPI_Wtime() ;
    if (time-lastEventLoop_ > eventLoopLatency_) 
    {
      checkNotifications() ;
      lastEventLoop_=time ;
    }

    CTimer::get("CRessourcesManager::eventLoop").suspend();
  }
  
  void CRessourcesManager::checkNotifications(void)
  {
    int commRank ;
    MPI_Comm_rank(xiosComm_, &commRank) ;
    CTimer::get("CRessourcesManager::checkNotifications lock").resume();
    winNotify_->lockWindowExclusive(commRank) ;
    CTimer::get("CRessourcesManager::checkNotifications lock").suspend();
    CTimer::get("CRessourcesManager::checkNotifications pop").resume();
    winNotify_->popFromLockedWindow(commRank, this, &CRessourcesManager::notificationsDumpIn) ;
    CTimer::get("CRessourcesManager::checkNotifications pop").suspend();
    CTimer::get("CRessourcesManager::checkNotifications unlock").resume();
    winNotify_->unlockWindowExclusive(commRank) ;
    CTimer::get("CRessourcesManager::checkNotifications unlock").suspend();
    if (notifyType_==NOTIFY_CREATE_POOL) createPool() ;
    else if (notifyType_==NOTIFY_FINALIZE) finalizeSignal() ;
  }

  void CRessourcesManager::createPool(void)
  {
    
    auto& arg=notifyCreatePool_ ;
    string poolId=get<0>(arg) ;
    int size=get<1>(arg) ;
    info(40)<<"CRessourcesManager::createPool : receive create pool notification : "<< poolId<<"  of size "<<size<<endl ;
    CServer::getServersRessource()->createPool(poolId,size) ;
  } 

  void CRessourcesManager::finalizeSignal(void)
  {
    info(40)<<"CRessourcesManager::createPool : receive finalize notification"<<endl ;
    CServer::getServersRessource()->finalize() ;
  }

  void CRessourcesManager::ressourcesDumpOut(CBufferOut& buffer)
  {
    
    buffer.realloc(maxBufferSize_) ;
    
    buffer<<ressourcesSize_<<freeRessourcesSize_<<serverLeader_ ;  
    buffer<<(int) pools_.size();
    for(auto it=pools_.begin();it!=pools_.end(); ++it)
    { 
      auto key = it->first ;
      auto val = it->second ; 
      buffer << key<<std::get<0>(val)  << std::get<1>(val)  << std::get<2>(val);
    }
  }

  void CRessourcesManager::ressourcesDumpIn(CBufferIn& buffer)
  {
    std::string poolId ;
    int size ;
    int freeSize ;
    int leader ;
   
    buffer>>ressourcesSize_>>freeRessourcesSize_>>serverLeader_ ;
    pools_.clear() ;
    int nbPools ;
    buffer>>nbPools ;
    for(int i=0;i<nbPools;i++) 
    {
      buffer>>poolId>>size>>freeSize>>leader ;
      pools_[poolId]=std::make_tuple(size, freeSize, leader) ;
    }
  }
  
  void CRessourcesManager::registerServerLeader(int serverLeaderRank)
  {
    winRessources_->lockWindowExclusive(managerGlobalLeader_) ;
    winRessources_->updateFromLockedWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
    serverLeader_ = serverLeaderRank ;
    winRessources_->updateToLockedWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpOut) ;
    winRessources_->unlockWindowExclusive(managerGlobalLeader_) ;    
  }
  
  void CRessourcesManager::registerRessourcesSize(int size)
  {
    winRessources_->lockWindowExclusive(managerGlobalLeader_) ;
    winRessources_->updateFromLockedWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
    ressourcesSize_ = size ;
    freeRessourcesSize_ = size ;
    winRessources_->updateToLockedWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpOut) ;
    winRessources_->unlockWindowExclusive(managerGlobalLeader_) ;    
  }

 
  void CRessourcesManager::registerPoolClient(const string& poolId, int size, int leader)
  {
    winRessources_->lockWindowExclusive(managerGlobalLeader_) ;
    winRessources_->updateFromLockedWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
    pools_[poolId] = make_tuple(size, size, leader) ;
    winRessources_->updateToLockedWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpOut) ;
    winRessources_->unlockWindowExclusive(managerGlobalLeader_) ;    
  }

  void CRessourcesManager::registerPoolServer(const string& poolId, int size, int leader)
  {
    winRessources_->lockWindowExclusive(managerGlobalLeader_) ;
    winRessources_->updateFromLockedWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
    pools_[poolId] = make_tuple(size, size, leader) ;
    freeRessourcesSize_-=size ;
    winRessources_->updateToLockedWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpOut) ;
    winRessources_->unlockWindowExclusive(managerGlobalLeader_) ;    
  }

  bool CRessourcesManager::getPoolInfo(const string& poolId, int& size, int& freeSize, int& leader)
  {
    winRessources_->updateFromSharedWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;

    auto it=pools_.find(poolId) ;
    if ( it == pools_.end()) return false ;
    else
    {
      size=get<0>(it->second) ;
      freeSize=get<1>(it->second) ;
      leader=get<2>(it->second) ;
      return true ;
    }
  }

  bool CRessourcesManager::decreasePoolFreeSize(const string& poolId, int size)
  {
    bool ret ;

    winRessources_->lockWindowExclusive(managerGlobalLeader_) ;
    winRessources_->updateFromLockedWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
   

    auto it=pools_.find(poolId) ;
    
    if ( it == pools_.end()) ret=false ;
    else 
    {
      get<1>(it->second)-=size ;
      ret=true ;
    }
    winRessources_->updateToLockedWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpOut) ;
    winRessources_->unlockWindowExclusive(managerGlobalLeader_) ; 

    return ret ;   
  }

  int CRessourcesManager::getRessourcesSize(void)
  {
    winRessources_->updateFromSharedWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
    
    return ressourcesSize_ ;
  }

  int CRessourcesManager::getFreeRessourcesSize(void)
  {
    winRessources_->updateFromSharedWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;

    return freeRessourcesSize_ ;
  } 

  bool CRessourcesManager::getPoolLeader(const string& poolId, int& leader)
  {
    int size, freeSize ;
    return getPoolInfo(poolId, size, freeSize, leader) ;
  }

  bool CRessourcesManager::getPoolSize(const string& poolId, int& size)
  {
    int leader,freeSize ;
    return getPoolInfo(poolId, size, freeSize, leader) ;
  }

  bool CRessourcesManager::getPoolFreeSize(const string& poolId, int& freeSize)
  {
    int leader,size ;
    return getPoolInfo(poolId, size, freeSize, leader) ;
  }

  bool CRessourcesManager::hasPool(const string& poolId)
  {
    int leader,size,freeSize ;
    return getPoolInfo(poolId, size, freeSize, leader) ;
  }

  void CRessourcesManager::waitPoolRegistration(const string& poolId)
  {
    while(!hasPool(poolId)) CXios::getDaemonsManager()->servicesEventLoop() ;
  }
}
