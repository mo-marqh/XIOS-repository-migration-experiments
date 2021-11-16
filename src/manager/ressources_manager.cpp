#include "ressources_manager.hpp"
#include "server.hpp"
#include "servers_ressource.hpp"
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
    winRessources_->lockWindow(managerGlobalLeader_,0) ;
    winRessources_->updateFromWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
    winRessources_->unlockWindow(managerGlobalLeader_,0) ;    
   
    notifyType_=NOTIFY_CREATE_POOL ;
    notifyCreatePool_=make_tuple(poolId, size) ;
    info(40)<<"CRessourcesManager::createPool : send notification creating pool to server leader "<<serverLeader_<<endl ;
    sendNotification(serverLeader_) ; 
  }
  
  void CRessourcesManager::finalize(void)
  {
    winRessources_->lockWindow(managerGlobalLeader_,0) ;
    winRessources_->updateFromWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
    winRessources_->unlockWindow(managerGlobalLeader_,0) ;    
   
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
    winNotify_->unlockWindow(rank) ;
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
    winNotify_->unlockWindow(commRank) ;
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
    
    buffer<<serverLeader_ ;  
    buffer<<(int) pools_.size();
    for(auto it=pools_.begin();it!=pools_.end(); ++it)
    { 
      auto key = it->first ;
      auto val = it->second ; 
      buffer << key<<std::get<0>(val) << std::get<1>(val)  ;
    }
  }

  void CRessourcesManager::ressourcesDumpIn(CBufferIn& buffer)
  {
    std::string poolId ;
    int size ;
    int leader ;
   
    buffer>>serverLeader_ ;
    pools_.clear() ;
    int nbPools ;
    buffer>>nbPools ;
    for(int i=0;i<nbPools;i++) 
    {
      buffer>>poolId>>size>>leader ;
      pools_[poolId]=std::make_tuple(size,leader) ;
    }
  }
  
  void CRessourcesManager::registerServerLeader(int serverLeaderRank)
  {
    winRessources_->lockWindow(managerGlobalLeader_,0) ;
    winRessources_->updateFromWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
    serverLeader_ = serverLeaderRank ;
    winRessources_->updateToWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpOut) ;
    winRessources_->unlockWindow(managerGlobalLeader_,0) ;    
  }
  
  void CRessourcesManager::registerRessourcesSize(int size)
  {
    winRessources_->lockWindow(managerGlobalLeader_,0) ;
    winRessources_->updateFromWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
    ressourcesSize_ = size ;
    freeRessourcesSize_ = size ;
    winRessources_->updateToWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpOut) ;
    winRessources_->unlockWindow(managerGlobalLeader_,0) ;    
  }

 
  void CRessourcesManager::registerPool(const string& poolId, int size, int leader)
  {
    winRessources_->lockWindow(managerGlobalLeader_,0) ;
    winRessources_->updateFromWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
    pools_[poolId] = make_tuple(size,leader) ;
    freeRessourcesSize_-=size ;
    winRessources_->updateToWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpOut) ;
    winRessources_->unlockWindow(managerGlobalLeader_,0) ;    
  }


  bool CRessourcesManager::getPoolInfo(const string& poolId, int& size, int& leader)
  {
    winRessources_->lockWindow(managerGlobalLeader_,0) ;
    winRessources_->updateFromWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
    winRessources_->unlockWindow(managerGlobalLeader_,0) ;

    auto it=pools_.find(poolId) ;
    if ( it == pools_.end()) return false ;
    else
    {
      size=get<0>(it->second) ;
      leader=get<1>(it->second) ;
      return true ;
    }
  }

  int CRessourcesManager::getRessourcesSize(void)
  {
    winRessources_->lockWindow(managerGlobalLeader_,0) ;
    winRessources_->updateFromWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
    winRessources_->unlockWindow(managerGlobalLeader_,0) ;

    return ressourcesSize_ ;
  }

  int CRessourcesManager::getFreeRessourcesSize(void)
  {
    winRessources_->lockWindow(managerGlobalLeader_,0) ;
    winRessources_->updateFromWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
    winRessources_->unlockWindow(managerGlobalLeader_,0) ;

    return freeRessourcesSize_ ;
  } 

  bool CRessourcesManager::getPoolLeader(const string& poolId, int& leader)
  {
    int size ;
    return getPoolInfo(poolId, size, leader) ;
  }

  bool CRessourcesManager::getPoolSize(const string& poolId, int& size)
  {
    int leader ;
    return getPoolInfo(poolId, size, leader) ;
  }

  bool CRessourcesManager::hasPool(const string& poolId)
  {
    int leader,size ;
    return getPoolInfo(poolId, size, leader) ;
  }
}