#include "ressources_manager.hpp"
#include "server.hpp"
#include "servers_ressource.hpp"





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
    winRessources_->lockWindow(managerGlobalLeader_,0) ;
    winRessources_->updateFromWindow(managerGlobalLeader_, this, &CRessourcesManager::ressourcesDumpIn) ;
    winRessources_->unlockWindow(managerGlobalLeader_,0) ;    
   
    notifyType_=NOTIFY_CREATE_POOL ;
    notifyCreatePool_=make_tuple(poolId, size) ;
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
      sendNotification(serverLeader_) ;
    } 
  }

  void CRessourcesManager::sendNotification(int rank)
  {
    winNotify_->lockWindow(rank,0) ;
    winNotify_->pushToWindow(rank, this, &CRessourcesManager::notificationsDumpOut) ;
    winNotify_->unlockWindow(rank,0) ;
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
    checkNotifications() ;
  }
  
  void CRessourcesManager::checkNotifications(void)
  {
    int commRank ;
    MPI_Comm_rank(xiosComm_, &commRank) ;
    winNotify_->lockWindow(commRank,0) ;
    winNotify_->popFromWindow(commRank, this, &CRessourcesManager::notificationsDumpIn) ;
    winNotify_->unlockWindow(commRank,0) ;
    if (notifyType_==NOTIFY_CREATE_POOL) createPool() ;
    else if (notifyType_==NOTIFY_FINALIZE) finalizeSignal() ;
  }

  void CRessourcesManager::createPool(void)
  {
    auto& arg=notifyCreatePool_ ;
    string poolId=get<0>(arg) ;
    int size=get<1>(arg) ;
    CServer::getServersRessource()->createPool(poolId,size) ;
  } 

  void CRessourcesManager::finalizeSignal(void)
  {
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