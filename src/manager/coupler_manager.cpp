#include "coupler_manager.hpp"
#include "cxios.hpp"
#include <functional>
#include <string>



namespace xios
{
  CCouplerManager::CCouplerManager(bool isXiosServer)
  {
    auto xiosComm_ = CXios::getXiosComm()  ;
    useWindowManager_ = CXios::servicesUseWindowManager ;

    int commRank ;  
    MPI_Comm_rank(xiosComm_, &commRank) ;
    if (commRank==0 && isXiosServer) MPI_Comm_rank(xiosComm_, &commRank) ; 
    else commRank=0 ;
    MPI_Allreduce(&commRank, &managerGlobalLeader_, 1, MPI_INT, MPI_SUM, xiosComm_) ;
    MPI_Comm_rank(xiosComm_, &commRank) ;
    
    if (useWindowManager_)
    {
      winRegistredCoupling_ = new CWindowManager(xiosComm_, maxBufferSize_,"CCouplerManager::winRegistredCoupling_") ;
      winNextCoupling_ = new CWindowManager(xiosComm_, maxBufferSize_,"CCouplerManager::winNextCoupling_") ;
      if (commRank==managerGlobalLeader_)
      {
        winRegistredCoupling_->updateToExclusiveWindow(managerGlobalLeader_, this, &CCouplerManager::registredCouplingDumpOut) ;
        winNextCoupling_->updateToExclusiveWindow(managerGlobalLeader_, this, &CCouplerManager::nextCouplingDumpOut) ;
      }
    }

    hashRegistredCoupling_ = hashString("CCouplerManager::registredCoupling_") ;
    hashNextCoupling_   = hashString("CCouplerManager::nextCoupling_") ;

    MPI_Barrier(xiosComm_)  ;    
  }

  CCouplerManager::~CCouplerManager()
  {
    if (useWindowManager_)
    {
      delete winRegistredCoupling_ ;
      delete winNextCoupling_ ;
    }
  }
  
  
  void CCouplerManager::registerCoupling(string srcCoupling, string dstCoupling, bool master)
  {
    hash<string> strHash ;
    size_t key = strHash(getStrCoupling(srcCoupling,dstCoupling)) ;
    
    if (useWindowManager_)
    {
      winRegistredCoupling_->lockWindowExclusive(managerGlobalLeader_) ;
      winRegistredCoupling_->updateFromLockedWindow(managerGlobalLeader_, this, &CCouplerManager::registredCouplingDumpIn) ;
      if (registredCoupling_.count(key)==1)
      {
        registredCoupling_.erase(key) ;
        winRegistredCoupling_->updateToLockedWindow(managerGlobalLeader_, this, &CCouplerManager::registredCouplingDumpOut) ;
        winNextCoupling_->lockWindowExclusive(managerGlobalLeader_) ;
        winNextCoupling_->updateFromLockedWindow(managerGlobalLeader_, this, &CCouplerManager::nextCouplingDumpIn) ;
        nextCoupling_.push_back(pair<size_t,int>(key,2)) ;
        winNextCoupling_->updateToLockedWindow(managerGlobalLeader_, this, &CCouplerManager::nextCouplingDumpOut) ;
        winNextCoupling_->unlockWindowExclusive(managerGlobalLeader_) ;
      }
      else 
      {
        registredCoupling_.insert(key) ;
        winRegistredCoupling_->updateToLockedWindow(managerGlobalLeader_, this, &CCouplerManager::registredCouplingDumpOut) ;
      }
      winRegistredCoupling_->unlockWindowExclusive(managerGlobalLeader_) ;
    }
    else
    {
      hashCurrentRegistredCoupling_ = key ;
      if (master) masterRegistred_.insert(key) ;
      CXios::getNotificationsManager()->sendNotification(hashRegistredCoupling_, this, &CCouplerManager::registredCouplingDumpOut) ;
    }

  }

  void CCouplerManager::eventLoop(void)
  { 
    if (!useWindowManager_)
    {
      while (CXios::getNotificationsManager()->recvNotification(hashRegistredCoupling_,   this, &CCouplerManager::registredCouplingDumpIn)) ;
      while (CXios::getNotificationsManager()->recvNotification(hashNextCoupling_,   this,  &CCouplerManager::nextCouplingDumpIn)) ;
    }
  }
  

  bool CCouplerManager::isNextCoupling(string srcCoupling, string dstCoupling)
  {
    
    bool ret ;
    hash<string> strHash ;
    size_t key = strHash(getStrCoupling(srcCoupling,dstCoupling)) ;
    
    if (useWindowManager_)
    {
      winNextCoupling_->lockWindowExclusive(managerGlobalLeader_) ;
      winNextCoupling_->updateFromLockedWindow(managerGlobalLeader_, this, &CCouplerManager::nextCouplingDumpIn) ;
      if (nextCoupling_.front().first==key)
     {
        ret=true ;
        if (nextCoupling_.front().second==1) nextCoupling_.pop_front() ;
        else nextCoupling_.front().second=1 ;
        winNextCoupling_->updateToLockedWindow(managerGlobalLeader_, this, &CCouplerManager::nextCouplingDumpOut) ;
      }
      else ret=false ;
      winNextCoupling_->unlockWindowExclusive(managerGlobalLeader_) ;
      return ret ;
    }
    else
    {
      if (nextCoupling_.front().first==key)
      {
        hashCurrentNextCoupling_ = key ;
        CXios::getNotificationsManager()->sendNotification(hashNextCoupling_, this, &CCouplerManager::nextCouplingDumpOut) ;
        return true ;
      }
      else return false ;
    }
  }
  
  void CCouplerManager::registredCouplingDumpOut(CBufferOut& buffer)
  {
    if (useWindowManager_) 
    {
      buffer.realloc(maxBufferSize_) ;
      buffer<<(int)registredCoupling_.size();
      for(auto hash : registredCoupling_) buffer << hash ;
    }
    else
    {
      buffer << hashCurrentRegistredCoupling_; ;
    }
  } 

  void CCouplerManager::registredCouplingDumpIn(CBufferIn& buffer)
  {
    if (useWindowManager_) 
    {
      registredCoupling_.clear() ;
      size_t hash ;
      int nbHash ;
      buffer>>nbHash ;
      for(int i=0;i<nbHash;i++) 
      {
        buffer>>hash ;
        registredCoupling_.insert(hash) ;
      }
    }
    else
    {
      if (buffer.bufferSize() == 0) return ;

      size_t hash ;
      buffer>>hash ;
      if (registredCoupling_.count(hash)!=0) 
      { 
        registredCoupling_.erase(hash) ;
        if (masterRegistred_.count(hash)!=0)
        {
          hashCurrentNextCoupling_ = hash ;
          CXios::getNotificationsManager()->sendLockedNotification(hashNextCoupling_, this, &CCouplerManager::nextCouplingDumpOut) ;
          masterRegistred_.erase(hash) ;
        } 
      } 
      else registredCoupling_.insert(hash) ;
    }
  }

  void CCouplerManager::nextCouplingDumpOut(CBufferOut& buffer)
  {
    if (useWindowManager_)
    {
      buffer.realloc(maxBufferSize_) ;
      buffer<<(int)nextCoupling_.size();
    
      for(auto hash : nextCoupling_) buffer << hash.first<<hash.second ;
    }
    else buffer << hashCurrentNextCoupling_ ;
  } 

  void CCouplerManager::nextCouplingDumpIn(CBufferIn& buffer)
  {
    if (useWindowManager_)
    {
      nextCoupling_.clear() ;
      size_t hash ;
      int count ;
      int nbHash ;
      buffer>>nbHash ;
      for(int i=0;i<nbHash;i++) 
      {
        buffer>>hash>>count ;
        nextCoupling_.push_back(pair<size_t,int>(hash,count)) ;
      }
    }
    else
    {
      if (buffer.bufferSize() == 0) return ;
      size_t hash ;
      buffer>>hash ;
      if (nextCoupling_.front().first==hash) 
      {
        if (nextCoupling_.front().second==2) nextCoupling_.pop_front() ;
        else nextCoupling_.front().second=2   ;
      }
      else nextCoupling_.push_back(pair<size_t,int>(hash,1)) ;
    }
  }
}