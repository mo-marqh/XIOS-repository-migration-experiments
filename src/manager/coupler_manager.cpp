#include "coupler_manager.hpp"
#include "cxios.hpp"
#include <functional>
#include <string>



namespace xios
{
  CCouplerManager::CCouplerManager(bool isXiosServer)
  {
    auto xiosComm_ = CXios::getXiosComm()  ;
    
    int commRank ;  
    MPI_Comm_rank(xiosComm_, &commRank) ;
    if (commRank==0 && isXiosServer) MPI_Comm_rank(xiosComm_, &commRank) ; 
    else commRank=0 ;
    MPI_Allreduce(&commRank, &managerGlobalLeader_, 1, MPI_INT, MPI_SUM, xiosComm_) ;
    MPI_Comm_rank(xiosComm_, &commRank) ;
    
    winRegistredCoupling_ = new CWindowManager(xiosComm_, maxBufferSize_,"CCouplerManager::winRegistredCoupling_") ;
    winNextCoupling_ = new CWindowManager(xiosComm_, maxBufferSize_,"CCouplerManager::winNextCoupling_") ;
    if (commRank==managerGlobalLeader_)
    {
      winRegistredCoupling_->updateToExclusiveWindow(managerGlobalLeader_, this, &CCouplerManager::registredCouplingDumpOut) ;
      winNextCoupling_->updateToExclusiveWindow(managerGlobalLeader_, this, &CCouplerManager::nextCouplingDumpOut) ;
    }

    MPI_Barrier(xiosComm_)  ;    
  }

  CCouplerManager::~CCouplerManager()
  {
    delete winRegistredCoupling_ ;
    delete winNextCoupling_ ;
  }
  
  
  void CCouplerManager::registerCoupling(string srcCoupling, string dstCoupling)
  {
    hash<string> strHash ;
    size_t key = strHash(getStrCoupling(srcCoupling,dstCoupling)) ;
    
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

  bool CCouplerManager::isNextCoupling(string srcCoupling, string dstCoupling)
  {
    bool ret ;
    hash<string> strHash ;
    size_t key = strHash(getStrCoupling(srcCoupling,dstCoupling)) ;

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
  void CCouplerManager::registredCouplingDumpOut(CBufferOut& buffer)
  {
    buffer.realloc(maxBufferSize_) ;
    buffer<<(int)registredCoupling_.size();
    
    for(auto hash : registredCoupling_) buffer << hash ;
  } 

  void CCouplerManager::registredCouplingDumpIn(CBufferIn& buffer)
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

  void CCouplerManager::nextCouplingDumpOut(CBufferOut& buffer)
  {
    buffer.realloc(maxBufferSize_) ;
    buffer<<(int)nextCoupling_.size();
    
    for(auto hash : nextCoupling_) buffer << hash.first<<hash.second ;
  } 

  void CCouplerManager::nextCouplingDumpIn(CBufferIn& buffer)
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

}