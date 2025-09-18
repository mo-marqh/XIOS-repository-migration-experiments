#include "notifications_manager.hpp"
#include "mpi.hpp"
#include "cxios.hpp"
#include <cstring>

namespace xios
{
  using namespace std ;
 
  CNotificationsManager::CNotificationsManager(bool isXiosServer) : voidBuffer_()
  {
    
    xios::MPI_Comm_dup(CXios::getXiosComm(), &communicator_)  ;
    MPI_Comm splitComm ;
    int commRank ;
    MPI_Comm_rank(communicator_,&commRank) ;
    xios::MPI_Comm_split(communicator_, isXiosServer, commRank, &splitComm) ;

    int xiosSize, clientSize ;
    MPI_Comm_size(communicator_,&xiosSize) ;
    MPI_Comm_size(splitComm,&clientSize) ;
    
    if (isXiosServer || clientSize == xiosSize) // Master is first server or first client if no servers
    {
      MPI_Comm_rank(splitComm,&commRank) ;
      if (commRank==0) isMaster_=true ;
      else isMaster_=false ;
    }
    else isMaster_=false ;

    xios::MPI_Comm_free(&splitComm) ;
   
    MPI_Comm_rank(communicator_, &commRank) ;
    if (!isMaster_) commRank=0 ;
    MPI_Allreduce(&commRank, &masterRank_, 1, MPI_INTEGER, MPI_SUM, communicator_ ) ;

    numPrefetchedRequest_ = CXios::getin<int>("number_prefetched_requests", defaultNumPrefectedRequest_);
    
    // launch prefetched asynchronous MPI_Ibcast requests
    if (!isMaster_) for(int i=0;i<numPrefetchedRequest_ ; i++)  postPendingBcastRequest() ;
  }
  
  CNotificationsManager::~CNotificationsManager()
  {
    finalize() ;
  }

  void  CNotificationsManager::checkSentBcastRequests(void)
  {
    MPI_Status status ;
    int flag = true ; 
    
    while(flag && !sentBcastRequests_.empty())
    {
      MPI_Test(&sentBcastRequests_.front().request, &flag, &status) ;
      if (flag) sentBcastRequests_.pop_front() ;
    }
  }

  void CNotificationsManager::checkPendingBcastRequest(void)
  {
    MPI_Status status ;
    int flag = true ; 
    
    while(flag && !pendingBcastRequests_.empty())
    {
      if (isMaster_) flag=true ;
      else MPI_Test(&pendingBcastRequests_.front().request, &flag, &status) ;
      
      if (flag) 
      {
        size_t hash ;
        int rank ;
        CBufferIn hashBuffer(pendingBcastRequests_.front().buffer,requestSize_) ;
        hashBuffer>>hash>>rank ;
        int myRank ;
        MPI_Comm_rank(communicator_, &myRank) ;
        
        if (rank == NO_RANK || myRank==rank)
        { 
          int size=requestSize_ ;
          receivedNotifications_[hash].emplace_back(size) ;
          CBufferIn&  bufferIn = receivedNotifications_[hash].back() ;
          std::memcpy(bufferIn.start(), pendingBcastRequests_.front().buffer, requestSize_) ;
          bufferIn>>hash>>rank ;
        }

        if (hash != hashFinalize_&& !isMaster_) postPendingBcastRequest() ; // post new request for prefetching
       
        pendingBcastRequests_.pop_front() ;
       
      }
    }
  }

  // not for master
  void CNotificationsManager::postPendingBcastRequest(void)
  {
    pendingBcastRequests_.emplace_back() ;
    SEventRequest& req = pendingBcastRequests_.back() ;
    MPI_Ibcast(req.buffer, requestSize_, MPI_BYTE, masterRank_, communicator_, &req.request) ;
  }

  
  void CNotificationsManager::checkSentP2PRequests(void)
  {
    MPI_Status status ;
    int flag=true ;
    
    while(flag && !sentP2PRequests_.empty())
    { 
      MPI_Test(&sentP2PRequests_.front().request, &flag, &status) ; 
      if (flag)  sentP2PRequests_.pop_front() ;
    }
  }

  void CNotificationsManager::checkRecvP2PRequests(void)
  {
    MPI_Status status ;
    int flag=true ; 

    while(flag)
    {
      MPI_Iprobe(MPI_ANY_SOURCE, 0, communicator_, &flag, &status) ;
      if (flag)
      {
        recvP2PRequests_.emplace_back() ;
        SEventRequest& req = recvP2PRequests_.back() ;
        MPI_Irecv(req.buffer, requestSize_, MPI_BYTE, MPI_ANY_SOURCE, 0, communicator_, &req.request) ;
      }
    }

    flag=true ;
    while(flag && !recvP2PRequests_.empty())
    {
      MPI_Test(&recvP2PRequests_.front().request, &flag, &status) ; 
      if (flag)  
      {
        sentBcastRequests_.emplace_back() ;
        SEventRequest& req = sentBcastRequests_.back() ;
        std::memcpy( req.buffer, recvP2PRequests_.front().buffer, requestSize_ );
        MPI_Ibcast(req.buffer, requestSize_, MPI_BYTE, masterRank_, communicator_, &req.request) ;
        
        pendingBcastRequests_.emplace_back() ;
        SEventRequest& reqPending = pendingBcastRequests_.back() ;
        std::memcpy( reqPending.buffer, recvP2PRequests_.front().buffer, requestSize_ );

        recvP2PRequests_.pop_front() ;
      }
    }
  }

  void CNotificationsManager::eventLoop(void)
  {
    if (isMaster_)
    {
      checkRecvP2PRequests() ;
      checkSentBcastRequests() ;
      checkPendingBcastRequest() ; 
    }
    else
    {
      checkSentP2PRequests(); 
      checkPendingBcastRequest() ;
    }
    checkLockNotifications() ;
  }
  
  void CNotificationsManager::notificationsLockOut(CBufferOut& buffer) 
  { 
    buffer << std::get<0>(this->hashToLock_) << std::get<1>(this->hashToLock_) ;
  }

  void CNotificationsManager::notificationsLockIn(CBufferIn& buffer)
  { 
    if (buffer.bufferSize() == 0) return ;
    buffer >> std::get<0>(this->hashToLock_) >>std::get<1>(this->hashToLock_) ;
  }

  void CNotificationsManager::checkLockNotifications(void)
  {
    while (recvNotification(hashLock_,  this, &CNotificationsManager::notificationsLockIn))
    {
      size_t hash = std::get<0>(hashToLock_) ;
      int rank = std::get<1>(hashToLock_) ;
      globalLock_[hash].push(rank) ;
      info(30)<<"received Lock : hash : "<< hash<< "rank " << rank<<endl ;
    }

    while (recvNotification(hashUnlock_,  this, &CNotificationsManager::notificationsLockIn))
    {
      size_t hash = std::get<0>(hashToLock_) ;
      int rank = std::get<1>(hashToLock_) ;
      auto it = globalLock_.find(hash) ;
      if (it == globalLock_.end() || it->second.front()!=rank) 
        ERROR(" void CNotificationsManager::checkLockNotifications(void)",<< "unlock notifications must be done in the same order than lock notifications");
      it->second.pop() ;
      if (it->second.empty()) globalLock_.erase(it) ;
      info(30)<<"received unLock : hash : "<< hash<< "rank " << rank<<endl ;
    }
  }

  void CNotificationsManager::finalize(void)
  {
    if (isMaster_)
    {
      for(int i=0;i<numPrefetchedRequest_;i++)
      {
        sentBcastRequests_.emplace_back() ;
        SEventRequest& req = sentBcastRequests_.back() ;
        *((size_t*) req.buffer) = hashFinalize_;
        MPI_Ibcast(req.buffer, requestSize_, MPI_BYTE, masterRank_, communicator_, &req.request) ;

      }
      while (!sentBcastRequests_.empty()) eventLoop() ; 
    }
    else
    {
      while(!pendingBcastRequests_.empty()) eventLoop() ; 
    }
   
    xios::MPI_Comm_free(&communicator_) ;
 
  }

}
