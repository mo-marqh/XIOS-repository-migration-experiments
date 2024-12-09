#ifndef __NOTIFICATIONS_MANAGER_HPP__
#define __NOTIFICATIONS_MANAGER_HPP__

#include "mpi.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "type.hpp"

#include <list>
#include <map>
#include <queue>
#include <tuple>


namespace xios
{

  class CNotificationsManager
  {
    private:
      static const int NO_RANK=-1 ;
      static const int LOCK=-2 ;
      static const int UNLOCK=-3 ;

    public:
    
      CNotificationsManager(bool isXiosServer) ;
      ~CNotificationsManager() ;

      template< class T >
      bool recvNotification(size_t hash, T* object, void (T::*dumpIn)(CBufferIn&) )
      {
        auto it = receivedNotifications_.find(hash) ;
        if (it != receivedNotifications_.end()) 
        {
          (object->*dumpIn)(it->second.front()) ;
          it->second.pop_front() ;
          if (it->second.empty()) receivedNotifications_.erase(it) ;
          return true ;
        }
        else
        { 
          (object->*dumpIn)(voidBuffer_) ;
          return false ;
        }
      }

      template< class T >
      void sendLockedNotification(size_t hash, T* object, void (T::*dumpOut)(CBufferOut&) )
      {
        while (!this->lock(hash)) this->eventLoop()  ;
        while (!this->isLocked(hash)) this->eventLoop()  ;
        sendNotification(hash, object, dumpOut ) ;
        this->unlock(hash) ;
        while (!this->isUnlocked(hash)) this->eventLoop()  ;
      }
 
      template< class T >
      void sendLockedNotification(int rank, size_t hash, T* object, void (T::*dumpOut)(CBufferOut&) )
      {
        while (!this->lock(hash)) this->eventLoop()  ;
        while (!this->isLocked(hash)) this->eventLoop()  ;
        sendNotification(rank, hash, object, dumpOut ) ;
        this->unlock(hash) ;
        while (!this->isUnlocked(hash)) this->eventLoop()  ;
      }

      template< class T >
      void sendNotification(size_t hash, T* object, void (T::*dumpOut)(CBufferOut&) )
      {
        int rank=NO_RANK ;
        sendNotification(rank, hash, object, dumpOut ) ;
      }
      
      template< class T >
      void sendNotification(int rank, size_t hash, T* object, void (T::*dumpOut)(CBufferOut&) )
      {
        if (isMaster_)
        {
          sentBcastRequests_.emplace_back() ;
          SEventRequest& req = sentBcastRequests_.back() ;
          CBufferOut bufferOut(req.buffer, requestSize_) ;
          bufferOut<<hash<<rank ;
          (object->*dumpOut)(bufferOut) ;
          MPI_Ibcast(req.buffer, requestSize_, MPI_BYTE, masterRank_, communicator_, &req.request) ;
          pendingBcastRequests_.emplace_back() ;
          std::memcpy(pendingBcastRequests_.back().buffer, req.buffer, requestSize_) ;
        }
        else
        {
          sentP2PRequests_.emplace_back() ;
          SEventRequest& req = sentP2PRequests_.back() ;
          CBufferOut bufferOut(req.buffer, requestSize_) ;
          bufferOut<<hash<<rank ;
          (object->*dumpOut)(bufferOut) ;
          MPI_Isend(req.buffer, requestSize_, MPI_BYTE, masterRank_, 0, communicator_, &req.request) ;
        }
      }
      void eventLoop(void) ; 

      void finalize(void) ;

      bool lock(size_t hash)
      {
        if (localLock_.count(hash)!=0) return false ;
        else
        {
          int rank ;
          MPI_Comm_rank(communicator_, &rank) ;
          hashToLock_ = { hash, rank } ;
          info(30)<<" Lock : hash : "<< hash<< "rank " << rank<<endl ;
          sendNotification(hashLock_, this, &CNotificationsManager::notificationsLockOut) ;
          localLock_.insert(hash) ;
          return true ;
        }
      }
      
      bool isLocked(size_t hash)
      {
        int rank ;
        MPI_Comm_rank(communicator_, &rank) ;
        auto it=globalLock_.find(hash) ;
        if (it!=globalLock_.end() && it->second.front()==rank) 
        {
          info(30)<<"Locked : hash : "<< hash<< "rank " << rank<<endl ;
          return true ;
        }
        else return false ;
      }

      void unlock(size_t hash)
      {
        int rank ;
        MPI_Comm_rank(communicator_, &rank) ; 
        hashToLock_ = { hash, rank } ;
        info(30)<<"send unLock : hash : "<< hash<< "rank " << rank<<endl ;
        sendNotification(hashUnlock_, this, &CNotificationsManager::notificationsLockOut) ;
        localLock_.erase(hash) ;
      }
      
      bool isUnlocked(size_t hash)
      {
        int rank ;
        MPI_Comm_rank(communicator_, &rank) ;
        auto it=globalLock_.find(hash) ;
        if (it!=globalLock_.end() && it->second.front()==rank) 
        {
          return false ;
        }
        else 
        {
          info(30)<<"isUnLocked : hash : "<< hash<< "rank " << rank<<endl ;
          localLock_.erase(hash) ;
          return true ;
        }
      }

    private:
      std::tuple<size_t,int> hashToLock_ ;
      void notificationsLockOut(CBufferOut& buffer) ;
      void notificationsLockIn(CBufferIn& buffer) ;
      void checkLockNotifications(void) ;
      std::map<size_t, std::queue<int>> globalLock_ ;
      std::set<size_t> localLock_ ; 

    private:
      
      const static int requestSize_=1024 ;
      const static size_t hashFinalize_ = 12345678900 ;
      const static size_t hashLock_ = 12345678901 ;
      const static size_t hashUnlock_ = 12345678902 ;

      struct SEventRequest
      {
        unsigned char buffer[requestSize_] ;
        MPI_Request request ;
      } ;
      
      void checkSentBcastRequests(void) ;
      void checkPendingBcastRequest(void) ;
      void postPendingBcastRequest(void) ;
      void checkSentP2PRequests(void) ;
      void checkRecvP2PRequests(void) ;


      bool isMaster_ ;
      MPI_Comm communicator_ ;
      int masterRank_ ;

      const int defaultNumPrefectedRequest_ = 100 ;
      int numPrefetchedRequest_ ;
      
      std::list<SEventRequest> pendingBcastRequests_ ;
      std::list<SEventRequest> sentBcastRequests_ ;
      std::map<size_t, std::list<CBufferIn>> receivedNotifications_ ;
      std::list<SEventRequest> sentP2PRequests_ ;
      std::list<SEventRequest> recvP2PRequests_ ;
      CBufferIn voidBuffer_ ;

  };

} 
#endif
