#ifndef __SERVERS_RESSOURCE_HPP__
#define __SERVERS_RESSOURCE_HPP__

#include "window_manager.hpp"
#include "event_scheduler.hpp"
#include "mpi.hpp"
#include <vector>
#include <string>



namespace xios
{

  class CPoolRessource ;

  class CServersRessource
  {

    const int NOTIFY_NOTHING=0 ;
    const int NOTIFY_CREATE_POOL=1 ;
    const int NOTIFY_FINALIZE=2 ;

    public:
    
    CServersRessource(MPI_Comm serverComm) ;
    ~CServersRessource() ;
    void createPool(const string& poolId, const int size) ;
    void createPool(void) ;
    bool eventLoop(bool serviceOnly=false) ;
    void sendNotification(int rank) ;
    void notificationsDumpOut(CBufferOut& buffer) ;
    void notificationsDumpIn(CBufferIn& buffer) ;
    void checkNotifications(void) ;
    CPoolRessource* getPoolRessource(void) { return poolRessource_; } 
    const MPI_Comm& getCommunicator(void) { return serverComm_ ;}
    bool isServerLeader(void) ;
    void finalize(void) ;
    void finalizeSignal(void) ;

    const int localLeader_=0 ;
    MPI_Comm serverComm_ ;
    MPI_Comm freeRessourcesComm_ ;

    bool isAssigned_=false ; // already assigned to a pool ?
    const size_t maxBufferSize_=1024*1024 ;
    CWindowManager* winNotify_ ;
    size_t hashNotify_ ;
    
    int notifyInType_,notifyOutType_ ;
    std::tuple<std::string, int> notifyInCreatePool_,notifyOutCreatePool_ ;
    CPoolRessource* poolRessource_ ;
    bool finalizeSignal_ ;

    const double eventLoopLatency_=0; 
    double lastEventLoop_=0. ;
    bool useWindowManager_ ;
    
    private:
      bool finished_=false;
    public:
      bool isFinished(void) { return finished_ ;}
    private:
      void synchronize(void) ;
      void threadEventLoop(void) ;
      shared_ptr<CEventScheduler> eventScheduler_ ;
      shared_ptr<CEventScheduler> freeRessourceEventScheduler_ ;
      bool isFirstSplit_=true ;
    public:
      shared_ptr<CEventScheduler> getEventScheduler(void) { return eventScheduler_ ;}
    friend class CWindowManager ;
  } ;

}



#endif