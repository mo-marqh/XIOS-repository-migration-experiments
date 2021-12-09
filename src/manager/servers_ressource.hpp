#ifndef __SERVERS_RESSOURCE_HPP__
#define __SERVERS_RESSOURCE_HPP__

#include "window_manager.hpp"
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
    bool isServerLeader(void) ;
    void finalize(void) ;
    void finalizeSignal(void) ;

    const int localLeader_=0 ;
    vector<int> freeRessourcesRank_ ; // only for leader
    MPI_Comm serverComm_ ;
    MPI_Comm freeRessourcesComm_ ;


    const size_t maxBufferSize_=1024*1024 ;
    CWindowManager* winNotify_ ;
    
    int notifyInType_,notifyOutType_ ;
    std::tuple<std::string, bool> notifyInCreatePool_,notifyOutCreatePool_ ;
    CPoolRessource* poolRessource_ ;
    bool finalizeSignal_ ;

    const double eventLoopLatency_=0; 
    double lastEventLoop_=0. ;

    friend class CWindowManager ;
  } ;

}



#endif