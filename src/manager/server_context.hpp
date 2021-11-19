#ifndef __SERVER_CONTEXT_HPP__
#define __SERVER_CONTEXT_HPP__
#include "xios_spl.hpp"
#include "window_manager.hpp"
#include "mpi.hpp"

namespace xios
{
  class CContext ;
  class CService ;

  class CServerContext
  {
    const int NOTIFY_NOTHING=0 ;
    const int NOTIFY_CREATE_INTERCOMM=1 ;
    
    public:
    CServerContext(CService* parentService, MPI_Comm contextComm, const std::string& poolId, const std::string& serviceId, 
                  const int& partitionId, const std::string& contextId) ;
    ~CServerContext() ;

    bool createIntercomm(const string& poolId, const string& serviceId, const int& partitionId, const string& contextId, 
                         const MPI_Comm& intraComm, MPI_Comm& interCommClient, MPI_Comm& interCommServer, bool wait=true) ;
    void createIntercomm(int remoteLeader, const string& sourceContext) ;

    void sendNotification(int rank);  
    void checkNotifications(void) ;

    bool eventLoop(bool serviceOnly=false) ;
    void notificationsDumpOut(CBufferOut& buffer) ;
    void notificationsDumpIn(CBufferIn& buffer) ;
    void finalizeSignal(void) ;
    void freeComm(void) ;
    bool isAttachedMode(void) { return isAttachedMode_ ;}
    CService* getParentService(void) {return parentService_ ; }

    private:
    void createIntercomm(void) ;
    
    static std::map<std::string, std::tuple<bool, MPI_Comm, MPI_Comm> > overlapedComm_ ;

    MPI_Comm contextComm_ ;
    MPI_Comm xiosComm_ ;

    CContext* context_ ;
    CService* parentService_ ;
    std::string name_ ;

    vector<MPI_Comm> clientsInterComm_ ;


    const size_t maxBufferSize_=1024*1024 ;
    CWindowManager* winNotify_ ;
    int notifyInType_, notifyOutType_ ;
    tuple<int, std::string> notifyInCreateIntercomm_, notifyOutCreateIntercomm_ ;

    const int localLeader_=0 ;
    int globalLeader_ ;
    bool finalizeSignal_ ;
    bool hasNotification_ ;
    bool isAttachedMode_ ;

    const double eventLoopLatency_=0; 
    double lastEventLoop_=0. ;

    friend class CWindowManager ;
  } ;

}

#endif
