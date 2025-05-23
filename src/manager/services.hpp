#ifndef __SERVICE_HPP__
#define __SERVICE_HPP__

#include "xios_spl.hpp"
#include "mpi.hpp"
#include "services_manager.hpp"
#include "event_scheduler.hpp"

namespace xios
{

  class CServerContext ;

  class CService
  {
    public:
    
    const int NOTIFY_NOTHING=0 ;
    const int NOTIFY_CREATE_CONTEXT=1 ;

    CService(MPI_Comm serviceComm, shared_ptr<CEventScheduler>, const std::string& poolId, const std::string& serviceId, const int& partitionId, 
             int type, int nbPartitions) ;
    ~CService() ;

    bool eventLoop(bool serviceOnly=false) ;
    void threadEventLoop(void) ;
    void createContext(const std::string& poolId, const std::string& serviceId, const int& partitionId, const std::string& contextId) ;
    CServerContext* getServerContext(const std::string& contextId) { return contexts_[contextId]; }
    void finalizeSignal(void) ;
    shared_ptr<CEventScheduler> getEventScheduler(void) ;

    std::string getPoolId(void) {return poolId_;}
    std::string getServiceId(void) {return serviceId_;}
    int getPartitionId(void) {return partitionId_;}
    int getType(void) {return type_;}
    int getNbPartitions(void) {return nbPartitions_;}
    const MPI_Comm& getCommunicator(void) { return serviceComm_ ;}
    
    private: 
    bool finished_=false ;
    public:
    bool isFinished(void) { return finished_; }

    private:
    void sendNotification(int rank) ;
    void notificationsDumpOut(CBufferOut& buffer) ;
    void notificationsDumpIn(CBufferIn& buffer) ;
    void checkNotifications(void) ;
    void createContext(void) ;

    MPI_Comm serviceComm_ ;
    MPI_Comm globalComm_ ;
    
    bool useWindowManager_ ;
    const size_t maxBufferSize_=1024*1024 ;
    const int localLeader_=0 ;
    int globalLeader_ ;
    CWindowManager* winNotify_ ;
    size_t hashNotify_ ;
    
    std::string name_ ;

    std::list<std::tuple<std::string, std::string, int, std::string>> notifications_;
    
    bool hasNotification_ ;
    int notifyInType_,notifyOutType_ ;
    std::tuple<std::string, std::string, int, std::string> notifyInCreateContext_, notifyOutCreateContext_ ;

    std::map<std::string, CServerContext*> contexts_ ;
    bool finalizeSignal_ ;
    shared_ptr<CEventScheduler> eventScheduler_ ;

    std::string poolId_ ;
    std::string serviceId_ ;
    int partitionId_ ;
    int type_ ;
    int nbPartitions_ ;

    const double eventLoopLatency_=0; 
    double lastEventLoop_=0. ;

  };

}



#endif