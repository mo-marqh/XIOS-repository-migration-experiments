#ifndef __POOL_RESSOURCE_HPP__
#define __POOL_RESSOURCE_HPP__

#include "mpi.hpp"
#include "window_manager.hpp"
#include "services_manager.hpp"
#include "event_scheduler.hpp"


namespace xios
{
  
  class CService ;

  class CPoolRessource  
  {
    private:
      
    const size_t maxBufferSize_=1024*1024 ;
    const int localLeader_ = 0 ;

    CWindowManager* winNotify_ ;
    size_t hashNotify_ ; 
    
    private:
    const int NOTIFY_NOTHING=0 ;
    const int NOTIFY_CREATE_SERVICE=1 ;
    const int NOTIFY_CREATE_SERVICE_ONTO=2 ;

    public:
    CPoolRessource(MPI_Comm poolComm, shared_ptr<CEventScheduler> eventScheduler, const std::string& Id, bool isServer) ;
    ~CPoolRessource() ;
    
    void createService(const std::string& serviceId, int type, int size, int nbPartition) ;
    void createService(MPI_Comm serviceComm, shared_ptr<CEventScheduler> eventScheduler, const std::string& serviceId, int partitionId, int type, int nbPartitions) ; 
    void createServiceOnto(const std::string& serviceId, int type, const std::string& OnServiceId) ;
    bool eventLoop(bool serviceOnly=false) ;
    void threadEventLoop(void) ;
    bool hasService(const std::string serviceId, int partitionId) {return services_.count(make_tuple(serviceId,partitionId))>0 ;}
    CService* getService(const std::string serviceId, int partitionId) { return services_[make_tuple(serviceId,partitionId)]; }
    void finalizeSignal(void) ;
    string getId(void) { return Id_; }
    const MPI_Comm& getCommunicator(void) { return poolComm_ ;}

  private:
    void createServiceNotify(int rank, const string& serviceId, int type, int size, int nbPartitions) ;
    void createServiceOntoNotify(int rank, const string& serviceId, int type, const string& onServiceId) ;
    void sendNotification(int rank) ;
    void checkNotifications(void) ;
    void notificationsDumpOut(CBufferOut& buffer) ;
    void notificationsDumpIn(CBufferIn& buffer) ;
    void createService(void) ;
    void createServiceOnto(void) ;
    void synchronize(void) ;
    
    void createNewService(const std::string& serviceId, int type, int size, int nbPartitions) ;
  public:
    void createNewServiceOnto(const std::string& serviceId, int type, const string& onServiceId) ;
  
  private:
    bool finished_=false ;
  public:
    bool isFinished(void) { return finished_ ;}

  private:
    bool useWindowManager_ ;
    MPI_Comm poolComm_ ;
    
    std::multimap<int,int> occupancy_ ;

//    std::list<std::tuple<std::string, int, int, int, bool> > notifications_;
    
    int notifyType_ ;
    tuple<std::string, int, int, int> notifyCreateService_ ;
    tuple<std::string, int, std::string> notifyCreateServiceOnto_ ;


    std::map< std::tuple<std::string, int>, CService*> services_ ;
    std::string Id_ ;
    bool finalizeSignal_ ;
        
    const double eventLoopLatency_=0; 
    double lastEventLoop_=0. ;

    private:
      size_t hashId_ ;
      shared_ptr<CEventScheduler> eventScheduler_ ;
      shared_ptr<CEventScheduler> freeRessourceEventScheduler_ ;
      bool isFirstSplit_=true ;
    public:
      shared_ptr<CEventScheduler> getEventScheduler(void) { return eventScheduler_ ;}

  };

}



#endif
