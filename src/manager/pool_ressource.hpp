#ifndef __POOL_RESSOURCE_HPP__
#define __POOL_RESSOURCE_HPP__

#include "mpi.hpp"
#include "window_manager.hpp"
#include "services_manager.hpp"


namespace xios
{
  
  class CService ;

  class CPoolRessource  
  {
    private:
      
    const size_t maxBufferSize_=1024*1024 ;
    const int localLeader_ = 0 ;

    CWindowManager* winNotify_ ;
    
   public:
    CPoolRessource(MPI_Comm poolComm, const std::string& Id) ;
    
    void createService(const std::string& serviceId, int type, int size, int nbPartition) ;
    void createService(MPI_Comm serviceComm, const std::string& serviceId, int partitionId, int type, int nbPartitions) ; 
    void createServiceNotify(int rank, const std::string& serviceId, int type, int size, int nbPartitions, bool in) ;
    void createServiceDumpOut(CBufferOut& buffer) ;
    void createServiceDumpIn(CBufferIn& buffer) ;
    void checkCreateServiceNotification(void) ;
    void createNewService(const std::string& serviceId, int type, int size, int nbPartitions, bool in) ;
     bool eventLoop(bool serviceOnly=false) ;
    CService* getService(const std::string serviceId, int partitionId) { return services_[make_tuple(serviceId,partitionId)]; }
    void finalizeSignal(void) ;
    string getId(void) { return Id_; }
    
   private:
    MPI_Comm poolComm_ ;
    
    std::multimap<int,int> occupancy_ ;
    std::list<std::tuple<std::string, int, int, int, bool> > notifications_;
    std::map< std::tuple<std::string, int>, CService*> services_ ;
    std::string Id_ ;
    bool finalizeSignal_ ;
    
    const double eventLoopLatency_=0; 
    double lastEventLoop_=0. ;
  };

}



#endif