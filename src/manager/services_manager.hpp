#ifndef __SERVICES_MANAGER_HPP__
#define __SERVICES_MANAGER_HPP__

#include "xios_spl.hpp"
#include "mpi.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"

#include <string>
#include <map>
#include "window_manager.hpp"



namespace xios
{
  class CServicesManager
  {

    public: 
    static const int CLIENT=0 ;
    static const int GATHERER=1 ;
    static const int WRITER=2 ;
    static const int READER=3 ;
    static const int IO_SERVER=4 ;
    static const int OUT_SERVER=5 ;
    static const int ALL_SERVICES=6 ;

    private:
    const int NOTIFY_NOTHING=0 ;
    const int NOTIFY_CREATE_SERVICE=1 ;
    const int NOTIFY_CREATE_SERVICE_ONTO=2 ;
    
    public:
    
    CServicesManager(bool isXiosServer) ;
    ~CServicesManager() ;
    
    bool createServices(const std::string& poolId, const std::string& serviceId, int type, int size, int nbPartition, bool wait=true) ;
    bool createServicesOnto(const std::string& poolId, const std::string& serviceId, int type, const std::string& onServiceId, bool wait=true) ;
    
    void eventLoop(void) ;
    
    void registerService(const std::string& poolId, const std::string& serviceId, const int& partitionId, int type, int size, int nbPartitions, int leader) ;
    bool getServiceInfo(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& type, int& size, int& nbPartition, int& leader) ;
    bool getServiceLeader(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& leader) ;
    bool getServiceType(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& type) ;
    bool getServiceNbPartitions(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& nbPartition) ;
    bool hasService(const std::string& poolId, const std::string& serviceId, const int& partitionId) ;
    void servicesDumpOut(CBufferOut& buffer) ;
    void servicesDumpIn(CBufferIn& buffer) ;

    private:

    void createService(void) ;
    void createServiceOnto(void) ;    
    void createServicesNotify(int rank, const string& serviceId, int type, int size, int nbPartitions) ;
    void createServicesOntoNotify(int rank, const string& serviceId, int type, const string& OnServiceId) ;
    void sendNotification(int rank) ;
    void checkNotifications(void) ;
    void notificationsDumpOut(CBufferOut& buffer) ;
    void notificationsDumpIn(CBufferIn& buffer) ;


    private :
   
    CWindowManager* winServices_ ;
    CWindowManager* winNotify_ ;
    const size_t maxBufferSize_=1024*1024 ;

    MPI_Comm xiosComm_ ;

    int notifyType_ ;
    tuple<std::string, int, int, int> notifyCreateService_ ;
    tuple<std::string, int, std::string> notifyCreateServiceOnto_ ;
   
    std::map<tuple<std::string, std::string, int>, std::tuple<int, int, int, int> > services_ ;

    int managerGlobalLeader_ ;

    const double eventLoopLatency_=0; 
    double lastEventLoop_=0. ;
    
    friend class CWindowManager ;
  } ;


}



#endif
