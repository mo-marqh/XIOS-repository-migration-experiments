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
    static const int IO_SERVER=2 ;
    static const int OUT_SERVER=3 ;

    public:
    
    CServicesManager(bool isXiosServer) ;
    bool createServices(const std::string& poolId, const std::string& serviceId, int type, int size, int nbPartition, bool wait=true) ;
    void createServicesNotify(int rank, const string& serviceId, int type, int size, int nbPartitions) ;
    void checkCreateServicesNotification(void) ;
    void eventLoop(void) ;
    void notificationsDumpOut(CBufferOut& buffer) ;
    void notificationsDumpIn(CBufferIn& buffer) ;
    
    void registerService(const std::string& poolId, const std::string& serviceId, const int& partitionId, int type, int size, int nbPartitions, int leader) ;
    bool getServiceInfo(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& type, int& size, int& nbPartition, int& leader) ;
    bool getServiceLeader(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& leader) ;
    bool getServiceType(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& type) ;
    bool getServiceNbPartitions(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& nbPartition) ;
    bool hasService(const std::string& poolId, const std::string& serviceId, const int& partitionId) ;
    void servicesDumpOut(CBufferOut& buffer) ;
    void servicesDumpIn(CBufferIn& buffer) ;
    

    private :
   
    CWindowManager* winServices_ ;
    CWindowManager* winNotify_ ;
    const size_t maxBufferSize_=1024*1024 ;

    MPI_Comm xiosComm_ ;

    std::list<std::tuple<std::string, int, int, int> > notifications_;
    
    std::map<tuple<std::string, std::string, int>, std::tuple<int, int, int, int> > services_ ;

    int managerGlobalLeader_ ;
    
    friend class CWindowManager ;
  } ;


}



#endif