#ifndef __CONTEXTS_MANAGER_HPP__
#define __CONTEXTS_MANAGER_HPP__

#include "xios_spl.hpp"
#include "mpi.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"

#include <string>
#include <map>
#include "window_manager.hpp"
#include "register_context_info.hpp"


namespace xios
{

 class CContextsManager
 { 
    private:

    const int NOTIFY_NOTHING=0 ;
    const int NOTIFY_CREATE_CONTEXT=1 ;
    const int NOTIFY_CREATE_INTERCOMM=2 ;

    
    public:
 
    CContextsManager(bool isXiosServer) ;
    ~CContextsManager() ;
    
    bool createServerContext(const std::string& poolId, const std::string& serviceId, const int& partitionId, const string& contextId, bool wait=true) ;
   
    bool createServerContextIntercomm(const std::string& poolId, const std::string& serviceId, const int& partitionId, 
                                      const std::string& contextId, const string& sourceContext, bool wait=true) ;

    string getServerContextName(const std::string& poolId, const std::string& serviceId, const int& partitionId, 
                                const int& type, const string& contextId) ;
  
 
    void sendNotification(int rank);  
    void checkNotifications(void) ;

    void eventLoop(void) ;
    void notificationsDumpOut(CBufferOut& buffer) ;
    void notificationsDumpIn(CBufferIn& buffer) ;
    
    void registerContext(const std::string& fullContextId, const SRegisterContextInfo& contextInfo) ;
    
    bool getContextInfo(const std::string& fullContextId, SRegisterContextInfo& contextInfo, MPI_Comm comm=MPI_COMM_NULL) ;
    bool getContextPoolId(const string& fullContextId, string& poolId, MPI_Comm comm=MPI_COMM_NULL) ;
    bool getContextServiceId(const string& fullContextId, string& serviceId, MPI_Comm comm=MPI_COMM_NULL) ;
    bool getContextPartitionId(const string& fullContextId, int& partitionId, MPI_Comm comm=MPI_COMM_NULL) ;
    bool getContextId(const string& fullContextId, string& contextId, MPI_Comm comm=MPI_COMM_NULL) ;
    bool getContextLeader(const string& fullContextId, int& leader, MPI_Comm comm=MPI_COMM_NULL) ;
    bool getContextSize(const string& fullContextId, int& size, MPI_Comm comm=MPI_COMM_NULL) ;
    bool getContextServiceType(const string& fullContextId, int& serviceType, MPI_Comm comm=MPI_COMM_NULL) ;
   
    bool hasContext(const std::string&  fullContextId, MPI_Comm comm=MPI_COMM_NULL) ;
  
    void contextsDumpOut(CBufferOut& buffer) ;
    void contextsDumpIn(CBufferIn& buffer) ;    

    void createServerContext(void) ; //private
    void createServerContextIntercomm(void) ; //private

    private :

    CWindowManager* winContexts_ ;
    CWindowManager* winNotify_ ;


    const size_t maxBufferSize_=1024*1024 ;

    MPI_Comm xiosComm_ ;

    int notifyType_ ;
    tuple<std::string, std::string, int, std::string> notifyCreateContext_ ;
    tuple<std::string, std::string, int, std::string, int, std::string> notifyCreateIntercomm_ ;

    std::map<std::string, SRegisterContextInfo> contexts_ ;

    int managerGlobalLeader_ ;

    const double eventLoopLatency_=1e-2; 
    double lastEventLoop_=0. ;

    friend class CWindowManager ;
 
  } ;

}



#endif
