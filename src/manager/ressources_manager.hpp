#ifndef __RESSOURCES_MANAGER_HPP__
#define __RESSOURCES_MANAGER_HPP__

#include "xios_spl.hpp"
#include "mpi.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"

#include <string>
#include <map>
#include "window_manager.hpp"
#include "pool_ressource.hpp"
#include "token_manager.hpp"



namespace xios
{


  class CRessourcesManager
  {

    const int NOTIFY_NOTHING=0 ;
    const int NOTIFY_CREATE_POOL=1 ;
    const int NOTIFY_FINALIZE=2 ;

    public:
    
    CRessourcesManager(bool isXiosServer) ;
    ~CRessourcesManager() ;

    void eventLoop(void) ;
    void createPool(const std::string& id, int size) ;
    void createPool(void) ;
    void finalize(void) ;
    void finalizeSignal(void) ;
    void sendNotification(int rank);  
    void checkNotifications(void) ;
    void notificationsDumpOut(CBufferOut& buffer) ;
    void notificationsDumpIn(CBufferIn& buffer) ;

    void ressourcesDumpOut(CBufferOut& buffer) ;
    void ressourcesDumpIn(CBufferIn& buffer) ;    

    int  getRessourcesSize(void) ;
    int  getFreeRessourcesSize(void) ;
    bool getPoolInfo(const string& poolId, int& size, int& freeSize, int& leader) ;
    bool getPoolLeader(const string& poolId, int& leader) ;
    bool getPoolSize(const string& poolId, int& size) ;
    bool getPoolFreeSize(const string& poolId, int& freeSize) ;
    bool hasPool(const string& poolId) ;
    bool decreasePoolFreeSize(const string& poolId, int size) ;
    void waitPoolRegistration(const string& poolId) ;
    

    void registerServerLeader(int leaderRank) ;
    void registerRessourcesSize(int size) ;
    void registerPoolClient(const std::string& poolId,int size,int leader) ;
    void registerPoolServer(const std::string& poolId,int size,int leader) ;
    CTokenManager* getTokenManager(void) {return tokenManager_ ;} 

    int managerGlobalLeader_ ;

    CWindowManager* winRessources_ ;

    CWindowManager* winNotify_ ;
    CTokenManager* tokenManager_ ;

    const size_t maxBufferSize_=1024*1024 ;

    MPI_Comm xiosComm_ ;

    int notifyType_ = NOTIFY_NOTHING;
    tuple<std::string, int> notifyCreatePool_ ;

    std::map<std::string, std::tuple<int,int,int>> pools_ ;
    int serverLeader_ ;
    int ressourcesSize_ ;
    int freeRessourcesSize_ ;

    const double eventLoopLatency_=0; 
    double lastEventLoop_=0. ;

    friend class CWindowManager ;
  } ;

}
#endif
