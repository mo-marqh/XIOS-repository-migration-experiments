#include "daemons_manager.hpp"
#include "services_manager.hpp"
#include "ressources_manager.hpp"
#include "cxios.hpp"
#include "pool_ressource.hpp"
#include "type.hpp"
#include "server.hpp"
#include "servers_ressource.hpp"
#include "timer.hpp"

namespace xios
{
  extern CLogType logTimers ;
  

  CServicesManager::CServicesManager(bool isXiosServer)
  {
    
    useWindowManager_ = CXios::servicesUseWindowManager ;
    int commRank ;  
    xiosComm_ = CXios::getXiosComm() ;
    MPI_Comm_rank(xiosComm_, &commRank) ;
    
    
    // The global manager leader will be the process of rank 0
    // By "xiosComm" communicator construction
    // - if servers exits it will be the root process of the servers communicator
    // - otherwise the root process of the first model
    
    managerGlobalLeader_ = 0 ;

    MPI_Comm_rank(xiosComm_, &commRank) ;
    if (useWindowManager_)
    {
      winNotify_ = new CWindowManager(xiosComm_, maxBufferSize_,"CServicesManager::winNotify_") ;
      winNotify_->updateToExclusiveWindow(commRank, this, &CServicesManager::notificationsDumpOut) ;

      winServices_ = new CWindowManager(xiosComm_, maxBufferSize_,"CServicesManager::winServices_") ;
      winServices_->updateToExclusiveWindow(commRank, this, &CServicesManager::servicesDumpOut) ;
    }

    std::hash<string> hashString ;
    hashServiceNotify_ = hashString("CServicesManager::serviceNotify_") ;
    hashServiceInfo_   = hashString("CServicesManager::serviceInfo_") ;

    MPI_Barrier(xiosComm_)  ;    
  }

  CServicesManager::~CServicesManager()
  {
    if (useWindowManager_) delete winNotify_ ;
    if (useWindowManager_) delete winServices_ ;
  }

  bool CServicesManager::createServices(const std::string& poolId, const std::string& serviceId, 
                                        int type, int size, int nbPartitions, bool wait) 
  {

    int leader ;
    int poolSize, poolFreeSize ;
    
    info(40)<<"CServicesManager : waiting for pool info : "<<poolId<<endl ; ;
    bool ok=CXios::getRessourcesManager()->getPoolInfo(poolId, poolSize, poolFreeSize, leader) ;
    if (wait)
    {
      while (!ok) 
      {
        CXios::getDaemonsManager()->eventLoop() ;
        ok=CXios::getRessourcesManager()->getPoolInfo(poolId, poolSize, poolFreeSize, leader) ;
      }
    }

    if (ok) 
    {
      info(40)<<"CServicesManager : create service notification to leader "<<leader<<", serviceId : "<<serviceId<<", size : "<<size<<endl ;
      CXios::getRessourcesManager()->decreasePoolFreeSize(poolId ,size) ;
      createServicesNotify(leader, serviceId, type, size, nbPartitions) ;
      return true ;
    }
    else return false ;
  }

  bool CServicesManager::createServicesOnto(const std::string& poolId, const std::string& serviceId, int type, const std::string& OnServiceId, bool wait)
  {

    int leader ;
    int poolSize ;
    int poolFreeSize ;
    
    info(40)<<"CServicesManager : waiting for pool info : "<<poolId<<endl ; ;
    bool ok=CXios::getRessourcesManager()->getPoolInfo(poolId, poolSize, poolFreeSize, leader) ;
    if (wait)
    {
      while (!ok) 
      {
        CXios::getDaemonsManager()->eventLoop() ;
        ok=CXios::getRessourcesManager()->getPoolInfo(poolId, poolSize, poolFreeSize, leader) ;
      }
    }

    if (ok) 
    {
      info(40)<<"CServicesManager : create service on other, notification to leader "<<leader<<", serviceId : "<<serviceId<<", service onto : "<<OnServiceId<<endl ;
      createServicesOntoNotify(leader, serviceId, type, OnServiceId) ;
      return true ;
    }
    else return false ;
  }

  void CServicesManager::createServicesNotify(int rank, const string& serviceId, int type, int size, int nbPartitions)
  {
    notifyType_=NOTIFY_CREATE_SERVICE ;
    notifyCreateService_=make_tuple(serviceId, type, size, nbPartitions ) ;
    sendNotification(rank) ;
  }


  void CServicesManager::createServicesOntoNotify(int rank, const string& serviceId, int type, const string& OnServiceId)
  {
    notifyType_=NOTIFY_CREATE_SERVICE_ONTO ;
    notifyCreateServiceOnto_=make_tuple(serviceId, type, OnServiceId) ;
    sendNotification(rank) ;
  }

/*
  void CServicesManager::sendNotification(int rank)
  {
    winNotify_->pushToExclusiveWindow(rank, this, &CServicesManager::notificationsDumpOut) ;
  }
*/

  void CServicesManager::sendNotification(int rank)
  {
    if (useWindowManager_) winNotify_->pushToExclusiveWindow(rank, this, &CServicesManager::notificationsDumpOut) ;
    else CXios::getNotificationsManager()->sendLockedNotification(rank, hashServiceNotify_, this, &CServicesManager::notificationsDumpOut) ;
  }

/*  
  void CServicesManager::eventLoop(void)
  {
    if (info.isActive(logTimers)) CTimer::get("CServicesManager::eventLoop").resume();
    int flag ;
    MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
    double time=MPI_Wtime() ;
    if (time-lastEventLoop_ > eventLoopLatency_) 
    {
      checkNotifications() ;
      lastEventLoop_=time ;
    }
    if (info.isActive(logTimers)) CTimer::get("CServicesManager::eventLoop").suspend();
  }
*/

  void CServicesManager::eventLoop(void)
  {
    if (info.isActive(logTimers)) CTimer::get("CServicesManager::eventLoop").resume();
    checkNotifications() ;
    if (info.isActive(logTimers)) CTimer::get("CServicesManager::eventLoop").suspend();
  }

/*
  void CServicesManager::checkNotifications(void)
  {
    int commRank ;
    MPI_Comm_rank(xiosComm_, &commRank) ;
    winNotify_->popFromExclusiveWindow(commRank, this, &CServicesManager::notificationsDumpIn) ;
    if (notifyType_==NOTIFY_CREATE_SERVICE) createService() ;
    else if (notifyType_==NOTIFY_CREATE_SERVICE_ONTO) createServiceOnto() ;
  }
*/


  void CServicesManager::checkNotifications(void)
  {
    if (useWindowManager_)
    {
      int flag ;
      MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
      double time=MPI_Wtime() ;
      if (time-lastEventLoop_ > eventLoopLatency_) 
      {
        int commRank ;
        MPI_Comm_rank(xiosComm_, &commRank) ;
        winNotify_->popFromExclusiveWindow(commRank, this, &CServicesManager::notificationsDumpIn) ;
        if (notifyType_==NOTIFY_CREATE_SERVICE) createService() ;
        else if (notifyType_==NOTIFY_CREATE_SERVICE_ONTO) createServiceOnto() ;
      }
    }
    else
    {
      while (CXios::getNotificationsManager()->recvNotification(hashServiceInfo_,   this, &CServicesManager::servicesDumpIn)) ;
      while (CXios::getNotificationsManager()->recvNotification(hashServiceNotify_, this, &CServicesManager::notificationsDumpIn))
      {
        if (notifyType_==NOTIFY_CREATE_SERVICE) createService() ;
        else if (notifyType_==NOTIFY_CREATE_SERVICE_ONTO) createServiceOnto() ;
      }
    }
  }

  void CServicesManager::createService(void)
  {
    auto& arg=notifyCreateService_ ;
    CServer::getServersRessource()->getPoolRessource()->createService(get<0>(arg), get<1>(arg), get<2>(arg), get<3>(arg)) ;
  }

  void CServicesManager::createServiceOnto(void)
  {
    auto& arg=notifyCreateServiceOnto_ ;
    CServer::getServersRessource()->getPoolRessource()->createServiceOnto(get<0>(arg), get<1>(arg), get<2>(arg)) ;
  }

/*
  void CServicesManager::notificationsDumpOut(CBufferOut& buffer)
  {

    buffer.realloc(maxBufferSize_) ;
    
    if (notifyType_==NOTIFY_CREATE_SERVICE)
    {
      auto& arg=notifyCreateService_ ;
      buffer << notifyType_<< get<0>(arg) << get<1>(arg) << std::get<2>(arg) << get<3>(arg) ;
    }
    else if (notifyType_==NOTIFY_CREATE_SERVICE_ONTO)
    {
      auto& arg=notifyCreateServiceOnto_ ;
      buffer << notifyType_<< get<0>(arg) << get<1>(arg) << get<2>(arg)  ;
    }
  }
*/
  
  void CServicesManager::notificationsDumpOut(CBufferOut& buffer)
  {
    if (useWindowManager_) buffer.realloc(maxBufferSize_) ;

    if (notifyType_==NOTIFY_CREATE_SERVICE)
    {
      auto& arg=notifyCreateService_ ;
      buffer << notifyType_<< get<0>(arg) << get<1>(arg) << std::get<2>(arg) << get<3>(arg) ;
    }
    else if (notifyType_==NOTIFY_CREATE_SERVICE_ONTO)
    {
      auto& arg=notifyCreateServiceOnto_ ;
      buffer << notifyType_<< get<0>(arg) << get<1>(arg) << get<2>(arg)  ;
    }
  }

  void CServicesManager::notificationsDumpIn(CBufferIn& buffer)
  {
    if (buffer.bufferSize() == 0) notifyType_= NOTIFY_NOTHING ;
    else
    {
      buffer>>notifyType_;
      if (notifyType_==NOTIFY_CREATE_SERVICE)
      {
        auto& arg=notifyCreateService_ ;
        buffer >> get<0>(arg) >> get<1>(arg) >> std::get<2>(arg)>> get<3>(arg) ;
      }
      else if (notifyType_==NOTIFY_CREATE_SERVICE_ONTO)
      {
        auto& arg=notifyCreateServiceOnto_ ;
        buffer >> get<0>(arg) >> get<1>(arg) >> get<2>(arg) ;
      }
    }
  }  
/*  
  void CServicesManager::servicesDumpOut(CBufferOut& buffer)
  {
    
    buffer.realloc(maxBufferSize_) ;
   
    buffer<<(int)services_.size();
    
    for(auto it=services_.begin();it!=services_.end(); ++it)
    { 
      auto key = it->first ;
      auto val = it->second ; 
      buffer << std::get<0>(key) << std::get<1>(key) << std::get<2>(key) 
             <<  static_cast<int>(std::get<0>(val)) << std::get<1>(val) << std::get<2>(val) << std::get<3>(val) ;
    }
  }
*/
  void CServicesManager::servicesDumpOut(CBufferOut& buffer)
  {
    if (useWindowManager_)
    {
      buffer.realloc(maxBufferSize_) ;
      buffer<<(int)services_.size();
    
      for(auto it=services_.begin();it!=services_.end(); ++it)
      { 
        auto key = it->first ;
        auto val = it->second ; 
        buffer << std::get<0>(key) << std::get<1>(key) << std::get<2>(key) 
               <<  static_cast<int>(std::get<0>(val)) << std::get<1>(val) << std::get<2>(val) << std::get<3>(val) ;
      }
    }
    else
    {
      auto key = registringService_.first ;
      auto val = registringService_.second ; 
   
      buffer << std::get<0>(key) << std::get<1>(key) << std::get<2>(key) 
             <<  static_cast<int>(std::get<0>(val)) << std::get<1>(val) << std::get<2>(val) << std::get<3>(val) ;
    }
  }

/*
  void CServicesManager::servicesDumpIn(CBufferIn& buffer)
  {
    std::string poolId, serviceId ;
    int partitionId ;
    int type ;
    int size; 
    int nbPartitions ;
    int leader ;

    int nbServices ;
    buffer>>nbServices ;
    bool newServices = nbServices != services_.size() ; 

    services_.clear() ;

    for(int i=0;i<nbServices;i++) 
    {
      buffer>>poolId>>serviceId>>partitionId>>type>>size>>nbPartitions>>leader ;
      services_[std::tuple<std::string,std::string,int>(poolId,serviceId,partitionId)]=std::make_tuple(type,size,nbPartitions,leader) ;
      if (newServices)
        info(40)<<"Receive new services informations : "<<poolId<<"::"<<serviceId<<"::"<<partitionId<<" => type : "<<type<<"  size : "<<size<<"  nbPartitions : "<<nbPartitions<<"  leader : "<<leader<<endl ;
    }
  }
*/

  void CServicesManager::servicesDumpIn(CBufferIn& buffer)
  {
    if (useWindowManager_)
    {
      std::string poolId, serviceId ;
      int partitionId ;
      int type ;
      int size; 
      int nbPartitions ;
      int leader ;

      int nbServices ;
      buffer>>nbServices ;
      bool newServices = nbServices != services_.size() ; 

      services_.clear() ;

      for(int i=0;i<nbServices;i++) 
      {
        buffer>>poolId>>serviceId>>partitionId>>type>>size>>nbPartitions>>leader ;
        services_[std::tuple<std::string,std::string,int>(poolId,serviceId,partitionId)]=std::make_tuple(type,size,nbPartitions,leader) ;
        if (newServices)
          info(40)<<"Receive new services informations : "<<poolId<<"::"<<serviceId<<"::"<<partitionId<<" => type : "<<type<<"  size : "<<size<<"  nbPartitions : "<<nbPartitions<<"  leader : "<<leader<<endl ;
      }
    }
    else
    {

      if (buffer.bufferSize()==0) return ;

      std::string poolId, serviceId ;
      int partitionId ;
      int type ;
      int size; 
      int nbPartitions ;
      int leader ;

      buffer>>poolId>>serviceId>>partitionId>>type>>size>>nbPartitions>>leader ;
      services_[std::tuple<std::string,std::string,int>(poolId,serviceId,partitionId)]=std::make_tuple(type,size,nbPartitions,leader) ;
      info(40)<<"Receive new services informations : "<<poolId<<"::"<<serviceId<<"::"<<partitionId<<" => type : "<<type<<"  size : "<<size<<"  nbPartitions : "<<nbPartitions<<"  leader : "<<leader<<endl ;
    }

  }

/*
  void CServicesManager::registerService(const std::string& poolId, const std::string& serviceId, const int& partitionId, int type, 
                                         int size, int nbPartitions, int leader)
  {
    
    info(40)<<"CServicesManager : registering service, poolId : "<<poolId<<", serviceId : "<<serviceId<<endl ; ;

    winServices_->lockWindowExclusive(managerGlobalLeader_) ;
    winServices_->updateFromLockedWindow(managerGlobalLeader_, this, &CServicesManager::servicesDumpIn) ;
    winServices_->flushWindow(managerGlobalLeader_) ;
    services_[std::tuple<std::string, std::string,int>(poolId,serviceId,partitionId)]=std::make_tuple(type,size,nbPartitions,leader) ;
    winServices_->updateToLockedWindow(managerGlobalLeader_, this, &CServicesManager::servicesDumpOut) ;
    winServices_->unlockWindowExclusive(managerGlobalLeader_) ;

  }
*/

  void CServicesManager::registerService(const std::string& poolId, const std::string& serviceId, const int& partitionId, int type, 
                                         int size, int nbPartitions, int leader)
  {
    
    info(40)<<"CServicesManager : registering service, poolId : "<<poolId<<", serviceId : "<<serviceId<<endl ; ;
    if (useWindowManager_)
    {
      winServices_->lockWindowExclusive(managerGlobalLeader_) ;
      winServices_->updateFromLockedWindow(managerGlobalLeader_, this, &CServicesManager::servicesDumpIn) ;
      winServices_->flushWindow(managerGlobalLeader_) ;
      services_[std::tuple<std::string, std::string,int>(poolId,serviceId,partitionId)]=std::make_tuple(type,size,nbPartitions,leader) ;
      winServices_->updateToLockedWindow(managerGlobalLeader_, this, &CServicesManager::servicesDumpOut) ;
      winServices_->unlockWindowExclusive(managerGlobalLeader_) ;
    }
    else
    {
      registringService_={ std::tuple<std::string, std::string,int>(poolId,serviceId,partitionId), std::make_tuple(type,size,nbPartitions,leader)} ;
      CXios::getNotificationsManager()->sendLockedNotification(hashServiceInfo_, this, &CServicesManager::servicesDumpOut) ;
    }
  }

  bool CServicesManager::getServiceInfo(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& type, 
                                        int& size, int& nbPartitions, int& leader, bool wait)
  {
    auto it=services_.find(std::tuple<std::string,std::string,int>(poolId,serviceId,partitionId)) ;
    if ( it == services_.end())
    {
      if (useWindowManager_) winServices_->updateFromSharedWindow(managerGlobalLeader_, this, &CServicesManager::servicesDumpIn) ;
      
      if (wait) waitServiceRegistration(poolId, serviceId, partitionId) ;
    }
    it=services_.find(std::tuple<std::string,std::string,int>(poolId,serviceId,partitionId)) ;
    if ( it == services_.end()) return false ;
    else
    {
      type= std::get<0>(it->second); 
      size= std::get<1>(it->second); 
      nbPartitions = std::get<2>(it->second); 
      leader = std::get<3>(it->second); 
      return true ;
    }
  }

  bool CServicesManager::getServiceLeader(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& leader, bool wait)
  {
    int type;
    int size ;
    int nbPartitions;
    return getServiceInfo(poolId, serviceId, partitionId, type, size, nbPartitions, leader, wait) ;
  }

  bool CServicesManager::getServiceType(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& type, bool wait)
  {
    int size ;
    int nbPartitions;
    int leader;
    return getServiceInfo(poolId, serviceId, partitionId, type, size, nbPartitions, leader, wait) ;
  }

  bool CServicesManager::getServiceNbPartitions(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& nbPartitions, bool wait)
  {
    int size ;
    int type;
    int leader;
    return getServiceInfo(poolId, serviceId, partitionId, type, size, nbPartitions, leader, wait) ;
  }

  bool CServicesManager::hasService(const std::string& poolId, const std::string& serviceId, const int& partitionId)
  {
    auto it=services_.find(std::tuple<std::string,std::string,int>(poolId,serviceId,partitionId)) ;
    if ( it == services_.end())
    {
      if (useWindowManager_) winServices_->updateFromSharedWindow(managerGlobalLeader_, this, &CServicesManager::servicesDumpIn) ;
    }
    it=services_.find(std::tuple<std::string, std::string, int>(poolId, serviceId, partitionId)) ;
    if ( it == services_.end()) return false ;
    else return true ;
  }
 
  void CServicesManager::waitServiceRegistration(const std::string& poolId, const std::string& serviceId, const int& partitionId)
  {
    while(!hasService(poolId,serviceId,partitionId)) CXios::getDaemonsManager()->servicesEventLoop() ;
  }

  void CServicesManager::waitServiceRegistration(const std::string& poolId, const std::string& serviceId)
  {
    int nbPartition ;
    getServiceNbPartitions(poolId,serviceId,0,nbPartition, true) ;
    for(int n=1;n<nbPartition;n++) waitServiceRegistration(poolId, serviceId, n) ;

  }
}
