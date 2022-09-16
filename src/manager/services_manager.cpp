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

  

  CServicesManager::CServicesManager(bool isXiosServer)
  {
    
    int commRank ;  
    xiosComm_ = CXios::getXiosComm() ;
    MPI_Comm_rank(xiosComm_, &commRank) ;
    
    
    // The global manager leader will be the process of rank 0
    // By "xiosComm" communicator construction
    // - if servers exits it will be the root process of the servers communicator
    // - otherwise the root process of the first model
    
    managerGlobalLeader_ = 0 ;

    MPI_Comm_rank(xiosComm_, &commRank) ;
    winNotify_ = new CWindowManager(xiosComm_, maxBufferSize_) ;
    winNotify_->lockWindow(commRank,0) ;
    winNotify_->updateToWindow(commRank, this, &CServicesManager::notificationsDumpOut) ;
    winNotify_->unlockWindow(commRank,0) ;

    winServices_ = new CWindowManager(xiosComm_, maxBufferSize_) ;
    winServices_->lockWindow(commRank,0) ;
    winServices_->updateToWindow(commRank, this, &CServicesManager::servicesDumpOut) ;
    winServices_->unlockWindow(commRank,0) ;

    MPI_Barrier(xiosComm_)  ;    
  }

  CServicesManager::~CServicesManager()
  {
    delete winNotify_ ;
    delete winServices_ ;
  }

  bool CServicesManager::createServices(const std::string& poolId, const std::string& serviceId, 
                                        int type, int size, int nbPartitions, bool wait) 
  {

    int leader ;
    int poolSize ;
    
    info(40)<<"CServicesManager : waiting for pool info : "<<poolId<<endl ; ;
    bool ok=CXios::getRessourcesManager()->getPoolInfo(poolId, poolSize, leader) ;
    if (wait)
    {
      while (!ok) 
      {
        CXios::getDaemonsManager()->eventLoop() ;
        ok=CXios::getRessourcesManager()->getPoolInfo(poolId, poolSize, leader) ;
      }
    }

    if (ok) 
    {
      info(40)<<"CServicesManager : create service notification to leader "<<leader<<", serviceId : "<<serviceId<<", size : "<<size<<endl ;
      createServicesNotify(leader, serviceId, type, size, nbPartitions) ;
      return true ;
    }
    else return false ;
  }

  bool CServicesManager::createServicesOnto(const std::string& poolId, const std::string& serviceId, const std::string& OnServiceId, bool wait)
  {

    int leader ;
    int poolSize ;
    
    info(40)<<"CServicesManager : waiting for pool info : "<<poolId<<endl ; ;
    bool ok=CXios::getRessourcesManager()->getPoolInfo(poolId, poolSize, leader) ;
    if (wait)
    {
      while (!ok) 
      {
        CXios::getDaemonsManager()->eventLoop() ;
        ok=CXios::getRessourcesManager()->getPoolInfo(poolId, poolSize, leader) ;
      }
    }

    if (ok) 
    {
      info(40)<<"CServicesManager : create service on other, notification to leader "<<leader<<", serviceId : "<<serviceId<<", service onto : "<<OnServiceId<<endl ;
      createServicesOntoNotify(leader, serviceId, OnServiceId) ;
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


  void CServicesManager::createServicesOntoNotify(int rank, const string& serviceId, const string& OnServiceId)
  {
    notifyType_=NOTIFY_CREATE_SERVICE_ONTO ;
    notifyCreateServiceOnto_=make_tuple(serviceId, OnServiceId) ;
    sendNotification(rank) ;
  }

  void CServicesManager::sendNotification(int rank)
  {
    winNotify_->lockWindowExclusive(rank) ;
    winNotify_->pushToLockedWindow(rank, this, &CServicesManager::notificationsDumpOut) ;
    winNotify_->unlockWindow(rank) ;
  }

  
  void CServicesManager::eventLoop(void)
  {
    CTimer::get("CServicesManager::eventLoop").resume();
    int flag ;
    MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
    double time=MPI_Wtime() ;
    if (time-lastEventLoop_ > eventLoopLatency_) 
    {
      checkNotifications() ;
      lastEventLoop_=time ;
    }
    CTimer::get("CServicesManager::eventLoop").suspend();
  }



  void CServicesManager::checkNotifications(void)
  {
    int commRank ;
    MPI_Comm_rank(xiosComm_, &commRank) ;
    winNotify_->lockWindowExclusive(commRank) ;
    winNotify_->popFromLockedWindow(commRank, this, &CServicesManager::notificationsDumpIn) ;
    winNotify_->unlockWindow(commRank) ;
    if (notifyType_==NOTIFY_CREATE_SERVICE) createService() ;
    else if (notifyType_==NOTIFY_CREATE_SERVICE_ONTO) createServiceOnto() ;
  }

  void CServicesManager::createService(void)
  {
    auto& arg=notifyCreateService_ ;
    CServer::getServersRessource()->getPoolRessource()->createService(get<0>(arg), get<1>(arg), get<2>(arg), get<3>(arg)) ;
  }

  void CServicesManager::createServiceOnto(void)
  {
    auto& arg=notifyCreateService_ ;
    //CServer::getServersRessource()->getPoolRessource()->createService(get<0>(arg), get<1>(arg), get<2>(arg), get<3>(arg)) ;
  }

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
      buffer << notifyType_<< get<0>(arg) << get<1>(arg)  ;
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
        buffer >> get<0>(arg) >> get<1>(arg) ;
      }
    }
  }  
  
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

  void CServicesManager::servicesDumpIn(CBufferIn& buffer)
  {
    std::string poolId, serviceId ;
    int partitionId ;
    int type ;
    int size; 
    int nbPartitions ;
    int leader ;

    services_.clear() ;
    int nbServices ;
    buffer>>nbServices ;
    for(int i=0;i<nbServices;i++) 
    {
      buffer>>poolId>>serviceId>>partitionId>>type>>size>>nbPartitions>>leader ;
      services_[std::tuple<std::string,std::string,int>(poolId,serviceId,partitionId)]=std::make_tuple(type,size,nbPartitions,leader) ;
    }
  }

  void CServicesManager::registerService(const std::string& poolId, const std::string& serviceId, const int& partitionId, int type, 
                                         int size, int nbPartitions, int leader)
  {
    
    info(40)<<"CServicesManager : registering service, poolId : "<<poolId<<", serviceId : "<<serviceId<<endl ; ;

    winServices_->lockWindowExclusive(managerGlobalLeader_) ;
    winServices_->updateFromLockedWindow(managerGlobalLeader_, this, &CServicesManager::servicesDumpIn) ;
    winServices_->flushWindow(managerGlobalLeader_) ;
    services_[std::tuple<std::string, std::string,int>(poolId,serviceId,partitionId)]=std::make_tuple(type,size,nbPartitions,leader) ;
    winServices_->updateToLockedWindow(managerGlobalLeader_, this, &CServicesManager::servicesDumpOut) ;
    winServices_->unlockWindow(managerGlobalLeader_) ;

  }

  bool CServicesManager::getServiceInfo(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& type, 
                                        int& size, int& nbPartitions, int& leader)
  {
    
    winServices_->lockWindowShared(managerGlobalLeader_) ;
    winServices_->updateFromLockedWindow(managerGlobalLeader_, this, &CServicesManager::servicesDumpIn) ;
    winServices_->unlockWindow(managerGlobalLeader_) ;

    auto it=services_.find(std::tuple<std::string,std::string,int>(poolId,serviceId,partitionId)) ;
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

  bool CServicesManager::getServiceLeader(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& leader)
  {
    int type;
    int size ;
    int nbPartitions;
    return getServiceInfo(poolId, serviceId, partitionId, type, size, nbPartitions, leader) ;
  }

  bool CServicesManager::getServiceType(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& type)
  {
    int size ;
    int nbPartitions;
    int leader;
    return getServiceInfo(poolId, serviceId, partitionId, type, size, nbPartitions, leader) ;
  }

  bool CServicesManager::getServiceNbPartitions(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& nbPartitions)
  {
    int size ;
    int type;
    int leader;
    return getServiceInfo(poolId, serviceId, partitionId, type, size, nbPartitions, leader) ;
  }

  bool CServicesManager::hasService(const std::string& poolId, const std::string& serviceId, const int& partitionId)
  {
    winServices_->lockWindow(managerGlobalLeader_,0) ;
    winServices_->updateFromWindow(managerGlobalLeader_, this, &CServicesManager::servicesDumpIn) ;
    winServices_->unlockWindow(managerGlobalLeader_,0) ;
    auto it=services_.find(std::tuple<std::string, std::string, int>(poolId, serviceId, partitionId)) ;
    if ( it == services_.end()) return false ;
    else return true ;
  }

}