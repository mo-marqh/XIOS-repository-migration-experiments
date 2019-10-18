#include "daemons_manager.hpp"
#include "services_manager.hpp"
#include "ressources_manager.hpp"
#include "cxios.hpp"
#include "pool_ressource.hpp"
#include "type.hpp"
#include "server.hpp"
#include "servers_ressource.hpp"

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

  bool CServicesManager::createServices(const std::string& poolId, const std::string& serviceId, 
                                        int type, int size, int nbPartitions, bool wait) 
  {

    int leader ;
    int poolSize ;
    
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
      createServicesNotify(leader, serviceId, type, size, nbPartitions) ;
      return true ;
    }
    else return false ;
  }


  void CServicesManager::createServicesNotify(int rank, const string& serviceId, int type, int size, int nbPartitions)
  {
    winNotify_->lockWindow(rank,0) ;
    winNotify_->updateFromWindow(rank, this, &CServicesManager::notificationsDumpIn) ;
    notifications_.push_back(std::make_tuple(serviceId,type,size,nbPartitions)) ;
    winNotify_->updateToWindow(rank, this, &CServicesManager::notificationsDumpOut) ;
    winNotify_->unlockWindow(rank,0) ;
  }

  
  void CServicesManager::checkCreateServicesNotification(void)
  {
    int commRank ;
    MPI_Comm_rank(xiosComm_,&commRank) ;
    winNotify_->lockWindow(commRank,0) ;
    winNotify_->updateFromWindow(commRank, this, &CServicesManager::notificationsDumpIn) ;
    
    if (!notifications_.empty())
    {
      auto info = notifications_.front() ;
      CServer::getServersRessource()->getPoolRessource()->createService(get<0>(info), get<1>(info), get<2>(info), get<3>(info)) ;
      notifications_.pop_front() ;
      winNotify_->updateToWindow(commRank, this, &CServicesManager::notificationsDumpOut) ;     
    }
    winNotify_->unlockWindow(commRank,0) ;

  }

  void CServicesManager::eventLoop(void)
  {
    checkCreateServicesNotification() ;
  }

  
  void CServicesManager::notificationsDumpOut(CBufferOut& buffer)
  {
    
    buffer.realloc(maxBufferSize_) ;
   
    buffer<<(int)notifications_.size();
    
    for(auto it=notifications_.begin();it!=notifications_.end(); ++it) 
      buffer << std::get<0>(*it) << static_cast<int>(std::get<1>(*it))<< std::get<2>(*it) << std::get<3>(*it)  ;
  }

  void CServicesManager::notificationsDumpIn(CBufferIn& buffer)
  {
    std::string id ;
    int type ;
    int size; 
    int nbPartitions ;

    notifications_.clear() ;
    int nbNotifications ;
    buffer>>nbNotifications ;
    for(int i=0;i<nbNotifications;i++) 
    {
      buffer>>id>>type>>size>>nbPartitions ;
      notifications_.push_back(std::make_tuple(id,type,size,nbPartitions)) ;
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
    
    winServices_->lockWindow(managerGlobalLeader_,0) ;
    winServices_->updateFromWindow(managerGlobalLeader_, this, &CServicesManager::servicesDumpIn) ;
    services_[std::tuple<std::string, std::string,int>(poolId,serviceId,partitionId)]=std::make_tuple(type,size,nbPartitions,leader) ;
    winServices_->updateToWindow(managerGlobalLeader_, this, &CServicesManager::servicesDumpOut) ;
    winServices_->unlockWindow(managerGlobalLeader_,0) ;
  }

  bool CServicesManager::getServiceInfo(const std::string& poolId, const std::string& serviceId, const int& partitionId, int& type, 
                                        int& size, int& nbPartitions, int& leader)
  {
    winServices_->lockWindow(managerGlobalLeader_,0) ;
    winServices_->updateFromWindow(managerGlobalLeader_, this, &CServicesManager::servicesDumpIn) ;
    winServices_->unlockWindow(managerGlobalLeader_,0) ;

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