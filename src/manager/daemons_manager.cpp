#include "daemons_manager.hpp"
#include "cxios.hpp"
#include "ressources_manager.hpp"
#include "services_manager.hpp"
#include "contexts_manager.hpp"
#include "servers_ressource.hpp"
#include "server.hpp"

namespace xios
{
  using namespace std ;

  CDaemonsManager::CDaemonsManager(bool isXiosServer) : isServer_(isXiosServer)
  {
    MPI_Comm xiosComm = CXios::getXiosComm() ;
    int commRank ;
    MPI_Comm_rank(xiosComm,&commRank) ;
    MPI_Comm splitComm ;
    xios::MPI_Comm_split(xiosComm,isXiosServer,commRank,&splitComm) ;
    
    int isMasterNotification ;
    if (isXiosServer)
    {
      MPI_Comm_rank(splitComm,&commRank) ;
      if (commRank==0) isMasterNotification=true ;
      else isMasterNotification=false ;
    }
    else 
    {
      int xiosSize, clientSize ;
      MPI_Comm_size(xiosComm,&xiosSize) ;
      MPI_Comm_size(splitComm,&clientSize) ;
      if (clientSize == xiosSize)
      {
        MPI_Comm_rank(splitComm,&commRank) ;
        if (commRank==0) isMasterNotification=true ;
        else isMasterNotification=false ;
      }
      else isMasterNotification=false ;
    }
    CXios::launchNotificationsManager(isMasterNotification) ;

    CXios::launchRegistryManager(isXiosServer) ;
    CXios::launchRessourcesManager(isXiosServer) ;
    CXios::launchServicesManager(isXiosServer) ;
    CXios::launchContextsManager(isXiosServer) ;
    CXios::launchCouplerManager(isXiosServer) ;
    CXios::launchThreadManager(isXiosServer) ;
 
    if (isXiosServer) CServer::launchServersRessource(splitComm) ;
    MPI_Barrier(xiosComm) ;
    xios::MPI_Comm_free(&splitComm) ;
  }

  CDaemonsManager::~CDaemonsManager()
  {
    finalize() ;
  }

  bool CDaemonsManager::eventLoop(void)
  {
    CXios::getNotificationsManager()->eventLoop() ;
    CXios::getRessourcesManager()->eventLoop() ;
    CXios::getServicesManager()->eventLoop() ;
    CXios::getContextsManager()->eventLoop() ;
    CXios::getCouplerManager()->eventLoop() ;
    if (isServer_) 
    {
      if (CServer::isRoot) 
      {
        CServer::listenOasisEnddef() ;
        CServer::listenRootOasisEnddef() ;
      }
      else CServer::listenRootOasisEnddef() ;
      
      if (CThreadManager::isUsingThreads()) return CServer::getServersRessource()->isFinished() ;
      else return CServer::getServersRessource()->eventLoop(false) ;
    }
    else  return CXios::getPoolRessource()->eventLoop(false) ;
  }
  
  bool CDaemonsManager::servicesEventLoop(void)
  {
    CXios::getRessourcesManager()->eventLoop() ;
    CXios::getServicesManager()->eventLoop() ;
    CXios::getContextsManager()->eventLoop() ;
    if (isServer_) return CServer::getServersRessource()->eventLoop(true) ;
    else  return CXios::getPoolRessource()->eventLoop(true) ;
  } 

  bool CDaemonsManager::finalize(void)
  {
    if (!isFinalized_)
    {
      if (isServer_) CServer::getServersRessource()->finalizeSignal() ;
      else CXios::getPoolRessource()->finalizeSignal() ;
      while(!eventLoop()) ;
      MPI_Barrier( CXios::getXiosComm()) ;
      CXios::finalizeContextsManager() ;
      CXios::finalizeCouplerManager() ;
      CXios::finalizeServicesManager() ;
      CXios::finalizeRessourcesManager() ;
      CXios::finalizeRegistryManager() ;
      CXios::finalizeThreadManager() ;
      CXios::finalizeNotificationsManager() ;
      isFinalized_=true ;
    }
    return isFinalized_;
  }

}
