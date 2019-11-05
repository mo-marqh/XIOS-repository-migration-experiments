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
    MPI_Comm_split(xiosComm,isXiosServer,commRank,&splitComm) ;
    
    CXios::launchRessourcesManager(isXiosServer) ;
    CXios::launchServicesManager(isXiosServer) ;
    CXios::launchContextsManager(isXiosServer) ;
    if (isXiosServer) CServer::launchServersRessource(splitComm) ;
    MPI_Comm_free(&splitComm) ;
  }

  CDaemonsManager::~CDaemonsManager()
  {
    CXios::finalizeContextsManager() ;
    CXios::finalizeServicesManager() ;
    CXios::finalizeRessourcesManager() ;
  }

  bool CDaemonsManager::eventLoop(void)
  {
    CXios::getRessourcesManager()->eventLoop() ;
    CXios::getServicesManager()->eventLoop() ;
    CXios::getContextsManager()->eventLoop() ;
    if (isServer_) return CServer::getServersRessource()->eventLoop(false) ;
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

}