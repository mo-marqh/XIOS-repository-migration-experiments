#include "registry_manager.hpp"
#include "cxios.hpp"

namespace xios
{
  CRegistryManager::CRegistryManager(bool isXiosServer)
  {
    
    int commRank ;  
    xiosComm_ = CXios::getXiosComm() ;
    MPI_Comm_rank(xiosComm_, &commRank) ;
    managerGlobalLeader_ = 0 ;
    if (commRank==managerGlobalLeader_) isManagerGlobalLeader_=true;
    else isManagerGlobalLeader_=false;
    registryIn_ = new CRegistry(xiosComm_) ;
    if (commRank==managerGlobalLeader_) registryIn_->fromFile(registryInFileName) ;
    registryIn_->bcastRegistry() ;
    registryOut_= new CRegistry(xiosComm_) ;
  }

  void CRegistryManager::finalize(void)
  {
    delete registryIn_ ;
    registryOut_->hierarchicalGatherRegistry() ;
    if (isManagerGlobalLeader_) registryOut_->toFile(registryOutFileName) ;
    delete registryOut_ ;
  }
}