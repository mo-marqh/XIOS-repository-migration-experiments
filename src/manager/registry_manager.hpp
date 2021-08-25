#ifndef __REGISTRY_MANAGER_HPP__
#define __REGISTRY_MANAGER_HPP__

#include "xios_spl.hpp"
#include "registry.hpp"
#include "mpi.hpp"

namespace xios
{
  class CRegistryManager
  {
    
    public:
      
    CRegistryManager(bool isXiosServer) ;
    ~CRegistryManager() { finalize() ;}
    const CRegistry& getRegistryIn(void)  {return *registryIn_ ;}
    const CRegistry& getRegistryOut(void) {return *registryOut_ ;}

    void merge(const CRegistry& inRegistry) { registryOut_->mergeRegistry(inRegistry) ;}
    
    private:
      void finalize(void) ;

      const std::string registryInFileName {"registry.bin"} ;
      const std::string registryOutFileName {"registry.bin"} ;
      MPI_Comm xiosComm_ ;
      int managerGlobalLeader_ ;
      bool isManagerGlobalLeader_ ;
      CRegistry* registryIn_ ;
      CRegistry* registryOut_ ;
  } ;
}

#endif