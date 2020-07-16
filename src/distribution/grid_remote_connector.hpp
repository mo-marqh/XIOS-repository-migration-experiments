#ifndef __GRID_REMOTE_CONNECTOR_HPP__
#define __GRID_REMOTE_CONNECTOR_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "mpi.hpp"
#include "local_view.hpp"
#include "distributed_view.hpp"
#include "context_client.hpp"


namespace xios
{
 
  class CGridRemoteConnector
  {

    public:

      CGridRemoteConnector(vector<CLocalView*>& srcView, vector<CDistributedView*>& dstView, MPI_Comm localComm) ;
      void computeConnector(void) ;
      void computeGenericMethod(void) ;
      std::map<int, CArray<size_t,1>>& getDistributedGlobalIndex(int pos) { return elements_[pos] ;} 

    private:
      vector<map<int, CArray<size_t,1>>> elements_ ;
      vector<CLocalView*> srcView_ ;
      vector<CDistributedView*> dstView_ ;
      MPI_Comm localComm_ ;

  } ;

}

#endif