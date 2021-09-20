#ifndef __GRID_CLIENT_SERVER_REMOTE_CONNECTOR_HPP__
#define __GRID_CLIENT_SERVER_REMOTE_CONNECTOR_HPP__

#include "grid_remote_connector.hpp"

namespace xios
{
 
  class CGridClientServerRemoteConnector : public CGridRemoteConnector
  {

    public:

      CGridClientServerRemoteConnector(vector<CLocalView*>& srcView, vector<CLocalView*>& worflowSrcView, vector<CDistributedView*>& dstView, MPI_Comm localComm, int remoteSize) ;
      void computeConnector(void) ;
      vector<CLocalView*> srcWorkflowView_ ;   
  } ;

}

#endif