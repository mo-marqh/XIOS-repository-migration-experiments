#ifndef __GRID_CLIENT_SERVER_REMOTE_CONNECTOR_HPP__
#define __GRID_CLIENT_SERVER_REMOTE_CONNECTOR_HPP__

#include "grid_remote_connector.hpp"

namespace xios
{
 
  class CGridClientServerRemoteConnector : public CGridRemoteConnector
  {

    public:

      CGridClientServerRemoteConnector(vector<shared_ptr<CLocalView>>& srcView, vector<shared_ptr<CLocalView>>& worflowSrcView, vector<shared_ptr<CDistributedView>>& dstView, MPI_Comm localComm, int remoteSize) ;
      void computeConnector(bool eliminateRedondant=true) ;
      void computeConnectorOut(void) ;
      void computeConnectorIn(void) ;
      vector<shared_ptr<CLocalView>> srcWorkflowView_ ;   
  } ;

}

#endif