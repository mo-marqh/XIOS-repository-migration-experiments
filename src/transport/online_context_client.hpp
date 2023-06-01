#ifndef __ONLINE_CONTEXT_CLIENT_HPP__
#define __ONLINE_CONTEXT_CLIENT_HPP__

#include "xios_spl.hpp"
#include "buffer_out.hpp"
#include "buffer_in.hpp"
#include "buffer_client.hpp"
#include "event_client.hpp"
#include "event_server.hpp"
#include "mpi.hpp"
#include "registry.hpp"
#include "context_client.hpp"

namespace xios
{
  class CContext;
  class CContextServer ;
  /*!
  \class CLegacyContextClient
  A context can be both on client and on server side. In order to differenciate the role of
  context on each side, e.x client sending events, server receiving and processing events, there is a need of
  concrete "context" classes for both sides.
  CLegacyContextClient processes and sends events from client to server where CContextServer receives these events
  and processes them.
  */
  class COnlineContextClient : public CContextClient
  {
    public:
      // Contructor
      COnlineContextClient(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* parentServer = 0):
                          CContextClient(parent, intraComm, interComm, parentServer) {}

      // Send event to server
      ETransport getType(void) { return online ;}
      
      void sendEvent(CEventClient& event) { ERROR("void COnlineContextClient::sendEvent(CEventClient& event)",<< "this method may not be called") ;}
      void eventLoop(void) { ERROR("void COnlineContextClient::eventLoop(void)",<< "this method may not be called") ;}
      void releaseBuffers(void) { ERROR("void COnlineContextClient::releaseBuffers(void)",<< "this method may not be called") ;}
      bool havePendingRequests(void) { ERROR(" bool COnlineContextClient::havePendingRequests(void)",<< "this method may not be called") ; return false ;}

      bool isNotifiedFinalized(void) { ERROR("bool COnlineContextClient::isNotifiedFinalized(void)",<< "this method may not be called") ;return false ;}
      void finalize(void) { ERROR("void COnlineContextClient::finalize(void)",<< "this method may not be called") ;}

      void setBufferSize(const std::map<int,StdSize>& mapSize) { ERROR("void setBufferSize(const std::map<int,StdSize>& mapSize)",<< "this method may not be called") ;}


    private:

  };
}

#endif // __Online_CONTEXT_CLIENT_HPP__
