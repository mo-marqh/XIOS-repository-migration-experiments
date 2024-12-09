#ifndef __CONTEXT_CLIENT_HPP__
#define __CONTEXT_CLIENT_HPP__

#include "xios_spl.hpp"
#include "buffer_out.hpp"
#include "buffer_in.hpp"
#include "buffer_client.hpp"
#include "event_client.hpp"
#include "event_server.hpp"
#include "mpi.hpp"
#include "registry.hpp"

namespace xios
{
  class CContext;
  class CContextServer ;
  /*!
  \class CContextClient
  A context can be both on client and on server side. In order to differenciate the role of
  context on each side, e.x client sending events, server receiving and processing events, there is a need of
  concrete "context" classes for both sides.
  CContextClient processes and sends events from client to server where CContextServer receives these events
  and processes them.
  */
  class COneSidedContextClient ;

  class CContextClient
  {
    public:
      enum ETransport { generic, legacy, legacyV2, oneSided, p2p, online}  ;
      
      template<ETransport transport=generic> 
      static CContextClient* getNew(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* parentServer = 0) ;

            // Contructor
      CContextClient(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* parentServer = 0);
      virtual ~CContextClient() {} 

      bool isServerLeader(void) const;
      bool isServerNotLeader(void) const;
      const std::list<int>& getRanksServerLeader(void) const;
      const std::list<int>& getRanksServerNotLeader(void) const;
      static void computeLeader(int clientRank, int clientSize, int serverSize,
                                std::list<int>& rankRecvLeader,
                                std::list<int>& rankRecvNotLeader);
      int getRemoteSize(void) {return serverSize_;}
      int getServerSize(void) {return serverSize_;}
      MPI_Comm getIntraComm(void)  {return intraComm_ ;} 
      int getIntraCommSize(void) {return clientSize_ ;}
      int getIntraCommRank(void) {return clientRank_ ;}
      /*! set the associated server (dual chanel client/server) */      
      void setAssociatedServer(CContextServer* associatedServer) { associatedServer=associatedServer_;}
      /*! get the associated server (dual chanel client/server) */      
      CContextServer* getAssociatedServer(void) { return associatedServer_;}
      


      virtual ETransport getType(void) = 0 ;
      // Send event to server
      virtual void sendEvent(CEventClient& event)=0;
      virtual void eventLoop(void)=0 ;
      virtual void releaseBuffers(void)=0;
      virtual bool havePendingRequests(void)=0;


      virtual bool isNotifiedFinalized(void)=0 ;
      virtual void finalize(void)=0;

      virtual void setBufferSize(const std::map<int,StdSize>& mapSize)=0;

      public: 
        static CContextClient* ONLINE(void) { return reinterpret_cast<CContextClient*>(0xdeaddead);}
    protected:

      CContext* context_; //!< Context for client

      CContext* parentServer_; //!< Context for server (Only used in attached mode)

      int clientRank_; //!< Rank of current client

      int clientSize_; //!< Size of client group

      int serverSize_; //!< Size of server group

      MPI_Comm interComm_; //!< Communicator of server group (interCommunicator)

      MPI_Comm intraComm_; //!< Communicator of client group
     
      std::list<int> ranksServerLeader_; //!< List of server ranks for which the client is leader

      std::list<int> ranksServerNotLeader_; //!< List of server ranks for which the client is not leader

      size_t hashId_ ; //!< hash id on the context client that will be used for context server to identify the remote calling context client.

      CContextServer* associatedServer_ ; //!< The server associated to the pair client/server
  };

  template<>
  CContextClient* CContextClient::getNew<CContextClient::generic>(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* parentServer) ;
 
  template<>
  CContextClient* CContextClient::getNew<CContextClient::oneSided>(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* parentServer) ;

  template<>
  CContextClient* CContextClient::getNew<CContextClient::p2p>(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* parentServer) ;

  template<>
  CContextClient* CContextClient::getNew<CContextClient::legacy>(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* parentServer) ;

  template<>
  CContextClient* CContextClient::getNew<CContextClient::legacyV2>(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* parentServer) ;

  template<>
  CContextClient* CContextClient::getNew<CContextClient::online>(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* parentServer) ;


}

#endif // __CONTEXT_CLIENT_HPP__
