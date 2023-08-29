#ifndef __LEGACY_CONTEXT_CLIENT_HPP__
#define __LEGACY_CONTEXT_CLIENT_HPP__

#include "xios_spl.hpp"
#include "buffer_out.hpp"
#include "buffer_in.hpp"
#include "buffer_client.hpp"
#include "event_client.hpp"
#include "event_server.hpp"
#include "mpi.hpp"
#include "registry.hpp"
#include "context_client.hpp"
#include "window_dynamic.hpp"

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
  class CLegacyContextClient : public CContextClient
  {
    public:
      // Contructor
      CLegacyContextClient(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* parentServer = 0);

      // Send event to server
      ETransport getType(void) {return legacy ;}
      void sendEvent(CEventClient& event);
      void eventLoop(void) ;
      void releaseBuffers(void);
      bool havePendingRequests(void);

      bool isNotifiedFinalized(void) ;
      void finalize(void);

      void setBufferSize(const std::map<int,StdSize>& mapSize);


    private:

      // Functions to set/get buffers
      void getBuffers(const size_t timeLine, const list<int>& serverList, const list<int>& sizeList, list<CBufferOut*>& retBuffers);
      void newBuffer(int rank);
      bool checkAttachWindows(CClientBuffer* buffer , int rank, map<int,MPI_Request>& attachList) ;
      bool checkBuffers(list<int>& ranks);
      bool checkBuffers(void);
      void callGlobalEventLoop() ;
      void yield(void) ;
      void synchronize(void) ;
      bool havePendingRequests(list<int>& ranks) ;
      void setGrowableBuffer(void) { isGrowableBuffer_=true;}
      void setFixedBuffer(void) { isGrowableBuffer_=false;}
      void lockBuffers(list<int>& ranks) ;
      void unlockBuffers(list<int>& ranks) ;


      size_t timeLine; //!< Timeline of each event

      MPI_Comm interCommMerged_; //!< Communicator of the client group + server group (intraCommunicator) needed for one sided communication.
      MPI_Comm commSelf_ ; //!< Communicator for proc alone from interCommMerged 

      map<int,CClientBuffer*> buffers; //!< Buffers for connection to servers

      bool pureOneSided ; //!< if true, client will communicated with servers only trough one sided communication. Otherwise the hybrid mode P2P /One sided is used.
      
      //! Mapping of server and buffer size for each connection to server
      std::map<int,StdSize> mapBufferSize_;
      //! Maximum event sizes estimated for each connection to server
      std::map<int,StdSize> maxEventSizes;
      //! Maximum number of events that can be buffered
      StdSize maxBufferedEvents;

      std::map<int, MPI_Comm> winComm_ ; //! Window communicators
      std::map<int, std::vector<CWindowDynamic*> >windows_ ; //! one sided mpi windows to expose client buffers to servers == windows[nbServers][2]
      bool isGrowableBuffer_ = true ;

      double latency_=0e-2 ;

      bool locked_ = false ; //!< The context client is locked to avoid recursive checkBuffer
      shared_ptr<CEventScheduler> eventScheduler_ ;
  };
}

#endif // __LEGACY_CONTEXT_CLIENT_HPP__
