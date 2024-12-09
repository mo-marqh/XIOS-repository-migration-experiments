#ifndef __LEGACY_CONTEXT_SERVER_HPP__
#define __LEGACY_CONTEXT_SERVER_HPP__
#include "xios_spl.hpp"
#include "event_server.hpp"
#include "buffer_server.hpp"
#include "mpi.hpp"
#include "event_scheduler.hpp"
#include "context_server.hpp"

namespace xios
{
  class CContext ;
  class CContextClient;

  class CLegacyContextServer : public CContextServer
  {
    public:

    CLegacyContextServer(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm) ;
    bool eventLoop(bool enableEventsProcessing = true);
    void releaseBuffers(void) ;

    private:

    void listen(void) ;
//    bool listenPendingRequest(MPI_Status& status) ;
    bool listenPendingRequest(MPI_Message &message, MPI_Status& status) ;
    void checkPendingProbe(void) ;
    void checkPendingRequest(void) ;
    void getBufferFromClient(size_t timeLine) ;
    void processRequest(int rank, char* buff,int count) ;
    void processEvents(bool enableEventsProcessing) ;
    bool hasFinished(void);
    void dispatchEvent(CEventServer& event) ;
    bool isCollectiveEvent(CEventServer& event) ;
    void setPendingEvent(void) ;
    bool hasPendingEvent(void) ;
    void notifyClientsFinalize(void) ;
    void freeWindows(void) ; // !<< free Windows for one sided communication

    MPI_Comm interCommMerged_; //!< Communicator of the client group + server group (intraCommunicator) needed for one sided communication.
    MPI_Comm commSelf_ ; //!< Communicator for proc alone from interCommMerged 

    map<int,CServerBuffer*> buffers_ ;
    map<int,size_t> lastTimeLine_ ; //!< last event time line for a processed request
    map<int,size_t>::iterator itLastTimeLine_ ; //!< iterator on lastTimeLine
    map<int, list<std::pair<MPI_Message,MPI_Status> > > pendingProbe_;
    map<int,MPI_Request> pendingRequest_ ;
    map<int,char*> bufferRequest_ ;

    map<size_t,CEventServer*> events_ ;
    size_t currentTimeLine_ ;
      
    bool finished_ ;
    bool pendingEvent_ ;
    bool scheduled_  ;    /*!< event of current timeline is alreading scheduled ? */
    bool pureOneSided_ ; //!< if true, client will communicated with servers only trough one sided communication. Otherwise the hybrid mode P2P /One sided is used.

    ~CLegacyContextServer() ;
  
      std::map<int, StdSize> mapBufferSize_;
      std::map<int,MPI_Comm> winComm_ ; //! Window communicators
      std::map<int,std::vector<CWindowDynamic*> >winDynamics_ ; //! one sided mpi windows to expose client buffers to servers ; No memory will be attached on server side.
      std::map<int,std::vector<CWindowDynamicView*> >windows_ ; //! one sided mpi windows to expose client buffers to servers ; No memory will be attached on server side.
      bool isProcessingEvent_ ;
      size_t remoteHashId_; //!< the hash is of the calling context client
      
      MPI_Comm processEventBarrier_ ;
      bool eventScheduled_=false;
      MPI_Request processEventRequest_ ;
  } ;

}

#endif
