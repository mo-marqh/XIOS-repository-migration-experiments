#ifndef __CONTEXT_SERVER_HPP__
#define __CONTEXT_SERVER_HPP__
#include "xios_spl.hpp"
#include "event_server.hpp"
#include "buffer_server.hpp"
#include "mpi.hpp"
#include "event_scheduler.hpp"

namespace xios
{
  class CContext ;
  class CContextClient;

  class CContextServer
  {
    public:

    CContextServer(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm) ;
    bool eventLoop(bool enableEventsProcessing = true);
    void listen(void) ;
//    bool listenPendingRequest(MPI_Status& status) ;
    bool listenPendingRequest(MPI_Message &message, MPI_Status& status) ;
    void checkPendingProbe(void) ;
    void checkPendingRequest(void) ;
    void getBufferFromClient(size_t timeLine) ;
    void processRequest(int rank, char* buff,int count) ;
    void processEvents(void) ;
    bool hasFinished(void);
    void dispatchEvent(CEventServer& event) ;
    bool isCollectiveEvent(CEventServer& event) ;
    void setPendingEvent(void) ;
    bool hasPendingEvent(void) ;
    bool isAttachedModeEnabled() const;
    void releaseBuffers(void) ;
    void notifyClientsFinalize(void) ;
    void freeWindows(void) ; // !<< free Windows for one sided communication
    
    MPI_Comm intraComm ;
    int intraCommSize ;
    int intraCommRank ;

    MPI_Comm interComm ;
    int commSize ;
    int clientSize_ ;

    MPI_Comm interCommMerged_; //!< Communicator of the client group + server group (intraCommunicator) needed for one sided communication.
    MPI_Comm commSelf_ ; //!< Communicator for proc alone from interCommMerged 

    map<int,CServerBuffer*> buffers ;
    map<int,size_t> lastTimeLine ; //!< last event time line for a processed request
    map<int,size_t>::iterator itLastTimeLine ; //!< iterator on lastTimeLine
    map<int, list<std::pair<MPI_Message,MPI_Status> > > pendingProbe;
    map<int,MPI_Request> pendingRequest ;
    map<int,char*> bufferRequest ;

    map<size_t,CEventServer*> events ;
    size_t currentTimeLine ;
    CContext* context ;
      
    bool finished ;
    bool pendingEvent ;
    bool scheduled  ;    /*!< event of current timeline is alreading scheduled ? */
    bool attachedMode ;  //! true if attached mode is enabled otherwise false
    bool pureOneSided ; //!< if true, client will communicated with servers only trough one sided communication. Otherwise the hybrid mode P2P /One sided is used.
         
    size_t hashId ;

    void setAssociatedClient(CContextClient* associatedClient) {associatedClient_=associatedClient ;}
    CContextClient* getAssociatedClient(void) { return associatedClient_ ;}

    ~CContextServer() ;

    private:
  
      std::map<int, StdSize> mapBufferSize_;
      std::map<int,MPI_Comm> winComm_ ; //! Window communicators
      std::map<int,std::vector<MPI_Win> >windows_ ; //! one sided mpi windows to expose client buffers to servers ; No memory will be attached on server side.
      CEventScheduler* eventScheduler_ ;
      bool isProcessingEvent_ ;
      CContextClient* associatedClient_ ;
      size_t remoteHashId_; //!< the hash is of the calling context client
      
      MPI_Comm processEventBarrier_ ;
      bool eventScheduled_=false;
      MPI_Request processEventRequest_ ;
  } ;

}

#endif
