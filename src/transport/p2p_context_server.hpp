#ifndef __P2P_CONTEXT_SERVER_HPP__
#define __P2P_CONTEXT_SERVER_HPP__
#include "xios_spl.hpp"
#include "event_server.hpp"
#include "buffer_server.hpp"
#include "p2p_server_buffer.hpp"
#include "p2p_server_base.hpp"
#include "mpi.hpp"
#include "event_scheduler.hpp"
#include "context_server.hpp"

namespace xios
{
  class CContext ;
  class CContextClient;

  class CP2pContextServer : public CContextServer, public CP2pServerBase
  {
    
    public:

      CP2pContextServer(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm) ;
      ~CP2pContextServer() ;
      bool eventLoop(bool enableEventsProcessing = true);
      void releaseBuffers(void) ;


    private:

      class CRequest
      {
        public : 
          CRequest(MPI_Comm& interComm, MPI_Status& status)
          {
            rank_=status.MPI_SOURCE ;
            MPI_Get_count(&status,MPI_CHAR,&count_);
            buffer_.resize(count_) ;
            MPI_Irecv(buffer_.data(), count_, MPI_CHAR, rank_, 20, interComm, &request_) ;
          }
          
          ~CRequest() { }

          bool test(void)
          {
            int flag ;
            MPI_Status status ;
            MPI_Test(&request_, &flag, &status) ;
            if (flag==true) return true ;
            else return false ;
          }
          int getCount(void) {return count_ ;}
          int getRank(void) {return rank_ ;}
          vector<char>& getBuffer(void) { return buffer_;}

        private:
          int rank_ ;
          int count_ ;
          vector<char> buffer_ ;
          MPI_Request request_ ;
      };
    
   
    void listen(void) ;
    void listenPendingRequest(void) ;
    void processRequest(CRequest& request) ;
    void checkBuffers(void) ;
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

    map<int,CP2pServerBuffer*> buffers_ ;
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

    private:
  
      std::map<int, StdSize> mapBufferSize_;
      std::map<int,MPI_Comm> winComm_ ; //! Window communicators
      std::map<int,std::vector<MPI_Win> >windows_ ; //! one sided mpi windows to expose client buffers to servers ; No memory will be attached on server side.
      bool isProcessingEvent_ ;
      size_t remoteHashId_; //!< the hash is of the calling context client
      
      MPI_Comm processEventBarrier_ ;
      bool eventScheduled_=false;
      MPI_Request processEventRequest_ ;

      std::map<int,std::list<CRequest*> >requests_ ;

      std::map<size_t, SPendingEvent> pendingEvents_   ;
      std::map<size_t, SPendingEvent> completedEvents_ ;

  } ;

}

#endif
