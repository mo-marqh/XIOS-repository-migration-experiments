#include "xios_spl.hpp"
#include "p2p_context_client.hpp"
#include "context_server.hpp"
#include "event_client.hpp"
#include "buffer_out.hpp"
#include "type.hpp"
#include "event_client.hpp"
#include "context.hpp"
#include "mpi.hpp"
#include "timer.hpp"
#include "cxios.hpp"
#include "server.hpp"
#include "services.hpp"
#include <random>
#include <chrono>

namespace xios
{
    /*!
    \param [in] parent Pointer to context on client side
    \param [in] intraComm_ communicator of group client
    \param [in] interComm_ communicator of group server
    \cxtSer [in] cxtSer Pointer to context of server side. (It is only used in case of attached mode --> obsolete ).
    */
    CP2pContextClient::CP2pContextClient(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* cxtSer)
     : CContextClient(parent, intraComm, interComm, cxtSer),
       mapBufferSize_(), maxBufferedEvents_(4)
    {
      
      pureOneSided_=CXios::getin<bool>("pure_one_sided",false); // pure one sided communication (for test)

      xios::MPI_Intercomm_merge(interComm_,false, &interCommMerged_) ;
      CXios::getMpiGarbageCollector().registerCommunicator(interCommMerged_) ;
      
      xios::MPI_Comm_split(intraComm_,clientRank_,clientRank_, &commSelf_) ; // for windows
      CXios::getMpiGarbageCollector().registerCommunicator(commSelf_) ;
      eventScheduler_ = parent->getEventScheduler() ;  
      timeLine_ = 1;
    }


    /*!
    In case of attached mode, the current context must be reset to context for client
    \param [in] event Event sent to server
    */
    void CP2pContextClient::sendEvent(CEventClient& event)
    {
      list<int> ranks = event.getRanks();
 
//      ostringstream str ;
//      for(auto& rank : ranks) str<<rank<<" ; " ;
//      info(100)<<"Event "<<timeLine_<<" of context "<<context_->getId()<<"  for ranks : "<<str.str()<<endl ;

      if (CXios::checkEventSync)
      {
        int typeId, classId, typeId_in, classId_in;
        long long timeLine_out;
        long long timeLine_in( timeLine_ );
        typeId_in=event.getTypeId() ;
        classId_in=event.getClassId() ;
//        MPI_Allreduce(&timeLine_,&timeLine_out, 1, MPI_UINT64_T, MPI_SUM, intraComm_) ; // MPI_UINT64_T standardized by MPI 3
        MPI_Allreduce(&timeLine_in,&timeLine_out, 1, MPI_LONG_LONG_INT, MPI_SUM, intraComm_) ; 
        MPI_Allreduce(&typeId_in,&typeId, 1, MPI_INT, MPI_SUM, intraComm_) ;
        MPI_Allreduce(&classId_in,&classId, 1, MPI_INT, MPI_SUM, intraComm_) ;
        if (typeId/clientSize_!=event.getTypeId() || classId/clientSize_!=event.getClassId() || timeLine_out/clientSize_!=timeLine_)
        {
           ERROR("void COneSidedContextClient::sendEvent(CEventClient& event)",
               << "Event are not coherent between client for timeline = "<<timeLine_);
        }
        
        vector<int> servers(serverSize_,0) ;
        auto ranks=event.getRanks() ;
        for(auto& rank : ranks) servers[rank]=1 ;
        MPI_Allreduce(MPI_IN_PLACE, servers.data(), serverSize_,MPI_INT,MPI_SUM,intraComm_) ;
        ostringstream osstr ;
        for(int i=0;i<serverSize_;i++)  if (servers[i]==0) osstr<<i<<" , " ;
        if (!osstr.str().empty())
        {
          ERROR("void COneSidedContextClient::sendEvent(CEventClient& event)",
                 <<" Some servers will not receive the message for timeline = "<<timeLine_<<endl
                 <<"Servers are : "<<osstr.str()) ;
        }


      }
      
      event.setFirst() ;
      while(!event.isEmpty())
      {
        int rank=event.getRank() ;
        auto itBuffer=buffers_.find(rank) ;
        if (itBuffer==buffers_.end()) 
        {  
          newBuffer(rank) ;
          itBuffer=buffers_.find(rank) ;
        }
        itBuffer->second->eventLoop() ;
        double time=CTimer::getTime() ;
        bool succed = itBuffer->second->writeEvent(timeLine_, event)  ;
        if (succed) 
        {
          time=CTimer::getTime()-time ;
          if (!CTimer::get("Blocking time").isSuspended()) CTimer::get("Blocking time").minus(time) ;
        }

        if (succed) event.remove() ;
        else event.next() ;
        if (event.isFirst())
        {
          if (CTimer::get("Blocking time").isSuspended()) CTimer::get("Blocking time").resume() ;
          yield() ;
        } 
      }
      if (!CTimer::get("Blocking time").isSuspended()) CTimer::get("Blocking time").suspend() ;


      synchronize() ;
      
      timeLine_++;
    }


   void CP2pContextClient::eventLoop(void)
   {
      if (!locked_) checkBuffers() ;
   }

   void CP2pContextClient::callGlobalEventLoop(void)
   {
     locked_=true ;
     context_->globalEventLoop() ;
     locked_=false ;
   }

   void CP2pContextClient::yield(void)
   {
     locked_=true ;
     context_->yield() ;
     locked_=false ;
   }

   void CP2pContextClient::synchronize(void)
   {
     if (context_->getServiceType()!=CServicesManager::CLIENT)
     {
       locked_=true ;
       context_->synchronize() ;
       locked_=false ;
     }    
   }

   /*!
   Make a new buffer for a certain connection to server with specific rank
   \param [in] rank rank of connected server
   */
   void CP2pContextClient::newBuffer(int rank)
   {
      if (!mapBufferSize_.count(rank))
      {
        error(0) << "WARNING: Unexpected request for buffer to communicate with server " << rank << std::endl;
        mapBufferSize_[rank] = CXios::minBufferSize;
        maxEventSizes[rank] = CXios::minBufferSize;
      }

      CP2pClientBuffer* buffer = buffers_[rank] = new CP2pClientBuffer(interComm_, rank, commSelf_, interCommMerged_, clientSize_+rank );
      if (isGrowableBuffer_) { buffer->setGrowable(growingFactor_) ; }
      else buffer->setFixed(mapBufferSize_[rank]) ;
  
   }

   /*!
   Verify state of buffers. Buffer is under pending state if there is no message on it
   \return state of buffers, pending(true), ready(false)
   */
   bool CP2pContextClient::checkBuffers(void)
   {
      bool pending = false;
      for (auto itBuff : buffers_)
      {
        itBuff.second->eventLoop() ;
        pending |= !(itBuff.second->isEmpty());
      }
      return pending;
   }

   //! Release all buffers
   void CP2pContextClient::releaseBuffers()
   {
      for (auto& itBuff : buffers_) delete itBuff.second;
      buffers_.clear();
   }


   /*!
   Verify state of buffers corresponding to a connection
   \param [in] ranks list rank of server to which client connects to
   \return state of buffers, pending(true), ready(false)
   */
   bool CP2pContextClient::checkBuffers(list<int>& ranks)
   {
      bool pending = false;
      for (auto& rank : ranks) 
      {
        buffers_[rank]->eventLoop() ;
        pending |= !(buffers_[rank]->isEmpty()) ;
      }
      return pending;
   }

   /*!
    * Set the buffer size for each connection. Warning: This function is collective.
    *
    * \param [in] mapSize maps the rank of the connected servers to the size of the correspoinding buffer
    * \param [in] maxEventSize maps the rank of the connected servers to the size of the biggest event
   */
   void CP2pContextClient::setBufferSize(const std::map<int,StdSize>& mapSize)
   {
     setFixedBuffer() ;
     for(auto& it : mapSize)
     {
      size_t size=std::max(CXios::minBufferSize*1.0,std::min(it.second*CXios::bufferSizeFactor*1.01,CXios::maxBufferSize*1.0)) ;
      mapBufferSize_[it.first]=size ;
      if (buffers_.count(it.first)>0) buffers_[it.first]->setFixed(size);
     }
   }


   /*!
   * Finalize context client and do some reports. Function is non-blocking.
   */
  void CP2pContextClient::finalize(void)
  {
    bool stop = false;

    int* nbServerConnectionLocal  = new int[serverSize_] ;
    int* nbServerConnectionGlobal  = new int[serverSize_] ;
    for(int i=0;i<serverSize_;++i) nbServerConnectionLocal[i]=0 ;
    for (auto itBuff = buffers_.begin(); itBuff != buffers_.end(); itBuff++)  nbServerConnectionLocal[itBuff->first]=1 ;
    for (auto ItServerLeader = ranksServerLeader_.begin(); ItServerLeader != ranksServerLeader_.end(); ItServerLeader++)  nbServerConnectionLocal[*ItServerLeader]=1 ;
    
    MPI_Allreduce(nbServerConnectionLocal, nbServerConnectionGlobal, serverSize_, MPI_INT, MPI_SUM, intraComm_);
    
    CEventClient event(CContext::GetType(), CContext::EVENT_ID_CONTEXT_FINALIZE);
    CMessage msg;

    for (int i=0;i<serverSize_;++i) if (nbServerConnectionLocal[i]==1) event.push(i, nbServerConnectionGlobal[i], msg) ;
    sendEvent(event);

    delete[] nbServerConnectionLocal ;
    delete[] nbServerConnectionGlobal ;


    CTimer::get("Blocking time").resume();
    checkBuffers();
    CTimer::get("Blocking time").suspend();

    std::map<int,StdSize>::const_iterator itbMap = mapBufferSize_.begin(),
                                          iteMap = mapBufferSize_.end(), itMap;

    StdSize totalBuf = 0;
    for (itMap = itbMap; itMap != iteMap; ++itMap)
    {
      report(10) << " Memory report : Context <" << context_->getId() << "> : client side : memory used for buffer of each connection to server" << endl
                 << "  +) To server with rank " << itMap->first << " : " << itMap->second << " bytes " << endl;
      totalBuf += itMap->second;
    }
    report(0) << " Memory report : Context <" << context_->getId() << "> : client side : total memory used for buffer " << totalBuf << " bytes" << endl;

  }


  /*!
  */
  bool CP2pContextClient::havePendingRequests(void)
  {
    return checkBuffers();
  }
  
  bool CP2pContextClient::havePendingRequests(list<int>& ranks)
  {
    return checkBuffers(ranks) ;
  }

  bool CP2pContextClient::isNotifiedFinalized(void)
  {

    bool finalized = true;
    for (auto& it : buffers_ ) finalized &= it.second->isNotifiedFinalized();
    return finalized;
  }

}
