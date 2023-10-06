#include "xios_spl.hpp"
#include "one_sided_context_client.hpp"
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
#include <boost/functional/hash.hpp>
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
    COneSidedContextClient::COneSidedContextClient(CContext* parent, MPI_Comm intraComm_, MPI_Comm interComm_, CContext* cxtSer)
     : CContextClient(parent, intraComm_, interComm_, cxtSer),
       mapBufferSize_(), maxBufferedEvents(4)
    {
      
      pureOneSided=CXios::getin<bool>("pure_one_sided",false); // pure one sided communication (for test)

      xios::MPI_Intercomm_merge(interComm_,false, &interCommMerged_) ;
      
      xios::MPI_Comm_split(intraComm_,clientRank,clientRank, &commSelf_) ; // for windows
      eventScheduler_ = parent->getEventScheduler() ;  
      timeLine = 1;
    }


    /*!
    In case of attached mode, the current context must be reset to context for client
    \param [in] event Event sent to server
    */
    void COneSidedContextClient::sendEvent(CEventClient& event)
    {
      list<int> ranks = event.getRanks();
 
//      ostringstream str ;
//      for(auto& rank : ranks) str<<rank<<" ; " ;
//      info(100)<<"Event "<<timeLine<<" of context "<<context_->getId()<<"  for ranks : "<<str.str()<<endl ;

      if (CXios::checkEventSync)
      {
        int typeId, classId, typeId_in, classId_in;
        long long timeLine_out;
        long long timeLine_in( timeLine );
        typeId_in=event.getTypeId() ;
        classId_in=event.getClassId() ;
//        MPI_Allreduce(&timeLine,&timeLine_out, 1, MPI_UINT64_T, MPI_SUM, intraComm) ; // MPI_UINT64_T standardized by MPI 3
        MPI_Allreduce(&timeLine_in,&timeLine_out, 1, MPI_LONG_LONG_INT, MPI_SUM, intraComm) ; 
        MPI_Allreduce(&typeId_in,&typeId, 1, MPI_INT, MPI_SUM, intraComm) ;
        MPI_Allreduce(&classId_in,&classId, 1, MPI_INT, MPI_SUM, intraComm) ;
        if (typeId/clientSize!=event.getTypeId() || classId/clientSize!=event.getClassId() || timeLine_out/clientSize!=timeLine)
        {
           ERROR("void COneSidedContextClient::sendEvent(CEventClient& event)",
               << "Event are not coherent between client for timeline = "<<timeLine);
        }
        
        vector<int> servers(serverSize,0) ;
        auto ranks=event.getRanks() ;
        for(auto& rank : ranks) servers[rank]=1 ;
        MPI_Allreduce(MPI_IN_PLACE, servers.data(), serverSize,MPI_INT,MPI_SUM,intraComm) ;
        ostringstream osstr ;
        for(int i=0;i<serverSize;i++)  if (servers[i]==0) osstr<<i<<" , " ;
        if (!osstr.str().empty())
        {
          ERROR("void COneSidedContextClient::sendEvent(CEventClient& event)",
                 <<" Some servers will not receive the message for timeline = "<<timeLine<<endl
                 <<"Servers are : "<<osstr.str()) ;
        }


      }
      
      event.setFirst() ;
      while(!event.isEmpty())
      {
        int rank=event.getRank() ;
        auto itBuffer=buffers.find(rank) ;
        if (itBuffer==buffers.end()) 
        {  
          newBuffer(rank) ;
          itBuffer=buffers.find(rank) ;
        }
        itBuffer->second->eventLoop() ;
        double time=CTimer::getTime() ;
        bool succed = itBuffer->second->writeEvent(timeLine, event)  ;
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
      
      timeLine++;
    }


   void COneSidedContextClient::eventLoop(void)
   {
      if (!locked_) checkBuffers() ;
   }

   void COneSidedContextClient::callGlobalEventLoop(void)
   {
     locked_=true ;
     context_->globalEventLoop() ;
     locked_=false ;
   }

   void COneSidedContextClient::yield(void)
   {
     locked_=true ;
     context_->yield() ;
     locked_=false ;
   }

   void COneSidedContextClient::synchronize(void)
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
   void COneSidedContextClient::newBuffer(int rank)
   {
      if (!mapBufferSize_.count(rank))
      {
        error(0) << "WARNING: Unexpected request for buffer to communicate with server " << rank << std::endl;
        mapBufferSize_[rank] = CXios::minBufferSize;
        maxEventSizes[rank] = CXios::minBufferSize;
      }

      COneSidedClientBuffer* buffer = buffers[rank] = new COneSidedClientBuffer(interComm, rank, commSelf_, interCommMerged_, clientSize+rank );
      if (isGrowableBuffer_) { buffer->setGrowable(growingFactor_) ; }
      else buffer->setFixed(mapBufferSize_[rank]) ;
  
   }

   /*!
   Verify state of buffers. Buffer is under pending state if there is no message on it
   \return state of buffers, pending(true), ready(false)
   */
   bool COneSidedContextClient::checkBuffers(void)
   {
      bool pending = false;
      for (auto itBuff : buffers)
      {
        itBuff.second->eventLoop() ;
        pending |= !(itBuff.second->isEmpty());
      }
      return pending;
   }

   //! Release all buffers
   void COneSidedContextClient::releaseBuffers()
   {
      for (auto& itBuff : buffers) delete itBuff.second;
      buffers.clear();
   }


   /*!
   Verify state of buffers corresponding to a connection
   \param [in] ranks list rank of server to which client connects to
   \return state of buffers, pending(true), ready(false)
   */
   bool COneSidedContextClient::checkBuffers(list<int>& ranks)
   {
      bool pending = false;
      for (auto& rank : ranks) 
      {
        buffers[rank]->eventLoop() ;
        pending |= !(buffers[rank]->isEmpty()) ;
      }
      return pending;
   }

   /*!
    * Set the buffer size for each connection. Warning: This function is collective.
    *
    * \param [in] mapSize maps the rank of the connected servers to the size of the correspoinding buffer
    * \param [in] maxEventSize maps the rank of the connected servers to the size of the biggest event
   */
   void COneSidedContextClient::setBufferSize(const std::map<int,StdSize>& mapSize)
   {
     setFixedBuffer() ;
     for(auto& it : mapSize)
     {
      size_t size=std::max(CXios::minBufferSize*1.0,std::min(it.second*CXios::bufferSizeFactor*1.01,CXios::maxBufferSize*1.0)) ;
      mapBufferSize_[it.first]=size ;
      if (buffers.count(it.first)>0) buffers[it.first]->setFixed(size);
     }
   }


   /*!
   * Finalize context client and do some reports. Function is non-blocking.
   */
  void COneSidedContextClient::finalize(void)
  {
    bool stop = false;

    int* nbServerConnectionLocal  = new int[serverSize] ;
    int* nbServerConnectionGlobal  = new int[serverSize] ;
    for(int i=0;i<serverSize;++i) nbServerConnectionLocal[i]=0 ;
    for (auto itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++)  nbServerConnectionLocal[itBuff->first]=1 ;
    for (auto ItServerLeader = ranksServerLeader.begin(); ItServerLeader != ranksServerLeader.end(); ItServerLeader++)  nbServerConnectionLocal[*ItServerLeader]=1 ;
    
    MPI_Allreduce(nbServerConnectionLocal, nbServerConnectionGlobal, serverSize, MPI_INT, MPI_SUM, intraComm);
    
    CEventClient event(CContext::GetType(), CContext::EVENT_ID_CONTEXT_FINALIZE);
    CMessage msg;

    for (int i=0;i<serverSize;++i) if (nbServerConnectionLocal[i]==1) event.push(i, nbServerConnectionGlobal[i], msg) ;
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
  bool COneSidedContextClient::havePendingRequests(void)
  {
    return checkBuffers();
  }
  
  bool COneSidedContextClient::havePendingRequests(list<int>& ranks)
  {
    return checkBuffers(ranks) ;
  }

  bool COneSidedContextClient::isNotifiedFinalized(void)
  {

    bool finalized = true;
    for (auto& it : buffers ) finalized &= it.second->isNotifiedFinalized();
    return finalized;
  }

}
