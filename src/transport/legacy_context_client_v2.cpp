#include "xios_spl.hpp"
#include "legacy_context_client_v2.hpp"
#include "context_server.hpp"
#include "event_client.hpp"
#include "buffer_out.hpp"
#include "legacy_buffer_client_v2.hpp"
#include "type.hpp"
#include "event_client.hpp"
#include "context.hpp"
#include "mpi.hpp"
#include "timer.hpp"
#include "cxios.hpp"
#include "server.hpp"
#include "services.hpp"
#include "ressources_manager.hpp"
#include <random>
#include <chrono>

namespace xios
{
    extern CLogType logTimers ;
  
    /*!
    \param [in] parent Pointer to context on client side
    \param [in] intraComm_ communicator of group client
    \param [in] interComm_ communicator of group server
    \cxtSer [in] cxtSer Pointer to context of server side. (It is only used in case of attached mode --> obsolete).
    */
    CLegacyContextClientV2::CLegacyContextClientV2(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* cxtSer)
                         : CContextClient(parent, intraComm, interComm, cxtSer),
                           mapBufferSize_(),  maxBufferedEvents_(4)
    {
      pureOneSided_=CXios::getin<bool>("pure_one_sided",false); // pure one sided communication (for test)
      xios::MPI_Intercomm_merge(interComm_,false, &interCommMerged_) ;
      CXios::getMpiGarbageCollector().registerCommunicator(interCommMerged_) ;
           
      int maxAttachedWindows = CXios::getin<int>("max_attached_mem_window", 32)/2; // max number of attached memory to a dynamic window by client
      if (serverSize_ <= maxAttachedWindows) numServerByWindow_=serverSize_ ;
      else numServerByWindow_ = maxAttachedWindows ;
      int nbWin =  serverSize_ / numServerByWindow_;
      if (serverSize_ % numServerByWindow_ != 0 ) nbWin++ ;
      winDynamics_.resize(nbWin) ;
      for(auto& win : winDynamics_ ) 
      {
        win = new CWindowDynamic ;
        win->create(interCommMerged_) ;
      }


      eventScheduler_ = parent->getEventScheduler() ;  
      timeLine_ = 1;
    }

    /*!
    \param [in] event Event sent to server
    */
    void CLegacyContextClientV2::sendEvent(CEventClient& event)
    {
      list<int> ranks = event.getRanks();
 
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
           ERROR("void CLegacyContextClientV2::sendEvent(CEventClient& event)",
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
          ERROR("void CLegacyContextClientV2::sendEvent(CEventClient& event)",
                 <<" Some servers will not receive the message for timeline = "<<timeLine_<<endl
                 <<"Servers are : "<<osstr.str()) ;
        }


      }

      if (!event.isEmpty())
      {
        list<int> sizes = event.getSizes();

         // We force the getBuffers call to be non-blocking on classical servers
        list<CBufferOut*> buffList;
        getBuffers(timeLine_, ranks, sizes, buffList) ;

        event.send(timeLine_, sizes, buffList);
       
        unlockBuffers(ranks) ;
        checkBuffers(ranks);
        
      }
      
      synchronize() ;
      timeLine_++;
    }


    /*!
     * Get buffers for each connection to the servers. This function blocks until there is enough room in the buffers unless
     * it is explicitly requested to be non-blocking.
     *
     *
     * \param [in] timeLine time line of the event which will be sent to servers
     * \param [in] serverList list of rank of connected server
     * \param [in] sizeList size of message corresponding to each connection
     * \param [out] retBuffers list of buffers that can be used to store an event
     * \param [in] nonBlocking whether this function should be non-blocking
     * \return whether the already allocated buffers could be used
    */
    void CLegacyContextClientV2::getBuffers(const size_t timeLine, const list<int>& serverList, const list<int>& sizeList, list<CBufferOut*>& retBuffers)
    {
      list<int>::const_iterator itServer, itSize;
      list<CLegacyClientBufferV2*> bufferList;
      map<int,CLegacyClientBufferV2*>::const_iterator it;
      list<CLegacyClientBufferV2*>::iterator itBuffer;
      bool areBuffersFree;
      map<int,MPI_Request> attachList ;
     
      for (itServer = serverList.begin(); itServer != serverList.end(); itServer++)
      {
        it = buffers.find(*itServer);
        if (it == buffers.end())
        {
          newBuffer(*itServer);
          it = buffers.find(*itServer);
        }
        bufferList.push_back(it->second);
      }
      
      double lastTimeBuffersNotFree=0. ;
      double time ;
      bool doUnlockBuffers ;
      CTimer::get("Blocking time").resume();
      do
      {
        areBuffersFree = true;
        doUnlockBuffers=false ;
        time=MPI_Wtime() ;
        if (time-lastTimeBuffersNotFree > latency_)
        {
          for (itBuffer = bufferList.begin(), itSize = sizeList.begin(); itBuffer != bufferList.end(); itBuffer++, itSize++)
          {
            areBuffersFree &= (*itBuffer)->isBufferFree(*itSize);
          }
          if (!areBuffersFree)
          {
            lastTimeBuffersNotFree = time ;
            doUnlockBuffers=true ;
          }          
        }
        else areBuffersFree = false ;

        if (!areBuffersFree)
        {
          if (doUnlockBuffers) for (itBuffer = bufferList.begin(); itBuffer != bufferList.end(); itBuffer++) (*itBuffer)->unlockBuffer();
          checkBuffers();

          yield() ;
        }

      } while (!areBuffersFree);
      CTimer::get("Blocking time").suspend();

      for (itBuffer = bufferList.begin(), itSize = sizeList.begin(); itBuffer != bufferList.end(); itBuffer++, itSize++)
        retBuffers.push_back((*itBuffer)->getBuffer(timeLine, *itSize));
   }


   void CLegacyContextClientV2::eventLoop(void)
   {
      if (!locked_) checkBuffers() ;
   }

   void CLegacyContextClientV2::callGlobalEventLoop(void)
   {
     locked_=true ;
     context_->yield() ;
     locked_=false ;
   }

   void CLegacyContextClientV2::yield(void)
   {
     locked_=true ;
     context_->yield() ;
     locked_=false ;
   }

   void CLegacyContextClientV2::synchronize(void)
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
   void CLegacyContextClientV2::newBuffer(int rank)
   {
      if (!mapBufferSize_.count(rank))
      {
        error(0) << "WARNING: Unexpected request for buffer to communicate with server " << rank << std::endl;
        mapBufferSize_[rank] = CXios::minBufferSize;
        maxEventSizes_[rank] = CXios::minBufferSize;
      }
      bool hasWindows = true ;
      
      int numWindow = rank / numServerByWindow_;
      
      CLegacyClientBufferV2* buffer = buffers[rank] = new CLegacyClientBufferV2(interCommMerged_, clientSize_+rank, mapBufferSize_[rank], hasWindows, winDynamics_[numWindow]);

      if (isGrowableBuffer_) buffer->setGrowableBuffer(1.2) ;
      else buffer->fixBuffer() ;
      // Notify the server
     
      CBufferOut* bufOut = buffer->getBuffer(0, 4*sizeof(MPI_Aint));
      MPI_Aint sendBuff[4] ;
      sendBuff[0]=hashId_;
      sendBuff[1]=mapBufferSize_[rank];
      sendBuff[2]=buffers[rank]->getWinBufferAddress(0); 
      sendBuff[3]=buffers[rank]->getWinBufferAddress(1); 
      info(100)<<"CLegacyContextClientV2::newBuffer : rank "<<rank<<" winAdress[0] "<<buffers[rank]->getWinBufferAddress(0)<<" winAdress[1] "<<buffers[rank]->getWinBufferAddress(1)<<endl;
      bufOut->put(sendBuff,4); 
      buffer->checkBuffer(true);

   }

  
  
   /*!
   Verify state of buffers. Buffer is under pending state if there is no message on it
   \return state of buffers, pending(true), ready(false)
   */
   bool CLegacyContextClientV2::checkBuffers(void)
   {
      map<int,CLegacyClientBufferV2*>::iterator itBuff;
      bool pending = false;
      for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++)
        pending |= itBuff->second->checkBuffer(!pureOneSided_);
      return pending;
   }

   //! Release all buffers
   void CLegacyContextClientV2::releaseBuffers()
   {
      map<int,CLegacyClientBufferV2*>::iterator itBuff;
      for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++)
      {
         delete itBuff->second;
      }
      buffers.clear();

      for(auto& it : winComm_)
      {
        int rank = it.first ;
      }
   }

      
  /*!
   Lock the buffers for one sided communications
   \param [in] ranks list rank of server to which client connects to
   */
   void CLegacyContextClientV2::lockBuffers(list<int>& ranks)
   {
      list<int>::iterator it;
      for (it = ranks.begin(); it != ranks.end(); it++) buffers[*it]->lockBuffer();
   }

  /*!
   Unlock the buffers for one sided communications
   \param [in] ranks list rank of server to which client connects to
   */
   void CLegacyContextClientV2::unlockBuffers(list<int>& ranks)
   {
      list<int>::iterator it;
      for (it = ranks.begin(); it != ranks.end(); it++) buffers[*it]->unlockBuffer();
   }
      
   /*!
   Verify state of buffers corresponding to a connection
   \param [in] ranks list rank of server to which client connects to
   \return state of buffers, pending(true), ready(false)
   */
   bool CLegacyContextClientV2::checkBuffers(list<int>& ranks)
   {
      list<int>::iterator it;
      bool pending = false;
      for (it = ranks.begin(); it != ranks.end(); it++) pending |= buffers[*it]->checkBuffer(!pureOneSided_);
      return pending;
   }

   /*!
    * Set the buffer size for each connection. Warning: This function is collective.
    *
    * \param [in] mapSize maps the rank of the connected servers to the size of the correspoinding buffer
    * \param [in] maxEventSize maps the rank of the connected servers to the size of the biggest event
   */
   void CLegacyContextClientV2::setBufferSize(const std::map<int,StdSize>& mapSize)
   {
     setFixedBuffer() ;
     for(auto& it : mapSize)
     {
      size_t size=std::max(CXios::minBufferSize*1.0,std::min(it.second*CXios::bufferSizeFactor*1.01,CXios::maxBufferSize*1.0)) ;
      mapBufferSize_[it.first]=size ;
      if (buffers.count(it.first)>0) buffers[it.first]->fixBufferSize(size);
     }
   }

   /*!
   * Finalize context client and do some reports. Function is non-blocking.
   */
  void CLegacyContextClientV2::finalize(void)
  {
    map<int,CLegacyClientBufferV2*>::iterator itBuff;
    std::list<int>::iterator ItServerLeader; 
    
    bool stop = false;

    int* nbServerConnectionLocal  = new int[serverSize_] ;
    int* nbServerConnectionGlobal  = new int[serverSize_] ;
    for(int i=0;i<serverSize_;++i) nbServerConnectionLocal[i]=0 ;
    for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++)  nbServerConnectionLocal[itBuff->first]=1 ;
    for (ItServerLeader = ranksServerLeader_.begin(); ItServerLeader != ranksServerLeader_.end(); ItServerLeader++)  nbServerConnectionLocal[*ItServerLeader]=1 ;
    
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
  bool CLegacyContextClientV2::havePendingRequests(void)
  {
    bool pending = false;
    map<int,CLegacyClientBufferV2*>::iterator itBuff;
    for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++)
      pending |= itBuff->second->hasPendingRequest();
    return pending;
  }
  
  bool CLegacyContextClientV2::havePendingRequests(list<int>& ranks)
  {
      list<int>::iterator it;
      bool pending = false;
      for (it = ranks.begin(); it != ranks.end(); it++) pending |= buffers[*it]->hasPendingRequest();
      return pending;
  }

  bool CLegacyContextClientV2::isNotifiedFinalized(void)
  {
    bool finalized = true;
    map<int,CLegacyClientBufferV2*>::iterator itBuff;
    for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++)
      finalized &= itBuff->second->isNotifiedFinalized();
    return finalized;
  }

}
