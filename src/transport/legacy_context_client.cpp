#include "xios_spl.hpp"
#include "legacy_context_client.hpp"
#include "context_server.hpp"
#include "event_client.hpp"
#include "buffer_out.hpp"
#include "buffer_client.hpp"
#include "type.hpp"
#include "event_client.hpp"
#include "context.hpp"
#include "mpi.hpp"
#include "timer.hpp"
#include "cxios.hpp"
#include "server.hpp"
#include "services.hpp"
#include "ressources_manager.hpp"
#include <boost/functional/hash.hpp>
#include <random>
#include <chrono>

namespace xios
{
    /*!
    \param [in] parent Pointer to context on client side
    \param [in] intraComm_ communicator of group client
    \param [in] interComm_ communicator of group server
    \cxtSer [in] cxtSer Pointer to context of server side. (It is only used in case of attached mode).
    */
    CLegacyContextClient::CLegacyContextClient(CContext* parent, MPI_Comm intraComm_, MPI_Comm interComm_, CContext* cxtSer)
                         : CContextClient(parent, intraComm_, interComm_, cxtSer),
                           mapBufferSize_(),  maxBufferedEvents(4)
    {
      pureOneSided=CXios::getin<bool>("pure_one_sided",false); // pure one sided communication (for test)
      if (isAttachedModeEnabled()) pureOneSided=false ; // no one sided in attach mode

      if (!isAttachedModeEnabled()) MPI_Intercomm_merge(interComm_,false, &interCommMerged_) ;
      
      MPI_Comm_split(intraComm_,clientRank,clientRank, &commSelf_) ; // for windows

      timeLine = 1;
    }

    CContextClient::ETransport getType(void) {return CContextClient::legacy ;}

    /*!
    In case of attached mode, the current context must be reset to context for client
    \param [in] event Event sent to server
    */
    void CLegacyContextClient::sendEvent(CEventClient& event)
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
           ERROR("void CLegacyContextClient::sendEvent(CEventClient& event)",
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
          ERROR("void CLegacyContextClient::sendEvent(CEventClient& event)",
                 <<" Some servers will not receive the message for timeline = "<<timeLine<<endl
                 <<"Servers are : "<<osstr.str()) ;
        }


      }

      if (!event.isEmpty())
      {
        list<int> sizes = event.getSizes();

         // We force the getBuffers call to be non-blocking on classical servers
        list<CBufferOut*> buffList;
        getBuffers(timeLine, ranks, sizes, buffList) ;

        event.send(timeLine, sizes, buffList);
       
        //for (auto itRank = ranks.begin(); itRank != ranks.end(); itRank++) buffers[*itRank]->infoBuffer() ;

        unlockBuffers(ranks) ;
        checkBuffers(ranks);
        
      }
      
      if (isAttachedModeEnabled()) // couldBuffer is always true in attached mode
      {
        while (checkBuffers(ranks)) callGlobalEventLoop() ;
      
        CXios::getDaemonsManager()->scheduleContext(hashId_) ;
        while (CXios::getDaemonsManager()->isScheduledContext(hashId_)) callGlobalEventLoop() ;
      }
      
      MPI_Request req ;
      MPI_Status status ;
      MPI_Ibarrier(intraComm,&req) ;
      int flag ;
      MPI_Test(&req,&flag,&status) ;
      while(!flag) 
      {
        callGlobalEventLoop() ;
        MPI_Test(&req,&flag,&status) ;
      }


      timeLine++;
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
    void CLegacyContextClient::getBuffers(const size_t timeLine, const list<int>& serverList, const list<int>& sizeList, list<CBufferOut*>& retBuffers)
    {
      list<int>::const_iterator itServer, itSize;
      list<CClientBuffer*> bufferList;
      map<int,CClientBuffer*>::const_iterator it;
      list<CClientBuffer*>::iterator itBuffer;
      bool areBuffersFree;
     
      for (itServer = serverList.begin(); itServer != serverList.end(); itServer++)
      {
        it = buffers.find(*itServer);
        if (it == buffers.end())
        {
          CTokenManager* tokenManager = CXios::getRessourcesManager()->getTokenManager() ;
          size_t token = tokenManager->getToken() ;
          while (!tokenManager->lockToken(token)) callGlobalEventLoop() ;
          newBuffer(*itServer);
          it = buffers.find(*itServer);
          checkAttachWindows(it->second,it->first) ;
          tokenManager->unlockToken(token) ;
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

          callGlobalEventLoop() ;
        }

      } while (!areBuffersFree);
      CTimer::get("Blocking time").suspend();

      for (itBuffer = bufferList.begin(), itSize = sizeList.begin(); itBuffer != bufferList.end(); itBuffer++, itSize++)
        retBuffers.push_back((*itBuffer)->getBuffer(timeLine, *itSize));
   }

   void CLegacyContextClient::eventLoop(void)
   {
      if (!locked_) checkBuffers() ;
   }

   void CLegacyContextClient::callGlobalEventLoop(void)
   {
     locked_=true ;
     context_->globalEventLoop() ;
     locked_=false ;
   }
   /*!
   Make a new buffer for a certain connection to server with specific rank
   \param [in] rank rank of connected server
   */
   void CLegacyContextClient::newBuffer(int rank)
   {
      if (!mapBufferSize_.count(rank))
      {
        error(0) << "WARNING: Unexpected request for buffer to communicate with server " << rank << std::endl;
        mapBufferSize_[rank] = CXios::minBufferSize;
        maxEventSizes[rank] = CXios::minBufferSize;
      }
      
      CClientBuffer* buffer = buffers[rank] = new CClientBuffer(interComm, rank, mapBufferSize_[rank], maxEventSizes[rank]);
      if (isGrowableBuffer_) buffer->setGrowableBuffer(1.2) ;
      else buffer->fixBuffer() ;
      // Notify the server
      CBufferOut* bufOut = buffer->getBuffer(0, 4*sizeof(MPI_Aint));
      MPI_Aint sendBuff[4] ;
      sendBuff[0]=hashId_;
      sendBuff[1]=mapBufferSize_[rank];
      sendBuff[2]=buffers[rank]->getWinAddress(0); 
      sendBuff[3]=buffers[rank]->getWinAddress(1); 
      info(100)<<"CLegacyContextClient::newBuffer : rank "<<rank<<" winAdress[0] "<<buffers[rank]->getWinAddress(0)<<" winAdress[1] "<<buffers[rank]->getWinAddress(1)<<endl;
      bufOut->put(sendBuff, 4); 
      buffer->checkBuffer(true);
/*
       // create windows dynamically for one-sided
      if (!isAttachedModeEnabled())
      { 
        CTimer::get("create Windows").resume() ;
        MPI_Comm interComm ;
        MPI_Intercomm_create(commSelf_, 0, interCommMerged_, clientSize+rank, 0, &interComm) ;
        MPI_Intercomm_merge(interComm, false, &winComm_[rank]) ;
        CXios::getMpiGarbageCollector().registerCommunicator(winComm_[rank]) ;
        MPI_Comm_free(&interComm) ;
        windows_[rank].resize(2) ;
        
        MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_[rank], &windows_[rank][0]);
        CXios::getMpiGarbageCollector().registerWindow(windows_[rank][0]) ;
        
        MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_[rank], &windows_[rank][1]);   
        CXios::getMpiGarbageCollector().registerWindow(windows_[rank][1]) ;

        CTimer::get("create Windows").suspend() ;
      }
      else
      {
        winComm_[rank] = MPI_COMM_NULL ;
        windows_[rank].resize(2) ;
        windows_[rank][0] = MPI_WIN_NULL ;
        windows_[rank][1] = MPI_WIN_NULL ;
      }
      buffer->attachWindows(windows_[rank]) ;
      if (!isAttachedModeEnabled()) MPI_Barrier(winComm_[rank]) ;
  */     
   }

   void CLegacyContextClient::checkAttachWindows(CClientBuffer* buffer, int rank)
   {
      if (!buffer->isAttachedWindows())
      {
           // create windows dynamically for one-sided
        if (!isAttachedModeEnabled())
        { 
          CTimer::get("create Windows").resume() ;
          MPI_Comm interComm ;
          MPI_Intercomm_create(commSelf_, 0, interCommMerged_, clientSize+rank, 0, &interComm) ;
          MPI_Intercomm_merge(interComm, false, &winComm_[rank]) ;
          CXios::getMpiGarbageCollector().registerCommunicator(winComm_[rank]) ;
          MPI_Comm_free(&interComm) ;
          windows_[rank].resize(2) ;
      
          MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_[rank], &windows_[rank][0]);
          CXios::getMpiGarbageCollector().registerWindow(windows_[rank][0]) ;
      
          MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_[rank], &windows_[rank][1]);   
          CXios::getMpiGarbageCollector().registerWindow(windows_[rank][1]) ;

          CTimer::get("create Windows").suspend() ;
          buffer->attachWindows(windows_[rank]) ;
          MPI_Barrier(winComm_[rank]) ;
        }
        else
        {
          winComm_[rank] = MPI_COMM_NULL ;
          windows_[rank].resize(2) ;
          windows_[rank][0] = MPI_WIN_NULL ;
          windows_[rank][1] = MPI_WIN_NULL ;
          buffer->attachWindows(windows_[rank]) ;
        }

      }
    }


  
   /*!
   Verify state of buffers. Buffer is under pending state if there is no message on it
   \return state of buffers, pending(true), ready(false)
   */
   bool CLegacyContextClient::checkBuffers(void)
   {
      map<int,CClientBuffer*>::iterator itBuff;
      bool pending = false;
      for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++)
        pending |= itBuff->second->checkBuffer(!pureOneSided);
      return pending;
   }

   //! Release all buffers
   void CLegacyContextClient::releaseBuffers()
   {
      map<int,CClientBuffer*>::iterator itBuff;
      for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++)
      {
         delete itBuff->second;
      }
      buffers.clear();

// don't know when release windows

      //if (!isAttachedModeEnabled())
      //{  
      //  for(auto& it : winComm_)
      //  {
      //    int rank = it.first ;
      //    MPI_Win_free(&windows_[rank][0]);
      //    MPI_Win_free(&windows_[rank][1]);
      //    MPI_Comm_free(&winComm_[rank]) ;
      //  }
      //} 
   }

      
  /*!
   Lock the buffers for one sided communications
   \param [in] ranks list rank of server to which client connects to
   */
   void CLegacyContextClient::lockBuffers(list<int>& ranks)
   {
      list<int>::iterator it;
      for (it = ranks.begin(); it != ranks.end(); it++) buffers[*it]->lockBuffer();
   }

  /*!
   Unlock the buffers for one sided communications
   \param [in] ranks list rank of server to which client connects to
   */
   void CLegacyContextClient::unlockBuffers(list<int>& ranks)
   {
      list<int>::iterator it;
      for (it = ranks.begin(); it != ranks.end(); it++) buffers[*it]->unlockBuffer();
   }
      
   /*!
   Verify state of buffers corresponding to a connection
   \param [in] ranks list rank of server to which client connects to
   \return state of buffers, pending(true), ready(false)
   */
   bool CLegacyContextClient::checkBuffers(list<int>& ranks)
   {
      list<int>::iterator it;
      bool pending = false;
      for (it = ranks.begin(); it != ranks.end(); it++) pending |= buffers[*it]->checkBuffer(!pureOneSided);
      return pending;
   }

   /*!
    * Set the buffer size for each connection. Warning: This function is collective.
    *
    * \param [in] mapSize maps the rank of the connected servers to the size of the correspoinding buffer
    * \param [in] maxEventSize maps the rank of the connected servers to the size of the biggest event
   */
   void CLegacyContextClient::setBufferSize(const std::map<int,StdSize>& mapSize)
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
  void CLegacyContextClient::finalize(void)
  {
    map<int,CClientBuffer*>::iterator itBuff;
    std::list<int>::iterator ItServerLeader; 
    
    bool stop = false;

    int* nbServerConnectionLocal  = new int[serverSize] ;
    int* nbServerConnectionGlobal  = new int[serverSize] ;
    for(int i=0;i<serverSize;++i) nbServerConnectionLocal[i]=0 ;
    for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++)  nbServerConnectionLocal[itBuff->first]=1 ;
    for (ItServerLeader = ranksServerLeader.begin(); ItServerLeader != ranksServerLeader.end(); ItServerLeader++)  nbServerConnectionLocal[*ItServerLeader]=1 ;
    
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
  bool CLegacyContextClient::havePendingRequests(void)
  {
    bool pending = false;
    map<int,CClientBuffer*>::iterator itBuff;
    for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++)
      pending |= itBuff->second->hasPendingRequest();
    return pending;
  }
  
  bool CLegacyContextClient::havePendingRequests(list<int>& ranks)
  {
      list<int>::iterator it;
      bool pending = false;
      for (it = ranks.begin(); it != ranks.end(); it++) pending |= buffers[*it]->hasPendingRequest();
      return pending;
  }

  bool CLegacyContextClient::isNotifiedFinalized(void)
  {
    if (isAttachedModeEnabled()) return true ;

    bool finalized = true;
    map<int,CClientBuffer*>::iterator itBuff;
    for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++)
      finalized &= itBuff->second->isNotifiedFinalized();
    return finalized;
  }

}
