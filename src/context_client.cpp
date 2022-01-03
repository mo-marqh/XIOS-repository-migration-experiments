#include "xios_spl.hpp"
#include "context_client.hpp"
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
    CContextClient::CContextClient(CContext* parent, MPI_Comm intraComm_, MPI_Comm interComm_, CContext* cxtSer)
     : mapBufferSize_(), parentServer(cxtSer), maxBufferedEvents(4), associatedServer_(nullptr)
    {
      
      context_ = parent;
      intraComm = intraComm_;
      interComm = interComm_;
      MPI_Comm_rank(intraComm, &clientRank);
      MPI_Comm_size(intraComm, &clientSize);

      int flag;
      MPI_Comm_test_inter(interComm, &flag);
      if (flag) isAttached_=false ;
      else  isAttached_=true ;

      pureOneSided=CXios::getin<bool>("pure_one_sided",false); // pure one sided communication (for test)
      if (isAttachedModeEnabled()) pureOneSided=false ; // no one sided in attach mode
      


      if (flag) MPI_Comm_remote_size(interComm, &serverSize);
      else  MPI_Comm_size(interComm, &serverSize);

      computeLeader(clientRank, clientSize, serverSize, ranksServerLeader, ranksServerNotLeader);

      if (flag) MPI_Intercomm_merge(interComm_,false, &interCommMerged_) ;
      
      MPI_Comm_split(intraComm_,clientRank,clientRank, &commSelf_) ; // for windows

      auto time=chrono::system_clock::now().time_since_epoch().count() ;
      std::default_random_engine rd(time); // not reproducible from a run to another
      std::uniform_int_distribution<size_t> dist;
      hashId_=dist(rd) ;
      MPI_Bcast(&hashId_,1,MPI_SIZE_T,0,intraComm) ; // Bcast to all server of the context

      timeLine = 1;
    }

    void CContextClient::computeLeader(int clientRank, int clientSize, int serverSize,
                                       std::list<int>& rankRecvLeader,
                                       std::list<int>& rankRecvNotLeader)
    {
      if ((0 == clientSize) || (0 == serverSize)) return;

      if (clientSize < serverSize)
      {
        int serverByClient = serverSize / clientSize;
        int remain = serverSize % clientSize;
        int rankStart = serverByClient * clientRank;

        if (clientRank < remain)
        {
          serverByClient++;
          rankStart += clientRank;
        }
        else
          rankStart += remain;

        for (int i = 0; i < serverByClient; i++)
          rankRecvLeader.push_back(rankStart + i);

        rankRecvNotLeader.resize(0);
      }
      else
      {
        int clientByServer = clientSize / serverSize;
        int remain = clientSize % serverSize;

        if (clientRank < (clientByServer + 1) * remain)
        {
          if (clientRank % (clientByServer + 1) == 0)
            rankRecvLeader.push_back(clientRank / (clientByServer + 1));
          else
            rankRecvNotLeader.push_back(clientRank / (clientByServer + 1));
        }
        else
        {
          int rank = clientRank - (clientByServer + 1) * remain;
          if (rank % clientByServer == 0)
            rankRecvLeader.push_back(remain + rank / clientByServer);
          else
            rankRecvNotLeader.push_back(remain + rank / clientByServer);
        }
      }
    }

    /*!
    In case of attached mode, the current context must be reset to context for client
    \param [in] event Event sent to server
    */
    void CContextClient::sendEvent(CEventClient& event)
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
           ERROR("void CContextClient::sendEvent(CEventClient& event)",
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
          ERROR("void CContextClient::sendEvent(CEventClient& event)",
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
        while (checkBuffers(ranks)) context_->globalEventLoop() ;
      
        CXios::getDaemonsManager()->scheduleContext(hashId_) ;
        while (CXios::getDaemonsManager()->isScheduledContext(hashId_)) context_->globalEventLoop() ;
      }
      
      timeLine++;
    }

    /*!
    If client is also server (attached mode), after sending event, it should process right away
    the incoming event.
    \param [in] ranks list rank of server connected this client
    */
    void CContextClient::waitEvent(list<int>& ranks)
    {
      while (checkBuffers(ranks))
      {
        context_->eventLoop() ;
      }

      MPI_Request req ;
      MPI_Status status ;

      MPI_Ibarrier(intraComm,&req) ;
      int flag=false ;

      do  
      {
        CXios::getDaemonsManager()->eventLoop() ;
        MPI_Test(&req,&flag,&status) ;
      } while (!flag) ;


    }


    void CContextClient::waitEvent_old(list<int>& ranks)
    {
      parentServer->server->setPendingEvent();
      while (checkBuffers(ranks))
      {
        parentServer->server->listen();
        parentServer->server->checkPendingRequest();
      }

      while (parentServer->server->hasPendingEvent())
      {
       parentServer->server->eventLoop();
      }
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
    bool CContextClient::getBuffers(const size_t timeLine, const list<int>& serverList, const list<int>& sizeList, list<CBufferOut*>& retBuffers,
                                    bool nonBlocking /*= false*/)
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

          context_->globalEventLoop() ;
        }

      } while (!areBuffersFree && !nonBlocking);
      CTimer::get("Blocking time").suspend();

      if (areBuffersFree)
      {
        for (itBuffer = bufferList.begin(), itSize = sizeList.begin(); itBuffer != bufferList.end(); itBuffer++, itSize++)
          retBuffers.push_back((*itBuffer)->getBuffer(timeLine, *itSize));
      }
      return areBuffersFree;
   }

   /*!
   Make a new buffer for a certain connection to server with specific rank
   \param [in] rank rank of connected server
   */
   void CContextClient::newBuffer(int rank)
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
      info(100)<<"CContextClient::newBuffer : rank "<<rank<<" winAdress[0] "<<buffers[rank]->getWinAddress(0)<<" winAdress[1] "<<buffers[rank]->getWinAddress(1)<<endl;
      bufOut->put(sendBuff, 4); 
      buffer->checkBuffer(true);
      
       // create windows dynamically for one-sided
      if (!isAttachedModeEnabled())
      { 
        CTimer::get("create Windows").resume() ;
        MPI_Comm interComm ;
        MPI_Intercomm_create(commSelf_, 0, interCommMerged_, clientSize+rank, 0, &interComm) ;
        MPI_Intercomm_merge(interComm, false, &winComm_[rank]) ;
        MPI_Comm_free(&interComm) ;
        windows_[rank].resize(2) ;
        MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_[rank], &windows_[rank][0]);
        MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_[rank], &windows_[rank][1]);   
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
       
   }

   /*!
   Verify state of buffers. Buffer is under pending state if there is no message on it
   \return state of buffers, pending(true), ready(false)
   */
   bool CContextClient::checkBuffers(void)
   {
      map<int,CClientBuffer*>::iterator itBuff;
      bool pending = false;
      for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++)
        pending |= itBuff->second->checkBuffer(!pureOneSided);
      return pending;
   }

   //! Release all buffers
   void CContextClient::releaseBuffers()
   {
      //map<int,CClientBuffer*>::iterator itBuff;
      //for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++)
      //{
      //   delete itBuff->second;
      //}
      //buffers.clear();

// don't know when release windows

      if (!isAttachedModeEnabled())
      {  
        for(auto& it : winComm_)
        {
          int rank = it.first ;
          MPI_Win_free(&windows_[rank][0]);
          MPI_Win_free(&windows_[rank][1]);
          MPI_Comm_free(&winComm_[rank]) ;
        }
      } 
   }

      
  /*!
   Lock the buffers for one sided communications
   \param [in] ranks list rank of server to which client connects to
   */
   void CContextClient::lockBuffers(list<int>& ranks)
   {
      list<int>::iterator it;
      for (it = ranks.begin(); it != ranks.end(); it++) buffers[*it]->lockBuffer();
   }

  /*!
   Unlock the buffers for one sided communications
   \param [in] ranks list rank of server to which client connects to
   */
   void CContextClient::unlockBuffers(list<int>& ranks)
   {
      list<int>::iterator it;
      for (it = ranks.begin(); it != ranks.end(); it++) buffers[*it]->unlockBuffer();
   }
      
   /*!
   Verify state of buffers corresponding to a connection
   \param [in] ranks list rank of server to which client connects to
   \return state of buffers, pending(true), ready(false)
   */
   bool CContextClient::checkBuffers(list<int>& ranks)
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
   void CContextClient::setBufferSize(const std::map<int,StdSize>& mapSize)
   {
     for(auto& it : mapSize) 
      buffers[it.first]->fixBufferSize(std::max(CXios::minBufferSize*1.0,std::min(it.second*CXios::bufferSizeFactor*1.01,CXios::maxBufferSize*1.0)));
   }

  /*!
  Get leading server in the group of connected server
  \return ranks of leading servers
  */
  const std::list<int>& CContextClient::getRanksServerNotLeader(void) const
  {
    return ranksServerNotLeader;
  }

  /*!
  Check if client connects to leading server
  \return connected(true), not connected (false)
  */
  bool CContextClient::isServerNotLeader(void) const
  {
    return !ranksServerNotLeader.empty();
  }

  /*!
  Get leading server in the group of connected server
  \return ranks of leading servers
  */
  const std::list<int>& CContextClient::getRanksServerLeader(void) const
  {
    return ranksServerLeader;
  }

  /*!
  Check if client connects to leading server
  \return connected(true), not connected (false)
  */
  bool CContextClient::isServerLeader(void) const
  {
    return !ranksServerLeader.empty();
  }

   /*!
   * Finalize context client and do some reports. Function is non-blocking.
   */
  void CContextClient::finalize(void)
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
  bool CContextClient::havePendingRequests(void)
  {
    bool pending = false;
    map<int,CClientBuffer*>::iterator itBuff;
    for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++)
      pending |= itBuff->second->hasPendingRequest();
    return pending;
  }
  
  bool CContextClient::havePendingRequests(list<int>& ranks)
  {
      list<int>::iterator it;
      bool pending = false;
      for (it = ranks.begin(); it != ranks.end(); it++) pending |= buffers[*it]->hasPendingRequest();
      return pending;
  }

  bool CContextClient::isNotifiedFinalized(void)
  {
    if (isAttachedModeEnabled()) return true ;

    bool finalized = true;
    map<int,CClientBuffer*>::iterator itBuff;
    for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++)
      finalized &= itBuff->second->isNotifiedFinalized();
    return finalized;
  }

}
