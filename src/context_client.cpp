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

namespace xios
{
    /*!
    \param [in] parent Pointer to context on client side
    \param [in] intraComm_ communicator of group client
    \param [in] interComm_ communicator of group server
    \cxtSer [in] cxtSer Pointer to context of server side. (It is only used on case of attached mode)
    */
    CContextClient::CContextClient(CContext* parent, MPI_Comm intraComm_, MPI_Comm interComm_, CContext* cxtSer)
     : mapBufferSize_(), parentServer(cxtSer)
    {
      context = parent;
      intraComm = intraComm_;
      interComm = interComm_;
      MPI_Comm_rank(intraComm, &clientRank);
      MPI_Comm_size(intraComm, &clientSize);

      int flag;
      MPI_Comm_test_inter(interComm, &flag);
      if (flag) MPI_Comm_remote_size(interComm, &serverSize);
      else  MPI_Comm_size(interComm, &serverSize);

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
          ranksServerLeader.push_back(rankStart + i);
      }
      else
      {
        int clientByServer = clientSize / serverSize;
        int remain = clientSize % serverSize;

        if (clientRank < (clientByServer + 1) * remain)
        {
          if (clientRank % (clientByServer + 1) == 0)
            ranksServerLeader.push_back(clientRank / (clientByServer + 1));
        }
        else
        {
          int rank = clientRank - (clientByServer + 1) * remain;
          if (rank % clientByServer == 0)
            ranksServerLeader.push_back(remain + rank / clientByServer);
        }
      }

      timeLine = 0;

    }

    /*!
    In case of attached mode, the current context must be reset to context for client
    \param [in] event Event sent to server
    */
    void CContextClient::sendEvent(CEventClient& event)
    {
      list<int> ranks = event.getRanks();
      if (!event.isEmpty())
      {
        list<int> sizes = event.getSizes();

        list<CBufferOut*> buffList = getBuffers(ranks, sizes);

        event.send(timeLine, sizes, buffList);

        checkBuffers(ranks);
      }

      if (isAttachedModeEnabled())
      {
        waitEvent(ranks);
        CContext::setCurrent(context->getId());
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
    Setup buffer for each connection to server and verify their state to put content into them
    \param [in] serverList list of rank of connected server
    \param [in] sizeList size of message corresponding to each connection
    \return List of buffer input which event can be placed
    */
    list<CBufferOut*> CContextClient::getBuffers(list<int>& serverList, list<int>& sizeList)
    {
      list<int>::iterator itServer, itSize;
      list<CClientBuffer*> bufferList;
      map<int,CClientBuffer*>::iterator it;
      list<CClientBuffer*>::iterator itBuffer;
      list<CBufferOut*>  retBuffer;
      bool free;

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
      free = false;

      CTimer::get("Blocking time").resume();
      while (!free)
      {
        free = true;
        for (itBuffer = bufferList.begin(), itSize = sizeList.begin(); itBuffer != bufferList.end(); itBuffer++, itSize++)
        {
          (*itBuffer)->checkBuffer();
         free &= (*itBuffer)->isBufferFree(*itSize);
        }
        if (!free)
          context->server->listen();
      }
      CTimer::get("Blocking time").suspend();

      for (itBuffer = bufferList.begin(), itSize = sizeList.begin(); itBuffer != bufferList.end(); itBuffer++, itSize++)
      {
        retBuffer.push_back((*itBuffer)->getBuffer(*itSize));
      }
      return retBuffer;
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
      }
      CClientBuffer* buffer = buffers[rank] = new CClientBuffer(interComm, rank, mapBufferSize_[rank]);
      // Notify the server
      CBufferOut* bufOut = buffer->getBuffer(sizeof(StdSize));
      bufOut->put(mapBufferSize_[rank]); // Stupid C++
      buffer->checkBuffer();
   }

   /*!
   Verify state of buffers. Buffer is under pending state if there is no message on it
   \return state of buffers, pending(true), ready(false)
   */
   bool CContextClient::checkBuffers(void)
   {
      map<int,CClientBuffer*>::iterator itBuff;
      bool pending = false;
      for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++) pending |= itBuff->second->checkBuffer();
      return pending;
   }

   //! Release all buffers
   void CContextClient::releaseBuffers(void)
   {
      map<int,CClientBuffer*>::iterator itBuff;
      for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++) delete itBuff->second;
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
      for (it = ranks.begin(); it != ranks.end(); it++) pending |= buffers[*it]->checkBuffer();
      return pending;
   }

   /*!
   Set buffer size for each connection
   \param [in] mapSize mapping rank of connected server to size of allocated buffer
   */
   void CContextClient::setBufferSize(const std::map<int,StdSize>& mapSize)
   {
     mapBufferSize_ = mapSize;
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
   * Check if the attached mode is used.
   *
   * \return true if and only if attached mode is used
   */
  bool CContextClient::isAttachedModeEnabled() const
  {
    return (parentServer != 0);
  }

   /*!
   Finalize context client and do some reports
   */
   void CContextClient::finalize(void)
   {
     map<int,CClientBuffer*>::iterator itBuff;
     bool stop = true;

     CEventClient event(CContext::GetType(), CContext::EVENT_ID_CONTEXT_FINALIZE);
     if (isServerLeader())
     {
       CMessage msg;
       const std::list<int>& ranks = getRanksServerLeader();
       for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
         event.push(*itRank, 1, msg);
       sendEvent(event);
     }
     else sendEvent(event);

     CTimer::get("Blocking time").resume();
     while (stop)
     {
       checkBuffers();
       stop = false;
       for (itBuff = buffers.begin(); itBuff != buffers.end(); itBuff++) stop |= itBuff->second->hasPendingRequest();
     }
     CTimer::get("Blocking time").suspend();

     std::map<int,StdSize>::const_iterator itbMap = mapBufferSize_.begin(),
                                           iteMap = mapBufferSize_.end(), itMap;
     StdSize totalBuf = 0;
     for (itMap = itbMap; itMap != iteMap; ++itMap)
     {
       report(10) << " Memory report : Context <" << context->getId() << "> : client side : memory used for buffer of each connection to server" << endl
                  << "  +) To server with rank " << itMap->first << " : " << itMap->second << " bytes " << endl;
       totalBuf += itMap->second;
     }
     report(0) << " Memory report : Context <" << context->getId() << "> : client side : total memory used for buffer " << totalBuf << " bytes" << endl;

     releaseBuffers();
   }
}
