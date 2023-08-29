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
#include "one_sided_context_client.hpp"
#include "legacy_context_client.hpp"
#include "online_context_client.hpp"


namespace xios
{
    /*!
    \param [in] parent Pointer to context on client side
    \param [in] intraComm_ communicator of group client
    \param [in] interComm_ communicator of group server
    \cxtSer [in] cxtSer Pointer to context of server side. (It is only used in case of attached mode).
    */
    CContextClient::CContextClient(CContext* parent, MPI_Comm intraComm_, MPI_Comm interComm_, CContext* cxtSer)
     : parentServer(cxtSer),  associatedServer_(nullptr)
    {
      
      context_ = parent;
      intraComm = intraComm_;
      interComm = interComm_;
      MPI_Comm_rank(intraComm, &clientRank);
      MPI_Comm_size(intraComm, &clientSize);

      int flag;
      
      MPI_Comm_remote_size(interComm, &serverSize);
      
      computeLeader(clientRank, clientSize, serverSize, ranksServerLeader, ranksServerNotLeader);

      auto time=chrono::system_clock::now().time_since_epoch().count() ;
      std::default_random_engine rd(time); // not reproducible from a run to another
      std::uniform_int_distribution<size_t> dist;
      hashId_=dist(rd) ;
      MPI_Bcast(&hashId_,1,MPI_SIZE_T,0,intraComm) ; // Bcast to all server of the context

    }

    template<>
    CContextClient* CContextClient::getNew<CContextClient::generic>(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* parentServer)
    { 
      string defaultProtocol = CXios::getin<string>("transport_protocol", "default") ;
      if (defaultProtocol=="one_sided") return getNew<CContextClient::oneSided>(parent, intraComm, interComm) ;
      else if  (defaultProtocol=="legacy") return getNew<CContextClient::legacy>(parent, intraComm, interComm) ;
      else if  (defaultProtocol=="online") return getNew<CContextClient::online>(parent, intraComm, interComm) ;
      else if  (defaultProtocol=="default") return getNew<CContextClient::legacy>(parent, intraComm, interComm) ;
      else ERROR("CContextClient* CContextClient::getNew<CContextClient::generic>(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* parentServer)",
                <<"Protocol name <"<<defaultProtocol<<"> is undefined,  must be <default>, <one_sided> or <legacy>" ) ;  
    }

    template<>
    CContextClient* CContextClient::getNew<CContextClient::oneSided>(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* parentServer)
    { 
      return new COneSidedContextClient(parent, intraComm, interComm, parentServer); 
    }

    template<>
    CContextClient* CContextClient::getNew<CContextClient::legacy>(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* parentServer)
    { 
      return new CLegacyContextClient(parent, intraComm, interComm, parentServer); 
    }

    template<>
    CContextClient* CContextClient::getNew<CContextClient::online>(CContext* parent, MPI_Comm intraComm, MPI_Comm interComm, CContext* parentServer)
    { 
      return new COnlineContextClient(parent, intraComm, interComm, parentServer); 
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



}
