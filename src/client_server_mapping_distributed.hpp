/*!
   \file client_server_mapping.hpp
   \author Ha NGUYEN
   \since 27 Feb 2015
   \date 09 Mars 2015

   \brief Mapping between index client and server.
   Clients pre-calculate all information of server distribution.
 */

#ifndef __XIOS_CLIENT_SERVER_MAPPING_DISTRIBUTED_HPP__
#define __XIOS_CLIENT_SERVER_MAPPING_DISTRIBUTED_HPP__

#include <client_server_mapping.hpp>
#include "xios_spl.hpp"
#include "array_new.hpp"
#include "mpi.hpp"
#include <boost/unordered_map.hpp>
#include "client_client_dht_template.hpp"

namespace xios
{
/*!
  \class CClientServerMappingDistributed
  This class computes index of data which are sent to server as well as index of data
on server side with a distributed alogrithm. Each client has a piece of information about the distribution
of servers. To find out all these info, first of all, all client join a discovering process in which each client
announces the others about the info they have as well as demand others info they are lacked of. After this process,
each client has enough info to decide to which client it need to send a demand for corresponding server of a global index.
The alogrithm depends on hashed index.
*/
class CClientServerMappingDistributed : public CClientServerMapping
{
  public:
    /** Default constructor */
    CClientServerMappingDistributed(const boost::unordered_map<size_t,int>& globalIndexOfServer,
                                    const MPI_Comm& clientIntraComm, bool isDataDistributed = true);

    virtual void computeServerIndexMapping(const CArray<size_t,1>& globalIndexOnClientSendToServer);

    /** Default destructor */
    virtual ~CClientServerMappingDistributed();



  protected:
    // Redistribute global index and server index among clients
    void computeDistributedServerIndex(const boost::unordered_map<size_t,int>& globalIndexOfServer,
                                       const MPI_Comm& clientIntraComm);

    // Send server index to clients
    void sendIndexServerToClients(int clientDestRank, std::vector<int>& indexServer,
                                  const MPI_Comm& clientIntraComm, std::list<MPI_Request>& requestSendIndexServer);

    // Send global index to clients
    void sendIndexGlobalToClients(int clientDestRank, std::vector<size_t>& indexGlobal,
                                  const MPI_Comm& clientIntraComm, std::list<MPI_Request>& requestSendIndexGlobal);

    // Verify sending request
    void testSendRequest(std::list<MPI_Request>& sendRequest);

    // Process request
    void processReceivedRequest(unsigned long* buffIndexGlobal, int* buffIndexServer, int count);

    // Probe and receive message of global index
    void probeIndexGlobalMessageFromClients(unsigned long* recvIndexGlobalBuff, int recvNbIndexCount);

    // Probe and receive message of server index
    void probeIndexServerMessageFromClients(int* recvIndexServerBuff, int recvNbIndexCount);

    // Compute range of hashing
    void computeHashIndex();

    // Compute size of receiving buffer for global index
    int computeBuffCountIndexGlobal(MPI_Request& requestRecv);

    // Compute size of receiving buffer for server index
    int computeBuffCountIndexServer(MPI_Request& requestRecv);

    // Reset request map
    void resetReceivingRequestAndCount();

  protected:
    //! Mapping of global index to the corresponding server
    boost::unordered_map<size_t,int> globalIndexToServerMapping_;

    //! Bounds of hash index
    std::vector<size_t> indexClientHash_;

    //! Number of client
    int nbClient_;

    //! Rank of client
    int clientRank_;

    //! Counting of buffer for receiving global index
    int countIndexGlobal_;

    //! Counting of buffer for receiving server index
    int countIndexServer_;

    //! intracommuntion of clients
    MPI_Comm clientIntraComm_;

    //! Request returned by MPI_IRecv function about global index
    std::map<int, MPI_Request> requestRecvIndexGlobal_;

    //! Request returned by MPI_IRecv function about index of server
    std::map<int, MPI_Request> requestRecvIndexServer_;

    //! Mapping client rank and the beginning position of receiving buffer for message of global index from this client
    std::map<int, unsigned long*> indexGlobalBuffBegin_;

    //! Mapping client rank and the begining position of receiving buffer for message of server index from this client
    std::map<int, int*> indexServerBuffBegin_;

    //! Flag to specify whether data is distributed or not
    bool isDataDistributed_;

//    CClientClientDHTTemplate<int>* ccDHT_;
    CClientClientDHTInt* ccDHT_;
};

} // namespace xios
#endif // __XIOS_CLIENT_SERVER_MAPPING_DISTRIBUTED_HPP__
