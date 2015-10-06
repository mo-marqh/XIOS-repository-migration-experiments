/*!
   \file client_client_dht.hpp
   \author Ha NGUYEN
   \since 15 Sep 2015
   \date 29 Sep 2015

   \brief Distributed hashed table implementation.
 */

#ifndef __XIOS_CLIENT_CLIENT_DHT_HPP__
#define __XIOS_CLIENT_CLIENT_DHT_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "mpi.hpp"
#include <boost/unordered_map.hpp>

namespace xios
{
/*!
  \class CClientClientDHT
  This class provides the similar features like \class CClientServerMappingDistributed, which implements a simple distributed hashed table;
Moreover, by extending with hierarchical structure, it allows to reduce the number of communication among processes greatly.
*/
class CClientClientDHT
{
  public:
    /** Default constructor */
    CClientClientDHT(const boost::unordered_map<size_t,int>& globalIndexOfServer,
                     const MPI_Comm& clientIntraComm, bool isDataDistributed = true,
                     int hierarLvl = 2);

    void computeServerIndexMapping(const CArray<size_t,1>& globalIndexOnClientSendToServer);

    const std::map<int, std::vector<size_t> >& getGlobalIndexOnServer() const {return indexGlobalOnServer_; }
    const boost::unordered_map<size_t,int>& getGlobalIndexServerMapping() const {return globalIndexToServerMapping_; }

    /** Default destructor */
    virtual ~CClientClientDHT();

  protected:
    // Redistribute global index and server index among clients
    void computeDistributedIndex(const boost::unordered_map<size_t,int>& globalIndexOfServer,
                                 const MPI_Comm& intraCommLevel,
                                 int level);

    void computeMPICommLevel(const MPI_Comm& mpiCommRoot);

    void divideMPICommLevel(const MPI_Comm& mpiCommLevel, int level);

    void computeHashIndex(std::vector<size_t>& indexClientHash, int nbClient);

    virtual void computeIndexMapping(const CArray<size_t,1>& globalIndexOnClientSendToServer,
                                     const MPI_Comm& intraCommLevel,
                                     int level);

  protected:
    void probeIndexMessageFromClients(unsigned long* recvIndexGlobalBuff,
                                            const int recvNbIndexCount,
                                            int& countIndexGlobal,
                                            std::map<int, unsigned long*>& indexGlobalBuffBegin,
                                            std::map<int, MPI_Request>& requestRecvIndexGlobal,
                                            const MPI_Comm& intraComm);

    void probeInfoMessageFromClients(int* recvIndexServerBuff,
                                            const int recvNbIndexCount,
                                            int& countIndexServer,
                                            std::map<int, int*>& indexServerBuffBegin,
                                            std::map<int, MPI_Request>& requestRecvIndexServer,
                                            const MPI_Comm& intraComm);

    // Send server index to clients
    void sendInfoToClients(int clientDestRank, std::vector<int>& indexServer,
                                  const MPI_Comm& clientIntraComm, std::list<MPI_Request>& requestSendIndexServer);

    // Send global index to clients
    void sendIndexToClients(int clientDestRank, std::vector<size_t>& indexGlobal,
                                  const MPI_Comm& clientIntraComm, std::list<MPI_Request>& requestSendIndexGlobal);

    // Verify sending request
    void testSendRequest(std::list<MPI_Request>& sendRequest);

    // Compute size of receiving buffer for global index
    int computeBuffCountIndexGlobal(MPI_Request& requestRecv);

    // Compute size of receiving buffer for server index
    int computeBuffCountIndexServer(MPI_Request& requestRecv);

  protected:
    //! Mapping of global index to the corresponding client
    boost::unordered_map<size_t,int> globalIndexToServerMapping_;

    //! A temporary mapping of index to the corresponding information in each level of hierarchy
    boost::unordered_map<size_t,int> globalIndexToInfoMappingLevel_;
    std::vector<MPI_Comm> commLevel_;

    int nbLevel_;

    //! Global index of data on SERVER, which are calculated by client(s)
    std::map<int, std::vector<size_t> > indexGlobalOnServer_;

//    //! Number of client
//    int nbClient_;
//
//    //! Rank of client
//    int clientRank_;

//    //! Counting of buffer for receiving global index
//    int countIndexGlobal_;
//
//    //! Counting of buffer for receiving server index
//    int countIndexServer_;

    //! intracommuntion of clients
    MPI_Comm intraCommRoot_;

//    //! Request returned by MPI_IRecv function about global index
//    std::map<int, MPI_Request> requestRecvIndexGlobal_;
//
//    //! Request returned by MPI_IRecv function about index of server
//    std::map<int, MPI_Request> requestRecvIndexServer_;
//
//    //! Mapping client rank and the beginning position of receiving buffer for message of global index from this client
//    std::map<int, unsigned long*> indexGlobalBuffBegin_;
//
//    //! Mapping client rank and the begining position of receiving buffer for message of server index from this client
//    std::map<int, int*> indexServerBuffBegin_;

    //! Flag to specify whether data is distributed or not
    bool isDataDistributed_;
};

} // namespace xios
#endif // __XIOS_CLIENT_CLIENT_DHT_HPP__
