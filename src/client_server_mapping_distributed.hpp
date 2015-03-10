#ifndef __XIOS_CLIENT_SERVER_MAPPING_DISTRIBUTED_HPP__
#define __XIOS_CLIENT_SERVER_MAPPING_DISTRIBUTED_HPP__

#include <client_server_mapping.hpp>
#include "xmlioserver_spl.hpp"
#include "array_new.hpp"
#include "mpi.hpp"
#include <boost/unordered_map.hpp>

namespace xios
{

class CClientServerMappingDistributed : public CClientServerMapping
{
  public:
    /** Default constructor */
    CClientServerMappingDistributed(const boost::unordered_map<size_t,int>& globalIndexOfServer,
                                    const MPI_Comm& clientIntraComm);

    virtual void computeServerIndexMapping(const CArray<size_t,1>& globalIndexOnClient,
                                           const CArray<int,1>& localIndexOnClient);

    /** Default destructor */
    virtual ~CClientServerMappingDistributed();

  protected:
    void computeDistributedServerIndex(const boost::unordered_map<size_t,int>& globalIndexOfServer,
                                       const MPI_Comm& clientIntraComm);

    void processReceivedRequest(unsigned long* buffIndexGlobal, int* buffIndexServer, int count);

    void testSendRequest(std::list<MPI_Request>& sendRequest);

    int computeBuffCount(MPI_Request& requestRecvIndexGlobal, MPI_Request& requestRecvIndexServer);

    void computeHashIndex();

//    void sendIndexServerToClients(std::map<int, std::vector<int> >& indexServer,
//                                  const MPI_Comm& clientIntraComm,
//                                  std::list<MPI_Request>& requestSendIndexServer);

//    void sendIndexGlobalToClients(std::map<int, std::vector<size_t> >& indexGlobal,
//                                  const MPI_Comm& clientIntraComm,
//                                  std::list<MPI_Request>& requestSendIndexGlobal);

    void sendIndexServerToClients(int clientDestRank, std::vector<int>& indexServer,
                                  const MPI_Comm& clientIntraComm, std::list<MPI_Request>& requestSendIndexServer);

    void sendIndexGlobalToClients(int clientDestRank, std::vector<size_t>& indexGlobal,
                                  const MPI_Comm& clientIntraComm, std::list<MPI_Request>& requestSendIndexGlobal);

    void resetRequestAndCount();

    void probeIndexGlobalMessageFromClients(unsigned long* recvIndexGlobalBuff, int recvNbIndexCount);

    void probeIndexServerMessageFromClients(int* recvIndexServerBuff, int recvNbIndexCount);

    int computeBuffCountIndexGlobal(MPI_Request& requestRecv);

    int computeBuffCountIndexServer(MPI_Request& requestRecv);
  private:
    boost::unordered_map<size_t,int> globalIndexToServerMapping_;

    std::map<int, MPI_Request> requestRecvIndexGlobal_;

    std::map<int, MPI_Request> requestRecvIndexServer_;

    std::vector<size_t> indexClientHash_;

    int nbClient_;

    int clientRank_;

    int countIndexGlobal_;

    int countIndexServer_;

    MPI_Comm clientIntraComm_;

    std::map<int, unsigned long*> indexGlobalBuffBegin_;

    std::map<int, int*> indexServerBuffBegin_;

};

} // namespace xios
#endif // __XIOS_CLIENT_SERVER_MAPPING_DISTRIBUTED_HPP__
