/*!
   \file client_client_dht_template.hpp
   \author Ha NGUYEN
   \since 01 Oct 2015
   \date 06 Oct 2015

   \brief Distributed hashed table implementation.
 */

#ifndef __XIOS_CLIENT_CLIENT_DHT_TEMPLATE_HPP__
#define __XIOS_CLIENT_CLIENT_DHT_TEMPLATE_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "mpi.hpp"
#include "policy.hpp"
#include <boost/unordered_map.hpp>
//#include "utils.hpp"
#include "dht_data_types.hpp"

namespace xios
{
template<typename T, class HierarchyPolicy = DivideCommByTwo> class CClientClientDHTTemplate;

/*!
  \class CClientClientDHTTemplate
  This class provides the similar features like \class CClientServerMappingDistributed,
which implements a simple distributed hashed table; Moreover, by extending with hierarchical structure,
it allows to reduce greatly the number of communication among processes.
*/
template<typename T, typename HierarchyPolicy>
class CClientClientDHTTemplate: public HierarchyPolicy
{
  public:
    typedef T InfoType;
    static const int infoTypeSize = sizeof(InfoType);
    typedef typename boost::unordered_map<InfoType, std::vector<size_t> > InfoType2IndexMap;
    typedef typename boost::unordered_map<size_t,InfoType> Index2InfoTypeMap;

  public:
    /** Default constructor */
    CClientClientDHTTemplate(const Index2InfoTypeMap& indexInfoInitMap,
                             const MPI_Comm& clientIntraComm,
                             int hierarLvl = 2);

    void computeIndexInfoMapping(const CArray<size_t,1>& indices);

    const Index2InfoTypeMap& getInfoIndexMap() const {return infoIndexMapping_; }

    /** Default destructor */
    virtual ~CClientClientDHTTemplate();

  protected:
    // Redistribute index and info among clients
    void computeDistributedIndex(const Index2InfoTypeMap& indexInfoInitMap,
                                 const MPI_Comm& intraCommLevel,
                                 int level);

    void computeHashIndex(std::vector<size_t>& indexClientHash, int nbClient);

    void computeIndexInfoMappingLevel(const CArray<size_t,1>& indices,
                                      const MPI_Comm& intraCommLevel,
                                      int level);

  protected:
    void probeIndexMessageFromClients(unsigned long* recvIndexGlobalBuff,
                                      const int recvNbIndexCount,
                                      int& countIndexGlobal,
                                      std::map<int, unsigned long*>& indexGlobalBuffBegin,
                                      std::map<int, MPI_Request>& requestRecvIndexGlobal,
                                      const MPI_Comm& intraComm);

    void probeInfoMessageFromClients(unsigned char* recvIndexServerBuff,
                                     const int recvNbIndexCount,
                                     int& countIndexServer,
                                     std::map<int, unsigned char*>& indexServerBuffBegin,
                                     std::map<int, MPI_Request>& requestRecvIndexServer,
                                     const MPI_Comm& intraComm);

    // Send server index to clients
    void sendInfoToClients(int clientDestRank, std::vector<InfoType>& indexServer,
                           const MPI_Comm& clientIntraComm, std::list<MPI_Request>& requestSendIndexServer);

    // Send global index to clients
    void sendIndexToClients(int clientDestRank, std::vector<size_t>& indexGlobal,
                            const MPI_Comm& clientIntraComm, std::list<MPI_Request>& requestSendIndexGlobal);

    // Verify sending request
    void testSendRequest(std::list<MPI_Request>& sendRequest);

    // Compute size of receiving buffer for global index
    int computeBuffCountIndex(MPI_Request& requestRecv);

    // Compute size of receiving buffer for server index
    int computeBuffCountInfo(MPI_Request& requestRecv);

  protected:
    //! Mapping of global index to the corresponding client
    Index2InfoTypeMap index2InfoMapping_;

    //! A temporary mapping of index to the corresponding information in each level of hierarchy
    Index2InfoTypeMap indexToInfoMappingLevel_;

    //! Data (information) corresponding to global index
    Index2InfoTypeMap infoIndexMapping_;

    //! intracommuntion of clients
    MPI_Comm intraCommRoot_;

    //! Flag to specify whether data is distributed or not
    bool isDataDistributed_;
};

typedef CClientClientDHTTemplate<int> CClientClientDHTInt;
typedef CClientClientDHTTemplate<PairIntInt> CClientClientDHTPairIntInt;

} // namespace xios
#endif // __XIOS_CLIENT_CLIENT_DHT_TEMPLATE_HPP__
