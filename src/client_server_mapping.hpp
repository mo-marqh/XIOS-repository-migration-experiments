/*!
   \file client_server_mapping.hpp
   \author Ha NGUYEN
   \since 04 Feb 2015
   \date 09 Mars 2015

   \brief Mapping between index client and server.
 */
#ifndef __XIOS_CLIENT_SERVER_MAPPING_HPP__
#define __XIOS_CLIENT_SERVER_MAPPING_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "mpi.hpp"

namespace xios {

/*!
  \class CClientServerMapping
  This class computes index of data which are sent to server as well as index of data
on server side.
*/
class CClientServerMapping
{
  public:
    /** Default constructor */
    CClientServerMapping();

    /** Default destructor */
    virtual ~CClientServerMapping();

    // Only need global index on client to calculate mapping (supposed client has info of distribution)
    virtual void computeServerIndexMapping(const CArray<size_t,1>& globalIndexOnClient) = 0;

//    // In case of computing local index on client sent to server
//    virtual void computeServerIndexMapping(const CArray<size_t,1>& globalIndexOnClient,
//                                           const CArray<int,1>& localIndexOnClient) = 0;

    // Simple case, global index on client and index on servers
    virtual void computeServerIndexMapping(const CArray<size_t,1>& globalIndexOnClient,
                                           const std::vector<CArray<size_t,1>* >& globalIndexOnServer);

    std::map<int,int> computeConnectedClients(int nbServer, int nbClient,
                                              MPI_Comm& clientIntraComm,
                                              const std::vector<int>& connectedServerRank);

    const std::map<int, std::vector<size_t> >& getGlobalIndexOnServer() const;
    const std::map<int, std::vector<int> >& getLocalIndexSendToServer() const;

  protected:
    void defaultComputeServerIndexMapping(const CArray<size_t,1>& globalIndexOnClient,
                                          const std::vector<CArray<size_t,1>* >& globalIndexOnServer,
                                          const CArray<int,1>* localIndexOnClient = 0);

  protected:
    //! Global index of data on SERVER, which are calculated by client(s)
    std::map<int, std::vector<size_t> > indexGlobalOnServer_;

    //! Index of the local data which will be sent to the corresponding server(s)
    std::map<int, std::vector<int> >  localIndexSend2Server_;

    //!< Number of clients connected to a server
    std::map<int, int> connectedClients_;
};

} // namespace xios
#endif // __XIOS_CLIENT_SERVER_MAPPING_HPP__
