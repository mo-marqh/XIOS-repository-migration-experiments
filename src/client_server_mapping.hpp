/*!
   \file client_server_mapping.hpp
   \author Ha NGUYEN
   \since 04 Feb 2015
   \date 06 Feb 2015

   \brief Mapping between index client and server.
 */
#ifndef __XIOS_CLIENT_SERVER_MAPPING_HPP__
#define __XIOS_CLIENT_SERVER_MAPPING_HPP__

#include "xmlioserver_spl.hpp"
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

    virtual void computeServerIndexMapping(const CArray<size_t,1>& globalIndexOnClient,
                                           const std::vector<CArray<size_t,1>* >& globalIndexOnServer);

    std::map<int,int> computeConnectedClients(int nbServer, int nbClient,
                                              MPI_Comm& clientIntraComm,
                                              const std::vector<int>& connectedServerRank);

    const std::map<int, std::vector<size_t> >& getGlobalIndexOnServer() const;
    const std::map<int, std::vector<int> >& getLocalIndexSendToServer() const;

  protected:
    void defaultComputeServerIndexMapping(const CArray<size_t,1>& globalIndexOnClient,
                                          const std::vector<CArray<size_t,1>* >& globalIndexOnServer);

  private:
    //! Global index of data on SERVER, which are calculated by client(s)
    std::map<int, std::vector<size_t> > indexGlobalOnServer_;

    //! Index of the local data which will be sent to the corresponding server(s)
    std::map<int, std::vector<int> >  localIndexSend2Server_;

    //!< Number of clients connected to a server
    std::map<int, int> connectedClients_;
};

} // namespace xios
#endif // __XIOS_CLIENT_SERVER_MAPPING_HPP__
