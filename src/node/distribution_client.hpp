#ifndef __XIOS_DISTRIBUTIONCLIENT_HPP__
#define __XIOS_DISTRIBUTIONCLIENT_HPP__

#include <distribution.hpp>
#include "axis.hpp"
#include "domain.hpp"
#include "grid.hpp"

namespace xios {

class CGrid;
class CDomain;
class CAxis;

/*!
  \class CDistributionClient
  This class bases on the knowledge of distribution on client side as well as on server side
to calculate the index mapping between client and server. Each client awaring of the existences of other clients
and servers, firstly, computes the global index of its local data then use this information on taking into account of
distribution of servers to calculate to which server(s) it connects (sends data)
*/
class CDistributionClient : public CDistribution
{
  public:
    enum ServerDistributionType
    {
      BAND_DISTRIBUTION, PLAN_DISTRIBUTION
    };

    /** Default constructor */
    CDistributionClient(int rank, int dims, CArray<size_t,1>* globalIndex = 0);
    CDistributionClient(int rank, CGrid* grid);
    CDistributionClient(const CDistributionClient& distClient); //! Not implement

    /** Default destructor */
    virtual ~CDistributionClient();

    void computeServerIndexMapping(int nServer, ServerDistributionType distType = BAND_DISTRIBUTION);
    std::map<int,int> computeConnectedClients(int nbServer, int nbClient, MPI_Comm& clientIntraComm);

    const CArray<int,1>& getLocalDataIndexOnClient() const;
    const std::map<int, std::vector<size_t> >& getGlobalIndexOnServer() const;
    const std::map<int, std::vector<int> >& getLocalIndexSendToServer() const;

  protected:
    void createGlobalIndex();
    void readDistributionInfo(CGrid* grid);
    void readDistributionInfo(const std::vector<CDomain*>& domList,
                              const std::vector<CAxis*>& axisList,
                              const CArray<bool,1>& axisDomainOrder,
                              const CArray<bool,3>& gridMask);

    inline int getDomainIndex(const int& dataIIndex, const int& dataJIndex,
                              const int& dataIBegin, const int& dataJBegin,
                              const int& dataDim, const int& ni, int& j);
    inline int getAxisIndex(const int& dataIndex, const int& dataBegin, const int& ni);

  private:
    //! Create local index of a domain
    void createLocalDomainDataIndex();
    //! Create local index of an axis
    void createLocalAxisDataIndex();

    //! Compute band distribution on server
    std::vector<CArray<size_t,1>* > computeServerBandDistribution(int nServer);
  private:
    //!< LocalData index on client
    CArray<int,1>* localDataIndex_;

    //! Index of the local data which will be sent to the corresponding server(s)
    std::map<int, std::vector<int> >  localIndexSend2Server_;

    //! Global index of data on SERVER, which are calculated by client(s)
    std::map<int, std::vector<size_t> > indexGlobalOnServer_;
  private:
    /*! Domains and axis are considered elements.
     * A grid composed of 1 domain and 1 axis has 2 elements */
    int numElement_;
    CArray<bool,1> axisDomainOrder_; //!<

    std::vector<int> nLocal_; //!< Local size of each dimension (ni, nj, etc, ...)
    std::vector<int> nGlob_; //!< Global size of each dimension (e.x: ni_glo, nj_glo, etc, ...)
    std::vector<int> nBeginLocal_;//!< Begin index of each dimension (e.x: for domain, it's always 0, for axis, it's zoom_begin, ...)
    std::vector<int> nBeginGlobal_; //!< Begin index of each dimension (e.x: ibegin, jbegin, ...)
    std::vector<int> nZoomBegin_; //!< Begin index of zoom of each dimension
    std::vector<int> nZoomEnd_; //!< End index of zoom of each dimension

    // Data_n_index of domain or axis (For now, axis uses its size as data_n_index
    std::vector<int> dataNIndex_; //!< Data_n_index in case of domain
    std::vector<int> dataDims_; //!< Data_dim, domain can have data_dim == 1 or 2
    std::vector<int> dataBegin_; //!< Data begin (data_ibegin, data_jbegin, etc)
    std::vector<CArray<int,1> > dataIndex_; //!< Data index

    std::vector<CArray<bool,2> > domainMasks_; //!< Domain mask
    std::vector<CArray<bool,1> > axisMasks_; //!< Axis mask

    // Just suppose that grid mask has 3 dimension. Need change
    CArray<bool,3> gridMask_; // TODO: more general grid mask

    std::vector<std::vector<int> > localDomainIndex_;
    std::vector<std::vector<int> > localAxisIndex_;
    std::vector<int> indexMap_; //!< Mapping element index to dimension index

    std::map<int, int> connectedClients_; //!< number of clients connected to a server
    bool isConnectedServerComputed_; //!< Guard flag

    // The real index of a domain has true value, the ghost one has false value
    std::vector<std::vector<bool> > indexDomainData_;
};

} // namespace xios
#endif // __XIOS_DISTRIBUTIONCLIENT_HPP__
