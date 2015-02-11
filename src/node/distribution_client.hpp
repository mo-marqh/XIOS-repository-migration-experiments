/*!
   \file distribution_client.hpp
   \author Ha NGUYEN
   \since 13 Jan 2015
   \date 09 Feb 2015

   \brief Index distribution on client side.
 */
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
  This class bases on the knowledge of distribution on client side (decided by users)
to calculate the global index of its local data.
*/
class CDistributionClient : public CDistribution
{
  public:
    /** Default constructor */
    CDistributionClient(int rank, int dims, CArray<size_t,1>* globalIndex = 0);
    CDistributionClient(int rank, CGrid* grid);

    /** Default destructor */
    virtual ~CDistributionClient();

    const CArray<int,1>& getLocalDataIndexOnClient() const;
    std::vector<int> getNGlob() { return nGlob_; }

  protected:
    void createGlobalIndex();
    void readDistributionInfo(CGrid* grid);
    void readDistributionInfo(const std::vector<CDomain*>& domList,
                              const std::vector<CAxis*>& axisList,
                              const CArray<bool,1>& axisDomainOrder,
                              const CArray<bool,3>& gridMask);

  private:
    //! Create local index of a domain
    void createLocalDomainDataIndex();

    //! Create local index of an axis
    void createLocalAxisDataIndex();

    inline int getDomainIndex(const int& dataIIndex, const int& dataJIndex,
                              const int& dataIBegin, const int& dataJBegin,
                              const int& dataDim, const int& ni, int& j);
    inline int getAxisIndex(const int& dataIndex, const int& dataBegin, const int& ni);

  private:
    //!< LocalData index on client
    CArray<int,1>* localDataIndex_;

  private:
    /*! Domains and axis are considered elements.
     * A grid composed of 1 domain and 1 axis has 2 elements */
    int numElement_;
    CArray<bool,1> axisDomainOrder_; //!< Order of axis and domain of a grid

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

    // The correct index of a domain has true value, the ghost one has false value
    std::vector<std::vector<bool> > indexDomainData_;
    std::vector<std::vector<bool> > indexAxisData_;

  private:
    CDistributionClient(const CDistributionClient& distClient); //! Not implement
};

} // namespace xios
#endif // __XIOS_DISTRIBUTIONCLIENT_HPP__
