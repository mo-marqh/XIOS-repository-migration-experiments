/*!
   \file server_distribution_description.hpp
   \author Ha NGUYEN
   \since 04 Jan 2015
   \date 09 Feb 2015

   \brief Description of index distribution on server(s).
 */

#ifndef __XIOS_SERVER_DISTRIBUTION_DESCRIPTION_HPP
#define __XIOS_SERVER_DISTRIBUTION_DESCRIPTION_HPP

#include "xios_spl.hpp"
#include "array_new.hpp"
#include <boost/unordered_map.hpp>

namespace xios
{
/*!
   \class CServerDistributionDescription
   This class contains information that describe distribution of servers.
*/
class CServerDistributionDescription
{
  public:
    enum ServerDistributionType
    {
      BAND_DISTRIBUTION, PLAN_DISTRIBUTION
    };

    /** Default constructor */
    CServerDistributionDescription(const std::vector<int>& globalDimensionSize);
    /** Default destructor */
    virtual ~CServerDistributionDescription();

    void computeServerDistribution(int nServer, bool doComputeGlobalIndex = false,
                                   ServerDistributionType type = BAND_DISTRIBUTION);

    void computeServerGlobalIndexInRange(int nServer,
                                         const std::pair<size_t, size_t>& indexBeginEnd,
                                         ServerDistributionType = BAND_DISTRIBUTION);

    std::vector<std::vector<int> > getServerIndexBegin() const;
    std::vector<std::vector<int> > getServerDimensionSizes() const;
    const std::vector<CArray<size_t,1> >& getGlobalIndex() const;
    const boost::unordered_map<size_t,int>& getGlobalIndexRange() const;

  protected:
    void computeBandDistribution(int nServer);
    void computePlanDistribution(int nServer);

  private:
    std::vector<std::vector<int> > indexBegin_;     //!< Begin index of each dimension
    std::vector<std::vector<int> > dimensionSizes_; //!< Size of each dimension
    std::vector<int> nGlobal_; //!< Global size of each dimension

    //!< General case, index describes distribution of each server (rarely use)
    std::vector<CArray<size_t,1> > vecGlobalIndex_;

    //!< In case we need only global index of one server with specific rank
    boost::unordered_map<size_t,int> globalIndex_;
};

} // namespace xios
#endif // __XIOS_SERVER_DISTRIBUTION_DESCRIPTION_HPP
