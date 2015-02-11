/*!
   \file distribution_server.hpp
   \author Ha NGUYEN
   \since 13 Jan 2015
   \date 04 Feb 2015

   \brief Index distribution on server side.
 */

#ifndef __XIOS_DISTRIBUTION_SERVER_HPP__
#define __XIOS_DISTRIBUTION_SERVER_HPP__

#include "distribution.hpp"

namespace xios {

/*!
  \class CDistributionServer
  The class, for now, plays a role of computing local index for writing data on server
*/
class CDistributionServer : public CDistribution
{
  public:
    /** Default constructor */
    CDistributionServer(int rank, int dims, CArray<size_t,1>* globalIndex = 0);
    CDistributionServer(int rank, const std::vector<int>& nZoomBegin,
                        const std::vector<int>& nZoomSize, const std::vector<int>& nGlobal);

    /** Default destructor */
    virtual ~CDistributionServer();

    virtual CArray<size_t,1> computeLocalIndex(const CArray<size_t,1>& globalIndex);
    virtual void computeLocalIndex(CArray<size_t,1>& globalIndex);

  protected:
    virtual void createGlobalIndex();
  private:
    std::vector<int> nGlobal_;
    std::vector<int> nZoomSize_;
    std::vector<int> nZoomBegin_;
};

} // namespace xios
#endif // __XIOS_DISTRIBUTION_SERVER_HPP__
