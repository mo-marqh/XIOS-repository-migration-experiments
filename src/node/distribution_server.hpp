#ifndef __XIOS_DISTRIBUTION_SERVER_HPP__
#define __XIOS_DISTRIBUTION_SERVER_HPP__

#include "distribution.hpp"

namespace xios {
class CDistributionServer : public CDistribution
{
  public:
    /** Default constructor */
    CDistributionServer(int rank, int dims, int nServer, CArray<size_t,1>* globalIndex = 0);
    CDistributionServer(int rank, int nServer, const std::vector<int>& nGlobal);

    /** Default destructor */
    virtual ~CDistributionServer();

  protected:
    virtual void createGlobalIndex();

    //! Read info
    void readDistributionInfo(const std::vector<int>& nGlobal);
  private:
    int nServer_;
    std::vector<int> nGlobal_;
};

}
#endif // __XIOS_DISTRIBUTION_SERVER_HPP__
