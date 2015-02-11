#ifndef __XIOS_DISTRIBUTION_HPP__
#define __XIOS_DISTRIBUTION_HPP__

#include "xmlioserver_spl.hpp"
#include "array_new.hpp"

namespace xios {
class CDistribution
{
  public:
    /** Default constructor */
    CDistribution(int rank, int dims, CArray<size_t,1>* globalIndex = 0);

    /** Default destructor */
    virtual ~CDistribution();

    int getDims() const;
    int getRank() const;
  protected:
    virtual void createGlobalIndex() = 0;
  protected:
    CArray<size_t,1>* globalIndex_;
    int dims_;
    int rank_;
  private:
};

} // namespace xios
#endif // __XIOS_DISTRIBUTION_HPP__
