/*!
   \file distribution.hpp
   \author Ha NGUYEN
   \since 13 Jan 2015
   \date 09 Feb 2015

   \brief Index distribution on server side.
 */
#ifndef __XIOS_DISTRIBUTION_HPP__
#define __XIOS_DISTRIBUTION_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"

namespace xios {
/*!
 \class CDistribution
 The parent class of CDistributionClient and CDistributionServer, which declares and defines
some basic methods and properties for its children. This class allows its descendants to calculate
distribution of index on client or server side.
*/
class CDistribution
{
  public:
    /** Default constructor */
    CDistribution(int rank, int dims, const CArray<size_t,1>& globalIndex = CArray<size_t,1>());

    /** Default destructor */
    virtual ~CDistribution();

    int getDims() const; //! Get dimension size
    int getRank() const; //! Get rank of current process

    //! Get global index
    const CArray<size_t,1>& getGlobalIndex() const;
  protected:
    virtual void createGlobalIndex() = 0;
  protected:
    CArray<size_t,1> globalIndex_;
    int dims_;
    int rank_;
};

} // namespace xios
#endif // __XIOS_DISTRIBUTION_HPP__
