/*!
   \file domain_algorithm_transformation.hpp
   \author Ha NGUYEN
   \since 02 Jul 2015
   \date 02 Jul 2015

   \brief Interface for all domain transformation algorithms.
 */
#ifndef __XIOS_DOMAIN_ALGORITHM_TRANSFORMATION_HPP__
#define __XIOS_DOMAIN_ALGORITHM_TRANSFORMATION_HPP__

#include "generic_algorithm_transformation.hpp"
#include "domain.hpp"

namespace xios {

/*!
  \class CDomainAlgorithmTransformation
  Algorithms for domain.
*/
class CDomainAlgorithmTransformation : public virtual CGenericAlgorithmTransformation
{
public:
  CDomainAlgorithmTransformation(CDomain* domainDestination, CDomain* domainSource);

  virtual ~CDomainAlgorithmTransformation();

protected:
  virtual void computeGlobalGridIndexFromGlobalIndexElement(int domainDestGlobalIndex,
                                                        const std::vector<int>& domainSrcGlobalIndex,
                                                        int domainPositionInGrid,
                                                        const std::vector<int>& gridDestGlobalDim,
                                                        const std::vector<int>& gridSrcGlobalDim,
                                                        const std::vector<size_t>& globalIndexGridDestSendToServer,
                                                        CArray<size_t,1>& globalIndexDestGrid,
                                                        std::vector<std::vector<size_t> >& globalIndexSrcGrid);
  void computeIndexSourceMapping();

protected:
  inline void domainGlobalIndex(const int& index, const int& niGlob, const int& njGlob,
                                int& iIndex, int& jIndex);

protected:
    //! Domain on grid destination
  CDomain* domainDest_;

  //! Domain on grid source
  CDomain* domainSrc_;
};

}
#endif // __XIOS_DOMAIN_ALGORITHM_TRANSFORMATION_HPP__
