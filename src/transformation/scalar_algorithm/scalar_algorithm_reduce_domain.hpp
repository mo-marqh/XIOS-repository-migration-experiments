/*!
   \file scalar_algorithm_reduce_scalar.hpp
   \author Ha NGUYEN
   \since 13 Oct 2016
   \date 13 Oct 2016

   \brief Algorithm for reduce an DOMAIN to a scalar
 */
#ifndef __XIOS_SCALAR_ALGORITHM_REDUCE_DOMAIN_HPP__
#define __XIOS_SCALAR_ALGORITHM_REDUCE_DOMAIN_HPP__

#include "algorithm_transformation_reduce.hpp"
#include "transformation.hpp"

namespace xios {

class CScalar;
class CDomain;
class CReduceDomainToScalar;
class CReductionAlgorithm;

/*!
  \class CScalarAlgorithmReduceDomain
  Reducing an DOMAIN to a scalar
*/
class CScalarAlgorithmReduceDomain : public CAlgorithmTransformationReduce
{
public:
  CScalarAlgorithmReduceDomain(bool isSource, CScalar* scalarDestination, CDomain* domainSource, CReduceDomainToScalar* algo);

  virtual ~CScalarAlgorithmReduceDomain();

  static bool registerTrans();

protected:
  CDomain* domainSrc_ ;


public:
  static CGenericAlgorithmTransformation* create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                CTransformation<CScalar>* transformation,
                                                int elementPositionInGrid,
                                                std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                std::map<int, int>& elementPositionInGridSrc2DOMAINPosition,
                                                std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                std::map<int, int>& elementPositionInGridDst2DOMAINPosition,
                                                std::map<int, int>& elementPositionInGridDst2DomainPosition);
  static bool dummyRegistered_;
};

}
#endif // __XIOS_SCALAR_ALGORITHM_REDUCE_DOMAIN_HPP__
