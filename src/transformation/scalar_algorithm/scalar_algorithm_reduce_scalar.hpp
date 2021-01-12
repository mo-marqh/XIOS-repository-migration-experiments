/*!
   \file scalar_algorithm_reduce_scalar.hpp
   \brief Algorithm for reduce an scalar to a scalar
 */
#ifndef __XIOS_SCALAR_ALGORITHM_REDUCE_SCALAR_HPP__
#define __XIOS_SCALAR_ALGORITHM_REDUCE_SCALAR_HPP__

#include "algorithm_transformation_reduce.hpp"
#include "transformation.hpp"

namespace xios {

class CScalar;
class CReduceScalarToScalar;
class CReductionAlgorithm;

/*!
  \class CScalarAlgorithmReduceScalar
  Reducing an scalar to a scalar
*/
class CScalarAlgorithmReduceScalar : public CAlgorithmTransformationReduce
{
public:
  CScalarAlgorithmReduceScalar(bool isSource, CScalar* scalarDestination, CScalar* scalarSource, CReduceScalarToScalar* algo);

  virtual ~CScalarAlgorithmReduceScalar();

  static bool registerTrans();
protected:
  

public:
  static CGenericAlgorithmTransformation* create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                CTransformation<CScalar>* transformation,
                                                int elementPositionInGrid,
                                                std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                std::map<int, int>& elementPositionInGridDst2DomainPosition);
  static bool dummyRegistered_;
};

}
#endif // __XIOS_SCALAR_ALGORITHM_REDUCE_SCALAR_HPP__
