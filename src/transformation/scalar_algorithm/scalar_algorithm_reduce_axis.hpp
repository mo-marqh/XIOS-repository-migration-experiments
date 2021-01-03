/*!
   \file scalar_algorithm_reduce_scalar.hpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for reduce an axis to a scalar
 */
#ifndef __XIOS_SCALAR_ALGORITHM_REDUCE_AXIS_HPP__
#define __XIOS_SCALAR_ALGORITHM_REDUCE_AXIS_HPP__

#include "algorithm_transformation_reduce.hpp"
#include "transformation.hpp"

namespace xios {

class CScalar;
class CAxis;
class CReduceAxisToScalar;
class CReductionAlgorithm;

/*!
  \class CScalarAlgorithmReduceAxis
  Reducing an axis to a scalar
*/
class CScalarAlgorithmReduceAxis : public CAlgorithmTransformationReduce
{
public:
  CScalarAlgorithmReduceAxis(bool isSource, CScalar* scalarDestination, CAxis* axisSource, CReduceAxisToScalar* algo);

  
  virtual ~CScalarAlgorithmReduceAxis();

  static bool registerTrans();

private:

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
#endif // __XIOS_SCALAR_ALGORITHM_REDUCE_AXIS_HPP__
