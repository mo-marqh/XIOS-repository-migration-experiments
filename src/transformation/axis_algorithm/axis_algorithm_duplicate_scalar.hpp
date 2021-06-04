/*!
   \file axis_algorithm_reduce_domain.hpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for reduce an axis to a axis
 */
#ifndef __XIOS_AXIS_ALGORITHM_DUPLICATE_SCALAR_HPP__
#define __XIOS_AXIS_ALGORITHM_DUPLICATE_SCALAR_HPP__

#include "algorithm_transformation_transfer.hpp"
#include "transformation.hpp"

namespace xios {

class CAxis;
class CScalar;
class CDuplicateScalarToAxis;


/*!
  \class CAxisAlgorithmDuplicateScalar
  Duplicate scalar into axis destination
*/
class CAxisAlgorithmDuplicateScalar : public CAlgorithmTransformationTransfer
{
public:
  CAxisAlgorithmDuplicateScalar(bool isSource, CAxis* axisDestination, CScalar* scalarSource, CDuplicateScalarToAxis* algo);

  virtual ~CAxisAlgorithmDuplicateScalar();

  static bool registerTrans();
  virtual StdString getAlgoName() {return "\\nduplicate_scalar";}

  static CGenericAlgorithmTransformation* create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                CTransformation<CAxis>* transformation,
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
#endif // __XIOS_AXIS_ALGORITHM_DUPLICATE_SCALAR_HPP__
