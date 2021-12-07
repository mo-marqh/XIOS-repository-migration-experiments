/*!
   \file scalar_algorithm_extract_scalar.hpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for extract an axis to a scalar
 */
#ifndef __XIOS_SCALAR_ALGORITHM_EXTRACT_AXIS_HPP__
#define __XIOS_SCALAR_ALGORITHM_EXTRACT_AXIS_HPP__

#include "algorithm_transformation_transfer.hpp"
#include "transformation.hpp"

namespace xios {

class CScalar;
class CAxis;
class CExtractAxisToScalar;
class CReductionAlgorithm;

/*!
  \class CScalarAlgorithmExtractAxis
  Extract a scalar from an axis
*/
class CScalarAlgorithmExtractAxis : public CAlgorithmTransformationTransfer
{
public:
  CScalarAlgorithmExtractAxis(bool isSource, CScalar* scalarDestination, CAxis* axisSource, CExtractAxisToScalar* algo);

  
  virtual ~CScalarAlgorithmExtractAxis() {} ;

  static bool registerTrans();
  virtual StdString getAlgoName() {return "\\nextract_axis";}

protected:
  int pos_;

public:
  static shared_ptr<CGenericAlgorithmTransformation> create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
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
#endif // __XIOS_SCALAR_ALGORITHM_EXTRACT_AXIS_HPP__
