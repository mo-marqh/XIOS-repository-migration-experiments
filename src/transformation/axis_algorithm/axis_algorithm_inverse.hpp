/*!
   \file axis_algorithm_inverse.hpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 09 June 2015

   \brief Algorithm for inversing an axis..
 */
#ifndef __XIOS_AXIS_ALGORITHM_INVERSE_HPP__
#define __XIOS_AXIS_ALGORITHM_INVERSE_HPP__

#include "algorithm_transformation_transfer.hpp"
#include "transformation.hpp"

namespace xios {

class CAxis;
class CInverseAxis;

/*!
  \class CAxisAlgorithmInverse
  Inversing an axis
*/
class CAxisAlgorithmInverse : public CAlgorithmTransformationTransfer
{
public:
  CAxisAlgorithmInverse(bool isSource, CAxis* axisDestination, CAxis* axisSource, CInverseAxis* inverseAxis);

  virtual ~CAxisAlgorithmInverse() {}

  static bool registerTrans();
  virtual StdString getAlgoName() {return "\\ninverse_axis";}


private:
  CAxis* axisSrc_;
  CAxis* axisDest_;
  void updateAxisValue();

public:
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
#endif // __XIOS_AXIS_ALGORITHM_INVERSE_HPP__
