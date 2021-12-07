/*!
   \file axis_algorithm_interpolate.hpp
   \author Ha NGUYEN
   \since 23 June 2015
   \date 23 June 2015

   \brief Algorithm for interpolation on an axis.
 */
#ifndef __XIOS_AXIS_ALGORITHM_INTERPOLATE_HPP__
#define __XIOS_AXIS_ALGORITHM_INTERPOLATE_HPP__

#include "algorithm_transformation_weight.hpp"
#include "transformation.hpp"

namespace xios {

class CAxis;
class CGrid;
class CInterpolateAxis;

/*!
  \class CAxisAlgorithmInterpolate
  Implementing interpolation on axis
  The values on axis source are assumed monotonic
*/
class CAxisAlgorithmInterpolate : public CAlgorithmTransformationWeight
{
public:
  CAxisAlgorithmInterpolate(bool isSource, CAxis* axisDestination, CAxis* axisSource, CInterpolateAxis* interpAxis);

  virtual ~CAxisAlgorithmInterpolate() {}

  static bool registerTrans();
  virtual StdString getAlgoName() {return "\\ninterpolate_axis";}

private:
  void computeRemap(const std::vector<CArray<double,1>* >& dataAuxInputs) ;
  void retrieveAllAxisValue(const CArray<double,1>& axisValue, const CArray<bool,1>& axisMask,
                            std::vector<double>& recvBuff, std::vector<int>& indexVec);
  void computeInterpolantPoint(const std::vector<double>& recvBuff, const std::vector<int>&, int transPos = 0);
  void computeWeightedValueAndMapping(const std::map<int, std::vector<std::pair<int,double> > >& interpolatingIndexValues, int transPos = 0);
  void fillInAxisValue(std::vector<CArray<double,1> >& vecAxisValue,
                       const std::vector<CArray<double,1>* >& dataAuxInputs);

private:
  // Interpolation order
  int order_;
  StdString coordinate_;
  std::vector<std::vector<int> > transPosition_;
  CAxis* axisSrc_=nullptr ;
  CAxis* axisDest_=nullptr;

public:
  static shared_ptr<CGenericAlgorithmTransformation> create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
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

#endif // __XIOS_AXIS_ALGORITHM_INTERPOLATE_HPP__
