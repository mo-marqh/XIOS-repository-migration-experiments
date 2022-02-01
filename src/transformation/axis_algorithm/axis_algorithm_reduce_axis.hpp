/*!
   \file axis_algorithm_reduce_axis.hpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for reduce an axis to a axis
 */
#ifndef __XIOS_AXIS_ALGORITHM_REDUCE_AXIS_HPP__
#define __XIOS_AXIS_ALGORITHM_REDUCE_AXIS_HPP__

#include "algorithm_transformation_reduce.hpp"
#include "transformation.hpp"

namespace xios {

class CAxis;
class CReduceAxisToAxis;
class CReductionAlgorithm;

/*!
  \class CAxisAlgorithmReduceAxis
  Reduce a axis to an axis
*/
class CAxisAlgorithmReduceAxis : public CAlgorithmTransformationReduce
{
public:
  CAxisAlgorithmReduceAxis(bool isSource, CAxis* axisDestination, CAxis* axisSource, CReduceAxisToAxis* algo);

  
  virtual ~CAxisAlgorithmReduceAxis();

  static bool registerTrans();
  virtual StdString getAlgoName() {return "\\nreduce_axis_to_axis";}

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

  virtual shared_ptr<CGridAlgorithm> createGridAlgorithm(CGrid* gridSrc, CGrid* gridDst, int pos);
  static bool dummyRegistered_;
};

}
#endif // __XIOS_AXIS_ALGORITHM_REDUCE_AXIS_HPP__
