/*!
   \file scalar_algorithm_reduce_scalar.cpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for reduce an axis to a scalar
 */
#include "scalar_algorithm_reduce_axis.hpp"
#include "axis.hpp"
#include "scalar.hpp"
#include "reduce_axis_to_scalar.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "reduction.hpp"

#include "reduction.hpp"

namespace xios {
shared_ptr<CGenericAlgorithmTransformation> CScalarAlgorithmReduceAxis::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                                     CTransformation<CScalar>* transformation,
                                                                     int elementPositionInGrid,
                                                                     std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                                     std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                                     std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                                     std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                                     std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                                     std::map<int, int>& elementPositionInGridDst2DomainPosition)
TRY
{
  std::vector<CScalar*> scalarListDestP = gridDst->getScalars();
  std::vector<CAxis*> axisListSrcP  = gridSrc->getAxis();

  CReduceAxisToScalar* reduceAxis = dynamic_cast<CReduceAxisToScalar*> (transformation);
  int scalarDstIndex = elementPositionInGridDst2ScalarPosition[elementPositionInGrid];
  int axisSrcIndex = elementPositionInGridSrc2AxisPosition[elementPositionInGrid];

  return make_shared<CScalarAlgorithmReduceAxis>(isSource, scalarListDestP[scalarDstIndex], axisListSrcP[axisSrcIndex], reduceAxis);
}
CATCH

bool CScalarAlgorithmReduceAxis::dummyRegistered_ = CScalarAlgorithmReduceAxis::registerTrans();
bool CScalarAlgorithmReduceAxis::registerTrans()
TRY
{
  return CGridTransformationFactory<CScalar>::registerTransformation(TRANS_REDUCE_AXIS_TO_SCALAR, create);
}
CATCH

CScalarAlgorithmReduceAxis::CScalarAlgorithmReduceAxis(bool isSource, CScalar* scalarDestination, CAxis* axisSource, CReduceAxisToScalar* algo)
 : CAlgorithmTransformationReduce(isSource)
TRY
{
  if (algo->operation.isEmpty())
    ERROR("CScalarAlgorithmReduceAxis::CScalarAlgorithmReduceAxis(CAxis* axisDestination, CAxis* axisSource, CReduceAxisToScalar* algo)",
           << "Operation must be defined."
           << "Axis source " <<axisSource->getId() << std::endl
           << "Scalar destination " << scalarDestination->getId());
  StdString op;
  switch (algo->operation)
  {
    case CReduceAxisToScalar::operation_attr::sum:
      operator_ = EReduction::sum;
      break;
    case CReduceAxisToScalar::operation_attr::min:
      operator_ = EReduction::min;
      break;
    case CReduceAxisToScalar::operation_attr::max:
      operator_ = EReduction::max;
      break;
    case CReduceAxisToScalar::operation_attr::average:
      operator_ = EReduction::average;
      break;
    default:
        ERROR("CScalarAlgorithmReduceAxis::CScalarAlgorithmReduceAxis(CScalar* scalarDestination, CAxis* axisSource, CReduceAxisToScalar* algo)",
         << "Operation is wrongly defined. Supported operations: sum, min, max, average." << std::endl
         << "Domain source " <<axisSource->getId() << std::endl
         << "Scalar destination " << scalarDestination->getId());

  } 
  
  int globalIndexSize = axisSource-> n_glo;
  for (int idx = 0; idx < globalIndexSize; ++idx)  transformationMapping_[0].push_back(idx);

  scalarDestination->checkAttributes() ;
  this->computeAlgorithm(axisSource->getLocalView(CElementView::WORKFLOW), scalarDestination->getLocalView(CElementView::WORKFLOW)) ;
  
}
CATCH


CScalarAlgorithmReduceAxis::~CScalarAlgorithmReduceAxis()
TRY
{
}
CATCH

}
