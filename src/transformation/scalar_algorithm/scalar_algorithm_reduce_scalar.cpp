/*!
   \file scalar_algorithm_reduce_scalar.cpp
 
   \brief Algorithm for reduce an scalar to a scalar
 */
#include "scalar_algorithm_reduce_scalar.hpp"
#include "scalar.hpp"
#include "reduce_scalar_to_scalar.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "reduction.hpp"


namespace xios {
CGenericAlgorithmTransformation* CScalarAlgorithmReduceScalar::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
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
  std::vector<CScalar*> scalarListSrcP  = gridSrc->getScalars();

  CReduceScalarToScalar* reduceScalar = dynamic_cast<CReduceScalarToScalar*> (transformation);
  int scalarDstIndex = elementPositionInGridDst2ScalarPosition[elementPositionInGrid];
  int scalarSrcIndex = elementPositionInGridSrc2AxisPosition[elementPositionInGrid];

  return (new CScalarAlgorithmReduceScalar(isSource, scalarListDestP[scalarDstIndex], scalarListSrcP[scalarSrcIndex], reduceScalar));
}
CATCH

bool CScalarAlgorithmReduceScalar::dummyRegistered_ = CScalarAlgorithmReduceScalar::registerTrans();
bool CScalarAlgorithmReduceScalar::registerTrans()
TRY
{
  return CGridTransformationFactory<CScalar>::registerTransformation(TRANS_REDUCE_SCALAR_TO_SCALAR, create);
}
CATCH

CScalarAlgorithmReduceScalar::CScalarAlgorithmReduceScalar(bool isSource, CScalar* scalarDestination, CScalar* scalarSource, CReduceScalarToScalar* algo)
 : CAlgorithmTransformationReduce(isSource)
TRY
{
  eliminateRedondantSrc_= false ;
  if (algo->operation.isEmpty())
    ERROR("CScalarAlgorithmReduceScalar::CScalarAlgorithmReduceScalar(CScalar* scalarDestination, CScalar* scalarSource, CReduceScalarToScalar* algo)",
           << "Operation must be defined."
           << "Scalar source " <<scalarSource->getId() << std::endl
           << "Scalar destination " << scalarDestination->getId());
  StdString op;
  switch (algo->operation)
  {
    case CReduceScalarToScalar::operation_attr::sum:
      operator_ = EReduction::sum;
      break;
    case CReduceScalarToScalar::operation_attr::min:
      operator_ = EReduction::min;
      break;
    case CReduceScalarToScalar::operation_attr::max:
      operator_ = EReduction::max;
      break;
    case CReduceScalarToScalar::operation_attr::average:
      operator_ = EReduction::average;
      break;
    default:
        ERROR("CScalarAlgorithmReduceScalar::CScalarAlgorithmReduceScalar(CScalar* scalarDestination, CScalar* scalarSource, CReduceScalarToScalar* algo)",
         << "Operation is wrongly defined. Supported operations: sum, min, max, average." << std::endl
         << "Scalar source " <<scalarSource->getId() << std::endl
         << "Scalar destination " << scalarDestination->getId());

  }
  transformationMapping_[0].push_back(0) ;

  scalarDestination->checkAttributes() ;
  this->computeAlgorithm(scalarSource->getLocalView(CElementView::WORKFLOW), scalarDestination->getLocalView(CElementView::WORKFLOW)) ; 
}
CATCH

CScalarAlgorithmReduceScalar::~CScalarAlgorithmReduceScalar()
TRY
{
}
CATCH


}
