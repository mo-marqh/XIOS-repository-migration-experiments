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
#include "grid_algorithm_reduce.hpp"



namespace xios {
shared_ptr<CGenericAlgorithmTransformation> CScalarAlgorithmReduceScalar::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
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

  return make_shared<CScalarAlgorithmReduceScalar>(isSource, scalarListDestP[scalarDstIndex], scalarListSrcP[scalarSrcIndex], reduceScalar);
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
 : CAlgorithmTransformationReduce(algo->getContext(), isSource)
TRY
{
  scalarDestination->checkAttributes() ;
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
  
  auto& transMap = this->transformationMapping_;
  
  CArray<size_t,1> dstGlobalIndex ;
  scalarDestination->getLocalView(CElementView::WORKFLOW)->getGlobalIndexView(dstGlobalIndex) ;
  size_t nbIdx = dstGlobalIndex.numElements();

  for (size_t idx = 0; idx < nbIdx; ++idx)
  {
    size_t globalIdx = dstGlobalIndex(idx);
    transMap[globalIdx].resize(1);
    transMap[globalIdx][0]=globalIdx ;      
  }

  this->computeAlgorithm(scalarSource->getLocalView(CElementView::WORKFLOW), scalarDestination->getLocalView(CElementView::WORKFLOW)) ; 
}
CATCH

shared_ptr<CGridAlgorithm> CScalarAlgorithmReduceScalar::createGridAlgorithm(CGrid* gridSrc, CGrid* gridDst, int pos)
{
  auto algo=make_shared<CGridAlgorithmReduce>(gridSrc, gridDst, pos, shared_from_this(), operator_) ;
  algo->computeAlgorithm(false) ;
  return algo ; 
}

CScalarAlgorithmReduceScalar::~CScalarAlgorithmReduceScalar()
TRY
{
}
CATCH


}
