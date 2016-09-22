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
#include "sum.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"

namespace xios {
CGenericAlgorithmTransformation* CScalarAlgorithmReduceScalar::create(CGrid* gridDst, CGrid* gridSrc,
                                                                     CTransformation<CScalar>* transformation,
                                                                     int elementPositionInGrid,
                                                                     std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                                     std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                                     std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                                     std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                                     std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                                     std::map<int, int>& elementPositionInGridDst2DomainPosition)
{
  std::vector<CScalar*> scalarListDestP = gridDst->getScalars();
  std::vector<CAxis*> axisListSrcP  = gridSrc->getAxis();

  CReduceAxisToScalar* reduceAxis = dynamic_cast<CReduceAxisToScalar*> (transformation);
  int scalarDstIndex = elementPositionInGridDst2ScalarPosition[elementPositionInGrid];
  int axisSrcIndex = elementPositionInGridSrc2AxisPosition[elementPositionInGrid];

  return (new CScalarAlgorithmReduceScalar(scalarListDestP[scalarDstIndex], axisListSrcP[axisSrcIndex], reduceAxis));
}

bool CScalarAlgorithmReduceScalar::registerTrans()
{
  CGridTransformationFactory<CScalar>::registerTransformation(TRANS_REDUCE_AXIS_TO_SCALAR, create);
}

CScalarAlgorithmReduceScalar::CScalarAlgorithmReduceScalar(CScalar* scalarDestination, CAxis* axisSource, CReduceAxisToScalar* algo)
 : CScalarAlgorithmTransformation(scalarDestination, axisSource),
   reduction_(0)
{
  if (algo->operation.isEmpty())
    ERROR("CScalarAlgorithmReduceScalar::CScalarAlgorithmReduceScalar(CAxis* axisDestination, CAxis* axisSource, CReduceAxisToScalar* algo)",
           << "Operation must be defined."
           << "Axis source " <<axisSource->getId() << std::endl
           << "Scalar destination " << scalarDestination->getId());
  StdString op = algo->operation;
  if (CReductionAlgorithm::ReductionOperations.end() == CReductionAlgorithm::ReductionOperations.find(op))
    ERROR("CScalarAlgorithmReduceScalar::CScalarAlgorithmReduceScalar(CAxis* axisDestination, CAxis* axisSource, CReduceAxisToScalar* algo)",
       << "Operation '" << op << "' not found. Please make sure to use a supported one"
       << "Axis source " <<axisSource->getId() << std::endl
       << "Scalar destination " << scalarDestination->getId());

  reduction_ = CReductionAlgorithm::createOperation(CReductionAlgorithm::ReductionOperations[op]);
}

void CScalarAlgorithmReduceScalar::apply(const std::vector<std::pair<int,double> >& localIndex,
                                         const double* dataInput,
                                         CArray<double,1>& dataOut,
                                         std::vector<bool>& flagInitial,
                                         const double& defaultValue)
{
  reduction_->apply(localIndex, dataInput, dataOut, flagInitial);
}

CScalarAlgorithmReduceScalar::~CScalarAlgorithmReduceScalar()
{
  if (0 != reduction_) delete reduction_;
}

void CScalarAlgorithmReduceScalar::computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs)
{
  this->transformationMapping_.resize(1);
  this->transformationWeight_.resize(1);

  TransformationIndexMap& transMap = this->transformationMapping_[0];
  TransformationWeightMap& transWeight = this->transformationWeight_[0];

  CArray<int,1>& axisSrcIndex = axisSrc_->index;
  int globalIndexSize = axisSrcIndex.numElements();

  for (int idx = 0; idx < globalIndexSize; ++idx)
  {
    transMap[0].push_back(axisSrcIndex(idx));
    transWeight[0].push_back(1.0);
  }
}

}
