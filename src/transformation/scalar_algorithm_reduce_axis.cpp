/*!
   \file scalar_algorithm_reduce_scalar.cpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for reduce an axis to a scalar
 */
#include "scalar_algorithm_reduce_axis.hpp"
//#include "context.hpp"
//#include "context_client.hpp"
//#include "client_client_dht_template.hpp"
#include "axis.hpp"
#include "scalar.hpp"
#include "reduce_axis_to_scalar.hpp"
#include "sum.hpp"

namespace xios {

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
                                         std::vector<bool>& flagInitial)
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
