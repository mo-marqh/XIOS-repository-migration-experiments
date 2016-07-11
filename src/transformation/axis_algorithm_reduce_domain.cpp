/*!
   \file axis_algorithm_reduce_domain.cpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for reduce a domain to an axis
 */
#include "axis_algorithm_reduce_domain.hpp"
#include "reduce_domain_to_axis.hpp"
#include "axis.hpp"
#include "domain.hpp"

#include "sum.hpp"

namespace xios {

CAxisAlgorithmReduceDomain::CAxisAlgorithmReduceDomain(CAxis* axisDestination, CDomain* domainSource, CReduceDomainToAxis* algo)
 : CAxisAlgorithmTransformation(axisDestination, domainSource), reduction_(0)
{
  algo->checkValid(axisDestination, domainSource);
  StdString op = algo->operation;
  StdString direction = algo->direction;
  dir_ = (0 == direction.compare("i")) ? iDir : jDir;
  reduction_ = CReductionAlgorithm::createOperation(CReductionAlgorithm::ReductionOperations[op]);
}

void CAxisAlgorithmReduceDomain::apply(const std::vector<std::pair<int,double> >& localIndex,
                                         const double* dataInput,
                                         CArray<double,1>& dataOut,
                                         std::vector<bool>& flagInitial)
{
  reduction_->apply(localIndex, dataInput, dataOut, flagInitial);
}

CAxisAlgorithmReduceDomain::~CAxisAlgorithmReduceDomain()
{
  if (0 != reduction_) delete reduction_;
}

void CAxisAlgorithmReduceDomain::computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs)
{
  this->transformationMapping_.resize(1);
  this->transformationWeight_.resize(1);

  TransformationIndexMap& transMap = this->transformationMapping_[0];
  TransformationWeightMap& transWeight = this->transformationWeight_[0];

  CArray<int,1>& axisDstIndex = axisDest_->index;
  int ni_glo = domainSrc_->ni_glo, nj_glo = domainSrc_->nj_glo;
  if (jDir == dir_)
  {
    int nbAxisIdx = axisDstIndex.numElements();
    for (int idxAxis = 0; idxAxis < nbAxisIdx; ++idxAxis)
    {
      int globalAxisIdx = axisDstIndex(idxAxis);
      transMap[globalAxisIdx].resize(ni_glo);
      transWeight[globalAxisIdx].resize(ni_glo);
      for (int idx = 0; idx < ni_glo; ++idx)
      {
        transMap[globalAxisIdx][idx] = globalAxisIdx * ni_glo + idx;
        transWeight[globalAxisIdx][idx] = 1.0;
      }
    }
  }
  else if (iDir == dir_)
  {
    int nbAxisIdx = axisDstIndex.numElements();
    for (int idxAxis = 0; idxAxis < nbAxisIdx; ++idxAxis)
    {
      int globalAxisIdx = axisDstIndex(idxAxis);
      transMap[globalAxisIdx].resize(nj_glo);
      transWeight[globalAxisIdx].resize(nj_glo);
      for (int idx = 0; idx < nj_glo; ++idx)
      {
        transMap[globalAxisIdx][idx] = globalAxisIdx + ni_glo*idx;
        transWeight[globalAxisIdx][idx] = 1.0;
      }
    }
  }
  else
  {}
}

}
