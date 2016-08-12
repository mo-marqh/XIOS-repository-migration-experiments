/*!
   \file axis_algorithm_reduce_domain.cpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for extract a domain to an axis
 */
#include "axis_algorithm_extract_domain.hpp"
#include "extract_domain_to_axis.hpp"
#include "axis.hpp"
#include "domain.hpp"

#include "sum.hpp"

namespace xios {

CAxisAlgorithmExtractDomain::CAxisAlgorithmExtractDomain(CAxis* axisDestination, CDomain* domainSource, CExtractDomainToAxis* algo)
 : CAxisAlgorithmTransformation(axisDestination, domainSource), pos_(-1), reduction_(0)
{
  algo->checkValid(axisDestination, domainSource);
  StdString op = "extract";
  StdString direction = algo->direction;
  dir_ = (0 == direction.compare("i")) ? iDir : jDir;
  pos_ = algo->position;
  reduction_ = CReductionAlgorithm::createOperation(CReductionAlgorithm::ReductionOperations[op]);
}

void CAxisAlgorithmExtractDomain::apply(const std::vector<std::pair<int,double> >& localIndex,
                                        const double* dataInput,
                                        CArray<double,1>& dataOut,
                                        std::vector<bool>& flagInitial,
                                        const double& defaultValue)
{
  reduction_->apply(localIndex, dataInput, dataOut, flagInitial);
}

CAxisAlgorithmExtractDomain::~CAxisAlgorithmExtractDomain()
{
  if (0 != reduction_) delete reduction_;
}

void CAxisAlgorithmExtractDomain::computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs)
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
      transMap[globalAxisIdx].resize(1);
      transWeight[globalAxisIdx].resize(1);
      transMap[globalAxisIdx][0] = globalAxisIdx * ni_glo + pos_;
      transWeight[globalAxisIdx][0] = 1.0;

    }
  }
  else if (iDir == dir_)
  {
    int nbAxisIdx = axisDstIndex.numElements();
    for (int idxAxis = 0; idxAxis < nbAxisIdx; ++idxAxis)
    {
      int globalAxisIdx = axisDstIndex(idxAxis);
      transMap[globalAxisIdx].resize(1);
      transWeight[globalAxisIdx].resize(1);
      transMap[globalAxisIdx][0] = globalAxisIdx + ni_glo * pos_;
      transWeight[globalAxisIdx][0] = 1.0;
    }
  }
  else
  {}
}

}
