#include "axis_algorithm_inverse.hpp"

namespace xios {

CAxisAlgorithmInverse::CAxisAlgorithmInverse(CAxis* axisDestination, CAxis* axisSource)
 : CAxisAlgorithmTransformation(axisDestination, axisSource)
{
  if (axisDestination->size.getValue() != axisSource->size.getValue())
  {
    ERROR("CAxisAlgorithmInverse::CAxisAlgorithmInverse(CAxis* axisDestination, CAxis* axisSource)",
           << "Two axis have different size"
           << "Size of axis source " <<axisSource->getId() << " is " << axisSource->size.getValue()  << std::endl
           << "Size of axis destionation " <<axisDestination->getId() << " is " << axisDestination->size.getValue());
  }


  axisDestGlobalSize_ = axisDestination->size.getValue();
  int niDest = axisDestination->ni.getValue();
  int ibeginDest = axisDestination->ibegin.getValue();

  for (int idx = 0; idx < niDest; ++idx)
    if ((axisDestination->mask)(idx)) axisDestGlobalIndex_.push_back(ibeginDest+idx);
  this->computeIndexSourceMapping();
}

void CAxisAlgorithmInverse::computeIndexSourceMapping()
{
  std::map<int, std::vector<int> >& transMap = this->transformationMapping_;

  int globalIndexSize = axisDestGlobalIndex_.size();
  for (int idx = 0; idx < globalIndexSize; ++idx)
    transMap[axisDestGlobalIndex_[idx]].push_back(axisDestGlobalSize_-axisDestGlobalIndex_[idx]-1);
}

}
