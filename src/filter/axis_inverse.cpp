#include "axis_inverse.hpp"

namespace xios {

CAxisInverse::CAxisInverse(CAxis* axisDestination, CAxis* axisSource)
 : CConcreteAlgo()
{
  if (axisDestination->size.getValue() != axisSource->size.getValue())
  {
    ERROR("CAxisInverse::CAxisInverse(CAxis* axisDestination, CAxis* axisSource)",
           << "Two axis have different size"
           << "Size of axis source " <<axisSource->getId() << " is " << axisSource->size.getValue()  << std::endl
           << "Size of axis destionation " <<axisDestination->getId() << " is " << axisDestination->size.getValue());
  }


  axisDestGlobalSize_ = axisDestination->size.getValue();
  int niDest = axisDestination->ni.getValue();
  int ibeginDest = axisDestination->ibegin.getValue();

  for (int idx = 0; idx < niDest; ++idx) axisDestGlobalIndex_.push_back(ibeginDest+idx);
}

void CAxisInverse::computeIndexSourceMapping(const std::map<int, std::vector<int> >& transformationMappingOfPreviousAlgo)
{
  std::map<int, std::vector<int> >& transMap = this->transformationMapping_;
  if (transformationMappingOfPreviousAlgo.empty())
  {
    int globalIndexSize = axisDestGlobalIndex_.size();
    for (int idx = 0; idx < globalIndexSize; ++idx)
      transMap[axisDestGlobalIndex_[idx]].push_back(axisDestGlobalSize_-axisDestGlobalIndex_[idx]-1);
  }
  else
  {
    std::map<int, std::vector<int> >::const_iterator itb = transformationMappingOfPreviousAlgo.begin(), it,
                                                     ite = transformationMappingOfPreviousAlgo.end();
    for (it = itb; it != ite; ++it)
    {
      transMap[it->first].push_back(axisDestGlobalSize_-it->first-1);
    }
  }
}

}
