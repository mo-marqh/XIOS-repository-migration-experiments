/*!
   \file average.cpp
   \author Ha NGUYEN
   \since 8 Sep 2016
   \date 8 Sep 2016

   \brief average reduction
 */
#include "average_reduction.hpp"

namespace xios {

CAverageReductionAlgorithm::CAverageReductionAlgorithm()
  : CReductionAlgorithm(), resetWeight_(true)
{
}

CReductionAlgorithm* CAverageReductionAlgorithm::create()
{
  return (new CAverageReductionAlgorithm());
}

bool CAverageReductionAlgorithm::registerTrans()
{
  return registerOperation(TRANS_REDUCE_AVERAGE, CAverageReductionAlgorithm::create);
}

void CAverageReductionAlgorithm::apply(const std::vector<std::pair<int,double> >& localIndex,
                                       const double* dataInput,
                                       CArray<double,1>& dataOut,
                                       std::vector<bool>& flagInitial)
{
  if (resetWeight_) { weights_.resize(flagInitial.size()); weights_ = 1.0; resetWeight_ = false; }

  int nbLocalIndex = localIndex.size();
  int currentlocalIndex = 0;
  double currentWeight  = 0.0;
  for (int idx = 0; idx < nbLocalIndex; ++idx)
  {
    currentlocalIndex = localIndex[idx].first;
    currentWeight     = localIndex[idx].second;

    if (flagInitial[currentlocalIndex])
    {
      dataOut(currentlocalIndex) = *(dataInput + idx);
      flagInitial[currentlocalIndex] = false;
    }
    else
    {
      dataOut(currentlocalIndex)  += *(dataInput + idx);
      weights_(currentlocalIndex) += 1.0;
    }
  }
}

void CAverageReductionAlgorithm::updateData(CArray<double,1>& dataOut)
{
  dataOut /= weights_;
  resetWeight_ = true;
}

}
