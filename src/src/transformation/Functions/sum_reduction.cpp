/*!
   \file sum.cpp
   \author Ha NGUYEN
   \since 27 June 2016
   \date 27 June 2016

   \brief sum reduction
 */
#include "sum_reduction.hpp"

namespace xios {

CSumReductionAlgorithm::CSumReductionAlgorithm()
  : CReductionAlgorithm()
{
}

CReductionAlgorithm* CSumReductionAlgorithm::create()
{
  return (new CSumReductionAlgorithm());
}

bool CSumReductionAlgorithm::registerTrans()
{
  return registerOperation(TRANS_REDUCE_SUM, CSumReductionAlgorithm::create);
}

void CSumReductionAlgorithm::apply(const std::vector<std::pair<int,double> >& localIndex,
                                   const double* dataInput,
                                   CArray<double,1>& dataOut,
                                   std::vector<bool>& flagInitial)
{
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
      dataOut(currentlocalIndex) += *(dataInput + idx);
    }
  }
}

}
