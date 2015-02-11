#include "distribution.hpp"

namespace xios {

CDistribution::CDistribution(int rank, int dims, CArray<size_t,1>* globalIndex)
  : rank_(rank), dims_(dims), globalIndex_(globalIndex)
{
  if (0 != globalIndex)
  {
    globalIndex_ = new CArray<size_t,1>(globalIndex->numElements());
    *globalIndex_ = *globalIndex;
  }
}

CDistribution::~CDistribution()
{
}

const CArray<size_t,1>* CDistribution::getGlobalIndex() const
{
  return globalIndex_;
}

int CDistribution::getDims() const
{
  return dims_;
}

int CDistribution::getRank() const
{
  return rank_;
}

} // namespace xios
