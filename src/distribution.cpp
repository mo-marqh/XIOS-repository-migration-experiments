/*!
   \file distribution.hpp
   \author Ha NGUYEN
   \since 13 Jan 2015
   \date 09 Feb 2015

   \brief Index distribution on server side.
 */
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
  if (0 != globalIndex_) delete globalIndex_;
}

const CArray<size_t,1>& CDistribution::getGlobalIndex() const
{
  return (*globalIndex_);
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
