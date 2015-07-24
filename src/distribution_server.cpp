/*!
   \file distribution_server.cpp
   \author Ha NGUYEN
   \since 13 Jan 2015
   \date 04 Feb 2015

   \brief Index distribution on server side.
 */
#include "distribution_server.hpp"

namespace xios {

CDistributionServer::CDistributionServer(int rank, int dims, const CArray<size_t,1>& globalIndex)
  : CDistribution(rank, dims, globalIndex), nGlobal_(), nZoomSize_(), nZoomBegin_()
{
}

CDistributionServer::CDistributionServer(int rank, const std::vector<int>& nZoomBegin,
                                         const std::vector<int>& nZoomSize, const std::vector<int>& nGlobal)
  : CDistribution(rank, nGlobal.size()), nGlobal_(nGlobal), nZoomSize_(nZoomSize), nZoomBegin_(nZoomBegin)
{
  createGlobalIndex();
}

CDistributionServer::CDistributionServer(int rank, const std::vector<int>& nZoomBegin,
                                         const std::vector<int>& nZoomSize,
                                         const std::vector<int>& nZoomBeginGlobal,
                                         const std::vector<int>& nGlobal)
  : CDistribution(rank, nGlobal.size()), nGlobal_(nGlobal), nZoomBeginGlobal_(nZoomBeginGlobal),
    nZoomSize_(nZoomSize), nZoomBegin_(nZoomBegin)
{
  createGlobalIndex();
}

CDistributionServer::~CDistributionServer()
{
}

/*!
  Create global index on server side
  Like the similar function on client side, this function serves on creating global index
for data written by the server. The global index is used to calculating local index of data
written on each server
*/
void CDistributionServer::createGlobalIndex()
{
  size_t idx = 0, ssize = 1;
  for (int i = 0; i < nZoomSize_.size(); ++i) ssize *= nZoomSize_[i];

  this->globalIndex_.resize(ssize);
  std::vector<int> idxLoop(this->getDims(),0);
  std::vector<int> currentIndex(this->getDims());
  int innerLoopSize = nZoomSize_[0];

  while (idx<ssize)
  {
    for (int i = 0; i < this->dims_-1; ++i)
    {
      if (idxLoop[i] == nZoomSize_[i])
      {
        idxLoop[i] = 0;
        ++idxLoop[i+1];
      }
    }

    for (int i = 1; i < this->dims_; ++i)  currentIndex[i] = idxLoop[i] + nZoomBegin_[i];

    size_t mulDim, globalIndex;
    for (int i = 0; i < innerLoopSize; ++i)
    {
      mulDim = 1;
      globalIndex = i + nZoomBegin_[0];

      for (int k = 1; k < this->dims_; ++k)
      {
        mulDim *= nGlobal_[k-1];
        globalIndex += (currentIndex[k])*mulDim;
      }
      this->globalIndex_(idx) = globalIndex;
      ++idx;
    }
    idxLoop[0] += innerLoopSize;
  }
}

/*!
  Compute local index for writing data on server
  \param [in] globalIndex global index received from client
  \return local index of written data
*/
CArray<size_t,1> CDistributionServer::computeLocalIndex(const CArray<size_t,1>& globalIndex)
{
  CArray<size_t,1>::const_iterator itBegin = this->globalIndex_.begin(),
                                   itEnd   = this->globalIndex_.end(), it;

  int ssize = globalIndex.numElements(), idx = 0;
  CArray<size_t,1> localIndex(ssize);
  it = itBegin;
  for (int i = 0; i < ssize; ++i)
  {
    it = std::find(itBegin, itEnd, globalIndex(i));
//    it = std::lower_bound(it, itEnd, globalIndex(i));
    if (itEnd != it)
    {
      localIndex(idx) = std::distance(itBegin, it);
      ++idx;
    }
  }

  return localIndex;
}

/*!
  Compute local index for writing data on server
  \param [in] globalIndex Global index received from client
*/
void CDistributionServer::computeLocalIndex(CArray<size_t,1>& globalIndex)
{
  CArray<size_t,1>::const_iterator itBegin = this->globalIndex_.begin(),
                                   itEnd   = this->globalIndex_.end(), it;

  int ssize = globalIndex.numElements(), idx = 0;
  CArray<size_t,1> localIndex(ssize);
  it = itBegin;
  for (int i = 0; i < ssize; ++i)
  {
    it = std::find(itBegin, itEnd, globalIndex(i));
//    it = std::lower_bound(it, itEnd, globalIndex(i));
    if (itEnd != it)
    {
      localIndex(idx) = std::distance(itBegin, it);
      ++idx;
    }
  }

  globalIndex = localIndex;
}


std::vector<int> CDistributionServer::getZoomBeginGlobal() const
{
  return nZoomBeginGlobal_;
}

std::vector<int> CDistributionServer::getZoomBeginServer() const
{
  return nZoomBegin_;
}

std::vector<int> CDistributionServer::getZoomSizeServer() const
{
  return nZoomSize_;
}
} // namespace xios
