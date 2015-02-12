/*!
   \file server_distribution_description.hpp
   \author Ha NGUYEN
   \since 04 Jan 2015
   \date 09 Feb 2015

   \brief Description of index distribution on server(s).
 */

#include "server_distribution_description.hpp"

namespace xios
{
CServerDistributionDescription::CServerDistributionDescription(const std::vector<int>& globalDimensionSize)
  : nGlobal_(globalDimensionSize), indexBegin_(), dimensionSizes_(), globalIndex_(0), vecGlobalIndex_()
{

}

CServerDistributionDescription::~CServerDistributionDescription()
{
  if (0 != globalIndex_) delete globalIndex_;
  if (!vecGlobalIndex_.empty())
    for (int i = 0; i < vecGlobalIndex_.size(); ++i) delete vecGlobalIndex_[i];
}

/*!
  Compute pre-defined global index distribution of server(s).
  \param [in] nServer number of server
  \param [in] doComputeGlobalIndex flag to compute global index on each server. By default, false
  \param [in] serType type of server distribution. For now, we can distribute server by band or plan
*/
void CServerDistributionDescription::computeServerDistribution(int nServer,
                                                               bool doComputeGlobalIndex,
                                                               ServerDistributionType serType)
{
  switch (serType) {
    case BAND_DISTRIBUTION:
      computeBandDistribution(nServer);
      break;
    default:
      break;
  }

  if (doComputeGlobalIndex)
  {
    vecGlobalIndex_.resize(nServer);
    int dim = nGlobal_.size();
    std::vector<int> currentIndex(dim);

    for (int idxServer = 0; idxServer < nServer; ++idxServer)
    {
      size_t ssize = 1, idx = 0;
      for (int j = 0; j < dim; ++j) ssize *= dimensionSizes_[idxServer][j];
      vecGlobalIndex_[idxServer] = new CArray<size_t,1>(ssize);

      std::vector<int> idxLoop(dim,0);

      int innerLoopSize = dimensionSizes_[idxServer][0];

      while (idx<ssize)
      {
        for (int idxDim = 0; idxDim < dim-1; ++idxDim)
        {
          if (idxLoop[idxDim] == dimensionSizes_[idxServer][idxDim])
          {
            idxLoop[idxDim] = 0;
            ++idxLoop[idxDim+1];
          }
        }

        for (int idxDim = 1; idxDim < dim; ++idxDim)  currentIndex[idxDim] = idxLoop[idxDim] + indexBegin_[idxServer][idxDim];

        size_t mulDim, globalIndex;
        for (int j = 0; j < innerLoopSize; ++j)
        {
          mulDim = 1;
          globalIndex = j + indexBegin_[idxServer][0];

          for (int k = 1; k < dim; ++k)
          {
            mulDim *= nGlobal_[k-1];
            globalIndex += (currentIndex[k])*mulDim;
          }
          (*vecGlobalIndex_[idxServer])(idx) = globalIndex;
          ++idx;
        }
        idxLoop[0] += innerLoopSize;
      }
    }
  }
}

/*!
  Compute global index of servers with band distribution
  \param [in] nServer number of server
*/
void CServerDistributionDescription::computeBandDistribution(int nServer)
{
  int dim = nGlobal_.size();
  indexBegin_.resize(nServer);
  dimensionSizes_.resize(nServer);

  for (int i = 0; i< nServer; ++i)
  {
    indexBegin_[i].resize(dim);
    dimensionSizes_[i].resize(dim);
  }

  int njRangeSize;
  int nGlobTemp = 0;
  std::vector<int> njRangeBegin(nServer,0);
  std::vector<int> njRangeEnd(nServer,0);

  if (1<dim) nGlobTemp = nGlobal_[1];
  else nGlobTemp = nGlobal_[0];

  for (int i = 0; i < nServer; ++i)
  {
    if (0 < i) njRangeBegin[i] = njRangeEnd[i-1];
    njRangeSize = nGlobTemp / nServer;
    if (i < nGlobTemp%nServer) ++njRangeSize;
    njRangeEnd[i] = njRangeSize + njRangeBegin[i];
  }
  njRangeEnd[nServer-1] = nGlobTemp;

  for (int i = 0; i < nServer; ++i)
  {
    for (int j = 0; j < dim; ++j)
    {
      if (1 != j)
      {
        if (1 == dim)
        {
          indexBegin_[i][j] = njRangeBegin[i];
          dimensionSizes_[i][j] = njRangeEnd[i] - njRangeBegin[i];
        }
        else
        {
          indexBegin_[i][j] = 0;
          dimensionSizes_[i][j] = nGlobal_[j];
        }
      }
      else
      {
        indexBegin_[i][j] = njRangeBegin[i];
        dimensionSizes_[i][j] = njRangeEnd[i] - njRangeBegin[i];
      }
    }
  }
}

/*!
  Get size of each dimension on distributed server
  \return size of dimensions on server(s)
*/
std::vector<std::vector<int> > CServerDistributionDescription::getServerDimensionSizes() const
{
  return dimensionSizes_;
}

/*!
  Get index begin of each dimension on distributed server
  \return index begin of dimensions on server(s)
*/
std::vector<std::vector<int> > CServerDistributionDescription::getServerIndexBegin() const
{
  return indexBegin_;
}

/*!
  Get global index on distributed server
  \return global index on server(s)
*/
const std::vector<CArray<size_t,1>* >& CServerDistributionDescription::getGlobalIndex() const
{
  return vecGlobalIndex_;
}

} // namespace xios
