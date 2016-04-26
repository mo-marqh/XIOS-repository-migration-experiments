/*!
   \file server_distribution_description.hpp
   \author Ha NGUYEN
   \since 04 Jan 2015
   \date 11 Jan 2016

   \brief Description of index distribution on server(s).
 */

#include "server_distribution_description.hpp"
#include "exception.hpp"

namespace xios
{
  /*!
  \param [in] globalDimensionSize global dimension of grid
  \param [in] nServer number of server
  \param [in] serType type of server distribution. For now, we can distribute server by band or plan
  */
CServerDistributionDescription::CServerDistributionDescription(const std::vector<int>& globalDimensionSize,
                                                               int nServer,
                                                               ServerDistributionType serType)
  : nGlobal_(globalDimensionSize), indexBegin_(), dimensionSizes_(), globalIndex_(),
    vecGlobalIndex_(), serverType_(serType), nServer_(nServer), positionDimensionDistributed_(1)
{
}

CServerDistributionDescription::~CServerDistributionDescription()
{ /* Nothing to do */ }

/*!
  Compute pre-defined global index distribution of server(s).
  \param [in] doComputeGlobalIndex flag to compute global index on each server. By default, false

*/
void CServerDistributionDescription::computeServerDistribution(bool doComputeGlobalIndex,
                                                               int positionDimensionDistributed)
{
  switch (serverType_) {
    case BAND_DISTRIBUTION:
      computeBandDistribution(nServer_, positionDimensionDistributed);
      break;
    default:
      break;
  }

  if (doComputeGlobalIndex)
  {
    vecGlobalIndex_.resize(nServer_);
    int dim = nGlobal_.size();
    std::vector<int> currentIndex(dim);

    for (int idxServer = 0; idxServer < nServer_; ++idxServer)
    {
      size_t ssize = 1, idx = 0;
      for (int j = 0; j < dim; ++j) ssize *= dimensionSizes_[idxServer][j];
      vecGlobalIndex_[idxServer].resize(ssize);

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
            globalIndex += currentIndex[k] * mulDim;
          }
          vecGlobalIndex_[idxServer](idx) = globalIndex;
          ++idx;
        }
        idxLoop[0] += innerLoopSize;
      }
    }
  }
}

/*!
  Compute global index assigned to a server with a range.E.g: if a grid has 100 points and
  there are 2 servers, the first one takes index from 0 to 49, the second has index from 50 to 99

  \param [in] indexBeginEnd begining and ending index of range
*/
void CServerDistributionDescription::computeServerGlobalIndexInRange(const std::pair<size_t, size_t>& indexBeginEnd,
                                                                     int positionDimensionDistributed)
{
  switch (serverType_) {
    case BAND_DISTRIBUTION:
      computeBandDistribution(nServer_, positionDimensionDistributed);
      break;
    default:
      break;
  }

  size_t indexBegin = indexBeginEnd.first;
  size_t indexEnd   = indexBeginEnd.second;
  if (indexBegin > indexEnd)
     ERROR("CServerDistributionDescription::computeServerGlobalIndexInRange",
           << "Index begin is larger than index end");

  globalIndex_.rehash(std::ceil((indexEnd-indexBegin+1)/globalIndex_.max_load_factor()));

  int dim = nGlobal_.size();
  std::vector<int> currentIndex(dim);

  for (int idxServer = 0; idxServer < nServer_; ++idxServer)
  {
    size_t ssize = 1, idx = 0;
    for (int j = 0; j < dim; ++j) ssize *= dimensionSizes_[idxServer][j];

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
        if ((indexBegin <= globalIndex) && (globalIndex <= indexEnd))
          globalIndex_[globalIndex] = idxServer;
        ++idx;
      }
      idxLoop[0] += innerLoopSize;
    }
  }

}

/*!
  Compute global index of servers with band distribution
  \param [in] nServer number of server
*/
void CServerDistributionDescription::computeBandDistribution(int nServer, int positionDimensionDistributed)
{
  int dim = nGlobal_.size();
  positionDimensionDistributed_ = positionDimensionDistributed;
  if (1 == dim) positionDimensionDistributed_ = 0;
  if (positionDimensionDistributed_ > dim)
    ERROR("CServerDistributionDescription::computeBandDistribution(int nServer, int positionDimensionDistributed)",
          << "Position of distributed dimension is invalid" << std::endl
          << "Position of distributed dimension is " << positionDimensionDistributed_
          << "Dimension " << dim)

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

  int positionDistributed = (1<dim) ? positionDimensionDistributed_ : 0;
  nGlobTemp = nGlobal_[positionDistributed];

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
      if (positionDistributed != j)
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
const std::vector<CArray<size_t,1> >& CServerDistributionDescription::getGlobalIndex() const
{
  return vecGlobalIndex_;
}

/*!
  Get global index calculated by computeServerGlobalIndexInRange
*/
const boost::unordered_map<size_t,int>& CServerDistributionDescription::getGlobalIndexRange() const
{
  return globalIndex_;
}

int CServerDistributionDescription::getDimensionDistributed()
{
  return ((1<nGlobal_.size()) ? positionDimensionDistributed_ : 0);
}

} // namespace xios
