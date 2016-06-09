/*!
   \file axis_algorithm_transformation.hpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 29 June 2015

   \brief Interface for all axis transformation algorithms.
 */

#include "axis_algorithm_transformation.hpp"
#include "axis_algorithm_inverse.hpp"
#include "axis_algorithm_zoom.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "client_client_dht_template.hpp"

namespace xios {

CAxisAlgorithmTransformation::CAxisAlgorithmTransformation(CAxis* axisDestination, CAxis* axisSource)
 : CGenericAlgorithmTransformation(), axisDest_(axisDestination), axisSrc_(axisSource)
{
  axisDestGlobalSize_ = axisDestination->n_glo.getValue();
  int niDest = axisDestination->n.getValue();
  int ibeginDest = axisDestination->begin.getValue();

  for (int idx = 0; idx < niDest; ++idx)
    if ((axisDestination->mask)(idx)) axisDestGlobalIndex_.push_back(ibeginDest+idx);
}

CAxisAlgorithmTransformation::~CAxisAlgorithmTransformation()
{
}

void CAxisAlgorithmTransformation::computeExchangeGlobalIndex(const CArray<size_t,1>& globalAxisIndex,
                                                              boost::unordered_map<int,std::vector<size_t> >& globalAxisIndexOnProc)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int clientRank = client->clientRank;
  int clientSize = client->clientSize;

  size_t globalIndex;
  int nIndexSize = axisSrc_->index.numElements();
  CClientClientDHTInt::Index2VectorInfoTypeMap globalIndex2ProcRank;
  globalIndex2ProcRank.rehash(std::ceil(nIndexSize/globalIndex2ProcRank.max_load_factor()));
  for (int idx = 0; idx < nIndexSize; ++idx)
  {
    globalIndex = axisSrc_->index(idx);
    globalIndex2ProcRank[globalIndex].push_back(clientRank);
  }

  CClientClientDHTInt dhtIndexProcRank(globalIndex2ProcRank, client->intraComm);
  dhtIndexProcRank.computeIndexInfoMapping(globalAxisIndex);

  std::vector<int> countIndex(clientSize,0);
  const CClientClientDHTInt::Index2VectorInfoTypeMap& computedGlobalIndexOnProc = dhtIndexProcRank.getInfoIndexMap();
  CClientClientDHTInt::Index2VectorInfoTypeMap::const_iterator itb = computedGlobalIndexOnProc.begin(), it,
                                                               ite = computedGlobalIndexOnProc.end();
  for (it = itb; it != ite; ++it)
  {
    const std::vector<int>& procList = it->second;
    for (int idx = 0; idx < procList.size(); ++idx) ++countIndex[procList[idx]];
  }

  globalAxisIndexOnProc.rehash(std::ceil(clientSize/globalAxisIndexOnProc.max_load_factor()));
  for (int idx = 0; idx < clientSize; ++idx)
  {
    if (0 != countIndex[idx])
    {
      globalAxisIndexOnProc[idx].resize(countIndex[idx]);
      countIndex[idx] = 0;
    }
  }

  for (it = itb; it != ite; ++it)
  {
    const std::vector<int>& procList = it->second;
    for (int idx = 0; idx < procList.size(); ++idx)
    {
      globalAxisIndexOnProc[procList[idx]][countIndex[procList[idx]]] = it->first;
      ++countIndex[procList[idx]];
    }
  }
}

void CAxisAlgorithmTransformation::computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs)
{
}

/*!
  Compute an array of global index from a global index on a axis
  \param[in] axisDestGlobalIndex global index on an axis of destination grid
  \param[in] axisSrcGlobalIndex global index on an axis of source grid (which are needed by one index on axis destination)
  \param[in] axisPositionInGrid position of the axis in the grid
  \param[in] gridDestGlobalDim dimension size of destination grid (it should share the same size for all dimension, maybe except the axis on which transformation is performed)
  \param[in] globalIndexGridDestSendToServer global index of destination grid which are to be sent to server(s), this array is already acsending sorted
  \param[in/out] globalIndexDestGrid array of global index (for 2d grid, this array is a line, for 3d, this array represents a plan). It should be preallocated
  \param[in/out] globalIndexSrcGrid array of global index of source grid (for 2d grid, this array is a line, for 3d, this array represents a plan). It should be preallocated
*/
void CAxisAlgorithmTransformation::computeGlobalGridIndexFromGlobalIndexElement(int axisDestGlobalIndex,
                                                                          const std::vector<int>& axisSrcGlobalIndex,
                                                                          const std::vector<int>& destGlobalIndexPositionInGrid,
                                                                          int axisPositionInGrid,
                                                                          const std::vector<int>& gridDestGlobalDim,
                                                                          const std::vector<int>& gridSrcGlobalDim,
                                                                          const GlobalLocalMap& globalLocalIndexDestSendToServerMap,
                                                                          std::vector<std::pair<size_t,int> >& globalLocalIndexDestMap,
                                                                          std::vector<std::vector<size_t> >& globalIndexSrcGrid)
{
  bool hasDestGlobalIndexPos = !destGlobalIndexPositionInGrid.empty();
  int globalDim = gridDestGlobalDim.size();

  std::vector<size_t> currentIndex(globalDim);
  std::vector<int> gridAxisGlobalDim(globalDim);
  std::vector<int> idxLoop(globalDim, 0);

  size_t ssize = 1, idx = 0, realGlobalIndexSize = 0;
  for (int i = 0; i< globalDim; ++i)
  {
    if (axisPositionInGrid == i) gridAxisGlobalDim[i] = 1;
    else
    {
      if (!hasDestGlobalIndexPos) gridAxisGlobalDim[i] = gridDestGlobalDim[i];
      else gridAxisGlobalDim[i] = 1;
    }
    ssize *= gridAxisGlobalDim[i];
  }

  GlobalLocalMap::const_iterator iteArr = globalLocalIndexDestSendToServerMap.end(), it;

  while (idx < ssize)
  {
    for (int i = 0; i < globalDim-1; ++i)
    {
      if (idxLoop[i] == gridAxisGlobalDim[i])
      {
        idxLoop[i] = 0;
        ++idxLoop[i+1];
      }
    }

    int j = 0;
    for (int i = 0; i < globalDim; ++i)
    {
      if (!hasDestGlobalIndexPos) currentIndex[i] = idxLoop[i];
      else
      {
        if (axisPositionInGrid == i) currentIndex[i] = axisDestGlobalIndex;
        else
        {
          currentIndex[i] = destGlobalIndexPositionInGrid[j];
          ++j;
        }
      }
    }

    currentIndex[axisPositionInGrid] = axisDestGlobalIndex;

    size_t globIndex = currentIndex[0];
    size_t mulDim = 1;
    for (int k = 1; k < globalDim; ++k)
    {
      mulDim *= gridDestGlobalDim[k-1];
      globIndex += (currentIndex[k])*mulDim;
    }

    if (iteArr != globalLocalIndexDestSendToServerMap.find(globIndex)) ++realGlobalIndexSize;
    ++idxLoop[0];
    ++idx;
  }

  if (globalLocalIndexDestMap.size() != realGlobalIndexSize)
    globalLocalIndexDestMap.resize(realGlobalIndexSize);

  if (realGlobalIndexSize != globalIndexSrcGrid.size()) globalIndexSrcGrid.resize(realGlobalIndexSize);
  for (int i = 0; i < globalIndexSrcGrid.size(); ++i)
    if (globalIndexSrcGrid[i].size() != axisSrcGlobalIndex.size())
      globalIndexSrcGrid[i].resize(axisSrcGlobalIndex.size());

  size_t realGlobalIndex = 0;
  idx = 0;
  idxLoop.assign(globalDim, 0);
  while (idx < ssize)
  {
    for (int i = 0; i < globalDim-1; ++i)
    {
      if (idxLoop[i] == gridAxisGlobalDim[i])
      {
        idxLoop[i] = 0;
        ++idxLoop[i+1];
      }
    }

    int j = 0;
    for (int i = 0; i < globalDim; ++i)
    {
      if (!hasDestGlobalIndexPos) currentIndex[i] = idxLoop[i];
      else
      {
        if (axisPositionInGrid == i) currentIndex[i] = axisDestGlobalIndex;
        else
        {
          currentIndex[i] = destGlobalIndexPositionInGrid[j];
          ++j;
        }
      }
    }
    currentIndex[axisPositionInGrid] = axisDestGlobalIndex;

    size_t globIndex = currentIndex[0];
    size_t mulDim = 1;
    for (int k = 1; k < globalDim; ++k)
    {
      mulDim *= gridDestGlobalDim[k-1];
      globIndex += (currentIndex[k])*mulDim;
    }

    it = globalLocalIndexDestSendToServerMap.find(globIndex);
    if (iteArr != it)
    {
      globalLocalIndexDestMap[realGlobalIndex] = (std::make_pair(it->first,it->second));
      for (int i = 0; i < globalIndexSrcGrid[realGlobalIndex].size(); ++i)
      {
        currentIndex[axisPositionInGrid] = axisSrcGlobalIndex[i];
        globIndex = currentIndex[0];
        mulDim = 1;
        for (int k = 1; k < globalDim; ++k)
        {
          mulDim *= gridDestGlobalDim[k-1];
          globIndex += (currentIndex[k])*mulDim;
        }
        (globalIndexSrcGrid[realGlobalIndex])[i] = globIndex;
      }
      ++realGlobalIndex;
    }

    ++idxLoop[0];
    ++idx;
  }
}
}
