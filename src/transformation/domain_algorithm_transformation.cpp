/*!
   \file domain_algorithm_transformation.hpp
   \author Ha NGUYEN
   \since 02 Jul 2015
   \date 02 Jul 2015

   \brief Interface for all domain transformation algorithms.
 */

#include "domain_algorithm_transformation.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "domain.hpp"

namespace xios {

CDomainAlgorithmTransformation::CDomainAlgorithmTransformation(CDomain* domainDestination, CDomain* domainSource)
 : CGenericAlgorithmTransformation(), domainDest_(domainDestination), domainSrc_(domainSource)
{
}

CDomainAlgorithmTransformation::~CDomainAlgorithmTransformation()
{
}

void CDomainAlgorithmTransformation::computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs)
{
}

void CDomainAlgorithmTransformation::computeExchangeGlobalIndex(const CArray<size_t,1>& globalDomainIndex,
                                                                CClientClientDHTInt::Index2VectorInfoTypeMap& globalDomainIndexOnProc)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int clientRank = client->clientRank;
  int clientSize = client->clientSize;

  int niGlob = domainSrc_->ni_glo.getValue();
  int njGlob = domainSrc_->nj_glo.getValue();
  size_t globalIndex;
  int nIndexSize = domainSrc_->i_index.numElements(), i_ind, j_ind;
  CClientClientDHTInt::Index2VectorInfoTypeMap globalIndex2ProcRank;
  globalIndex2ProcRank.rehash(std::ceil(nIndexSize/globalIndex2ProcRank.max_load_factor()));
  for (int idx = 0; idx < nIndexSize; ++idx)
  {
    i_ind=domainSrc_->i_index(idx) ;
    j_ind=domainSrc_->j_index(idx) ;

    globalIndex = i_ind + j_ind * niGlob;
    globalIndex2ProcRank[globalIndex].push_back(clientRank);
  }

  CClientClientDHTInt dhtIndexProcRank(globalIndex2ProcRank, client->intraComm);
  dhtIndexProcRank.computeIndexInfoMapping(globalDomainIndex);
  globalDomainIndexOnProc = dhtIndexProcRank.getInfoIndexMap();


//  std::vector<int> countIndex(clientSize,0);
//  const CClientClientDHTInt::Index2VectorInfoTypeMap& computedGlobalIndexOnProc = dhtIndexProcRank.getInfoIndexMap();
//  CClientClientDHTInt::Index2VectorInfoTypeMap::const_iterator itb = computedGlobalIndexOnProc.begin(), it,
//                                                               ite = computedGlobalIndexOnProc.end();
//  for (it = itb; it != ite; ++it)
//  {
//    const std::vector<int>& procList = it->second;
//    for (int idx = 0; idx < procList.size(); ++idx) ++countIndex[procList[idx]];
//  }
//
//  globalDomainIndexOnProc.rehash(std::ceil(clientSize/globalDomainIndexOnProc.max_load_factor()));
//  for (int idx = 0; idx < clientSize; ++idx)
//  {
//    if (0 != countIndex[idx])
//    {
//      globalDomainIndexOnProc[idx].resize(countIndex[idx]);
//      countIndex[idx] = 0;
//    }
//  }
//
//  for (it = itb; it != ite; ++it)
//  {
//    const std::vector<int>& procList = it->second;
//    for (int idx = 0; idx < procList.size(); ++idx)
//    {
//      globalDomainIndexOnProc[procList[idx]][countIndex[procList[idx]]] = it->first;
//      ++countIndex[procList[idx]];
//    }
//  }
}

/*!
  Compute an array of global index from a global index on a domain
  \param[in] domainDestGlobalIndex global index on an domain of destination grid
  \param[in] domainSrcGlobalIndex global index on an domain of source grid (which are needed by one index on domain destination)
  \param[in] domainPositionInGrid position of the domain in the grid
  \param[in] gridDestGlobalDim dimension size of destination grid (it should share the same size for all dimension, maybe except the domain on which transformation is performed)
  \param[in] gridSrcGlobalDim dimension size of source grid (it should share the same size for all dimension, maybe except the domain on which transformation is performed)
  \param[in] globalIndexGridDestSendToServer global index of destination grid which are to be sent to server(s), this array is already acsending sorted
  \param[in/out] globalIndexDestGrid array of global index (for 2d grid, this array is a line, for 3d, this array represents a plan). It should be preallocated
  \param[in/out] globalIndexSrcGrid array of global index of source grid (for 2d grid, this array is a line, for 3d, this array represents a plan). It should be preallocated
*/
void CDomainAlgorithmTransformation::computeGlobalGridIndexFromGlobalIndexElement(int domainDestGlobalIndex,
                                                                          const std::vector<int>& domainSrcGlobalIndex,
                                                                          const std::vector<int>& destGlobalIndexPositionInGrid,
                                                                          int domainPositionInGrid,
                                                                          const std::vector<int>& gridDestGlobalDim,
                                                                          const std::vector<int>& gridSrcGlobalDim,
                                                                          const GlobalLocalMap& globalLocalIndexDestSendToServerMap,
                                                                          std::vector<std::pair<size_t,int> >& globalLocalIndexDestMap,
                                                                          std::vector<std::vector<size_t> >& globalIndexSrcGrid)
{
  int globalDim = gridDestGlobalDim.size();
  int numElement = globalDim - 1;
  int domainElementPosition = 0;
  std::vector<int> currentIndex(globalDim);
  std::vector<int> gridDomainGlobalDim(numElement), indexMap(numElement);
  std::vector<int> idxLoop(numElement, 0), domainSrcGlobalDim(2), domainDestGlobalDim(2);


  size_t ssize = 1, idx = 0, realGlobalIndexSize = 0;
  for (int i = 0; i< globalDim; ++i)
  {
    if (domainPositionInGrid == i) continue;
    if (domainPositionInGrid == (i+1))
    {
      domainDestGlobalDim[0] = gridDestGlobalDim[i];
      domainDestGlobalDim[1] = gridDestGlobalDim[i+1];
      domainSrcGlobalDim[0] = gridSrcGlobalDim[i];
      domainSrcGlobalDim[1] = gridSrcGlobalDim[i+1];
      gridDomainGlobalDim[idx] = 1;
      domainElementPosition = idx;
    }
    else
    {
      gridDomainGlobalDim[idx] = gridDestGlobalDim[i];
    }
    indexMap[idx] = i;
    ++idx;
  }
  int iIndex = domainDestGlobalIndex % domainDestGlobalDim[0];
  int jIndex = domainDestGlobalIndex / domainDestGlobalDim[0];

  for (int i = 0; i< numElement; ++i) ssize *= gridDomainGlobalDim[i];

  GlobalLocalMap::const_iterator iteArr = globalLocalIndexDestSendToServerMap.end(), it;

  idx = 0;
  while (idx < ssize)
  {
    for (int i = 0; i < numElement; ++i)
    {
      if (idxLoop[i] == gridDomainGlobalDim[i])
      {
        idxLoop[i] = 0;
        ++idxLoop[i+1];
      }
    }

    for (int i = 0; i < numElement; ++i)
    {
      if (domainElementPosition == i)
      {
        currentIndex[indexMap[i]]   = iIndex;
        currentIndex[indexMap[i]+1] = jIndex;
      }
      else
        currentIndex[indexMap[i]] = idxLoop[i];
    }

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
    if (globalIndexSrcGrid[i].size() != domainSrcGlobalIndex.size())
      globalIndexSrcGrid[i].resize(domainSrcGlobalIndex.size());

  size_t realGlobalIndex = 0;
  idx = 0;
  idxLoop.assign(globalDim, 0);
  while (idx < ssize)
  {
    for (int i = 0; i < numElement; ++i)
    {
      if (idxLoop[i] == gridDomainGlobalDim[i])
      {
        idxLoop[i] = 0;
        ++idxLoop[i+1];
      }
    }

    for (int i = 0; i < numElement; ++i)
    {
      if (domainElementPosition == i)
      {
        currentIndex[indexMap[i]]   = iIndex;
        currentIndex[indexMap[i]+1] = jIndex;
      }
      else
        currentIndex[indexMap[i]] = idxLoop[i];
    }

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
        domainGlobalIndex(domainSrcGlobalIndex[i], domainSrcGlobalDim[0], domainSrcGlobalDim[1],
                          currentIndex[indexMap[domainElementPosition]],
                          currentIndex[indexMap[domainElementPosition]+1]);

        globIndex = currentIndex[0];
        mulDim = 1;
        for (int k = 1; k < globalDim; ++k)
        {
          mulDim *= gridSrcGlobalDim[k-1];
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

void CDomainAlgorithmTransformation::domainGlobalIndex(const int& index, const int& niGlob, const int& njGlob,
                                                       int& iIndex, int& jIndex)
{
   iIndex = index % niGlob;
   jIndex = index / niGlob;
}
}
