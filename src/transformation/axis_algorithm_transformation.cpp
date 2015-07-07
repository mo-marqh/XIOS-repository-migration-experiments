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

namespace xios {

CAxisAlgorithmTransformation::CAxisAlgorithmTransformation(CAxis* axisDestination, CAxis* axisSource)
 : CGenericAlgorithmTransformation(), axisDest_(axisDestination), axisSrc_(axisSource)
{
  axisDestGlobalSize_ = axisDestination->size.getValue();
  int niDest = axisDestination->ni.getValue();
  int ibeginDest = axisDestination->ibegin.getValue();

  for (int idx = 0; idx < niDest; ++idx)
    if ((axisDestination->mask)(idx)) axisDestGlobalIndex_.push_back(ibeginDest+idx);
}

CAxisAlgorithmTransformation::~CAxisAlgorithmTransformation()
{
}

void CAxisAlgorithmTransformation::computeIndexSourceMapping()
{
}

/*!
  Compute an array of global index from a global index on a axis
  \param[in] axisDestGlobalIndex global index on an axis of destination grid
  \param[in] axisSrcGlobalIndex global index on an axis of source grid (which are needed by one index on axis destination)
  \param[in] axisPositionInGrid position of the axis in the grid
  \param[in] gridDestGlobalDim dimension size of destination grid (it should share the same size for all dimension, maybe except the axis on which transformation is performed)
  \param[in] globalIndexGridDestSendToServer global index of destination grid which are to be sent to server(s)
  \param[in/out] globalIndexDestGrid array of global index (for 2d grid, this array is a line, for 3d, this array represents a plan). It should be preallocated
  \param[in/out] globalIndexSrcGrid array of global index of source grid (for 2d grid, this array is a line, for 3d, this array represents a plan). It should be preallocated
*/
void CAxisAlgorithmTransformation::computeGlobalGridIndexFromGlobalIndexElement(int axisDestGlobalIndex,
                                                                          const std::vector<int>& axisSrcGlobalIndex,
                                                                          int axisPositionInGrid,
                                                                          const std::vector<int>& gridDestGlobalDim,
                                                                          const CArray<size_t,1>& globalIndexGridDestSendToServer,
                                                                          CArray<size_t,1>& globalIndexDestGrid,
                                                                          std::vector<std::vector<size_t> >& globalIndexSrcGrid)
{
  int globalDim = gridDestGlobalDim.size();

  std::vector<size_t> currentIndex(globalDim);
  std::vector<int> gridAxisGlobalDim(globalDim);
  std::vector<int> idxLoop(globalDim, 0);

  size_t ssize = 1, idx = 0, realGlobalIndexSize = 0;
  for (int i = 0; i< globalDim; ++i)
  {
    if (axisPositionInGrid == i) gridAxisGlobalDim[i] = 1;
    else gridAxisGlobalDim[i] = gridDestGlobalDim[i];
    ssize *= gridAxisGlobalDim[i];
  }

  CArray<size_t,1>::const_iterator itbArr = globalIndexGridDestSendToServer.begin(), itArr,
                                   iteArr = globalIndexGridDestSendToServer.end();

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

    for (int i = 0; i < globalDim; ++i) currentIndex[i] = idxLoop[i];
    currentIndex[axisPositionInGrid] = axisDestGlobalIndex;

    size_t globIndex = currentIndex[0];
    size_t mulDim = 1;
    for (int k = 1; k < globalDim; ++k)
    {
      mulDim *= gridDestGlobalDim[k-1];
      globIndex += (currentIndex[k])*mulDim;
    }

    itArr = std::find(itbArr, iteArr, globIndex);
    if (iteArr != itArr) ++realGlobalIndexSize;
    ++idxLoop[0];
    ++idx;
  }

  if (globalIndexDestGrid.numElements() != realGlobalIndexSize)
    globalIndexDestGrid.resize(realGlobalIndexSize);

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

    for (int i = 0; i < globalDim; ++i) currentIndex[i] = idxLoop[i];
    currentIndex[axisPositionInGrid] = axisDestGlobalIndex;

    size_t globIndex = currentIndex[0];
    size_t mulDim = 1;
    for (int k = 1; k < globalDim; ++k)
    {
      mulDim *= gridDestGlobalDim[k-1];
      globIndex += (currentIndex[k])*mulDim;
    }

    itArr = std::find(itbArr, iteArr, globIndex);
    if (iteArr != itArr)
    {
      globalIndexDestGrid(realGlobalIndex) = globIndex;
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
