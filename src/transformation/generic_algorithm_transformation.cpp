/*!
   \file generic_algorithm_transformation.hpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 29 June 2015

   \brief Interface for all transformation algorithms.
 */
#include "generic_algorithm_transformation.hpp"

namespace xios {

CGenericAlgorithmTransformation::CGenericAlgorithmTransformation()
 : transformationMapping_(), transformationWeight_(), transformationPosition_(), idAuxInputs_()
{
}

/*!
  This function computes the global indexes of grid source, which the grid destination is in demand.
  \param[in] elementPositionInGrid position of an element in a grid .E.g: if grid is composed of domain and axis (in order),
                then position of axis in grid is 2 (since a domain is considered to contain 2 elements (axis)
  \param[in] gridDestGlobalDim global size of each dimension of grid source (all dimension must have the same size except of the one on which transformation is performed)
  \param[in] gridSrcGlobalDim dimension size of source grid (it should share the same size for all dimension, maybe except the domain on which transformation is performed)
  \param[in] globalIndexGridDestSendToServer global index of grid destination on the current client to send to server
  \param[in/out] globaIndexWeightFromDestToSource mapping between transformed global index of grid destination
             and the weighted value as long as global index from grid index source
*/
void CGenericAlgorithmTransformation::computeGlobalSourceIndex(int elementPositionInGrid,
                                                             const std::vector<int>& gridDestGlobalDim,
                                                             const std::vector<int>& gridSrcGlobalDim,
                                                             const std::vector<size_t>& globalIndexGridDestSendToServer,
                                                             std::map<size_t, std::vector<std::pair<size_t,double> > >& globaIndexWeightFromDestToSource)
{
  bool isTransPosEmpty = transformationPosition_.empty();
  std::vector<size_t> vecGlobalIndexGridSendToServer(globalIndexGridDestSendToServer.begin(), globalIndexGridDestSendToServer.end());
  std::sort(vecGlobalIndexGridSendToServer.begin(), vecGlobalIndexGridSendToServer.end());
  for (size_t idxTrans = 0; idxTrans < transformationMapping_.size(); ++idxTrans)
  {
    std::map<int, std::vector<int> >::const_iterator itbTransMap = transformationMapping_[idxTrans].begin(), itTransMap,
                                                     iteTransMap = transformationMapping_[idxTrans].end();
    std::map<int, std::vector<double> >::const_iterator itTransWeight = transformationWeight_[idxTrans].begin();

    // If transformation position exists
    std::map<int, std::vector<int> >::const_iterator itTransPos, iteTransPos;
    if (!isTransPosEmpty)
    {
      itTransPos  = transformationPosition_[idxTrans].begin(),
      iteTransPos = transformationPosition_[idxTrans].end();
    }
    std::vector<int> emptyTransPos;

    std::vector<std::vector<size_t> > globalIndexSrcGrid;
    CArray<size_t,1> globalIndexDestGrid;
//    std::vector<size_t> vecGlobalIndexGridSendToServer(globalIndexGridDestSendToServer.begin(), globalIndexGridDestSendToServer.end());
//    std::sort(vecGlobalIndexGridSendToServer.begin(), vecGlobalIndexGridSendToServer.end());
    for (itTransMap = itbTransMap; itTransMap != iteTransMap; ++itTransMap, ++itTransWeight)
    {
      if (!isTransPosEmpty)
      {
        this->computeGlobalGridIndexFromGlobalIndexElement(itTransMap->first,
                                                       itTransMap->second,
                                                       itTransPos->second,
                                                       elementPositionInGrid,
                                                       gridDestGlobalDim,
                                                       gridSrcGlobalDim,
                                                       vecGlobalIndexGridSendToServer,
                                                       globalIndexDestGrid,
                                                       globalIndexSrcGrid);
        ++itTransPos;
      }
      else
      {
        this->computeGlobalGridIndexFromGlobalIndexElement(itTransMap->first,
                                                       itTransMap->second,
                                                       emptyTransPos,
                                                       elementPositionInGrid,
                                                       gridDestGlobalDim,
                                                       gridSrcGlobalDim,
                                                       vecGlobalIndexGridSendToServer,
                                                       globalIndexDestGrid,
                                                       globalIndexSrcGrid);
      }
      size_t globalIndexSize = globalIndexDestGrid.numElements();
      const std::vector<double>& currentVecWeight = itTransWeight->second;
      for (size_t idx = 0; idx < globalIndexSize; ++idx)
      {
        size_t globalIndex = globalIndexDestGrid(idx);
        for (int i = 0; i < globalIndexSrcGrid[idx].size(); ++i)
        {
          globaIndexWeightFromDestToSource[globalIndex].push_back(make_pair(globalIndexSrcGrid[idx][i], currentVecWeight[i]));
        }
      }
    }
  }
}

void CGenericAlgorithmTransformation::computeIndexSourceMapping(const std::vector<CArray<double,1>* >& dataAuxInputs)
{
  computeIndexSourceMapping_(dataAuxInputs);
}

std::vector<StdString> CGenericAlgorithmTransformation::getIdAuxInputs()
{
  return idAuxInputs_;
}

}
