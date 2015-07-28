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
 : transformationMapping_(), transformationWeight_()
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
                                                             const CArray<size_t,1>& globalIndexGridDestSendToServer,
                                                             std::map<size_t, std::vector<std::pair<size_t,double> > >& globaIndexWeightFromDestToSource)
{
  std::map<int, std::vector<int> >::const_iterator itbTransMap = transformationMapping_.begin(),
                                                   itTransMap = itbTransMap,
                                                   iteTransMap = transformationMapping_.end();
  std::map<int, std::vector<double> >::const_iterator itTransWeight = transformationWeight_.begin();
  std::map<size_t, std::vector<std::pair<size_t,double> > >::iterator iteWeight, itWeight;
  std::vector<int>::const_iterator itbVec, itVec, iteVec;
  std::vector<std::vector<size_t> > globalIndexSrcGrid;
  CArray<size_t,1> globalIndexDestGrid;

  for (itTransMap = itbTransMap; itTransMap != iteTransMap; ++itTransMap, ++itTransWeight)
  {
    this->computeGlobalGridIndexFromGlobalIndexElement(itTransMap->first,
                                                   itTransMap->second,
                                                   elementPositionInGrid,
                                                   gridDestGlobalDim,
                                                   gridSrcGlobalDim,
                                                   globalIndexGridDestSendToServer,
                                                   globalIndexDestGrid,
                                                   globalIndexSrcGrid);
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
