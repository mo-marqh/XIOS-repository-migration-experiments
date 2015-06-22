#ifndef __XIOS_GRID_TRANSFORMATION_HPP__
#define __XIOS_GRID_TRANSFORMATION_HPP__

#include <map>
#include <vector>
#include "grid.hpp"
#include "generic_algorithm_transformation.hpp"

namespace xios {

class CGrid;

class CGridTransformation
{
public:
  /** Default constructor */
  CGridTransformation(CGrid* destination, CGrid* source);
  ~CGridTransformation();

  void computeTransformationMapping();
  std::map<int, CArray<int,1>* > getLocalIndexToSendFromGridSource();
  std::map<int, std::vector<CArray<int,1>* > > getLocalIndexToReceiveOnGridDest();

private:
  void computeTransformation();
  void initializeAlgorithms();
  void initializeAxisAlgorithms();
  void initializeDomainAlgorithms();

private:
  CGrid* gridSource_;
  CGrid* gridDestination_;

  std::vector<int> gridSourceDimensionSize_;
  std::vector<int> gridDestinationDimensionSize_;

  // Mapping between position of an element in grid and its transformation (if any)
  std::map<int, std::vector<CGenericAlgorithmTransformation*> > algoTransformation_;
  std::map<size_t, std::set<size_t> > globaIndexMapFromDestToSource_;

  std::map<int, CArray<int,1>* > localIndexToSendFromGridSource_;
  std::map<int, std::vector<CArray<int,1>* > > localIndexToReceiveOnGridDest_;
};

}
#endif // __XIOS_GRID_TRANSFORMATION_HPP__
