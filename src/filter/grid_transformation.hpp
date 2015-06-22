#ifndef __XIOS_GRID_TRANSFORMATION_HPP__
#define __XIOS_GRID_TRANSFORMATION_HPP__

#include <map>
#include <vector>
#include "grid.hpp"
#include "generic_algorithm_transformation.hpp"
#include "transformation_enum.hpp"

namespace xios {

class CGrid;

class CGridTransformation
{
public:
  /** Default constructor */
  CGridTransformation(CGrid* destination, CGrid* source);
  ~CGridTransformation();

  void computeAll();


  std::map<int, CArray<int,1>* > getLocalIndexToSendFromGridSource();
  std::map<int, std::vector<CArray<int,1>* > > getLocalIndexToReceiveOnGridDest();

private:
  void computeTransformation();
  void initializeAlgorithms();
  void initializeAxisAlgorithms();
  void initializeDomainAlgorithms();
  void initializeMappingOfOriginalGridSource();

  void selectAxisAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder);
  void selectDomainAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder);
  void selectAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder);
  void setUpGrid(int elementPositionInGrid, ETranformationType transType);
  void computeFinalTransformationMapping();
  void computeTransformationFromOriginalGridSource(const std::map<size_t, std::set<size_t> >& globaIndexMapFromDestToSource);

private:
  CGrid* gridSource_;
  CGrid* gridDestination_;
  CGrid* originalGridSource_;

  std::vector<int> gridSourceDimensionSize_;
  std::vector<int> gridDestinationDimensionSize_;

private:
  typedef std::list<std::pair<int,std::pair<ETranformationType,int> > > ListAlgoType;

  // Mapping between position of an element in grid and its transformation (if any)
  std::list<CGenericAlgorithmTransformation*> algoTransformation_;
  ListAlgoType listAlgos_;
  std::map<size_t, std::set<size_t> > globaIndexMapFromDestToSource_;

  std::map<int, CArray<int,1>* > localIndexToSendFromGridSource_;
  std::map<int, std::vector<CArray<int,1>* > > localIndexToReceiveOnGridDest_;

  std::map<int, int> elementPosition2AxisPositionInGrid_, elementPosition2DomainPositionInGrid_;

  CArray<size_t,1>* globalIndexOfCurrentGridSource_;
  CArray<size_t,1>* globalIndexOfOriginalGridSource_;
};

}
#endif // __XIOS_GRID_TRANSFORMATION_HPP__
