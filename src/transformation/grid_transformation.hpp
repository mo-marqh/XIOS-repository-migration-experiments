/*!
   \file grid_transformation.hpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 18 June 2015

   \brief Interface for all transformations.
 */
#ifndef __XIOS_GRID_TRANSFORMATION_HPP__
#define __XIOS_GRID_TRANSFORMATION_HPP__

#include <map>
#include <vector>
#include "grid.hpp"
#include "generic_algorithm_transformation.hpp"
#include "transformation_enum.hpp"

namespace xios {

class CGrid;

/*!
  \class CGridTransformation
  This class is an interface for all transformations to interact with the rest of XIOS.
The class, firstly, tries to get all information relating to requested transformations by retrieving directly from grid.
Then with all these information, all necessary transformations will be be created by generic class \class CGenericAlgorithmTransformation.
Because there are information exchange among clients to accomplish the transformations (e.g: some index need retrieving from other clients),
this class uses class \class CTransformationMapping to fulfill this demand.
For each transformation, a new temporary grid source is created.
For a consequential transformations (e.g: inversing -> zoom -> inversing -> ...),
the grid destination of current transformation will be grid source of the next transformation
*/
class CGridTransformation
{
public:
  /** Default constructor */
  CGridTransformation(CGrid* destination, CGrid* source);
  ~CGridTransformation();

  void computeAll();

  const std::map<int, CArray<int,1>* >& getLocalIndexToSendFromGridSource() const;
  const std::map<int, std::vector<CArray<int,1>* > >& getLocalIndexToReceiveOnGridDest() const;

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
  void updateFinalGridDestination();

private:
  //! Grid source on transformation
  CGrid* gridSource_;

  //! Grid destination on transformation
  CGrid* gridDestination_;

  //! The grid source of the first transformation (original grid source)
  CGrid* originalGridSource_;

  //! Grid source dimension size
  std::vector<int> gridSourceDimensionSize_;

  //! Grid destination dimension size
  std::vector<int> gridDestinationDimensionSize_;

private:
  typedef std::list<std::pair<int,std::pair<ETranformationType,int> > > ListAlgoType;
  //! List of algorithm types and their order
  ListAlgoType listAlgos_;

  // Mapping between position of an element in grid and its transformation (if any)
  std::list<CGenericAlgorithmTransformation*> algoTransformation_;

  //! Mapping of (grid) global index representing tranformation.
  std::map<size_t, std::set<size_t> > globaIndexMapFromDestToSource_;

  //! Local index of data to send from grid source
  std::map<int, CArray<int,1>* > localIndexToSendFromGridSource_;

  //! Local index of data to receive on grid destination
  std::map<int, std::vector<CArray<int,1>* > > localIndexToReceiveOnGridDest_;

  //! Position of axis and domain in grid
  std::map<int, int> elementPosition2AxisPositionInGrid_, elementPosition2DomainPositionInGrid_;

  //! (Grid) Global index of grid source
  CArray<size_t,1>* globalIndexOfCurrentGridSource_;
  CArray<size_t,1>* globalIndexOfOriginalGridSource_;
};

}
#endif // __XIOS_GRID_TRANSFORMATION_HPP__
