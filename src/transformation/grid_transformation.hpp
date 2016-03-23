/*!
   \file grid_transformation.hpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 26 Aug 2015

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
Then with all these information, all necessary transformations will be created by generic class \class CGenericAlgorithmTransformation.
Because there are information exchange among clients to accomplish the transformations (e.g: some index need retrieving from other clients),
this class uses class \class CTransformationMapping to fulfill this demand.
For each transformation, a new temporary grid source is created.
For a consequential transformations (e.g: inversing -> zoom -> inversing -> ...),
the grid destination of current transformation will be grid source of the next transformation
*/
class CGridTransformation
{
public:
  typedef std::list<std::pair<int,std::pair<ETranformationType,int> > > ListAlgoType;

public:
  /** Default constructor */
  CGridTransformation(CGrid* destination, CGrid* source);
  ~CGridTransformation();

  void computeAll(const std::vector<CArray<double,1>* >& dataAuxInput=std::vector<CArray<double,1>* >());

  const std::map<int, CArray<int,1> >& getLocalIndexToSendFromGridSource() const;
  const std::map<int, std::vector<std::vector<std::pair<int,double> > > >& getLocalIndexToReceiveOnGridDest() const;
  CGrid* getGridSource() { return originalGridSource_; }
  CGrid* getGridDestination() { return gridDestination_; }
  ListAlgoType getAlgoList() const {return listAlgos_; }
  int getNbAlgo() { return nbAlgos_; }
  const std::vector<StdString>& getAuxInputs() const { return auxInputs_; }

protected:
  void computeTransformation();
  void initializeAlgorithms();
  void initializeAxisAlgorithms(int axisPositionInGrid);
  void initializeDomainAlgorithms(int domPositionInGrid);
  void initializeMappingOfOriginalGridSource();

  void selectAxisAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder);
  void selectDomainAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder);
  void selectAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder, bool isDomainAlgo);
  void setUpGrid(int elementPositionInGrid, ETranformationType transType, int nbTransformation);
  void computeFinalTransformationMapping();
  void computeTransformationFromOriginalGridSource(const std::map<size_t, std::vector<std::pair<size_t,double> > >& globaIndexMapFromDestToSource);
  void updateFinalGridDestination();
  bool isSpecialTransformation(ETranformationType transType);

protected:
  //! Grid source on transformation
  CGrid* gridSource_;

  //! Grid destination on transformation
  CGrid* gridDestination_;

  //! The grid source of the first transformation (original grid source)
  CGrid* originalGridSource_;

protected:
  //! List of algorithm types and their order
  ListAlgoType listAlgos_;

  //! Number of algorithm
  int nbAlgos_;

  typedef std::map<size_t, std::vector<std::pair<size_t,double> > > GlobalIndexMap;

  // true if domain algorithm and false if axis algorithm (can be replaced by tuple with listAlgos_
  std::vector<bool> algoTypes_;

  // Mapping between position of an element in grid and its transformation (if any)
  std::vector<CGenericAlgorithmTransformation*> algoTransformation_;

  //! Mapping of (grid) global index representing tranformation.
  std::map<size_t, std::set<size_t> > globaIndexMapFromDestToSource_;

  //! Local index of data to send from grid source
  std::map<int, CArray<int,1> > localIndexToSendFromGridSource_;

  //! Local index of data to receive on grid destination
  std::map<int,std::vector<std::vector<std::pair<int,double> > > > localIndexToReceiveOnGridDest_;

  //! Position of axis and domain in grid
  std::map<int, int> elementPosition2AxisPositionInGrid_, elementPosition2DomainPositionInGrid_;

  //! (Grid) Global index of grid source
  GlobalIndexMap currentGridIndexToOriginalGridIndex_;

  std::vector<CGrid*> tempGrids_;
  std::vector<StdString> auxInputs_;
  bool dynamicalTransformation_;
};

}
#endif // __XIOS_GRID_TRANSFORMATION_HPP__
