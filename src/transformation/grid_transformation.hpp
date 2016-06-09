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
#include "generic_algorithm_transformation.hpp"
#include "transformation_enum.hpp"
#include "duration.hpp"

namespace xios {

class CGrid;

/*!
  \class CGridTransformation
  This class is an interface for all transformations to interact with the rest of XIOS.
The class, firstly, tries to get all information relating to requested transformations by retrieving directly from grid.
Secondly, with all these information, all necessary transformations will be created by generic class \class CGenericAlgorithmTransformation.
Then this class accomplishes the transformations by exchanging information among clients (e.g: some index need retrieving index from other clients),
For each transformation, a new temporary grid source is created.
For a consequential transformations (e.g: inversing -> zoom -> inversing -> ...),
the grid destination of current transformation will be grid source of the next transformation
*/
class CGridTransformation
{
public:
  typedef std::list<std::pair<int,std::pair<ETranformationType,int> > > ListAlgoType;
  typedef boost::unordered_map<size_t, std::vector<std::pair<int, std::pair<size_t,double> > > > DestinationIndexMap;
  typedef std::map<int, CArray<int,1> > SendingIndexGridSourceMap;
  typedef std::map<int,std::vector<std::pair<int,double> > > RecvIndexGridDestinationMap;
  typedef CGenericAlgorithmTransformation::SourceDestinationIndexMap SourceDestinationIndexMap;

public:
  /** Default constructor */
  CGridTransformation(CGrid* destination, CGrid* source);
  ~CGridTransformation();

  void computeAll(const std::vector<CArray<double,1>* >& dataAuxInput=std::vector<CArray<double,1>* >(), Time timeStamp = 0);

  const std::list<SendingIndexGridSourceMap>& getLocalIndexToSendFromGridSource() const;
  const std::list<RecvIndexGridDestinationMap>& getLocalIndexToReceiveOnGridDest() const;
  const std::list<size_t>& getNbLocalIndexToReceiveOnGridDest() const;
  const std::list<std::vector<bool> >& getLocalMaskIndexOnGridDest() const;

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
  void initializeTransformations();

  void selectAxisAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder);
  void selectDomainAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder);
  void selectAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder, bool isDomainAlgo);
  void setUpGridSource(int elementPositionInGrid, ETranformationType transType, int nbTransformation);
  void setUpGridDestination(int elementPositionInGrid, ETranformationType transType, int nbTransformation);
  void computeTransformationMapping(const SourceDestinationIndexMap& globalIndexWeightFromSrcToDest);
  bool isSpecialTransformation(ETranformationType transType);

protected:
  //! Grid source on transformation
  CGrid* gridSource_;

  //! Grid destination on transformation
  CGrid* gridDestination_, *tmpGridDestination_;

  //! The grid source of the first transformation (original grid source)
  CGrid* originalGridSource_;

protected:
  //! List of algorithm types and their order
  ListAlgoType listAlgos_;

  //! Number of algorithm
  int nbAlgos_;

  // true if domain algorithm and false if axis algorithm (can be replaced by tuple with listAlgos_
  std::vector<bool> algoTypes_;

  // Mapping between position of an element in grid and its transformation (if any)
  std::vector<CGenericAlgorithmTransformation*> algoTransformation_;

  //! Mapping of (grid) global index representing tranformation.
  std::map<size_t, std::set<size_t> > globaIndexMapFromDestToSource_;

  //! Local index of data to send from grid source
  std::list<SendingIndexGridSourceMap> localIndexToSendFromGridSource_;

  //! Local index of data to receive on grid destination
  std::list<RecvIndexGridDestinationMap> localIndexToReceiveOnGridDest_;

  //! Number of local index of data to receive on grid destination
  std::list<size_t> nbLocalIndexOnGridDest_;
  std::list<std::vector<bool> > localMaskOnGridDest_;

  //! Position of axis and domain in grid
  std::map<int, int> elementPosition2AxisPositionInGrid_, elementPosition2DomainPositionInGrid_;

  std::vector<CGrid*> tempGridSrcs_, tempGridDests_;
  std::vector<StdString> auxInputs_;
  bool dynamicalTransformation_;

  std::set<Time> timeStamp_; //! Time stamps for auxillary inputs
};

}
#endif // __XIOS_GRID_TRANSFORMATION_HPP__
