/*!
   \file grid_generate.hpp
   \author Ha NGUYEN
   \since 28 Aug 2015
   \date 28 Aug 2015

   \brief A special transformation to generate a grid.
 */
#ifndef __XIOS_GRID_GENERATE_HPP__
#define __XIOS_GRID_GENERATE_HPP__

#include "xios_spl.hpp"
#include "transformation_enum.hpp"
#include "generic_algorithm_transformation.hpp"

namespace xios {

class CGrid;

/*!
  \class CGridGenerate
  This class creates a grid from scratch, e.g: only global dimension of grid and its type are provided,
then the generated grid plays the destination in a transformation. Not only some attributes of grid and its subcomponents are filled in
automatically but it also have a distribution which might be different from one of grid source.
This class only plays a role of interface between XIOS and specific algorithm of auto filling-in and auto distributing on sub-component
*/
class CGridGenerate
{
public:
  typedef std::list<std::pair<int,std::pair<ETranformationType,int> > > ListAlgoType;

public:
  /** Default constructor */
  CGridGenerate(CGrid* destination, CGrid* source);
  ~CGridGenerate();

  void completeGrid();
  ListAlgoType getAlgoList() const {return listAlgos_; }

protected:
  void initializeAlgorithms();
  void initializeAxisAlgorithms(int axisPositionInGrid);
  void initializeDomainAlgorithms(int domPositionInGrid);

  void selectAxisAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder);
  void selectDomainAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder);
  void selectAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder, bool isDomainAlgo);

protected:
  //! Grid source on transformation
  CGrid* gridSource_;

  //! Grid destination on transformation
  CGrid* gridDestination_;

protected:
  //! List of algorithm types and their order
  ListAlgoType listAlgos_;
  // true if domain algorithm and false if axis algorithm (can be replaced by tuple with listAlgos_
  std::vector<bool> algoTypes_;

  // Mapping between position of an element in grid and its transformation (if any)
  std::list<CGenericAlgorithmTransformation*> algoTransformation_;

  //! Position of axis and domain in grid
  std::map<int, int> elementPosition2AxisPositionInGrid_, elementPosition2DomainPositionInGrid_;
};

}
#endif // __XIOS_GRID_GENERATE_HPP__
