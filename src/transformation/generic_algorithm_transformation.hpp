/*!
   \file generic_algorithm_transformation.hpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 29 June 2015

   \brief Interface for all transformation algorithms.
 */
#ifndef __XIOS_GENERIC_ALGORITHM_TRANSFORMATION_HPP__
#define __XIOS_GENERIC_ALGORITHM_TRANSFORMATION_HPP__

#include <map>
#include <set>
#include "array_new.hpp"
#include "client_client_dht_template.hpp"

namespace xios {
  class CGrid;
  class CDomain;
  class CAxis;

  /*!
  \class CGenericAlgorithmTransformation
  This class defines the interface for all other inherted algorithms class
  */
class CGenericAlgorithmTransformation
{
protected:
  typedef std::vector<std::pair<int, std::pair<size_t,double> > > DestinationGlobalIndex;
public:
  // Stupid global index map, it must be replaced by tuple
  // Mapping between global index map of DESTINATION and its local index with pair of global index of SOURCE and weights
  typedef boost::unordered_map<size_t, DestinationGlobalIndex> DestinationIndexMap;
  //
  typedef boost::unordered_map<int, boost::unordered_map<size_t, std::vector<std::pair<size_t,double> > > > SourceDestinationIndexMap;

protected:
  typedef boost::unordered_map<size_t,int> GlobalLocalMap;
protected:
  typedef boost::unordered_map<int, std::vector<int> > TransformationIndexMap;
  typedef boost::unordered_map<int, std::vector<double> > TransformationWeightMap;
  typedef boost::unordered_map<int, std::vector<int> > TransformationPositionMap;

public:
  CGenericAlgorithmTransformation();

  virtual ~CGenericAlgorithmTransformation() {}

//  void computeGlobalSourceIndex(int elementPositionInGrid,
//                                const std::vector<int>& gridDestGlobalDim,
//                                const std::vector<int>& gridSrcGlobalDim,
//                                const GlobalLocalMap& globalLocalIndexGridDestSendToServer,
//                                DestinationIndexMap& globaIndexWeightFromDestToSource);

  void computeGlobalSourceIndex(int elementPositionInGrid,
                               CGrid* gridSrc,
                               CGrid* gridDst,
                               SourceDestinationIndexMap& globaIndexWeightFromSrcToDst);

  std::vector<StdString> getIdAuxInputs();

  /*!
  Compute global index mapping from one element of destination grid to the corresponding element of source grid
  */
  void computeIndexSourceMapping(const std::vector<CArray<double,1>* >& dataAuxInputs = std::vector<CArray<double,1>* >());

protected:
  /*!
  Compute an array of global index from a global index on an element
    \param[in] destGlobalIndex global index on an element of destination grid
    \param[in] srcGlobalIndex global index(es) on an element of source grid (which are needed by one index on element destination)
    \param[in] elementPositionInGrid position of the element in the grid (for example: a grid with one domain and one axis, position of domain is 1, position of axis is 2)
    \param[in] gridDestGlobalDim dimension size of destination grid (it should share the same size for all dimension, maybe except the element on which transformation is performed)
    \param[in] globalLocalIndexDestSendToServerMap pair of global index and local index of destination grid which are to be sent to server(s), this array is already acsending sorted
    \param[in/out] globalLocalIndexDestMap array of global index (for 2d grid, this array maybe a line, for 3d, this array may represent a plan). It should be preallocated
    \param[in/out] globalIndexSrcGrid array of global index of source grid (for 2d grid, this array is a line, for 3d, this array represents a plan). It should be preallocated
  */
  virtual void computeGlobalGridIndexFromGlobalIndexElement(int destGlobalIndex,
                                                        const std::vector<int>& srcGlobalIndex,
                                                        const std::vector<int>& destGlobalIndexPositionInGrid,
                                                        int elementPositionInGrid,
                                                        const std::vector<int>& gridDestGlobalDim,
                                                        const std::vector<int>& gridSrcGlobalDim,
                                                        const GlobalLocalMap& globalLocalIndexDestSendToServerMap,
                                                        std::vector<std::pair<size_t,int> >& globalLocalIndexDestMap,
                                                        std::vector<std::vector<size_t> >& globalIndexSrcGrid) = 0;

  virtual void computeIndexSourceMapping_(const std::vector<CArray<double,1>* >&) = 0;

  virtual void computeExchangeGlobalIndex(const CArray<size_t,1>& globalElementIndex,
                                          CClientClientDHTInt::Index2VectorInfoTypeMap& globalElementIndexOnProc) = 0;

  void computeGlobalGridIndexMapping(int elementPositionInGrid,
                                     const std::vector<int>& srcRank,
                                     boost::unordered_map<int, std::vector<std::pair<int,double> > >& src2DstMap,
                                     CGrid* gridDst,
                                     CGrid* gridSrc,
                                     std::vector<boost::unordered_map<int,std::vector<size_t> > >& globalElementIndexOnProc,
                                     SourceDestinationIndexMap& globaIndexWeightFromSrcToDst);

  void computeExchangeDomainIndex(CDomain* domainDst,
                                  CDomain* domainSrc,
                                  CArray<size_t,1>& destGlobalIndexPositionInGrid,
                                  boost::unordered_map<int,std::vector<size_t> >& globalDomainIndexOnProc);

  void computeExchangeAxisIndex(CAxis* axisDst,
                                CAxis* axisSrc,
                                CArray<size_t,1>& destGlobalIndexPositionInGrid,
                                boost::unordered_map<int,std::vector<size_t> >& globalAxisIndexOnProc);

protected:
  //! Map between global index of destination element and source element
  std::vector<TransformationIndexMap> transformationMapping_;
  //! Weight corresponding of source to destination
  std::vector<TransformationWeightMap> transformationWeight_;
  //! Map of global index of destination element and corresponding global index of other elements in the same grid
  //! By default, one index of an element corresponds to all index of remaining element in the grid. So it's empty
  std::vector<TransformationPositionMap> transformationPosition_;

  //! Id of auxillary inputs which help doing transformation dynamically
  std::vector<StdString> idAuxInputs_;
};

}
#endif // __XIOS_GENERIC_ALGORITHM_TRANSFORMATION_HPP__
