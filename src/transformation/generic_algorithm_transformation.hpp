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
  class CScalar;

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

  void computeGlobalSourceIndex(int elementPositionInGrid,
                               CGrid* gridSrc,
                               CGrid* gridDst,
                               SourceDestinationIndexMap& globaIndexWeightFromSrcToDst);

    /*!
    Apply a reduction operation on local data.
    \param [in] localIndex vector contains local index of local data output and the corresponding weight
    \param [in] dataInput Pointer to the first element of data input array (in form of buffer)
    \param [in/out] dataOut Array contains local data
    \param [in/out] flagInitial vector of boolean to mark the local index already initialized. True means there is a need for initalization
  */
  virtual void apply(const std::vector<std::pair<int,double> >& localIndex,
                     const double* dataInput,
                     CArray<double,1>& dataOut,
                     std::vector<bool>& flagInitial,
                     const double& defaultValue);

  std::vector<StdString> getIdAuxInputs();

  /*!
  Compute global index mapping from one element of destination grid to the corresponding element of source grid
  */
  void computeIndexSourceMapping(const std::vector<CArray<double,1>* >& dataAuxInputs = std::vector<CArray<double,1>* >());

protected:
  virtual void computeIndexSourceMapping_(const std::vector<CArray<double,1>* >&) = 0;

  /*!
  Compute proc which contains global index of an element
    \param[in] globalElementIndex demanding global index of an element of source grid
    \param[in] elementType type of source element, 2: domain, 1: axis and 0: scalar
    \param[out] globalElementIndexOnProc Proc contains the demanding global index
  */
  virtual void computeExchangeGlobalIndex(const CArray<size_t,1>& globalElementIndex,
                                          int elementType,
                                          CClientClientDHTInt::Index2VectorInfoTypeMap& globalElementIndexOnProc) = 0;

protected:
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

  void computeExchangeScalarIndex(CScalar* scalarDst,
                                  CScalar* scalarSrc,
                                  CArray<size_t,1>& destGlobalIndexPositionInGrid,
                                  boost::unordered_map<int,std::vector<size_t> >& globalScalarIndexOnProc);

  void computePositionElements(CGrid* dst, CGrid* src);

protected:
  //! Map between global index of destination element and source element
  std::vector<TransformationIndexMap> transformationMapping_;
  //! Weight corresponding of source to destination
  std::vector<TransformationWeightMap> transformationWeight_;
  //! Map of global index of destination element and corresponding global index of other elements in the same grid
  //! By default, one index of an element corresponds to all index of remaining element in the grid. So it's empty
  std::vector<TransformationPositionMap> transformationPosition_;

  //! Id of auxillary inputs which helps doing transformation dynamically
  std::vector<StdString> idAuxInputs_;

  std::map<int, int> elementPositionInGridSrc2AxisPosition_, elementPositionInGridSrc2DomainPosition_, elementPositionInGridSrc2ScalarPosition_;
  std::map<int, int> elementPositionInGridDst2AxisPosition_, elementPositionInGridDst2DomainPosition_, elementPositionInGridDst2ScalarPosition_;
};

}
#endif // __XIOS_GENERIC_ALGORITHM_TRANSFORMATION_HPP__
