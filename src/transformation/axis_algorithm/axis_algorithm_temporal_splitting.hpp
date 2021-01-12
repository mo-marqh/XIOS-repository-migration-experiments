/*!
   \file axis_algorithm_reduce_domain.hpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for reduce an axis to a axis
 */
#ifndef __XIOS_AXIS_ALGORITHM_TEMPORAL_SPLITTING_HPP__
#define __XIOS_AXIS_ALGORITHM_TEMPORAL_SPLITTING_HPP__

#include "transformation.hpp"
#include "algorithm_transformation_no_data_modification.hpp" 
namespace xios 
{

  class CAxis;
  class CScalar;
  class CTemporalSplitting;

  /*!
    \class CAxisAlgorithmExtractDomain
    Extract a domain to an axis
  */
  class CAxisAlgorithmTemporalSplitting : public CAlgorithmTransformationNoDataModification  
  {
    public:
      CAxisAlgorithmTemporalSplitting(bool isSource, CAxis* axisDestination, CScalar* scalarSource, CTemporalSplitting* algo);
      virtual ~CAxisAlgorithmTemporalSplitting();
      static bool registerTrans();
      virtual CTransformFilter* createTransformFilter(CGarbageCollector& gc, CGridAlgorithm* algo, bool detectMissingValues, double defaultValue) ;

      static CGenericAlgorithmTransformation* create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                     CTransformation<CAxis>* transformation,
                                                     int elementPositionInGrid,
                                                     std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                     std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                     std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                     std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                     std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                     std::map<int, int>& elementPositionInGridDst2DomainPosition);
    private:
    int nrecords_;
    static bool dummyRegistered_;
  };

}
#endif // __XIOS_AXIS_ALGORITHM_TEMPORAL_SPLITTING_HPP__
