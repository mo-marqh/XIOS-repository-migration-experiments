#ifndef __XIOS_ALGORITHM_TRANSFORMATION_REDUCE_HPP__
#define __XIOS_ALGORITHM_TRANSFORMATION_REDUCE_HPP__

#include "generic_algorithm_transformation.hpp"
#include "array_new.hpp"
#include "local_view.hpp"
#include "transform_connector.hpp"
#include "reduce_transform_connector.hpp"

namespace xios
{

  class CAlgorithmTransformationReduce : public CGenericAlgorithmTransformation
  {
    public:

      CAlgorithmTransformationReduce(bool isSource) : CGenericAlgorithmTransformation(isSource) {}
      virtual ~CAlgorithmTransformationReduce() {};
      virtual void apply(int dimBefore, int dimAfter, const CArray<double,1>& dataIn, CArray<double,1>& dataOut);
      void computeRecvElement(CLocalView* srcView, CLocalView* dstView) ;
      
    protected:
      virtual void computeAlgorithm(CLocalView* srcView, CLocalView* dstView) ;
       
      //! Map between global index of destination element and source element
      EReduction operator_ ;
      TransformationIndexMap transformationMapping_;
      CTransformConnector* transformConnector_ ;
      CReduceTransformConnector* reduceTransformConnector_ ;
      bool detectMissingValue_=true ;
      bool eliminateRedondantSrc_ = true ; 
  };

}
#endif //__XIOS_ALGORITHM_TRANSFORMATION_REDUCE_HPP__
