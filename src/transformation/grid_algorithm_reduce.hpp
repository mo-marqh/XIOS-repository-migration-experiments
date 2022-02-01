#ifndef __XIOS_GRID_ALGORITHM_REDUCE_HPP__
#define __XIOS_GRID_ALGORITHM_REDUCE_HPP__

#include "grid_transform_connector.hpp"
#include <map>
#include "array_new.hpp"
#include "local_view.hpp"
#include "grid_algorithm_generic.hpp" 
#include "reduction_types.hpp"

namespace xios
{
  class CGrid;
  class CGenericAlgorithmTransformation ;

  /*!
  \class CGenericAlgorithmTransformation
  This class defines the interface for all other inherited algorithms class
  */
  class CTransformFilter ;
  class CGarbageCollector ;

  class CGridAlgorithmReduce : public CGridAlgorithmGeneric
  {
    public:
      CGridAlgorithmReduce(CGrid* gridSrc, CGrid* gridDst, int pos,  shared_ptr<CGenericAlgorithmTransformation> algo, EReduction op) 
                          : CGridAlgorithmGeneric(gridSrc, gridDst, pos, algo), operator_(op) {}
      virtual ~CGridAlgorithmReduce() {} 

       virtual void apply(const CArray<double,1>& dataIn, CArray<double,1>& dataOut) ;
   
    protected:

     EReduction operator_ ; // reduction operator to apply

  };

}

#endif // __XIOS_GRID_ALGORITHM_REDUCE_HPP__