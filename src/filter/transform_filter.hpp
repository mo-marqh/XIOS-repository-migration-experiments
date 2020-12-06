#ifndef __XIOS_TRANSFORM_FILTER_HPP__
#define __XIOS_TRANSFORM_FILTER_HPP__

#include "filter.hpp"
#include "generic_algorithm_transformation.hpp"

namespace xios
{
  
  /*!
   * A generic filter with multiple input slots wrapping any type of spatial transformations.
   */
  class CTransformFilter : public CFilter, IFilterEngine
  {
    public:

      CTransformFilter(CGarbageCollector& gc, CGenericAlgorithmTransformation* algo, int dimBefore, int dimAfter, 
                              bool detectMissingValues, double defaultValue) ;

    protected:
      /*!
        Overriding this function to process transformations with auxillary inputs
      */
      CDataPacketPtr virtual apply(std::vector<CDataPacketPtr> data) ;
      void apply(const CArray<double, 1>& dataSrc, CArray<double,1>& dataDest);

    private :
     
     CGenericAlgorithmTransformation* algorithm_ ;
     int dimBefore_ ;
     int dimAfter_ ;
     bool detectMissingValues_ ;
     bool defaultValue_ ;

  }; // class CTransformFilter


} // namespace xios

#endif //__XIOS_CSpatialTransformFilter__
