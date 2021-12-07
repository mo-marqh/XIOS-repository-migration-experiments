#ifndef __XIOS_TRANSFORM_FILTER_HPP__
#define __XIOS_TRANSFORM_FILTER_HPP__

#include "filter.hpp"
#include "grid_algorithm.hpp"

namespace xios
{
  
  /*!
   * A generic filter with multiple input slots wrapping any type of spatial transformations.
   */
  class CTransformFilter : public CFilter, IFilterEngine
  {
    public:

      CTransformFilter(CGarbageCollector& gc, int slots, shared_ptr<CGridAlgorithm> algo, bool detectMissingValues, double defaultValue) ;

    protected:
      /*!
        Overriding this function to process transformations with auxillary inputs
      */
      CDataPacketPtr virtual apply(std::vector<CDataPacketPtr> data) ;
      void buildWorkflowGraph(std::vector<CDataPacketPtr> data, CDataPacketPtr packet, shared_ptr<CGridAlgorithm> algorithm);
      
//      void apply(const CArray<double, 1>& dataSrc, CArray<double,1>& dataDest);
     
      shared_ptr<CGridAlgorithm> algorithm_ ;
      bool detectMissingValues_ ;
      bool defaultValue_ ;

  }; // class CTransformFilter


} // namespace xios

#endif //__XIOS_CSpatialTransformFilter__
