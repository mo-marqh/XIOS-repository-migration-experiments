#ifndef __XIOS_TEMPORAL_TRANSFORM_FILTER_HPP__
#define __XIOS_TEMPORAL_TRANSFORM_FILTER_HPP__

#include "transform_filter.hpp"

namespace xios
{
  
  /*!
   * A generic filter with multiple input slots wrapping any type of spatial transformations.
   */
  class CTemporalTransformFilter : public CTransformFilter
  {
    public:

      CTemporalTransformFilter(CGarbageCollector& gc, int slots, shared_ptr<CGridAlgorithm> algo, int nrecords, bool detectMissingValues, double defaultValue) ;

    protected:
      /*!
        Overriding this function to process transformations with auxillary inputs
      */
      CDataPacketPtr virtual apply(std::vector<CDataPacketPtr> data) ;
      void buildWorkflowGraph(std::vector<CDataPacketPtr> data);
      bool graphCycleCompleted;
//      void apply(const CArray<double, 1>& dataSrc, CArray<double,1>& dataDest);

       /*!
        Overriding this function to process transformations with auxillary inputs
      */
//      void virtual onInputReady(std::vector<CDataPacketPtr> data);
      //! Current record in the filter
      int record_=0 ;
      //! Maximum number of records
      int nrecords_;
      //! Temporary storage for output flux
      vector<CArray<double, 1>> tmpData_; 
  }; // class CTransformFilter


} // namespace xios

#endif //__XIOS_TEMPORAL_TRANSFORM_FILTER_HPP__