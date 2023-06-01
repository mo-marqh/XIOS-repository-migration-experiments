#ifndef __XIOS_GRID_REDISTRIBUTE_FILTER_HPP__
#define __XIOS_GRID_REDISTRIBUTE_FILTER_HPP__

#include "filter.hpp"

namespace xios
{
  
  class CGridTransformConnector ;
  /*!
   * A filter to redistribute data from source grid to destination grid.
   */
    class CGridRedistributeFilter : public CFilter, IFilterEngine
  {
    public:

      CGridRedistributeFilter(CGarbageCollector& gc, CField* fieldIn, CField* &fieldOut) ;

    protected:
      /*!
        Overriding this function to process transformations with auxillary inputs
      */
      CDataPacketPtr virtual apply(std::vector<CDataPacketPtr> data) ;
    
    private:

    shared_ptr<CGridTransformConnector> redistributeConnector_ ;
  }; // class CTransformFilter


} // namespace xios

#endif //__XIOS_CSpatialTransformFilter__
