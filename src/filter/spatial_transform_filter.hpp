#ifndef __XIOS_CSpatialTransformFilter__
#define __XIOS_CSpatialTransformFilter__

#include "filter.hpp"

namespace xios
{
  class CGrid;
  class CGridTransformation;
  class CSpatialTransformFilterEngine;

  /*!
   * A generic filter with one input slot wrapping any type of spatial transformations.
   */
  class CSpatialTransformFilter : public CFilter
  {
    public:
      /*!
       * Constructs a filter wrapping the specified spatial transformation.
       *
       * \param gc the associated garbage collector
       * \param engine the engine defining the spatial transformation
       */
      CSpatialTransformFilter(CGarbageCollector& gc, CSpatialTransformFilterEngine* engine);

      /*!
       * Builds the filter graph needed to transform the specified source grid into the specified destination grid.
       *
       * \param gc the associated garbage collector
       * \param srcGrid the source grid
       * \param destGrid the destination grid
       * \return the first and the last filters of the filter graph
       */
      static std::pair<boost::shared_ptr<CSpatialTransformFilter>, boost::shared_ptr<CSpatialTransformFilter> >
      buildFilterGraph(CGarbageCollector& gc, CGrid* srcGrid, CGrid* destGrid);
  }; // class CSpatialTransformFilter

  /*!
   * A generic filter engine wrapping a grid transformation.
   */
  class CSpatialTransformFilterEngine : public IFilterEngine
  {
    public:
      /*!
       * Returns the engine wrapping the specified grid transformation.
       * If the engine already exists it is reused, otherwise it is created.
       *
       * \param gridTransformation the grid transformation the engine will use
       * \return the engine wrapping the specified grid transformation
       */
      static CSpatialTransformFilterEngine* get(CGridTransformation* gridTransformation);

      /*!
       * Applies the grid transformation to the input data and returns the result.
       *
       * \param data a vector of packets corresponding to each slot (one in this case)
       * \return the result of the grid transformation
       */
      CDataPacketPtr virtual apply(std::vector<CDataPacketPtr> data);

    protected:
      /*!
       * Constructs a filter engine wrapping the specified grid transformation.
       *
       * \param gridTransformation the grid transformation the engine will use
       */
      CSpatialTransformFilterEngine(CGridTransformation* gridTransformation);

      /*!
       * Applies the grid transformation to the input data and returns the result.
       * This helper function handles all the communications.
       *
       * \param dataSrc the source data
       * \param dataDest the resulting transformed data
       */
      void apply(const CArray<double, 1>& dataSrc, CArray<double,1>& dataDest);

      CGridTransformation* gridTransformation; //!< The grid transformation used by the engine

      //! The allocated engines
      static std::map<CGridTransformation*, boost::shared_ptr<CSpatialTransformFilterEngine> > engines;
  }; // class CSpatialTransformFilterEngine
} // namespace xios

#endif //__XIOS_CSpatialTransformFilter__
