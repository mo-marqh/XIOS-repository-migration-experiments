#ifndef __XIOS_MODEL_TO_CLIENT_SOURCE_FILTER_HPP__
#define __XIOS_MODEL_TO_CLIENT_SOURCE_FILTER_HPP__

#include <map>

#include "output_pin.hpp"

namespace xios
{
  class CGrid;

  /*!
   * A source filter is the entry point of the data in the graph of filters.
   */
  class CModelToClientSourceFilter : public COutputPin
  {
    public:
      /*!
       * Constructs a source filter accepting data attached to the specified grid.
       *
       * \param gc the garbage collector associated with this filter
       * \param grid the grid to which the data is attached
       * \param hasMissingValue whether data has missing value
       * \param defaultValue missing value to detect
       */
      CModelToClientSourceFilter(CGarbageCollector& gc, CGrid* grid, bool hasMissingValue = false, double defaultValue = 0.0);

      /*!
       * Transforms the data received from the model into a packet and send it
       * in the filter graph. The array containing the data can safely be reused
       * immediately after this method returns.
       *
       * \param date the date associated to the data
       * \param data an array containing the data
       */
      template <int N>
      void streamData(CDate date, const CArray<double, N>& data);

      /*!
       * Transforms the data received from the server into a packet and send it
       * in the filter graph. The array containing the data can safely be reused
       * immediately after this method returns.
       *
       * \param date the date associated to the data
       * \param data an array containing the data
       */

    private:
      CGrid* grid_;             //!< The grid attached to the data the filter can accept
      const bool hasMissingValue_;
      const double defaultValue_;
  }; // class CSourceFilter
} // namespace xios

#endif // __XIOS_MODEL_TO_CLIENT_SOURCE_FILTER_HPP__