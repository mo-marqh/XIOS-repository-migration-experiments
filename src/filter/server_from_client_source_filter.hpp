#ifndef __XIOS_SERVER_FROM_CLIENT_SOURCE_FILTER__
#define __XIOS_SERVER_FROM_CLIENT_SOURCE_FILTER__

#include "output_pin.hpp"
#include "event_server.hpp"

namespace xios
{
  class CGrid;

  /*!
   * A source filter is the entry point of the data in the graph of filters.
   */
  class CServerFromClientSourceFilter : public COutputPin
  {
    public:
      /*!
       * Constructs a source filter accepting data attached to the specified grid.
       *
       * \param gc the garbage collector associated with this filter
       * \param grid the grid to which the data is attached
       * \param compression
       * \param mask
       * \param offset the offset applied to the timestamp of all packets
       * \param manualTrigger whether the output should be triggered manually
       * \param hasMissingValue whether data has missing value
       * \param defaultValue missing value to detect
       */
      CServerFromClientSourceFilter(CGarbageCollector& gc, CGrid* grid);

      /*!
       * Transforms the data received from the model into a packet and send it
       * in the filter graph. The array containing the data can safely be reused
       * immediately after this method returns.
       *
       * \param date the date associated to the data
       * \param data an array containing the data
       */
      void streamData(CEventServer& event);

    private:
      CGrid* grid_;             //!< The grid attached to the data the filter can accept

  }; // class CServerFromClientSourceFilter
} // namespace xios

#endif // __XIOS_SERVER_FROM_CLIENT_SOURCE_FILTER__
