#ifndef __XIOS_CSourceFilter__
#define __XIOS_CSourceFilter__

#include <map>

#include "output_pin.hpp"

namespace xios
{
  class CGrid;

  /*!
   * A source filter is the entry point of the data in the graph of filters.
   */
  class CSourceFilter : public COutputPin
  {
    public:
      /*!
       * Constructs a source filter accepting data attached to the specified grid.
       *
       * \param grid the grid to which the data is attached
       */
      CSourceFilter(CGrid* grid);

      /*!
       * Transforms the data received from the model into a packet and send it
       * in the filter graph. The array containing the data can safely be reused
       * immediately after this method returns.
       *
       * \param timestamp the timestamp of the data
       * \param data an array containing the data
       */
      template <int N>
      void streamData(Time timestamp, const CArray<double, N>& data);

      /*!
       * Transforms the data received from the server into a packet and send it
       * in the filter graph. The array containing the data can safely be reused
       * immediately after this method returns.
       *
       * \param timestamp the timestamp of the data
       * \param data an array containing the data
       */
      void streamDataFromServer(Time timestamp, const std::map<int, CArray<double, 1> >& data);

      /*!
       * Signals the filter graph that the end of stream was reached.
       *
       * \param timestamp the timestamp at which the end of stream occurred
       */
      void signalEndOfStream(Time timestamp);

    private:
      CGrid* grid; //!< The grid attached to the data the filter can accept
  }; // class CSourceFilter
} // namespace xios

#endif //__XIOS_CSourceFilter__
