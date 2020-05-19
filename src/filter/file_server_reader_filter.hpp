#ifndef __XIOS_FILE_SERVER_READER_HPP__
#define __XIOS_FILE_SERVER_READER_HPP__

#include <map>

#include "output_pin.hpp"

namespace xios
{
  class CGrid;
  class CField;

  /*!
   * A source filter is the entry point of the data in the graph of filters. It is in charge to read data in a file on server side and
   * injecting it into the workflow
   */
  class CFileServerReaderFilter : public COutputPin
  {
    public:
      /*!
       * Constructs a source filter accepting data attached to the specified grid.
       *
       * \param gc the garbage collector associated with this filter
       * \param associatedField Field associated to the filter
       */
       CFileServerReaderFilter(CGarbageCollector& gc, CField* associatedField) ;

      /*!
       * Transforms the data received from the model into a packet and send it
       * in the filter graph. The array containing the data can safely be reused
       * immediately after this method returns.
       *
       * \param date the date associated to the data
       * \param data an array containing the data
       */
      void streamData(CDate date, const CArray<double, 1>& data);

      /*!
       * Signals the filter graph that the end of stream was reached.
       *
       * \param date the date at which the end of stream occurred
       */
      void signalEndOfStream(CDate date);

    private:
      CField* field_;             //!< The field attached to the data the filter can accept
//      const bool hasMissingValue;
//      const double defaultValue;
  }; // class CFileServerReaderFilter
} // namespace xios

#endif //__XIOS_FILE_SERVER_READER__
