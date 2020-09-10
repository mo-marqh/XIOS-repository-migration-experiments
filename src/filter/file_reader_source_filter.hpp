#ifndef __XIOS_FILE_READER_SOURCE_FILTER__
#define __XIOS_FILE_READER_SOURCE_FILTER__

#include "output_pin.hpp"

namespace xios
{
  class CField;
  class CFile ;
  class CGrid ;

  /*!
   * A source filter is the entry point of the data in the graph of filters.
   */
  class CFileReaderSourceFilter : public COutputPin
  {
    public:
      /*!
       * Constructs a source filter accepting data attached to the specified grid.
       *
       * \param gc the garbage collector associated with this filter
       * \param field the field to which the data is related
       */
      CFileReaderSourceFilter(CGarbageCollector& gc, CField* field);

      /*!
       * Transforms the data received from the model into a packet and send it
       * in the filter graph. The array containing the data can safely be reused
       * immediately after this method returns.
       *
       * \param date the date associated to the data
       * \param data an array containing the data
       */
      void streamData(void);
      void initialize(void) ;
      void readData(CArray<double,1>& data) ;

    private:
      CGrid* grid_;             //!< The grid attached to the data the filter can accept
      CField* field_ ;
      CFile* file_ ;
      bool hasScaleFactor_ = false;
      bool hasAddOffset_ = false ;
      bool isInitialized_ = false ;
      bool isCyclic_ = false ;
      double scaleFactor_= 1.0 ;
      double addOffset_= 0. ;
      int nStepMax_ ;
      int nStep_ ;

  }; // class CFileReaderSourceFilter
} // namespace xios

#endif // __XIOS_FILE_READER_SOURCE_FILTER__