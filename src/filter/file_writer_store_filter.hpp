#ifndef __XIOS_FILE_WRITER_STORE_FILTER_HPP__
#define __XIOS_FILE_WRITER_STORE_FILTER_HPP__

#include "input_pin.hpp"

namespace xios
{
  class CField;
  class CFile;
  class CGrid;

  /*!
   * A terminal filter which writes the packets it receives in a file.
   */
  class CFileWriterStoreFilter : public CInputPin
  {
    public:
      /*!
       * Constructs the filter (with one input slot) associated to the specified field
       * and a garbage collector.
       *
       * \param gc the associated garbage collector
       * \param field the associated field
       */
      CFileWriterStoreFilter(CGarbageCollector& gc, CField* field);

      /*!
       * Tests if the filter must auto-trigger.
       *
       * \return true if the filter must auto-trigger
       */
      bool virtual mustAutoTrigger() const;

      /*!
       * Tests whether data is expected for the specified date.
       *
       * \param date the date associated to the data
       */
      bool virtual isDataExpected(const CDate& date) const;

    protected:
      /*!
       * Callbacks a field to write a packet to a file.
       *
       * \param data a vector of packets corresponding to each slot
       */
      void virtual onInputReady(std::vector<CDataPacketPtr> data);

    private:
      CField* field_; //<! The associated field
      CFile* file_ ;
      CGrid* grid_ ;
      CDate lastWrite_ ;
      CDate lastFileSplit_ ;
      CDuration freqWrite_ ;
      int nstep_ ;
      bool needToWrite_ ;
      double scaleFactor_ = 1.0;
      double addOffset_ = 0.0;
      bool hasScaleFactor_ = false ;
      bool hasAddOffset_ = false ;
      double defaultValue_ ;
      bool hasDefaultValue_=false;
      bool hasRounding_=false ;

  }; // class CFileWriterStoreFilter
} // namespace xios

#endif //__XIOS_FILE_WRITER_STORE_FILTER_HPP__
