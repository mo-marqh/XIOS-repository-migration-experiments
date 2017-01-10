#ifndef __XIOS_CFileServerWriterFilter__
#define __XIOS_CFileServerWriterFilter__

#include "input_pin.hpp"

namespace xios
{
  class CField;

  /*!
   * A terminal filter which writes the packets it receives in a file.
   */
  class CFileServerWriterFilter : public CInputPin
  {
    public:
      /*!
       * Constructs the filter (with one input slot) associated to the specified field
       * and a garbage collector.
       *
       * \param gc the associated garbage collector
       * \param field the associated field
       */
      CFileServerWriterFilter(CGarbageCollector& gc, CField* field);

    protected:
      /*!
       * Callbacks a field to write a packet to a file.
       *
       * \param data a vector of packets corresponding to each slot
       */
      void virtual onInputReady(std::vector<CDataPacketPtr> data);

    private:
      CField* field; //<! The associated field
      std::map<Time, CDataPacketPtr> packets; //<! The stored packets
  }; // class CFileServerWriterFilter
} // namespace xios

#endif //__XIOS_CFileServerWriterFilter__
