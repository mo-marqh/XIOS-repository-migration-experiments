#ifndef __XIOS_CLIENT_TO_SERVER_STORE_FILTER__
#define __XIOS_CLIENT_TO_SERVER_STORE_FILTER__

#include "input_pin.hpp"

namespace xios
{
  class CField;
  class CContextClient ;

  /*!
   * A terminal filter which transmits the packets it receives to a field for writting in a file.
   */
  class CClientToServerStoreFilter : public CInputPin
  {
    public:
      /*!
       * Constructs the filter (with one input slot) associated to the specified field
       * and a garbage collector.
       *
       * \param gc the associated garbage collector
       * \param field the associated field
       */
      CClientToServerStoreFilter(CGarbageCollector& gc, CField* field, CContextClient* client);

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
      CContextClient* client_ ; //! the associated context client
  }; // class CClientToServerStoreFilter
} // namespace xios

#endif //__XIOS_CLIENT_TO_SERVER_STORE_FILTER__
