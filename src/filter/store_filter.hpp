#ifndef __XIOS_CStoreFilter__
#define __XIOS_CStoreFilter__

#include "input_pin.hpp"

namespace xios
{
  class CContext;
  class CGrid;

  /*!
   * A terminal filter which stores all the packets it receives.
   */
  class CStoreFilter : public CInputPin
  {
    public:
      /*!
       * Constructs the filter with one input slot for the specified grid
       * and context.
       *
       * \param context the context to which the data belongs
       * \param grid the grid to which the data is attached
       */
      CStoreFilter(CContext* context, CGrid* grid);

      /*!
       * Accesses the filter storage and retuns the packet corresponding
       * to the specified timestamp. If there is no packet available for
       * the specified timestamp, the function waits until the data is
       * received or a timeout occurs.
       *
       * \param timestamp the timestamp of the requested packet
       * \return a pointer to a read-only packet
       */
      CConstDataPacketPtr getPacket(Time timestamp);

      /*!
       * Accesses the filter storage and retuns the data corresponding
       * to the specified timestamp. If there is no data available for
       * the specified timestamp, the function waits until the data is
       * received or a timeout occurs.
       *
       * \param timestamp the timestamp of the requested data
       * \param data the array where the data is to be copied
       * \return the status code associated with the data
       */
      template <int N>
      CDataPacket::StatusCode getData(Time timestamp, CArray<double, N>& data);

    protected:
      /*!
       * Stores the packet for later access.
       *
       * \param data a vector of packets corresponding to each slot
       */
      void virtual onInputReady(std::vector<CDataPacketPtr> data);

    private:
      CContext* context; //!< The context to which the data belongs
      CGrid* grid; //!< The grid attached to the data the filter can accept
      std::map<Time, CDataPacketPtr> packets; //<! The stored packets
  }; // class CStoreFilter
} // namespace xios

#endif //__XIOS_CStoreFilter__
