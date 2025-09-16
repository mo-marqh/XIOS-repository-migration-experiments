#ifndef __XIOS_CStoreFilter__
#define __XIOS_CStoreFilter__

#include "input_pin.hpp"
#include "graph_package.hpp"

namespace xios
{
  class CContext;
  class CGrid;
  class CField ;

  /*!
   * A terminal filter which stores all the packets it receives.
   */
  class CClientToModelStoreFilter : public CInputPin
  {
    public:
      /*!
       * Constructs the filter with one input slot and an associated
       * garbage collector for the specified grid and context.
       *
       * \param gc the garbage collector associated with this input pin
       * \param context the context to which the data belongs
       * \param grid the grid to which the data is attached
       * \param detectMissingValues whether missing values should be detected
       * \param missingValue the value to use to replace missing values
       */
      CClientToModelStoreFilter(CGarbageCollector& gc, CField* field);

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

      /*!
       * Removes all pending packets which are older than the specified timestamp.
       *
       * \param timestamp the timestamp used for invalidation
       */
      void virtual invalidate(Time timestamp);

      CGraphPackage * graphPackage;
      bool graphEnabled;

    protected:
      /*!
       * Stores the packet for later access.
       *
       * \param data a vector of packets corresponding to each slot
       */
      void virtual onInputReady(std::vector<CDataPacketPtr> data);

    private:
      CGarbageCollector& gc_; //!< The garbage collector associated to the filter
      CGrid* grid_; //!< The grid attached to the data the filter can accept
      bool detectMissingValues_; //!< Whether missing values should be detected
      double missingValue_; //!< The value to use to replace missing values
      bool hasMissingValue_ ;
      std::map<Time, CDataPacketPtr> packets_; //<! The stored packets
  }; // class CClientToModelStoreFilter
} // namespace xios

#endif //__XIOS_CStoreFilter__
