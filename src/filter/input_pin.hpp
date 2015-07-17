#ifndef __XIOS_CInputPin__
#define __XIOS_CInputPin__

#include <vector>
#include <map>

#include "data_packet.hpp"

namespace xios
{
  class CGarbageCollector;

  /*!
   * An input pin handles the data packets received by a filter.
   */
  class CInputPin
  {
    public:
      /*!
       * Constructs an input pin with the specified number of slots
       * and an associated garbage collector.
       *
       * \param gc the garbage collector associated with this input pin
       * \param slotsCount the number of slots
       */
      CInputPin(CGarbageCollector& gc, size_t slotsCount);

      /*!
       * Receives a data packet from an upstream filter on
       * the specified input slot.
       * The receiving filter takes ownership of the packet.
       *
       * \param inputSlot the input slot number
       * \param packet the data packet to be received
       */
      void setInput(size_t inputSlot, CDataPacketPtr packet);

      /*!
       * Removes all pending packets which are older than the specified timestamp.
       *
       * \param timestamp the timestamp used for invalidation
       */
      void virtual invalidate(Time timestamp);

    protected:
      CGarbageCollector& gc; //!< The garbage collector associated to the input pin

      /*!
       * Function triggered when all slots have been filled for a specific timestamp.
       * It should be implemented by the filter class to process the data.
       *
       * \param data a vector of packets corresponding to each slot
       */
      void virtual onInputReady(std::vector<CDataPacketPtr> data) = 0;

    private:
      /*!
       * Helper structure, groups a vector of packets corresponding to each slot
       * with the number of slots currently filled
       */
      struct InputBuffer
      {
        size_t slotsFilled; //!< Number of slots currently filled
        std::vector<CDataPacketPtr> packets; //< Vector of packets corresponding to each slot

        /*!
         * Initialize an empty input buffer for the specified number of slots.
         *
         * \param slotsCount the number of slots
         */
        InputBuffer(size_t slotsCount)
          : slotsFilled(0)
          , packets(slotsCount)
        { /* Nothing to do */ };
      };

      size_t slotsCount; //!< The number of slots

      //! Input buffer, store the packets until all slots are full for a timestep
      std::map<Time, InputBuffer> inputs;
  }; // class CInputPin
} // namespace xios

#endif //__XIOS_CInputPin__
