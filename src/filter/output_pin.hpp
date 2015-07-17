#ifndef __XIOS_COutputPin__
#define __XIOS_COutputPin__

#include "input_pin.hpp"

namespace xios
{
  /*!
   * An output pin handles the connections with downstream filters.
   */
  class COutputPin
  {
    public:
      /*!
       * Connects to a specific slot of the input pin of a downstream filter.
       * Note that the output pin holds a reference on the downstream filter.
       *
       * \param inputPin the input pin to connect
       * \param inputSlot the input slot number
       */
      void connectOutput(boost::shared_ptr<CInputPin> inputPin, size_t inputSlot);

    protected:
      /*!
       * Delivers an output packet to the downstreams filter.
       *
       * \param packet the packet to output
       */
      void deliverOuput(CDataPacketPtr packet);

    private:
      //!< The list of connected filters and the corresponding slot numbers
      std::vector<std::pair<boost::shared_ptr<CInputPin>, size_t> > outputs;
  }; // class COutputPin
} // namespace xios

#endif //__XIOS_COutputPin__
