#ifndef __XIOS_CFilter__
#define __XIOS_CFilter__

#include "input_pin.hpp"
#include "output_pin.hpp"
#include "filter_engine.hpp"

namespace xios
{
  /*!
   * A generic filter with an input pin and an output pin.
   * A filter can delegate the internal work to an engine
   * which may be shared by multiple filter instances.
   */
  class CFilter : public CInputPin, public COutputPin
  {
    public:
      /*!
       * Constructs a filter with the specified number of input slots
       * and the specified engine.
       *
       * \param gc the associated garbage collector
       * \param inputSlotsCount the number of input slots
       * \param engine the filter engine
       */
      CFilter(CGarbageCollector& gc, size_t inputSlotsCount, IFilterEngine* engine);

    protected:
      IFilterEngine* engine; //!< The filter engine, might be the filter itself

      /*!
       * Generic implementation of the input pin notification function, processes
       * the data using the filter engine and passes the resulting packet (if any)
       * to the downstreams filters.
       *
       * \param data a vector of packets corresponding to each slot
       */
      void virtual onInputReady(std::vector<CDataPacketPtr> data);
  }; // class CFilter
} // namespace xios

#endif //__XIOS_CFilter__
