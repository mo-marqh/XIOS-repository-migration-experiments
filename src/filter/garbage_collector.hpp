#ifndef __XIOS_CGarbageCollector__
#define __XIOS_CGarbageCollector__

#include <map>
#include <set>

#include "input_pin.hpp"

namespace xios
{
  /*!
   * A basic garbage collector which ensures no old packets linger in the filter graph.
   */
  class CGarbageCollector
  {
    public:
      /*!
       * Constructs a garbage collector.
       */
      CGarbageCollector()
      { /* Nothing to do */ };

      /*!
       * Registers a filter for a specified timestamp.
       *
       * \param inputPin the input pin of the filter to register
       * \param timestamp the timestamp for which the filter is registered
       */
      void registerFilter(CInputPin* inputPin, Time timestamp);

      /*!
       * Removes a filter previously registered for a specified timestamp.
       *
       * \param inputPin the input pin of the filter to unregister
       * \param timestamp the timestamp for which the filter is unregistered
       */
      void unregisterFilter(CInputPin* inputPin, Time timestamp);

      /*!
       * Ensures all registered filters invalidate packets older than the specified timestamp.
       *
       * \param timestamp the timestamp used for invalidation
       */
      void invalidate(Time timestamp);

    private:
      CGarbageCollector(const CGarbageCollector&);
      CGarbageCollector& operator=(const CGarbageCollector&);

      std::map<Time, std::set<CInputPin*> > registeredFilters; //!< Currently registered filters
  }; // class CGarbageCollector
} // namespace xios

#endif //__XIOS_CGarbageCollector__
