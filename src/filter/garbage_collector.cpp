#include "garbage_collector.hpp"

namespace xios
{
  void CGarbageCollector::registerFilter(CInputPin* inputPin, Time timestamp)
  {
    registeredFilters[timestamp].insert(inputPin);
  }

  void CGarbageCollector::unregisterFilter(CInputPin* inputPin, Time timestamp)
  {
    std::map<Time, std::set<CInputPin*> >::iterator it = registeredFilters.find(timestamp);
    if (it != registeredFilters.end())
      it->second.erase(inputPin);
  }

  void CGarbageCollector::invalidate(Time timestamp)
  {
    std::map<Time, std::set<CInputPin*> >::iterator it    = registeredFilters.begin(),
                                                    itEnd = registeredFilters.lower_bound(timestamp);
    for (; it != itEnd; ++it)
    {
      std::set<CInputPin*>::iterator itFilter    = it->second.begin(),
                                     itFilterEnd = it->second.end();
      for (; itFilter != itFilterEnd; ++itFilter)
        (*itFilter)->invalidate(timestamp);
    }
    registeredFilters.erase(registeredFilters.begin(), itEnd);
  }
} // namespace xios
