#include "input_pin.hpp"
#include "garbage_collector.hpp"
#include "exception.hpp"

namespace xios
{
  CInputPin::CInputPin(CGarbageCollector& gc, size_t slotsCount)
    : gc(gc)
    , slotsCount(slotsCount)
  { /* Nothing to do */ }

  void CInputPin::setInput(size_t inputSlot, CDataPacketPtr packet)
  {
    if (inputSlot >= slotsCount)
      ERROR("void CInputPin::setInput(size_t inputSlot, CDataPacketPtr packet)",
            "The input slot " << inputSlot << " does not exist.");
    if (!packet)
      ERROR("void CInputPin::setInput(size_t inputSlot, CDataPacketPtr packet)",
            "The packet cannot be null.");

    std::map<Time, InputBuffer>::iterator it = inputs.find(packet->timestamp);
    if (it == inputs.end())
    {
      it = inputs.insert(std::make_pair(packet->timestamp, InputBuffer(slotsCount))).first;
      gc.registerFilter(this, packet->timestamp);
    }
    it->second.slotsFilled++;
    it->second.packets[inputSlot] = packet;

    if (it->second.slotsFilled == slotsCount)
    {
      // Unregister before calling onInputReady in case the filter registers again
      gc.unregisterFilter(this, packet->timestamp);
      onInputReady(it->second.packets);
      inputs.erase(it);
    }
  }

  void CInputPin::invalidate(Time timestamp)
  {
    inputs.erase(inputs.begin(), inputs.lower_bound(timestamp));
  }
} // namespace xios
