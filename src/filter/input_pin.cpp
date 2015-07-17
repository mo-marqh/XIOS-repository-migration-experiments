#include "input_pin.hpp"
#include "exception.hpp"

namespace xios
{
  CInputPin::CInputPin(size_t slotsCount)
    : slotsCount(slotsCount)
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
      it = inputs.insert(std::make_pair(packet->timestamp, InputBuffer(slotsCount))).first;
    it->second.slotsFilled++;
    it->second.packets[inputSlot] = packet;

    if (it->second.slotsFilled == slotsCount)
    {
      onInputReady(it->second.packets);
      inputs.erase(it);
    }
  }
} // namespace xios
