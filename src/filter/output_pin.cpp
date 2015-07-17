#include "output_pin.hpp"
#include "exception.hpp"

namespace xios
{
  void COutputPin::connectOutput(boost::shared_ptr<CInputPin> inputPin, size_t inputSlot)
  {
    if (!inputPin)
      ERROR("void COutputPin::connectOutput(CInputPin* inputPin, size_t inputSlot)",
            "The input pin cannot be null.");

    outputs.push_back(std::make_pair(inputPin, inputSlot));
  }

  void COutputPin::deliverOuput(CDataPacketPtr packet)
  {
    if (!packet)
      ERROR("void COutputPin::deliverOuput(CDataPacketPtr packet)",
            "The packet cannot be null.");

    std::vector<std::pair<boost::shared_ptr<CInputPin>, size_t> >::iterator it, itEnd;
    for (it = outputs.begin(), itEnd = outputs.end(); it != itEnd; ++it)
      it->first->setInput(it->second, packet);
  }
} // namespace xios
