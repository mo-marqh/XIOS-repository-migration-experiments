#include "filter.hpp"

namespace xios
{
  CFilter::CFilter(size_t inputSlotsCount, IFilterEngine* engine)
    : CInputPin(inputSlotsCount)
    , COutputPin()
    , engine(engine)
  { /* Nothing to do */ }

  void CFilter::onInputReady(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr outputPacket = engine->apply(data);
    if (outputPacket)
      deliverOuput(outputPacket);
  }
} // namespace xios
