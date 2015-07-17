#include "temporal_filter.hpp"
#include "functor_type.hpp"
#include "calendar_util.hpp"

namespace xios
{
  CTemporalFilter::CTemporalFilter(CGarbageCollector& gc, const std::string& opId,
                                   const CDate& initDate, const CDuration samplingFreq, const CDuration samplingOffset, const CDuration opFreq,
                                   bool ignoreMissingValue /*= false*/, double missingValue /*= 0.0*/)
    : CFilter(gc, 1, this)
    , samplingFreq(samplingFreq)
    , opFreq(opFreq)
    , nextSamplingDate(initDate + samplingOffset + initDate.getRelCalendar().getTimeStep())
    , nextOperationDate(initDate + opFreq)
    , isFirstOperation(true)
  {
#define DECLARE_FUNCTOR(MType, mtype) \
    if (opId.compare(#mtype) == 0) \
    { \
      if (ignoreMissingValue) \
      { \
        functor.reset(new func::C##MType(tmpData, missingValue)); \
      } \
      else \
      { \
        functor.reset(new func::C##MType(tmpData)); \
      } \
    }

#include "functor_type.conf"

    if (!functor)
      ERROR("CTemporalFilter::CTemporalFilter(CGarbageCollector& gc, const std::string& opId, ...)",
            << "\"" << opId << "\" is not a valid operation.");

    isOnceOperation = (functor->timeType() == func::CFunctor::once);
  }

  CDataPacketPtr CTemporalFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet;

    if (data[0]->status != CDataPacket::END_OF_STREAM)
    {
      const bool usePacket = isOnceOperation ? isFirstOperation : (data[0]->date >= nextSamplingDate);
      if (usePacket)
      {
        if (!tmpData.numElements())
          tmpData.resize(data[0]->data.numElements());

        (*functor)(data[0]->data);

        nextSamplingDate = nextSamplingDate + samplingFreq;
      }

      const bool outputResult = isOnceOperation ? isFirstOperation : (data[0]->date + samplingFreq > nextOperationDate);
      if (outputResult)
      {
        functor->final();

        packet = CDataPacketPtr(new CDataPacket);
        packet->date = data[0]->date;
        packet->timestamp = data[0]->timestamp;
        packet->status = data[0]->status;
        packet->data.resize(tmpData.numElements());
        packet->data = tmpData;

        isFirstOperation = false;
        nextOperationDate = nextOperationDate + opFreq;
      }
    }

    return packet;
  }
} // namespace xios
