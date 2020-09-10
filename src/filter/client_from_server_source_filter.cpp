#include "client_from_server_source_filter.hpp"
#include "grid.hpp"
#include "field.hpp"
#include "file.hpp"
#include "exception.hpp"
#include "calendar_util.hpp"
#include "context.hpp"
#include <limits> 

namespace xios
{
  CClientFromServerSourceFilter::CClientFromServerSourceFilter(CGarbageCollector& gc, CField* field)
    : COutputPin(gc, true)
  {
    CContext* context = CContext::getCurrent();
    grid_= field->getGrid();
    freqOp_ = field->fileIn_->output_freq ;
    lastDateReceived_ = context->getCalendar()->getInitDate();
    offset_ = field->freq_offset ;
  }

  void CClientFromServerSourceFilter::streamData(CEventServer& event)
  {
    const bool wasEOF = isEOF_;
    int record ;
    for (auto& subEvent : event.subEvents) (*subEvent.buffer) >> record  ;
    isEOF_ = (record == int(-1));

    
    if (wasDataAlreadyReceived_) lastDateReceived_ = lastDateReceived_ + freqOp_ ;
    else wasDataAlreadyReceived_ = true;
    
    CDataPacketPtr packet(new CDataPacket);
    packet->date = lastDateReceived_ + offset_;
    packet->timestamp = packet->date ;

    if (isEOF_) 
    {
      if (!wasEOF) dateEOF_ = lastDateReceived_;
      packet->status = CDataPacket::END_OF_STREAM;
    }
    else 
    {
      grid_->getServerFromClientConnector()->transfer(event, packet->data) ;
      packet->status = CDataPacket::NO_ERROR;
    }
    onOutputReady(packet);

  }
 
  bool CClientFromServerSourceFilter::isDataLate(void)
  {
    bool isDataLate ;
    CDate currentDate = CContext::getCurrent()->getCalendar()->getCurrentDate() ;

    const CDate nextDataDue = wasDataAlreadyReceived_ ? (lastDateReceived_ + freqOp_) : CContext::getCurrent()->getCalendar()->getInitDate();
    isDataLate = (nextDataDue <= currentDate);
    
    return isDataLate ; 
    
  }
} // namespace xios
