#include "client_from_server_source_filter.hpp"
#include "grid.hpp"
#include "field.hpp"
#include "file.hpp"
#include "exception.hpp"
#include "calendar_util.hpp"
#include "context.hpp"
#include "event_client.hpp"
#include "timer.hpp"
#include "tracer.hpp"
#include <limits> 
#include "workflow_graph.hpp"
#include "grid_redistribute_filter_in.hpp"

namespace xios
{
  CClientOnlineReaderFilter::CClientOnlineReaderFilter(CGarbageCollector& gc, CField* fieldOut)
    : CFilter(gc, 1, this)
  {
    CContext* context = CContext::getCurrent();
 
    CField* fieldIn ;
    redistributeFilter_ = std::shared_ptr<CGridRedistributeFilterIn>(new CGridRedistributeFilterIn(gc, fieldOut, fieldIn));
    fieldIn->setFileIn(fieldOut->getFileIn());
    fieldOut->getFileIn()->replaceEnabledFields(fieldOut, fieldIn) ;
    fileReaderSourceFilter_ = std::shared_ptr<CFileReaderSourceFilter>(new CFileReaderSourceFilter(gc, fieldIn));
    fieldIn->solveServerOperation() ; // might not be called, create a new time functor.... find a better solution later

// connect filters
    fileReaderSourceFilter_->connectOutput(redistributeFilter_, 0);

    field_ = fieldIn ;
    freqOp_ = fieldOut->getRelFile()->output_freq ;
    offset_ = fieldOut->freq_offset ;
  }


  void CClientOnlineReaderFilter::connectOutput(std::shared_ptr<CInputPin> inputPin, size_t inputSlot)
  {
    // connection to redistributeFilter cannot be done at construction time due to shared_from_this(), doing it now
    redistributeFilter_->connectOutput(shared_from_this(), 0);
    CFilter::connectOutput(inputPin,inputSlot) ;
  }

  CDataPacketPtr CClientOnlineReaderFilter::apply(std::vector<CDataPacketPtr> data)
  {
    const CDate& currentDate = CContext::getCurrent()->getCalendar()->getCurrentDate();
    
    data[0]->date = currentDate ;
    data[0]->timestamp = currentDate ;

    if (data[0]->status == CDataPacket::END_OF_STREAM)
    {  
      isEOF_=true ;
      dateEOF_ = data[0]->date ;
    }
    return data[0];
  }

  bool CClientOnlineReaderFilter::sendReadDataRequest(const CDate& tsDataRequested)
  {
    CContext* context = CContext::getCurrent();
    const CDate& currentDate = CContext::getCurrent()->getCalendar()->getCurrentDate();
    CDate checkDate ;
    if (isFirstDataSent_) checkDate = lastDateReceived_ + freqOp_ ;
    else checkDate = context->getCalendar()->getInitDate() + offset_ ;
    
    if (currentDate >= checkDate)  
    {
      fileReaderSourceFilter_->streamData();
      if (isFirstDataSent_)  lastDateReceived_ = lastDateReceived_ + freqOp_ ;
      else lastDateReceived_ = context->getCalendar()->getInitDate() + offset_ ;
      isFirstDataSent_ = true ;
    }
    return !isEOF_;
  }

  bool CClientOnlineReaderFilter::sendReadDataRequestIfNeeded(void)
  TRY
  {
    const CDate& currentDate = CContext::getCurrent()->getCalendar()->getCurrentDate();
    return sendReadDataRequest(currentDate);
  }
  CATCH

  void CClientOnlineReaderFilter::checkForLateData(void)
  TRY
  {
    return ;
  }
  CATCH

  
  bool CClientOnlineReaderFilter::isDataLate(void)
  {
    return false ; 
  }


} // namespace xios
