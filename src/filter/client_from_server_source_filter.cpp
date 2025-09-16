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

namespace xios
{
  CClientFromServerSourceFilter::CClientFromServerSourceFilter(CGarbageCollector& gc, CField* field)
    : COutputPin(gc, true)
  {
    CContext* context = getContext();
    field_ = field ;
    grid_= field->getSentGrid();
    freqOp_ = field->getRelFile()->output_freq ;
    client_= field->getRelFile()->getContextClient() ;
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
      info(20)<<"Receiv Data from server to client: FieldId : "<<field_->getId()<<endl ;
      info(20)<<"lastDateReceived_ "<<lastDateReceived_<< "  date "<<packet->date<<"  ----> EOF"<<endl; 

    }
    else 
    {
      CContextClient* client = event.getContextServer()->getAssociatedClient() ;
      grid_->getClientFromServerConnector(client)->transfer(event, packet->data) ; // to avoid to make a search in map for corresponding client connector, 
     
      info(20)<<"Receiv Data from server to client: FieldId : "<<field_->getId()<<endl ;
      info(20)<<"lastDateReceived_ "<<lastDateReceived_<< "  date "<<packet->date<<endl;                                                                                    // make a registration at initialization once
      packet->status = CDataPacket::NO_ERROR;
    }

    if(this->graphEnabled)
    {
      this->graphPackage->filterId = CWorkflowGraph::getNodeSize();
      CWorkflowGraph::addNode(getContext(), "Client from Server Source filter", 1, false, 0, packet);
    }
    onOutputReady(packet);

  }
 
  int CClientFromServerSourceFilter::sendReadDataRequest(const CDate& tsDataRequested)
  {
    CContext* context = getContext();
    lastDataRequestedFromServer_ = tsDataRequested;

    // No need to send the request if we are sure that we are already at EOF
    if (!isEOF_ || context->getCalendar()->getCurrentDate() <= dateEOF_)
    {
      CEventClient event(field_->getType(), CField::EVENT_ID_READ_DATA);
      if (client_->isServerLeader())
      {
        CMessage msg;
        msg << field_->getId();
        for(auto& rank : client_->getRanksServerLeader()) event.push(rank, 1, msg);
        client_->sendEvent(event);
      }
      else client_->sendEvent(event);
    }
    else 
    {
      CDataPacketPtr packet(new CDataPacket);
      packet->date = tsDataRequested;
      packet->timestamp = packet->date ;
      packet->status = CDataPacket::END_OF_STREAM;
      onOutputReady(packet);
    }

    wasDataRequestedFromServer_ = true;

    return !isEOF_;
  }

  bool CClientFromServerSourceFilter::sendReadDataRequestIfNeeded(void)
  TRY
  {
    const CDate& currentDate = getContext()->getCalendar()->getCurrentDate();

    bool dataRequested = false;

    while (currentDate >= lastDataRequestedFromServer_)
    {
      info(20) << "currentDate : " << currentDate << endl ;
      info(20) << "Field : " << field_->getId() << endl ;
      info(20) << "lastDataRequestedFromServer : " << lastDataRequestedFromServer_ << endl ;
      info(20) << "freqOp : " << freqOp_ << endl ;
      info(20) << "lastDataRequestedFromServer + fileIn_->output_freq.getValue() : " << lastDataRequestedFromServer_ + freqOp_ << endl ;

      dataRequested |= sendReadDataRequest(lastDataRequestedFromServer_ + freqOp_);
    }

    return dataRequested;
  }
  CATCH

  void CClientFromServerSourceFilter::checkForLateData(void)
  TRY
  {
    CContext* context = getContext();
    // Check if data previously requested has been received as expected
    if (wasDataRequestedFromServer_ && ! isEOF_)
    {
      CTimer timer("CClientFromServerSourceFilter::checkForLateDataFromServer");
      timer.resume();
      traceOff() ;
      timer.suspend();
      
      bool isLate;
      do
      {
        isLate = isDataLate();
        if (isLate)
        {
          timer.resume();
          context->globalEventLoop();
          context->eventLoop() ; 
          timer.suspend();
        }
      }
      while (isLate && timer.getCumulatedTime() < CXios::recvFieldTimeout);
      timer.resume();
      traceOn() ;
      timer.suspend() ;


      if (isLate)
        ERROR("void CClientFromServerSourceFilter::checkForLateDataFromServer(void)",
              << "Late data at timestep = " << context->getCalendar()->getCurrentDate());
    }
  }
  CATCH

  
  bool CClientFromServerSourceFilter::isDataLate(void)
  {
    bool isDataLate ;
    CDate currentDate = getContext()->getCalendar()->getCurrentDate() ;

    const CDate nextDataDue = wasDataAlreadyReceived_ ? (lastDateReceived_ + freqOp_) : getContext()->getCalendar()->getInitDate();
    isDataLate = (nextDataDue <= currentDate);
    
    return isDataLate ; 
    
  }


} // namespace xios
