#include "grid.hpp"
#include "exception.hpp"
#include "calendar_util.hpp"
#include "field.hpp"
#include "context.hpp"
#include "grid.hpp"
#include <limits> 
#include "workflow_graph.hpp"

namespace xios
{
    CClientFromClientSourceFilter::CClientFromClientSourceFilter(CGarbageCollector& gc, CField* field)
     : COutputPin(gc, true)
    {
      CContext* context = getContext();
      field_=field ;
      grid_= field-> getGrid();

      freqOp_ = field->freq_op ;
      lastDateReceived_ = context->getCalendar()->getInitDate();
      offset_ = field->freq_offset ;
    }

  void CClientFromClientSourceFilter::streamData(CEventServer& event)
  {
    // unlikely to input from file server where data are received at ts=0
    // for coupling, it would be after the first freq_op, because for now we don't have
    // restart mecanism to send the value at ts=0. It must be changed in future
  
    if (wasDataAlreadyReceived_) lastDateReceived_ = lastDateReceived_ + freqOp_;
    else wasDataAlreadyReceived_ = true ;

    CDate date = lastDateReceived_ + offset_; // not sure is usefull check it for all filters

    CDataPacketPtr packet(new CDataPacket);
    packet->date = date;
    packet->status = CDataPacket::NO_ERROR;
    for (auto& subEvent : event.subEvents) (*subEvent.buffer) >> packet->timestamp  ; // better management of time lien later...
    packet->timestamp = date;
         
    grid_->getClientFromClientConnector()->transfer(event,packet->data) ;

    if(this->graphEnabled)
    {
      this->graphPackage->filterId = CWorkflowGraph::getNodeSize();
      CWorkflowGraph::addNode(getContext(), "Client to Client Source filter", 1, false, 0, packet);
    }
    onOutputReady(packet);
  }

  bool CClientFromClientSourceFilter::isDataLate(void)
  {
    bool isDataLate ;
    CDate currentDate = getContext()->getCalendar()->getCurrentDate() ;
    if (wasDataAlreadyReceived_) isDataLate = lastDateReceived_ + offset_ + freqOp_ <= currentDate ;
    else isDataLate = getContext()->getCalendar()->getInitDate()+ offset_ <= currentDate ;
    return isDataLate ;
  }

} // namespace xios
