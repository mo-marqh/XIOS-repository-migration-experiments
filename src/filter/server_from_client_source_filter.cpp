#include "server_from_client_source_filter.hpp"
#include "grid.hpp"
#include "exception.hpp"
#include "calendar_util.hpp"
#include "context.hpp"


namespace xios
{
  CServerFromClientSourceFilter::CServerFromClientSourceFilter(CGarbageCollector& gc, CGrid* grid)
    : COutputPin(gc)
    , grid_(grid)
  {
    if (!grid_)
      ERROR("CServerFromClientSourceFilter::CServerFromClientSourceFilter(CGarbageCollector& gc, CGrid* grid)",
            "Impossible to construct a source filter without providing a grid.");
  }

  void CServerFromClientSourceFilter::streamData(CEventServer& event)
  {
    Time timeStamp ;
    for (auto& subEvent : event.subEvents) (*subEvent.buffer) >> timeStamp  ;
    CDataPacketPtr packet(new CDataPacket);
    packet->date = CContext::getCurrent()->getCalendar()->getTimeOrigin() + timeStamp*Second; // very bad, better to pass directly the date
    packet->timestamp = timeStamp;
    packet->status = CDataPacket::NO_ERROR;
    grid_->getServerFromClientConnector()->transfer(event,packet->data) ;

    onOutputReady(packet);
  }

 
  
} // namespace xios
