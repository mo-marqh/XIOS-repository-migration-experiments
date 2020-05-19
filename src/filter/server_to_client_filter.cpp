#include "server_to_client_filter.hpp"
#include "exception.hpp"
#include "field.hpp"
#include "utils.hpp"
#include "context_client.hpp"

namespace xios
{
  CServerToClientFilter::CServerToClientFilter(CGarbageCollector& gc, CField* field, CContextClient* client)
    : CInputPin(gc, 1)
    , field_(field), client_(client)
  {
    if (!field) ERROR("CServerToClientFilter::CServerToClientFilter(CField* field)", "The field cannot be null.");
  }

  void CServerToClientFilter::onInputReady(std::vector<CDataPacketPtr> packets)
  {
    
    if (packets[0]->status == CDataPacket::NO_ERROR)
    {
      field_->sendUpdateDataServerToClient(false, packets[0]->data, client_) ;
    }
    else if (packets[0]->status == CDataPacket::END_OF_STREAM)
    {
      field_->sendUpdateDataServerToClient(true, packets[0]->data, client_) ;
    }
  }


  bool CServerToClientFilter::mustAutoTrigger() const
  {
    return true;
  }

  bool CServerToClientFilter::isDataExpected(const CDate& date) const
  {
    return true;
  }
} // namespace xios
