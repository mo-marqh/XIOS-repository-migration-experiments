#include "client_to_server_store_filter.hpp"
#include "exception.hpp"
#include "field.hpp"
#include "utils.hpp"
#include "context_client.hpp"
#include "timer.hpp"
#include "field.hpp"
#include "grid.hpp"

namespace xios
{
  CClientToServerStoreFilter::CClientToServerStoreFilter(CGarbageCollector& gc, CField* field, CContextClient* client)
    : CInputPin(gc, 1)
    , field_(field), client_(client)
  {
    if (!field)
      ERROR("CClientToServerStoreFilter::CClientToServerStoreFilter(CGarbageCollector& gc, CField* field, CContextClient* client)",
            "The field cannot be null.");
  }

  void CClientToServerStoreFilter::onInputReady(std::vector<CDataPacketPtr> data)
  {
    CTimer::get("Field : send data").resume();
    CEventClient event(field_->getType(), CField::EVENT_ID_UPDATE_DATA);
    CMessage message ;
    message<<field_->getId() << data[0]->timestamp ;
    field_->getSentGrid()->getClientToServerConnector(client_)->transfer(data[0]->data, client_, event, message) ;
    CTimer::get("Field : send data").suspend();
  }
  
  CContextClient* CClientToServerStoreFilter::getTransferedDataSize(map<int,int>& size)
  {
    size = field_->getSentGrid()->getClientToServerConnector(client_)->getTransferedDataSize() ;
    return client_ ;
  }

  bool CClientToServerStoreFilter::mustAutoTrigger() const
  {
    return true;
  }

  bool CClientToServerStoreFilter::isDataExpected(const CDate& date) const
  {
    return true;
  }
} // namespace xios
