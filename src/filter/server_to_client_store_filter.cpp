#include "server_to_client_store_filter.hpp"
#include "exception.hpp"
#include "field.hpp"
#include "grid.hpp"
#include "utils.hpp"
#include "context_client.hpp"

namespace xios
{
  CServerToClientStoreFilter::CServerToClientStoreFilter(CGarbageCollector& gc, CField* field, CContextClient* client)
    : CInputPin(gc, 1)
    , field_(field), client_(client)
  {
    if (!field) ERROR("CServerToClientFilter::CServerToClientFilter(CField* field)", "The field cannot be null.");
    grid_ = field_ -> getGrid() ;
  }

  void CServerToClientStoreFilter::onInputReady(std::vector<CDataPacketPtr> packets)
  {
    bool isEOF ;
    if (packets[0]->status == CDataPacket::NO_ERROR) isEOF = false ;
    else if (packets[0]->status == CDataPacket::END_OF_STREAM) isEOF = true ;

    CEventClient event(field_->getType(), CField::EVENT_ID_READ_DATA_READY);

    auto connector = grid_->getServerToClientConnector() ;
    CMessage msg ;
    msg<<field_->getId() ;

    if (isEOF) 
    {
      msg<<(int)(-1) ;
      connector->transfer(client_,event, msg) ;
      info(20)<<"Send Data from server to client: FieldId : "<<field_->getId()<<"  step : "<<nStep_<< " -->EOF"<<endl; 
    }
    else 
    {
      msg<<nStep_ ;
      connector->transfer(packets[0]->data, client_, event, msg) ;
      info(20)<<"Send Data from server to client: FieldId : "<<field_->getId()<<"  step : "<<nStep_<<endl; 
    }

    nStep_++ ;
  }


  bool CServerToClientStoreFilter::mustAutoTrigger() const
  {
    return true;
  }

  bool CServerToClientStoreFilter::isDataExpected(const CDate& date) const
  {
    return true;
  }
} // namespace xios
