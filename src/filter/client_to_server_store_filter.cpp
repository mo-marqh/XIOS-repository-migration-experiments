#include "client_to_server_store_filter.hpp"
#include "exception.hpp"
#include "field.hpp"
#include "utils.hpp"
#include "context_client.hpp"
#include "timer.hpp"
#include "field.hpp"
#include "grid.hpp"
#include "workflow_graph.hpp"

namespace xios
{
  CClientToServerStoreFilter::CClientToServerStoreFilter(CGarbageCollector& gc, CField* field, CContextClient* client)
    : CInputPin(gc, 1)
    , field_(field), client_(client), graphEnabled(false)
  {
    if (!field)
      ERROR("CClientToServerStoreFilter::CClientToServerStoreFilter(CGarbageCollector& gc, CField* field, CContextClient* client)",
            "The field cannot be null.");
  }

  void CClientToServerStoreFilter::onInputReady(std::vector<CDataPacketPtr> data)
  {
    buildWorkflowGraph(data);

    CTimer::get("Field : send data").resume();
    CEventClient event(field_->getType(), CField::EVENT_ID_UPDATE_DATA);
    CMessage message ;
    message<<field_->getId() << data[0]->timestamp ;
    field_->getSentGrid()->getClientToServerConnector(client_)->transfer(data[0]->data, client_, event, message) ;
    CTimer::get("Field : send data").suspend();
  }

  void CClientToServerStoreFilter::buildWorkflowGraph(std::vector<CDataPacketPtr> data)
  {
    if(this->graphEnabled)
    {
      this->graphPackage->filterId = CWorkflowGraph::getNodeSize();
      if(!data[0]->graphPackage) data[0]->graphPackage = new CGraphDataPackage;
      
      std::rotate(this->graphPackage->inFields.begin(), this->graphPackage->inFields.begin() + 1, this->graphPackage->inFields.end());
      
      CWorkflowGraph::addNode("Client to Server Store filter", 6, true, 1, data[0]);
     
      CWorkflowGraph::addEdge(data[0]->graphPackage->fromFilter, this->graphPackage->filterId, data[0]);
      // flux can be redirected to other filters. So don't change the 'from' parameter
      data[0]->graphPackage->currentField = this->graphPackage->inFields[0];
    }
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
