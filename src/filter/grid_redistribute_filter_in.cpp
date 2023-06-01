#include "grid_redistribute_filter_in.hpp"
#include "workflow_graph.hpp"
namespace xios
{
  
  CGridRedistributeFilterIn::CGridRedistributeFilterIn(CGarbageCollector& gc, CField* fieldIn, CField* &fieldOut) 
                                    : CFilter(gc, 1, this)
  {
    CGrid* redistributedGrid = fieldIn->getGrid()->redistributeGridToWriter() ;
    fieldOut = CField::create() ;
    fieldOut->field_ref=fieldIn->getId();
    fieldOut->solveRefInheritance() ;
    fieldOut->grid_ref=redistributedGrid->getId();
    fieldOut->domain_ref.reset();
    fieldOut->axis_ref.reset();
    fieldOut->scalar_ref.reset();
    fieldOut-> solveGridReference() ;
    fieldOut-> getGrid()-> solveElementsRefInheritance() ;
    CGrid* newGrid ;
    std::pair<std::shared_ptr<CFilter>, std::shared_ptr<CFilter> > filters = redistributedGrid->buildTransformationGraph(gc, false,  fieldIn->getGrid(), false, 0. , newGrid, false, fieldOut) ;
    fieldOut -> setGrid(newGrid) ;
    fieldOut -> grid_ref = fieldOut->getGrid()->getId() ; // for server 
    fieldOut->getGrid()->checkElementsAttributes() ;

    redistributeConnector_ = fieldIn->getGrid()->getRedistributeToWriterConnector(fieldOut->getGrid()) ;    

  }

  CDataPacketPtr CGridRedistributeFilterIn::apply(std::vector<CDataPacketPtr> data)
  {
    // for now, no auxilliairy field
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    if (packet->status == CDataPacket::NO_ERROR) 
    {
      // buildWorkflowGraph(data, packet, algorithm_);
      
      redistributeConnector_->transfer(data[0]->data, packet->data);
    }
    return packet;
  }
/*
  void CTransformFilter::buildWorkflowGraph(std::vector<CDataPacketPtr> data, CDataPacketPtr packet, shared_ptr<CGridAlgorithm> algorithm)
  {
    if(this->graphEnabled)
    {
      this->graphPackage->filterId = CWorkflowGraph::getNodeSize();
    
      packet->graphPackage = new CGraphDataPackage;
      if(data[0]->graphPackage)
      {
        packet->graphPackage->fromFilter = data[0]->graphPackage->fromFilter;
      }
      packet->graphPackage->toFilter = data[0]->graphPackage->toFilter;
      packet->graphPackage->current_filter_name = data[0]->graphPackage->current_filter_name;
      packet->graphPackage->contextId = data[0]->graphPackage->contextId;          
    
      int tmp_from = packet->graphPackage->fromFilter;
      if(this->graphPackage->show)
      {
        packet->graphPackage->currentField = this->graphPackage->inFields[0];
        CWorkflowGraph::addNode("Spatial transform filter "+algorithm->getAlgoName(), 4, false, 1, packet);
        CWorkflowGraph::addEdge(packet->graphPackage->fromFilter, this->graphPackage->filterId, packet);
        packet->graphPackage->fromFilter = this->graphPackage->filterId;
        packet->graphPackage->currentField = this->graphPackage->inFields[0];
        std::rotate(this->graphPackage->inFields.begin(), this->graphPackage->inFields.begin() + 1, this->graphPackage->inFields.end());
      }
      else
      {
        packet->graphPackage->currentField = this->graphPackage->inFields[0];
        if(CXios::isClient) CWorkflowGraph::vectorOfNodes_->at(tmp_from).filter_name += algorithm->getAlgoName();
        else                CWorkflowGraph::vectorOfNodes_srv_->at(tmp_from).filter_name += algorithm->getAlgoName();
        
      }     
    }
  }
*/
}
