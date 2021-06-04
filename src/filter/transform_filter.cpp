#include "transform_filter.hpp"
#include "grid_algorithm.hpp"
#include "workflow_graph.hpp"
namespace xios
{
  
  CTransformFilter::CTransformFilter( CGarbageCollector& gc, int slots, CGridAlgorithm* algo, bool detectMissingValues, double defaultValue) 
                                    : CFilter(gc, slots, this), algorithm_(algo), 
                                      detectMissingValues_(detectMissingValues), defaultValue_(defaultValue)
  {

  }

  CDataPacketPtr CTransformFilter::apply(std::vector<CDataPacketPtr> data)
  {
    // for now, no auxilliairy field
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    if (packet->status == CDataPacket::NO_ERROR) 
    {
      buildWorkflowGraph(data, packet, algorithm_);
      
      if (data.size()>1)
      {
        vector<CArray<double,1>> auxData(data.size()-1); 
        for(int i=0;i<data.size()-1 ;i++) auxData[i].reference(data[i+1]->data) ;
        algorithm_->apply(data[0]->data, auxData, packet->data);
      }
      else algorithm_->apply(data[0]->data, packet->data);
    }
    return packet;
  }

  void CTransformFilter::buildWorkflowGraph(std::vector<CDataPacketPtr> data, CDataPacketPtr packet, CGridAlgorithm* algorithm)
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

}
