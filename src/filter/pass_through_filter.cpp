#include "pass_through_filter.hpp"
#include "workflow_graph.hpp"
#include <algorithm>

namespace xios
{
  CPassThroughFilter::CPassThroughFilter(CGarbageCollector& gc)
    : CFilter(gc, 1, this)
  { /* Nothing to do */ }

  CDataPacketPtr CPassThroughFilter::apply(std::vector<CDataPacketPtr> data)
  {
    buildWorkflowGraph(data);
    return data[0];
  }

  void CPassThroughFilter::buildWorkflowGraph(std::vector<CDataPacketPtr> data)
  {
    if(this->graphEnabled)
    {
      if(data[0]->graphPackage && data[0]->graphPackage->currentField->getId() == this->graphPackage->inFields[0]->getId())
      {
        std::cout<<"PASS THROUGH FILTER OMITTED "<<this<<std::endl;
        return;
      }
      this->graphPackage->filterId = CWorkflowGraph::getNodeSize();
      
      if(!data[0]->graphPackage) 
      {
        data[0]->graphPackage = new CGraphDataPackage;
        data[0]->graphPackage->fromFilter = -1;
        data[0]->graphPackage->currentField = this->graphPackage->inFields[0];
      }

      for(int i=0; i<this->graphPackage->filterId; i++)
      {
        if(CXios::isClient 
          &&  (*CWorkflowGraph::vectorOfNodes_)[i].label_field_id == this->label_field_id  
          && (*CWorkflowGraph::vectorOfNodes_)[i].timestamp == data[0]->timestamp
          && (*CWorkflowGraph::vectorOfNodes_)[i].filter_name == "Pass through filter" )
        {
          data[0]->graphPackage->fromFilter = i;
          return;
        }  
      }
      
      CWorkflowGraph::addNode(getContext(), "Pass through filter", 2, false, 1, data[0]);
      if(CXios::isClient) (*CWorkflowGraph::vectorOfNodes_)[this->graphPackage->filterId].label_field_id = this->label_field_id;
      CWorkflowGraph::addEdge(getContext(), data[0]->graphPackage->fromFilter, this->graphPackage->filterId, data[0]);
      
      data[0]->graphPackage->currentField = this->graphPackage->inFields[0];
      std::rotate(this->graphPackage->inFields.begin(), this->graphPackage->inFields.begin() + 1, this->graphPackage->inFields.end());
      
      data[0]->graphPackage->fromFilter = this->graphPackage->filterId;
        
    } 
  }


} // namespace xios
