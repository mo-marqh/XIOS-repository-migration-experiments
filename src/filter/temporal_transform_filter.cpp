#include "temporal_transform_filter.hpp"
#include "workflow_graph.hpp"

namespace xios
{

  CTemporalTransformFilter::CTemporalTransformFilter(CGarbageCollector& gc, int slots, CGridAlgorithm* algo, int nrecords, bool detectMissingValues, double defaultValue)
  : CTransformFilter(gc, slots, algo, detectMissingValues, defaultValue), nrecords_(nrecords), graphCycleCompleted(true)
  {
  }

  void CTemporalTransformFilter::buildWorkflowGraph(std::vector<CDataPacketPtr> data)
  {
    if(this->graphEnabled )
    {
      if(!data[0]->graphPackage)
      {
        data[0]->graphPackage = new CGraphDataPackage;
      }
      
      if(graphCycleCompleted)
      {  
        this->graphPackage->filterId = CWorkflowGraph::getNodeSize();
        CWorkflowGraph::addNode("Temporal splitting filter", 7, false, 0, data[0]);
        graphCycleCompleted = false;
      }
      
      data[0]->graphPackage->currentField = this->graphPackage->inFields[0];
      std::rotate(this->graphPackage->inFields.begin(), this->graphPackage->inFields.begin() + 1, this->graphPackage->inFields.end());
      
      CWorkflowGraph::addEdge(data[0]->graphPackage->fromFilter, this->graphPackage->filterId, data[0]);
      data[0]->graphPackage->fromFilter = this->graphPackage->filterId;
      data[0]->graphPackage->currentField = this->graphPackage->inFields[0];
      std::rotate(this->graphPackage->inFields.begin(), this->graphPackage->inFields.begin() + 1, this->graphPackage->inFields.end());
    }

  }
  
  CDataPacketPtr CTemporalTransformFilter::apply(std::vector<CDataPacketPtr> data)
  {
    if (data[0]->status == CDataPacket::NO_ERROR)
    {
      buildWorkflowGraph(data);
      if (record_==0) tmpData_.resize(nrecords_) ;
      algorithm_->apply(data[0]->data, tmpData_[record_]);
      record_++ ;
      if (record_==nrecords_)
      {
        size_t size=0 ;
        for(auto& it : tmpData_) size += it.numElements() ;
        // for now, no auxilliairy field
        CDataPacketPtr packet(new CDataPacket);
        packet->date = data[0]->date;
        packet->timestamp = data[0]->timestamp;
        packet->status = data[0]->status;
        packet->data.resize(size) ;
        packet->graphPackage = data[0]->graphPackage;
        double* out = packet->data.dataFirst() ;
        for(auto& it : tmpData_) 
        {
          size = it.numElements() ; 
          double* tmp = it.dataFirst() ;
          for(size_t i=0 ; i<size ; i++, out++, tmp++) *out=*tmp ;   
        }
        tmpData_.clear() ;
        record_=0 ;
        graphCycleCompleted = true;
        return packet ;
      }
      else return nullptr ;
    }
    else // error
    {
      CDataPacketPtr packet(new CDataPacket);
      packet->date = data[0]->date;
      packet->timestamp = data[0]->timestamp;
      packet->status = data[0]->status;
      return packet ;
    }    
  }
}