#include "binary_arithmetic_filter.hpp"
#include "workflow_graph.hpp"

namespace xios
{
  CScalarFieldArithmeticFilter::CScalarFieldArithmeticFilter(CGarbageCollector& gc, const std::string& op, double value)
    : CFilter(gc, 1, this)
    , op(operatorExpr.getOpScalarField(op))
    , value(value)
  { /* Nothing to do */ };

  std::pair<int, int> CScalarFieldArithmeticFilter::buildGraph(std::vector<CDataPacketPtr> data)
  {
    bool building_graph = this->graphEnabled;
    int unique_filter_id;
    bool firstround = true;
    
    if(building_graph)
    {
      if(!data[0]->graphPackage)
      {
        data[0]->graphPackage = new CGraphDataPackage;
        data[0]->graphPackage->currentField = this->graphPackage->inFields[0];
        data[0]->graphPackage->fromFilter = -1;
      }

      if(!CWorkflowGraph::mapHashFilterID_) CWorkflowGraph::mapHashFilterID_ = new std::unordered_map <size_t, int>;

      size_t filterhash = std::hash<StdString>{}(this->graphPackage->inFields[0]->content+to_string(data[0]->timestamp)+this->graphPackage->inFields[0]->getId());

      // first round
      if(CWorkflowGraph::mapHashFilterID_->find(filterhash) == CWorkflowGraph::mapHashFilterID_->end())
      {
        this->graphPackage->filterId = CWorkflowGraph::getNodeSize();
        unique_filter_id = this->graphPackage->filterId;
        CWorkflowGraph::addNode(getContext(), "Arithmetic filter\\n ("+this->graphPackage->inFields[0]->content+")", 4, false, 0, data[0]);        

        CWorkflowGraph::addEdge(getContext(), data[0]->graphPackage->fromFilter, this->graphPackage->filterId, data[0]);
        data[0]->graphPackage->fromFilter = this->graphPackage->filterId;
        data[0]->graphPackage->currentField = this->graphPackage->inFields[0];
        std::rotate(this->graphPackage->inFields.begin(), this->graphPackage->inFields.begin() + 1, this->graphPackage->inFields.end());

       
        (*CWorkflowGraph::mapHashFilterID_)[filterhash] = unique_filter_id; 
        
      }
      // not first round
      else 
      {
        unique_filter_id = (*CWorkflowGraph::mapHashFilterID_)[filterhash];
        if(data[0]->graphPackage->fromFilter != unique_filter_id)
        {
          CWorkflowGraph::addEdge(getContext(), data[0]->graphPackage->fromFilter, unique_filter_id, data[0]);  
        }
      }  
    }

    return std::make_pair(building_graph, unique_filter_id);
  }

  CDataPacketPtr CScalarFieldArithmeticFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;
    
    std::pair<int, int> graph = buildGraph(data);

    if(std::get<0>(graph))
    {  
      packet->graphPackage = new CGraphDataPackage;
      packet->graphPackage->fromFilter = std::get<1>(graph);
      packet->graphPackage->currentField = this->graphPackage->inFields[0]; 
    }

    if (packet->status == CDataPacket::NO_ERROR)
      packet->data.reference(op(value, data[0]->data));

    return packet;
  }

  CFieldScalarArithmeticFilter::CFieldScalarArithmeticFilter(CGarbageCollector& gc, const std::string& op, double value)
    : CFilter(gc, 1, this)
    , op(operatorExpr.getOpFieldScalar(op))
    , value(value)
  { /* Nothing to do */ };

  std::pair<int, int> CFieldScalarArithmeticFilter::buildGraph(std::vector<CDataPacketPtr> data)
  {
    bool building_graph = this->graphEnabled;
    int unique_filter_id;
    bool firstround = true;

    if(building_graph)
    {
      if(!data[0]->graphPackage)
      {
        data[0]->graphPackage = new CGraphDataPackage;
        data[0]->graphPackage->currentField = this->graphPackage->inFields[0];
        data[0]->graphPackage->fromFilter = -1;
      }

      if(!CWorkflowGraph::mapHashFilterID_) CWorkflowGraph::mapHashFilterID_ = new std::unordered_map <size_t, int>;

      size_t filterhash = std::hash<StdString>{}(this->graphPackage->inFields[0]->content+to_string(data[0]->timestamp)+this->graphPackage->inFields[0]->getId());

      // first round
      if(CWorkflowGraph::mapHashFilterID_->find(filterhash) == CWorkflowGraph::mapHashFilterID_->end())
      {
        this->graphPackage->filterId = CWorkflowGraph::getNodeSize();
        unique_filter_id = this->graphPackage->filterId;
        CWorkflowGraph::addNode(getContext(), "Arithmetic filter\\n ("+this->graphPackage->inFields[0]->content+")", 4, false, 0, data[0]);        

        CWorkflowGraph::addEdge(getContext(), data[0]->graphPackage->fromFilter, this->graphPackage->filterId, data[0]);
        data[0]->graphPackage->fromFilter = this->graphPackage->filterId;
        data[0]->graphPackage->currentField = this->graphPackage->inFields[0];
        std::rotate(this->graphPackage->inFields.begin(), this->graphPackage->inFields.begin() + 1, this->graphPackage->inFields.end());

       
        (*CWorkflowGraph::mapHashFilterID_)[filterhash] = unique_filter_id; 
        
      }
      // not first round
      else 
      {
        unique_filter_id = (*CWorkflowGraph::mapHashFilterID_)[filterhash];
        if(data[0]->graphPackage->fromFilter != unique_filter_id)
        {
          CWorkflowGraph::addEdge(getContext(), data[0]->graphPackage->fromFilter, unique_filter_id, data[0]);  
        }
      }  
    }

    return std::make_pair(building_graph, unique_filter_id);
  }

  CDataPacketPtr CFieldScalarArithmeticFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    std::pair<int, int> graph = buildGraph(data);

    if(std::get<0>(graph))
    {  
      packet->graphPackage = new CGraphDataPackage;
      packet->graphPackage->fromFilter = std::get<1>(graph);
      packet->graphPackage->currentField = this->graphPackage->inFields[0]; 
    }

    if (packet->status == CDataPacket::NO_ERROR)
      packet->data.reference(op(data[0]->data, value));

    return packet;
  }

  CFieldFieldArithmeticFilter::CFieldFieldArithmeticFilter(CGarbageCollector& gc, const std::string& op)
    : CFilter(gc, 2, this)
    , op(operatorExpr.getOpFieldField(op))
  { /* Nothing to do */ };

  std::pair<int, int> CFieldFieldArithmeticFilter::buildGraph(std::vector<CDataPacketPtr> data)
  {
    bool building_graph = this->graphEnabled;
    int unique_filter_id;
    bool firstround = true;

    if(building_graph)
    {
      if(!data[0]->graphPackage)
      {
        data[0]->graphPackage = new CGraphDataPackage;
        data[0]->graphPackage->currentField = this->graphPackage->inFields[0];
        data[0]->graphPackage->fromFilter = -1;
      }
      if(!data[1]->graphPackage)
      {
        data[1]->graphPackage = new CGraphDataPackage;
        data[1]->graphPackage->currentField = this->graphPackage->inFields[0];
        data[1]->graphPackage->fromFilter = -1;
      }

      if(!CWorkflowGraph::mapHashFilterID_) CWorkflowGraph::mapHashFilterID_ = new std::unordered_map <size_t, int>;

      size_t filterhash = std::hash<StdString>{}(this->graphPackage->inFields[0]->content+to_string(data[0]->timestamp)+this->graphPackage->inFields[0]->getId());

      // first round
      if(CWorkflowGraph::mapHashFilterID_->find(filterhash) == CWorkflowGraph::mapHashFilterID_->end())
      {
        this->graphPackage->filterId = CWorkflowGraph::getNodeSize();
        unique_filter_id = this->graphPackage->filterId;
        CWorkflowGraph::addNode(getContext(), "Arithmetic filter\\n ("+this->graphPackage->inFields[0]->content+")", 4, false, 0, data[0]);        

        CWorkflowGraph::addEdge(getContext(), data[0]->graphPackage->fromFilter, this->graphPackage->filterId, data[0]);
        data[0]->graphPackage->fromFilter = this->graphPackage->filterId;
        data[0]->graphPackage->currentField = this->graphPackage->inFields[0];

        CWorkflowGraph::addEdge(getContext(), data[1]->graphPackage->fromFilter, this->graphPackage->filterId, data[1]);
        data[1]->graphPackage->fromFilter = this->graphPackage->filterId;
        data[1]->graphPackage->currentField = this->graphPackage->inFields[0];

        std::rotate(this->graphPackage->inFields.begin(), this->graphPackage->inFields.begin() + 1, this->graphPackage->inFields.end());

        (*CWorkflowGraph::mapHashFilterID_)[filterhash] = unique_filter_id; 
        
      }
      // not first round
      else 
      {
        unique_filter_id = (*CWorkflowGraph::mapHashFilterID_)[filterhash];
        if(data[0]->graphPackage->fromFilter != unique_filter_id)
        {
          CWorkflowGraph::addEdge(getContext(), data[0]->graphPackage->fromFilter, unique_filter_id, data[0]);  
        }
        if(data[1]->graphPackage->fromFilter != unique_filter_id)
        {
          CWorkflowGraph::addEdge(getContext(), data[1]->graphPackage->fromFilter, unique_filter_id, data[1]);  
        }
      }  
    }

    return std::make_pair(building_graph, unique_filter_id);
  }


  CDataPacketPtr CFieldFieldArithmeticFilter::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;

    std::pair<int, int> graph = buildGraph(data);

    if(std::get<0>(graph))
    {  
      packet->graphPackage = new CGraphDataPackage;
      packet->graphPackage->fromFilter = std::get<1>(graph);
      packet->graphPackage->currentField = this->graphPackage->inFields[0]; 
    }

    if (data[0]->status != CDataPacket::NO_ERROR)
      packet->status = data[0]->status;
    else if (data[1]->status != CDataPacket::NO_ERROR)
      packet->status = data[1]->status;
    else
    {
      packet->status = CDataPacket::NO_ERROR;
      packet->data.reference(op(data[0]->data, data[1]->data));
    }

    return packet;
  }
} // namespace xios
