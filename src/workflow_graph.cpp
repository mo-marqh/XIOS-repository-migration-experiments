#include "workflow_graph.hpp"
#include "cxios.hpp"

namespace xios
{

  std::vector<graph_node_object> *CWorkflowGraph::vectorOfNodes_ = 0;
  std::vector<graph_edge_object> *CWorkflowGraph::vectorOfEdges_ = 0;
  std::vector<StdString> *CWorkflowGraph::vectorOfContexts_ = 0;

  std::vector<graph_node_object> *CWorkflowGraph::vectorOfNodes_srv_ = 0;
  std::vector<graph_edge_object> *CWorkflowGraph::vectorOfEdges_srv_ = 0;
  std::vector<StdString> *CWorkflowGraph::vectorOfContexts_srv_ = 0;

  bool CWorkflowGraph::clientGraphBuilt = false;
  bool CWorkflowGraph::serverGraphBuilt = false;
  bool CWorkflowGraph::build_begin = false;

  std::unordered_map <size_t, int> *CWorkflowGraph::mapHashFilterID_ = 0;
  std::unordered_map <size_t, int> *CWorkflowGraph::mapHashFilterID_srv_ = 0;


  CWorkflowGraph::CWorkflowGraph()
  { }


//******************************************************

  void CWorkflowGraph::outputWorkflowGraph_client_stdout()
  {
    std::cout<<"\n\nbuild workflow graph ..."<<std::endl;
    for(int i=0; i<vectorOfNodes_->size(); i++)
    {
      std::cout<<"Node["<<i<<"] is "<<(*vectorOfNodes_)[i].filter_name<<std::endl;
      info(100)<<"Node["<<i<<"] is "<<(*vectorOfNodes_)[i].filter_name<<std::endl;
    }
  
    for(int i=0; i<vectorOfEdges_->size(); i++)
    {
      std::cout<<"Edge["<<i<<"] from "<<(*vectorOfEdges_)[i].from<<" to "<<(*vectorOfEdges_)[i].to<<std::endl;
      info(100)<<"Edge["<<i<<"] from "<<(*vectorOfEdges_)[i].from<<" to "<<(*vectorOfEdges_)[i].to<<std::endl;
    }
    std::cout<<"\nend workflow graph ...\n\n"<<std::endl;
  }
  
  void CWorkflowGraph::outputWorkflowGraph_server_stdout()
  {
    std::cout<<"\n\nServer side : build workflow graph ..."<<std::endl;
    for(int i=0; i<vectorOfNodes_srv_->size(); i++)
    {
      info(100)<<"Node["<<i<<"] is "<<(*vectorOfNodes_srv_)[i].filter_name<<std::endl;
    }
  
    for(int i=0; i<vectorOfEdges_srv_->size(); i++)
    {
      info(100)<<"Edge["<<i<<"] from "<<(*vectorOfEdges_srv_)[i].from<<" to "<<(*vectorOfEdges_srv_)[i].to<<std::endl;
    }
    std::cout<<"\nend workflow graph ...\n\n"<<std::endl;
  }


  void CWorkflowGraph::drawWorkFlowGraph_client()
  TRY
  {
    if(vectorOfNodes_ && vectorOfEdges_) 
    {
      outputWorkflowGraph_client();
    }
    else info(100)<<"Client side : no graph information"<<std::endl;
  }
  CATCH
  

  void CWorkflowGraph::drawWorkFlowGraph_server()
  TRY
  {
    if(vectorOfNodes_srv_ && vectorOfEdges_srv_) 
    {
      outputWorkflowGraph_server();
    }
    else info(100)<<"Server side : no graph information"<<std::endl;
  }
  CATCH
  
  void CWorkflowGraph::addEdge(int from, int to, CDataPacketPtr packet)
  TRY
  {
    if(from<0) return;

    if(CXios::isClient)
    {
      // if(vectorOfEdges_&&vectorOfNodes_) outputWorkflowGraph_client_stdout();
      // std::cout<<"Trying to add an edge from "<<from<<" to "<<to<<std::endl;
      if(!vectorOfEdges_) vectorOfEdges_ = new std::vector<graph_edge_object>;
      std::string currentContextId = CContext::getCurrent()->getId();
      
      graph_edge_object edge_obj;    
      edge_obj.from = from;
      edge_obj.to = to;
      edge_obj.date = packet->date;
      edge_obj.timestamp = packet->timestamp;
      edge_obj.field = packet->graphPackage->currentField;
      edge_obj.show = true;
      
      if(vectorOfNodes_->at(from).filter_class == 2) // from pass through filter
      {
        edge_obj.label_info = vectorOfNodes_->at(from).label_field_id;
      }
      
      if(vectorOfNodes_->at(to).filter_class == 3) // to temporal filter
      {
        vectorOfNodes_->at(to).expected_entry_nb++;
      }

      for(int i=0; i<vectorOfContexts_->size(); i++)
      {
        if(vectorOfContexts_->at(i) == currentContextId)
        {
          edge_obj.context = i;      
          edge_obj.context_id = currentContextId;      
          break;
        }
      } 
      edge_obj.attributes = packet->graphPackage->currentField->recordXiosAttributes();
      
      vectorOfEdges_->push_back(edge_obj);
      //info(100)<<"****************** Add Edge from "<<from<<" to "<<to<<std::endl; 
      vectorOfNodes_->at(from).filter_filled = true;
    }
    else
    {
      if(!vectorOfEdges_srv_) vectorOfEdges_srv_ = new std::vector<graph_edge_object>;
      std::string currentContextId = CContext::getCurrent()->getId();
      
      graph_edge_object edge_obj;    
      edge_obj.from = from;
      edge_obj.to = to;
      edge_obj.date = packet->date;
      edge_obj.timestamp = packet->timestamp;
      edge_obj.field = packet->graphPackage->currentField;
      edge_obj.show = true;
      for(int i=0; i<vectorOfContexts_srv_->size(); i++)
      {
        if(vectorOfContexts_srv_->at(i) == currentContextId)
        {
          edge_obj.context = i;      
          edge_obj.context_id = currentContextId;      
          break;
        }
      }
      edge_obj.attributes = packet->graphPackage->currentField->recordXiosAttributes();
      
      vectorOfEdges_srv_->push_back(edge_obj);
      //info(100)<<"****************** Server side : Add Edge from "<<from<<" to "<<to<<std::endl; 
      vectorOfNodes_srv_->at(from).filter_filled = true;

    }
  }
  CATCH
  

  void CWorkflowGraph::addNode(StdString filterName, int filterClass, bool filterFilled, int entryNb, CDataPacketPtr packet)
  TRY
  {
    if(CXios::isClient)
    {
      //if(vectorOfEdges_&&vectorOfNodes_) outputWorkflowGraph_client_stdout();
      // std::cout<<"Trying to add a node naming "<<filterName<<std::endl;
      if(!vectorOfNodes_) vectorOfNodes_ = new std::vector<graph_node_object>;
      if(!vectorOfContexts_) vectorOfContexts_ = new std::vector<StdString>;
      if(!mapHashFilterID_) mapHashFilterID_ = new std::unordered_map <size_t, int>;

      std::string currentContextId = CContext::getCurrent()->getId();
      if ( std::find(vectorOfContexts_->begin(), vectorOfContexts_->end(), currentContextId) == vectorOfContexts_->end() )
         vectorOfContexts_->push_back(currentContextId);
      
      
      graph_node_object node_obj;    
      node_obj.filter_name = filterName;
      node_obj.filter_class = filterClass;
      node_obj.filter_filled = filterFilled;
      node_obj.expected_entry_nb = entryNb;
      node_obj.date = packet->date;
      node_obj.timestamp = packet->timestamp;
      
      for(int i=0; i<vectorOfContexts_->size(); i++)
      {
        if(vectorOfContexts_->at(i) == currentContextId)
        {
          node_obj.context = i;      
          node_obj.context_id = currentContextId;      
          break;
        }
      }    
      
      node_obj.attributes = packet->graphPackage->currentField->recordXiosAttributes();
      
      vectorOfNodes_->push_back(node_obj);
    }
    else
    { 
      if(!vectorOfNodes_srv_) vectorOfNodes_srv_ = new std::vector<graph_node_object>;
      if(!vectorOfContexts_srv_) vectorOfContexts_srv_ = new std::vector<StdString>;
      if(!mapHashFilterID_srv_) mapHashFilterID_srv_ = new std::unordered_map <size_t, int>;

      std::string currentContextId = CContext::getCurrent()->getId();
      if ( std::find(vectorOfContexts_srv_->begin(), vectorOfContexts_srv_->end(), currentContextId) == vectorOfContexts_srv_->end() )
         vectorOfContexts_srv_->push_back(currentContextId);
      
      graph_node_object node_obj;    
      node_obj.filter_name = filterName;
      node_obj.filter_class = filterClass;
      node_obj.filter_filled = filterFilled;
      node_obj.expected_entry_nb = entryNb;
      node_obj.date = packet->date;
      node_obj.timestamp = packet->timestamp;
      for(int i=0; i<vectorOfContexts_srv_->size(); i++)
      {
        if(vectorOfContexts_srv_->at(i) == currentContextId)
        {
          node_obj.context = i;      
          node_obj.context_id = currentContextId;      
          break;
        }
      }  
      node_obj.attributes = packet->graphPackage->currentField->recordXiosAttributes();

      vectorOfNodes_srv_->push_back(node_obj);
    }

  }
  CATCH

  int CWorkflowGraph::getNodeSize()
  TRY
  {
    if(CXios::isClient)
    {
      return !vectorOfNodes_? 0 : vectorOfNodes_->size();
    }
    else
    {
      return !vectorOfNodes_srv_? 0 : vectorOfNodes_srv_->size();
    }
  }
  CATCH



  void CWorkflowGraph::outputWorkflowGraph_client()
  {
    int graph_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &graph_rank);
    std::ofstream *outfiles;

    outfiles = new std::ofstream[vectorOfContexts_->size()];

    for(int ctx=0; ctx<vectorOfContexts_->size(); ctx++)
    {
      StdString graphFileName="graph_data_"+vectorOfContexts_->at(ctx)+"_client_"+to_string(graph_rank)+".json";
      outfiles[ctx].open(graphFileName); 
    
      outfiles[ctx] << "{\"nodes\":["<<std::endl;
    }
    for(int i=0; i<vectorOfNodes_->size(); i++)
    {
      int ctx = vectorOfNodes_->at(i).context;
      if(i!=0) outfiles[ctx] << ",";
      outfiles[ctx] << "{\"id\":"<<i<<","<<std::endl;
      outfiles[ctx] << "\"label\":"<<"\""<<vectorOfNodes_->at(i).filter_name<<"\","<<std::endl;
      outfiles[ctx] << "\"class\":"<<vectorOfNodes_->at(i).filter_class<<","<<std::endl;
      outfiles[ctx] << "\"filled\":"<<!(vectorOfNodes_->at(i).filter_filled)<<","<<std::endl;
      outfiles[ctx] << "\"context\":"<<"\""<<vectorOfNodes_->at(i).context_id<<"\","<<std::endl;
      outfiles[ctx] << "\"entry\":"<<"\""<<vectorOfNodes_->at(i).expected_entry_nb<<"\","<<std::endl;
      outfiles[ctx] << "\"attributes\":"<<"\""<<vectorOfNodes_->at(i).attributes<<"\","<<std::endl;
      outfiles[ctx] << "\"type\":"<<"\"\"}"<<std::endl;
    } 
    for(int ctx=0; ctx<vectorOfContexts_->size(); ctx++)
    {
      outfiles[ctx] << std::endl<<"],"<<std::endl<<"\"edges\" : ["<<std::endl;
    }
    for(int i=0; i<vectorOfEdges_->size(); i++)
    {
      int ctx = vectorOfEdges_->at(i).context;
      if(i!=0) outfiles[ctx] << ",";
      outfiles[ctx] << "{\"id\":"<<i<<","<<std::endl;
      outfiles[ctx] << "\"from\":"<<vectorOfEdges_->at(i).from<<","<<std::endl;
      outfiles[ctx] << "\"to\":"<<vectorOfEdges_->at(i).to<<","<<std::endl;
      if(vectorOfEdges_->at(i).label_info != "none")
      {
        if(vectorOfEdges_->at(i).show) outfiles[ctx] << "\"label\":"<<"\""<<vectorOfEdges_->at(i).label_info<<"\\n"<<vectorOfEdges_->at(i).date<<"\","<<std::endl;
        else outfiles[ctx] << "\"label\":"<<"\"\\n"<<vectorOfEdges_->at(i).date<<"\","<<std::endl;
      }
      else
      {
        if(vectorOfEdges_->at(i).show) outfiles[ctx] << "\"label\":"<<"\""<<vectorOfEdges_->at(i).field->getId()<<"\\n"<<vectorOfEdges_->at(i).date<<"\","<<std::endl;
        else outfiles[ctx] << "\"label\":"<<"\"\\n"<<vectorOfEdges_->at(i).date<<"\","<<std::endl;
      }
      outfiles[ctx] << "\"context\":"<<"\""<<vectorOfEdges_->at(i).context_id<<"\","<<std::endl;
      outfiles[ctx] << "\"attributes\":"<<"\""<<vectorOfEdges_->at(i).attributes<<"\","<<std::endl;
      outfiles[ctx] << "\"date\":"<<"\""<<vectorOfEdges_->at(i).date<<"\"}"<<std::endl;
    }
    for(int ctx=0; ctx<vectorOfContexts_->size(); ctx++)
    {
      outfiles[ctx] << std::endl<<"]}"<<std::endl;
      outfiles[ctx].close();
    }
  }
  

  void CWorkflowGraph::outputWorkflowGraph_server()
  {
    int graph_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &graph_rank);
    std::ofstream *outfiles;

    outfiles = new std::ofstream[vectorOfContexts_srv_->size()];

    for(int ctx=0; ctx<vectorOfContexts_srv_->size(); ctx++)
    {
      StdString graphFileName="graph_data_"+vectorOfContexts_srv_->at(ctx)+"_client_"+to_string(graph_rank)+".json";
      outfiles[ctx].open(graphFileName); 
    
      outfiles[ctx] << "{\"nodes\":["<<std::endl;
    }
    for(int i=0; i<vectorOfNodes_srv_->size(); i++)
    {
      int ctx = vectorOfNodes_srv_->at(i).context;
      if(i!=0) outfiles[ctx] << ",";
      outfiles[ctx] << "{\"id\":"<<i<<","<<std::endl;
      outfiles[ctx] << "\"label\":"<<"\""<<vectorOfNodes_srv_->at(i).filter_name<<"\","<<std::endl;
      outfiles[ctx] << "\"class\":"<<vectorOfNodes_srv_->at(i).filter_class<<","<<std::endl;
      outfiles[ctx] << "\"filled\":"<<!(vectorOfNodes_srv_->at(i).filter_filled)<<","<<std::endl;
      outfiles[ctx] << "\"context\":"<<"\""<<vectorOfNodes_srv_->at(i).context_id<<"\","<<std::endl;
      outfiles[ctx] << "\"entry\":"<<"\""<<vectorOfNodes_srv_->at(i).expected_entry_nb<<"\","<<std::endl;
      outfiles[ctx] << "\"attributes\":"<<"\""<<vectorOfNodes_srv_->at(i).attributes<<"\","<<std::endl;
      outfiles[ctx] << "\"type\":"<<"\"\"}"<<std::endl;
    } 
    for(int ctx=0; ctx<vectorOfContexts_srv_->size(); ctx++)
    {
      outfiles[ctx] << std::endl<<"],"<<std::endl<<"\"edges\" : ["<<std::endl;
    }
    for(int i=0; i<vectorOfEdges_srv_->size(); i++)
    {
      int ctx = vectorOfEdges_srv_->at(i).context;
      if(i!=0) outfiles[ctx] << ",";
      outfiles[ctx] << "{\"id\":"<<i<<","<<std::endl;
      outfiles[ctx] << "\"from\":"<<vectorOfEdges_srv_->at(i).from<<","<<std::endl;
      outfiles[ctx] << "\"to\":"<<vectorOfEdges_srv_->at(i).to<<","<<std::endl;
      if(vectorOfEdges_srv_->at(i).show) outfiles[ctx] << "\"label\":"<<"\""<<vectorOfEdges_srv_->at(i).field->getId()<<"\\n"<<vectorOfEdges_srv_->at(i).date<<"\","<<std::endl;
      else                               outfiles[ctx] << "\"label\":"<<"\"\\n"<<vectorOfEdges_srv_->at(i).date<<"\","<<std::endl;
      outfiles[ctx] << "\"context\":"<<"\""<<vectorOfEdges_srv_->at(i).context_id<<"\","<<std::endl;
      outfiles[ctx] << "\"attributes\":"<<"\""<<vectorOfEdges_srv_->at(i).attributes<<"\","<<std::endl;
      outfiles[ctx] << "\"date\":"<<"\""<<vectorOfEdges_srv_->at(i).date<<"\"}"<<std::endl;
    }
    for(int ctx=0; ctx<vectorOfContexts_srv_->size(); ctx++)
    {
      outfiles[ctx] << std::endl<<"]}"<<std::endl;
      outfiles[ctx].close();
    }
  }
}

