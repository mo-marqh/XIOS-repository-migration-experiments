#include "graphviz.hpp"
#include "workflow_graph.hpp"

namespace xios
{

  CGraphviz::CGraphviz()
  { }

  /*!
   *
   */
  void CGraphviz::buildWorkflowGraphDot()
  TRY
  {
    if (CWorkflowGraph::mapFieldToFilters_ptr_with_info !=0 && !CWorkflowGraph::mapFieldToFilters_ptr_with_info->empty())
    {
      CWorkflowGraph::buildStaticWorkflow();
    
      typedef boost::property<boost::edge_name_t, std::string> EdgeProperty;
      typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, boost::no_property, EdgeProperty> Graph;

      // Input 1: nodes (=filters)
      vector<StdString>& nodes = CWorkflowGraph::filters;
      const int nbNodes = nodes.size();

      // Input 2: edges (=fields)
      vector<StdString>& edges = CWorkflowGraph::fields;

      // Input 3: nodes to edges connectivity
      vector<pair<int, int> >& nodesToEdges = CWorkflowGraph::fieldsToFilters;
      const int nbEdges = nodesToEdges.size();

      // Declare a graph object, adding the edges and edge properties
      Graph g(nbNodes);
      for (int j = 0; j < nbEdges; ++j)
      {
        graph_traits<Graph>::edge_descriptor e;
        bool inserted;
        boost::tie(e, inserted) = boost::add_edge(nodesToEdges[j].first, nodesToEdges[j].second, edges[j], g);
      }

      std::for_each(vertices(g).first, vertices(g).second, exercise_vertex<Graph>(g));

      std::map<std::string,std::string> graph_attr, vertex_attr, edge_attr;
      graph_attr["size"] = "5,5";
      graph_attr["rankdir"] = "LR";
      graph_attr["ratio"] = "fill";
      vertex_attr["shape"] = "record";
      vertex_attr["width"] = "2.2";
      vertex_attr["fontsize"] = "16";

      const std::string filename = "graph.dot";
      std::ofstream file (filename.c_str());

      boost::write_graphviz(file, g,
          boost::make_label_writer(&nodes[0]),
          boost::make_label_writer(get(edge_name, g)),
          boost::make_graph_attributes_writer(graph_attr, vertex_attr, edge_attr));
         
    }
  }
  CATCH




  void CGraphviz::buildWorkflowGraphVisjs_with_info()
  TRY
  {
    if (CWorkflowGraph::mapFilters_ptr_with_info !=0 && !CWorkflowGraph::mapFilters_ptr_with_info->empty())
    {
      CWorkflowGraph::buildStaticWorkflow_with_info();

      StdString color_table[7] = {"black", "red", "blue", "green", "purple", "yellow", "gray"};
    
      std::ofstream fs_json;
      fs_json.open ("graph_data.json", std::fstream::out);

      fs_json << "{\"nodes\":["<<std::endl<<"      ";
      static bool firstnode=true;
      static bool firstedge=true;
    
      for (auto it=CWorkflowGraph::mapFilters_ptr_with_info->begin(); it != CWorkflowGraph::mapFilters_ptr_with_info->end(); it++)
      {
        if(firstnode) 
        {
          fs_json << "{\"id\": "<<it->first +1<<", "<<std::endl;
          firstnode = false;
        }
        else
        {
          fs_json << ",{\"id\": "<<it->first +1<<", "<<std::endl;
        }
        if(it->second.filter_class == 1) // source filter
          fs_json << "       \"label\": \""<<it->second.filter_name<<"\\n("<<it->second.field_id<<")\", "<<std::endl;
        else
          fs_json << "       \"label\": \""<<it->second.filter_name<<"\", "<<std::endl;
        fs_json << "       \"class\": "<<it->second.filter_class<<", "<<std::endl;
        fs_json << "       \"filled\": "<<it->second.filter_filled<<", "<<std::endl;
        fs_json << "       \"type\": \""<<it->second.transform_type<<"\", "<<std::endl;
        fs_json << "       \"entry\": "<<it->second.expected_entry_nb<<", "<<std::endl;
        fs_json << "       \"inputs\": "<<it->second.inputs_complete<<", "<<std::endl;
        fs_json << "       \"tag\": "<<it->second.filter_tag<<", "<<std::endl;
        fs_json << "       \"cid\": "<<it->second.clusterID<<", "<<std::endl;
        fs_json << "       \"distance\": "<<it->second.distance<<", "<<std::endl;
        fs_json << "       \"attributes\": \""<<it->second.attributes<<"\"}"<<std::endl<<"      ";
      }
      fs_json << "    ],"<<std::endl;


      fs_json << " \"edges\" : ["<<std::endl<<"      ";

      for (auto it=CWorkflowGraph::mapFieldToFilters_ptr_with_info->begin(); it != CWorkflowGraph::mapFieldToFilters_ptr_with_info->end(); it++)
      {
        if(firstedge)
        {
          fs_json << "{\"id\": "<<it->first +1<<", "<<std::endl;
          firstedge = false;
        }
        else
        {
          fs_json << ",{\"id\": "<<it->first +1<<", "<<std::endl;
        }
        fs_json << "       \"from\": "<<it->second.from+1<<", "<<std::endl;
        fs_json << "       \"to\": "<<it->second.to+1<<", "<<std::endl;
        fs_json << "       \"label\": \""<<it->second.field_id<<"\\n"<<it->second.date<<"\", "<<std::endl;
        // fs_json << "       \"title\": \""<<"Show more information about this field"<<"\", "<<std::endl;
        // fs_json << "       \"fid\": \""<<it->second.field_id<<"\", "<<std::endl;
        // fs_json << "       \"fname\": \""<<it->second.field_name<<"\", "<<std::endl;
        // fs_json << "       \"gid\": \""<<it->second.grid_id<<"\", "<<std::endl;
        fs_json << "       \"date\": \""<<it->second.date<<"\", "<<std::endl;
        fs_json << "       \"attributes\": \"id = "<<it->second.field_id<<"</br>"<<it->second.attributes<<"\"}"<<std::endl<<"      ";

      }
      fs_json << "    ]}"<<std::endl;

      fs_json.close();
    }
  }
  CATCH



  void CGraphviz::showStaticWorkflowGraph()
  TRY
  {
    CWorkflowGraph::showStaticWorkflow();
  }
  CATCH
}
