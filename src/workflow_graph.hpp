#ifndef __WORKFLOW_GRAPH_HPP__
#define __WORKFLOW_GRAPH_HPP__

#include "xios_spl.hpp"
#include "field.hpp"
#include "grid.hpp"
#include "garbage_collector.hpp"
#include "date.hpp"
#include "duration.hpp"
#include "context.hpp"

namespace xios
{
  class CField;
  class CContext;

  struct graph_node_object
  {
    StdString filter_name;

    int filter_class;
    bool filter_filled;
    int expected_entry_nb;
    CDate date;
    Time timestamp;
    StdString transform_type;
    StdString attributes;
    StdString field_id;
    bool inputs_complete;
    int filter_tag;
    int clusterID;
    int distance;
    StdString context_id;
    int context;
    StdString label_field_id;
    bool show;


    graph_node_object():show(true){}

  };

  struct graph_edge_object
  {
    int from;
    int to;

    StdString field_id;
    StdString field_name;
    StdString grid_id;
    CDate date;
    Time timestamp;
    CField *field;
    StdString attributes;
    StdString context_id;
    int context;
    bool show;
    StdString label_info;
    
    graph_edge_object():show(true), label_info("none"){}
  };

  class CWorkflowGraph
  {
    public:

      CWorkflowGraph();

      /*! Map between fields identified by its id and their filters identified by an integer.
       * It is filled up during reconstruction of a workflow (in function CField::buildFilterGraph()).
      */

      static std::vector<graph_node_object> *vectorOfNodes_;
      static std::vector<graph_edge_object> *vectorOfEdges_; 
      static std::vector<StdString> *vectorOfContexts_; 

      static std::vector<graph_node_object> *vectorOfNodes_srv_;
      static std::vector<graph_edge_object> *vectorOfEdges_srv_;
      static std::vector<StdString> *vectorOfContexts_srv_; 

      static std::unordered_map <size_t, int> *mapHashFilterID_;
      static std::unordered_map <size_t, int> *mapHashFilterID_srv_;

      // these variables are not yet used
      static bool clientGraphBuilt;
      static bool serverGraphBuilt;
      static bool build_begin;


      static void drawWorkFlowGraph_client();
      static void drawWorkFlowGraph_server();


      static void addNode(CContext* context, StdString filterName, int filter_class, bool filter_filled, int entry_nb, CDataPacketPtr packet);
      static void addEdge(CContext* context, int from, int to, CDataPacketPtr packet);
      
      // write to file the graph info
      static void outputWorkflowGraph_client();
      static void outputWorkflowGraph_server();
      
      // output on screen the graph info
      static void outputWorkflowGraph_client_stdout();
      static void outputWorkflowGraph_server_stdout();

      static int  getNodeSize();

  };

}

#endif

