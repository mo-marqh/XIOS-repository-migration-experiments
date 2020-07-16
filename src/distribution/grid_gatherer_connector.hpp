#ifndef __GRID_GATHERER_CONNECTOR_HPP__
#define __GRID_GATHERER_CONNECTOR_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "distributed_view.hpp"
#include "mpi.hpp"
#include "local_view.hpp"
#include "distributed_view.hpp"
#include "context_client.hpp"
#include "gatherer_connector.hpp"


namespace xios
{
 
  class CGridGathererConnector
  {
    private:
    
      vector<CGathererConnector*> elementsConnector_ ;
      int dstSize_ ;

    public:
      CGridGathererConnector(vector<CGathererConnector*> elementsConnector) : elementsConnector_(elementsConnector)
      {
        dstSize_ = 1 ;
        for(auto& connector : elementsConnector_) dstSize_=dstSize_*connector->getDstSize() ;
      }

      template<typename T> 
      void transfer(const map<int, CArray<T,1>>& input, CArray<T,1>& output)
      {
        int n = elementsConnector_.size()-1 ;
        CGathererConnector** connector = elementsConnector_.data() + n ;
        output.resize(dstSize_) ;
        for(auto& rankDataIn : input) 
        {
          elementsConnector_[n]->transfer(rankDataIn.first, connector, n, rankDataIn.second.dataFirst(), output.dataFirst()) ;
        }
      } 

      template<typename T>
      void transfer(CEventServer& event, CArray<T,1>& dataOut)
      {
        map<int, CArray<T,1>> dataIn ;
        for (auto& subEvent : event.subEvents) 
        {
          auto& data = dataIn[subEvent.rank]; 
          (*subEvent.buffer) >> data ;
        }
        transfer(dataIn, dataOut) ;
      }
  
  };
}

#endif