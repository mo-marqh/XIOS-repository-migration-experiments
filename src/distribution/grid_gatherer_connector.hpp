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
      void transfer(const map<int, CArray<T,1>>& input, CArray<T,1>& output, T missingValue)
      {
        int n = elementsConnector_.size()-1 ;
        CGathererConnector** connector = elementsConnector_.data() + n ;
        output.resize(dstSize_) ;
        output = missingValue ;
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
      

      void transfer_or(CEventServer& event, CArray<bool,1>& dataOut)
      {
        map<int, CArray<bool,1>> dataIn ;
        for (auto& subEvent : event.subEvents) 
        {
          auto& data = dataIn[subEvent.rank]; 
          (*subEvent.buffer) >> data ;
        }
        transfer_or(dataIn, dataOut) ;
      }

      template<typename T>
      void transfer(CEventServer& event, CArray<T,1>& dataOut, T missingValue)
      {
        map<int, CArray<T,1>> dataIn ;
        for (auto& subEvent : event.subEvents) 
        {
          auto& data = dataIn[subEvent.rank]; 
          (*subEvent.buffer) >> data ;
        }
        transfer(dataIn, dataOut, missingValue) ;
      }

      void transfer_or(const map<int, CArray<bool,1>>& input, CArray<bool,1>& output)
      {
        int n = elementsConnector_.size()-1 ;
        CGathererConnector** connector = elementsConnector_.data() + n ;
        output.resize(dstSize_) ;
        output = false ;
        for(auto& rankDataIn : input) 
        {
          elementsConnector_[n]->transfer_or(rankDataIn.first, connector, n, rankDataIn.second.dataFirst(), output.dataFirst()) ;
        }
      } 

  };
}

#endif