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
#include "reduction_types.hpp"


namespace xios
{
 
  class CGridGathererConnector
  {
    private:
    
      vector<shared_ptr<CGathererConnector>> elementsConnector_ ;
      int dstSize_ ;

    public:
      CGridGathererConnector(vector<shared_ptr<CGathererConnector>> elementsConnector) : elementsConnector_(elementsConnector)
      {
        dstSize_ = 1 ;
        for(auto& connector : elementsConnector_) dstSize_=dstSize_*connector->getDstSize() ;
      }

      template<typename T> 
      void transfer(const map<int, CArray<T,1>>& input, CArray<T,1>& output, EReduction op = EReduction::none)
      {
        int n = elementsConnector_.size()-1 ;
        shared_ptr<CGathererConnector>* connector = elementsConnector_.data() + n ;
        output.resize(dstSize_) ;
       
        if (op == EReduction::none)
          for(auto& rankDataIn : input) 
            elementsConnector_[n]->transfer(rankDataIn.first, connector, n, rankDataIn.second.dataFirst(), output.dataFirst()) ;
        else
        {
          T defaultValue = std::numeric_limits<T>::quiet_NaN();
          vector<int> count(dstSize_,0) ;
          for(auto& rankDataIn : input) 
            elementsConnector_[n]->transfer(rankDataIn.first, connector, n, rankDataIn.second.dataFirst(), output.dataFirst(), op, count.data()) ;

          for(int i=0;i<dstSize_;i++) if (count[i]==0) output(i)=defaultValue ;
        }
      } 

      template<typename T> 
      void transfer(const map<int, CArray<T,1>>& input, CArray<T,1>& output, T missingValue)
      {
        int n = elementsConnector_.size()-1 ;
        shared_ptr<CGathererConnector>* connector = elementsConnector_.data() + n ;
        output.resize(dstSize_) ;
        output = missingValue ;
        for(auto& rankDataIn : input) 
        {
          elementsConnector_[n]->transfer(rankDataIn.first, connector, n, rankDataIn.second.dataFirst(), output.dataFirst()) ;
        }
      } 

      template<typename T>
      void transfer(CEventServer& event, CArray<T,1>& dataOut, EReduction op = EReduction::none)
      {
        map<int, CArray<T,1>> dataIn ;
        for (auto& subEvent : event.subEvents) 
        {
          auto& data = dataIn[subEvent.rank]; 
          (*subEvent.buffer) >> data ;
        }
        transfer(dataIn, dataOut, op) ;
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
        shared_ptr<CGathererConnector>* connector = elementsConnector_.data() + n ;
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