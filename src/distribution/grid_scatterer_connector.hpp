#ifndef __GRID_SCATTERER_CONNECTOR_HPP__
#define __GRID_SCATTERER_CONNECTOR_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "distributed_view.hpp"
#include "mpi.hpp"
#include "local_view.hpp"
#include "distributed_view.hpp"
#include "context_client.hpp"
#include "scatterer_connector.hpp"


namespace xios
{
 
  class CGridScattererConnector
  {
    private:
     vector<CScattererConnector*> elementsConnector_ ;
     map<int,int> nbSenders_ ;
     vector<int> ranks_ ;
     map<int,int> dstSize_ ;
     

    public:
      CGridScattererConnector(vector<CScattererConnector*> elementsConnector) : elementsConnector_(elementsConnector) 
      {
        nbSenders_ = elementsConnector[0]->getNbSenders() ;
        for(auto& rank : nbSenders_) 
        {
          ranks_.push_back(rank.first) ;
          dstSize_[rank.first] = 1 ;
        }

        // init dstSize_
        for(auto& connector : elementsConnector_) 
        {
          auto& sizes = connector->getDstSize() ;
          for(auto& rankSize : sizes) dstSize_[rankSize.first] = dstSize_[rankSize.first] * rankSize.second ;
        }    
      }     


      template<typename T, int N> 
      void transfer(const CArray<T,N>& input, map<int, CArray<T,1>>& output)
      {
        int n = elementsConnector_.size()-1 ;
        CScattererConnector** connector = elementsConnector_.data() + n ;
        for(int rank : ranks_) 
        {
          auto& out = output[rank] ;
          out.resize(dstSize_[rank]) ;
          elementsConnector_[n]->transfer(rank, connector, n, input.dataFirst(), out.dataFirst()) ;
        }
      }
 
      template<typename T, int N> 
      void transfer(const CArray<T,N>& input, CContextClient* client, CEventClient& event, const CMessage& messageHeader)
      {
        map<int, CArray<T,1>> output ;
        transfer(input, output) ;
        sendToServer(output, client, event, messageHeader) ;
      }
      
      template<typename T>
      void sendToServer(const map<int, CArray<T,1>>& dataOut, CContextClient* client, CEventClient& event, const CMessage& messageHeader)
      {
        list<CMessage> messages;
        for(auto ranksData : dataOut)
        {
          int rank = ranksData.first ;
          auto& data = ranksData.second ;

          messages.push_back(CMessage(messageHeader));
          messages.back().push(data) ;
          event.push(rank, nbSenders_[rank], messages.back());
        }
        client->sendEvent(event) ;
      }
 
  };
}

#endif