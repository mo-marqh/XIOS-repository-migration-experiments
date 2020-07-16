#ifndef __REMOTE_CONNECTOR_HPP__
#define __REMOTE_CONNECTOR_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "distributed_view.hpp"
#include "mpi.hpp"
#include "local_view.hpp"
#include "distributed_view.hpp"
#include "context_client.hpp"


namespace xios
{
 
  class CRemoteConnector
  {
    private:
      CLocalView* srcView_;
      CDistributedView* dstView_;
      map<int, vector<int>> connector_ ; // connector[rank][srcIndex]

      MPI_Comm localComm_ ;
      map<int,int> nbSenders_ ; // number of participant when sending remote buffer
      map<int, CArray<size_t,1>> element_ ; // global index of elements to send
    
    public:
      CRemoteConnector(CLocalView* srcView, CDistributedView* dstView, MPI_Comm localComm) : srcView_(srcView), dstView_(dstView), localComm_(localComm){} ;
      void computeConnector(void) ;
      map<int, CArray<size_t,1>>& getDistributedGlobalIndex() { return element_ ;} 
      
      template<typename T>
      void transfer(const CArray<T,1>& dataIn, map<int, CArray<T,1>>& dataOut)
      {
        auto ptrDataIn = dataIn.dataFirst() ;
        for(auto& indexRank : connector_) 
        {
          int rank=indexRank.first ;
          auto& index=indexRank.second ;
          auto it = dataOut.emplace(rank, CArray<T,1>(index.size())).first ; // return an iterator on the obect inserted
          auto ptrDataOut = it->second.dataFirst() ;
          for(auto ind : index) 
          {
            *ptrDataOut = ptrDataIn[ind] ;
            ptrDataOut++ ;
          }
        }
      }
      
      template<typename T>
      void sendToServer(map<int, CArray<T,1>>& dataOut, CContextClient* client, CEventClient& event, const CMessage& messageHeader)
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

      template<typename T>
      void transferToServer(const CArray<T,1>& dataIn, CContextClient* client, CEventClient& event, const CMessage& messageHeader)
      {
        map<int, CArray<T,1>> dataOut ;
        transfer(dataIn, dataOut) ;
        sendToServer(dataOut, client, event,messageHeader) ;
      }

  } ;

}

#endif