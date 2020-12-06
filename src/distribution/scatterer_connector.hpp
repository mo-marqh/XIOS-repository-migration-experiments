#ifndef __SCATTERER_CONNECTOR_HPP__
#define __SCATTERER_CONNECTOR_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "distributed_view.hpp"
#include "mpi.hpp"
#include "local_view.hpp"
#include "distributed_view.hpp"
#include "context_client.hpp"


namespace xios
{
 
  class CScattererConnector
  {

    private:
      map<int, vector<int>> connector_ ;
      map<int, vector<bool>> mask_ ;  // mask is on dst view
      MPI_Comm localComm_ ;
      int remoteCommSize_ ;

      CLocalView* srcView_ ;
      CDistributedView* dstView_ ;
      map<int,int> nbSenders_ ; // number of participant when sending remote buffer
      int srcSize_ ;
      map<int,int> dstSize_ ;

    public:

    CScattererConnector(CLocalView* srcView, CDistributedView* dstView, MPI_Comm localComm, int remoteCommSize) 
                       : srcView_(srcView), dstView_(dstView), localComm_(localComm), remoteCommSize_(remoteCommSize) {}
    void computeConnector(void) ;
    
    template<typename T, int n>
    void transfer(const CArray<T,n>& dataIn, map<int, CArray<T,1>>& dataOut)
    {
      transfer(1,1, dataIn, dataOut) ;
    }

    template<typename T, int n>
    void transfer(const CArray<T,n>& dataIn, map<int, CArray<T,1>>& dataOut, T missingValue)
    {
      transfer(1, 1, dataIn, dataOut, missingValue) ;
    }

    template<typename T, int n>
    void transfer(int sizeT, const CArray<T,n>& dataIn, map<int, CArray<T,1>>& dataOut)
    {
      transfer(1, sizeT, dataIn, dataOut)
    }
    
    template<typename T, int n>
    void transfer(int repeat, int sizeT, const CArray<T,n>& dataIn, map<int, CArray<T,1>>& dataOut)
    {
      // for future, make a specific transfer function for sizeT=1 to avoid multiplication (increasing performance)
      size_t srcSlice = sizeT*srcSize_ ;
      for(auto& rankConnector : connector_)
      {
        int rank = rankConnector.first ;
        auto& connector = rankConnector.second ;
        auto& mask = mask_[rank] ;
        int dstSize = mask.size() ;
        auto& data = dataOut[rank] ;
        size_t dstSlice = dstSize*sizeT ;
        data.resize(repeat*dstSlice) ;
        T* dstData = data.dataFirst() ;
        const T* srcData = dataIn.dataFirst() ;
        for(int l=0; l<repeat; l++)
        {
          for(int i=0, j=0; i<dstSize; i++)
            if (mask[i]) 
            {
              for(int k=0;k<sizeT;k++) dstData[i*sizeT+k] = srcData[connector[j]*sizeT+k] ;
              j++ ;
            }
          dstData+=dstSlice ;
          srcData+=srcSlice ;
        }
      }
    }

    template<typename T, int n>
    void transfer(int repeat, int sizeT, const CArray<T,n>& dataIn, map<int, CArray<T,1>>& dataOut, T missingValue)
    {
      // for future, make a specific transfer function for sizeT=1 to avoid multiplication (increasing performance)
      size_t srcSlice = sizeT*srcSize_ ;
      for(auto& rankConnector : connector_)
      {
        int rank = rankConnector.first ;
        auto& connector = rankConnector.second ;
        auto& mask = mask_[rank] ;
        int dstSize = mask.size() ;
        auto& data = dataOut[rank] ;
        size_t dstSlice = dstSize*sizeT ;
        data.resize(repeat * dstSlice) ;
        T* dstData = data.dataFirst() ;
        const T* srcData = dataIn.dataFirst() ;
        for(int l=0; l<repeat; l++)
        {
          for(int i=0, j=0; i<dstSize; i++)
            if (mask[i]) 
            {
              for(int k=0;k<sizeT;k++) dstData[i*sizeT+k] = srcData[connector[j]*sizeT+k] ;
              j++ ;
            }
            else 
            {
              for(int k=0;k<sizeT;k++) dstData[i*sizeT+k] = missingValue ;
              j++ ;
            }
          dstData+=dstSlice ;
          srcData+=srcSlice ;
        }
      }
    }
    
    template<typename T,int n>
    void transfer(const CArray<T,n>& dataIn, CContextClient* client, CEventClient& event, const CMessage& messageHeader)
    {
      transfer( 1, dataIn, client, event, messageHeader) ;
    }

    template<typename T,int n>
    void transfer(const CArray<T,n>& dataIn, T missingValue, CContextClient* client, CEventClient& event, const CMessage& messageHeader)
    {
      transfer( 1, dataIn, missingValue, client, event, messageHeader) ;
    }

    template<typename T, int n>
    void transfer(int sizeT, const CArray<T,n>& dataIn, CContextClient* client, CEventClient& event, const CMessage& messageHeader)
    {
      map<int, CArray<T,1>> dataOut ;
      transfer(1, sizeT, dataIn, dataOut) ;
      sendToServer(dataOut, client, event, messageHeader) ;
    }

    template<typename T, int n>
    void transfer(int sizeT, const CArray<T,n>& dataIn, T missingValue, CContextClient* client, CEventClient& event, const CMessage& messageHeader)
    {
      map<int, CArray<T,1>> dataOut ;
      transfer(1, sizeT, dataIn, dataOut, missingValue) ;
      sendToServer(dataOut, client, event, messageHeader) ;
    }

    template<typename T> 
    void transfer(int rank, CScattererConnector** connectors, int nConnectors, const T* input, T* output)
    {
      auto& connector = connector_[rank] ; // probably costly, find a better way to avoid the map
      auto& mask = mask_[rank] ; 
      int dstSize = mask.size() ;
      if (nConnectors==0)
      {
        for(int i=0, j=0; i<dstSize; i++)
          if (mask[i]) 
          {
            *(output+i)=*(input+connector[j]) ;
            j++ ;
          }

      }
      else
      {
        int srcSliceSize = (*(connectors-1))->getSrcSliceSize(connectors-1, nConnectors-1) ;
        int dstSliceSize = (*(connectors-1))->getDstSliceSize(rank, connectors-1, nConnectors-1) ;

        T* out = output ; 
        for(int i=0,j=0;i<dstSize;i++) 
        {
          if (mask[i]) 
          {
            (*(connectors-1))->transfer(rank, connectors-1, nConnectors-1, input+connector[j]*srcSliceSize, out) ; // the multiplication must be avoid in further optimization
            j++ ;
          }
          out += dstSliceSize ;
        }
      }
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

    int getSrcSliceSize(CScattererConnector** connectors, int nConnectors) 
    { if (nConnectors==0) return srcSize_ ; else return srcSize_ * (*(connectors-1))->getSrcSliceSize(connectors-1,nConnectors-1) ; }

    int getDstSliceSize(int rank, CScattererConnector** connectors, int nConnectors) 
    { if (nConnectors==0) return dstSize_[rank] ; else return dstSize_[rank] * (*(connectors-1))->getDstSliceSize(rank, connectors-1,nConnectors-1) ; }

    const map<int,int>& getNbSenders(void) {return nbSenders_ ;} 
    const map<int,int>& getDstSize(void) { return dstSize_ ;}
  } ;
} 

#endif