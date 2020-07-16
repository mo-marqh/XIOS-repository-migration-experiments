#ifndef __GATHERER_CONNECTOR_HPP__
#define __GATHERER_CONNECTOR_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "distributed_view.hpp"
#include "mpi.hpp"
#include "local_view.hpp"
#include "distributed_view.hpp"
#include "context_client.hpp"


namespace xios
{
 
  class CGathererConnector
  {
    private:
      CDistributedView* srcView_;
      CLocalView* dstView_;
      map<int, vector<int>> connector_ ;
      map<int, vector<bool>> mask_ ;  // mask is on src view
      int dstSize_ ; 
      map<int,int> srcSize_ ;

    public:
      CGathererConnector(CDistributedView* srcView, CLocalView* dstView) : srcView_(srcView), dstView_(dstView) {} ;
      void computeConnector(void) ;
      
      template<typename T>
      void transfer(map<int, CArray<T,1>>& dataIn, CArray<T,1>& dataOut)
      {
        dataOut.resize(dstSize_) ;
        T* output = dataOut.dataFirst() ;
        for(auto& data : dataIn)
        {
          int rank=data.first ;
          auto input  = data.second.dataFirst() ;
          auto& connector=connector_[rank] ;
          auto& mask=mask_[rank] ;
          int size=data.second.numElements() ;

          for(int i=0, j=0 ;i<size;i++)
          {
            if (mask[i]) 
            {
              output[connector[j]] = input[i] ;
              j++ ;
            }
          }
        }
      }
    

      template<typename T>
      void transfer(int rank,  CGathererConnector** connectors, int nConnectors, const T* input, T* output)
      {
        auto& connector = connector_[rank] ; // probably costly, find a better way to avoid the map
        auto& mask = mask_[rank] ; 
        int srcSize = mask.size() ;
      
        if (nConnectors==0)
        {
          for(int i=0, j=0; i<srcSize; i++)
            if (mask[i]) 
            {
              *(output+connector[j]) = *(input + i) ;
              j++ ;
            }

        }
        else
       {
          int srcSliceSize = (*(connectors-1))->getSrcSliceSize(rank, connectors-1, nConnectors-1) ;
          int dstSliceSize = (*(connectors-1))->getDstSliceSize(connectors-1, nConnectors-1) ;

          const T* in = input ; 
          for(int i=0,j=0;i<srcSize;i++) 
          {
            if (mask[i]) 
            {
              (*(connectors-1))->transfer(rank, connectors-1, nConnectors-1, in, output+connector[j]*dstSliceSize) ; // the multiplication must be avoid in further optimization
              j++ ;
            }
            in += srcSliceSize ;
          }
        }

      }


      template<typename T>
      void transfer(map<int, CArray<T,1>>& dataIn, CArray<T,1>& dataOut, T missingValue)
      {
        dataOut.resize(dstSize_) ;
        dataOut=missingValue ;
        transfer(map<int, CArray<T,1>>& dataIn, CArray<T,1>& dataOut) ;
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

    int getSrcSliceSize(int rank, CGathererConnector** connectors, int nConnectors) 
    { if (nConnectors==0) return srcSize_[rank] ; else return srcSize_[rank] * (*(connectors-1))->getSrcSliceSize(rank, connectors-1,nConnectors-1) ; }

    int getDstSliceSize(CGathererConnector** connectors, int nConnectors) 
    { if (nConnectors==0) return dstSize_ ; else return dstSize_ * (*(connectors-1))->getDstSliceSize(connectors-1,nConnectors-1) ; }
  
    int getDstSize(void) {return dstSize_ ;}
  } ;

}

#endif