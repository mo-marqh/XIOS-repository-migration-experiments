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
      void transfer(int repeat, int sizeT, map<int, CArray<T,1>>& dataIn, CArray<T,1>& dataOut)
      {
        // for future, make a specific transfer function for sizeT=1 to avoid multiplication (increasing performance)
        size_t dstSlice = dstSize_*sizeT ;
        dataOut.resize(repeat* dstSlice) ;  
        
        for(auto& data : dataIn)
        {
          T* output = dataOut.dataFirst() ;
          int rank=data.first ;
          auto input  = data.second.dataFirst() ;
          auto& connector=connector_[rank] ;
          auto& mask=mask_[rank] ;
          int size=mask.size() ;
          size_t srcSlice = size * sizeT ;
          for(int l=0; l<repeat; l++)
          {   
            for(int i=0, j=0 ;i<size;i++)
            {
              if (mask[i]) 
              {
                int cj = connector[j]*sizeT ;
                int ci = i*sizeT ;
                for (int k=0;k<sizeT;k++) output[cj+k] = input[ci+k] ;
                j++ ;
              }
            }
            input+=srcSlice ;
            output+=dstSlice ;
          }
        }
      }

      template<typename T>
      void transfer(int sizeT, map<int, CArray<T,1>>& dataIn, CArray<T,1>& dataOut)
      {
        transfer(1, sizeT, dataIn, dataOut) ;
      }
    
      template<typename T>
      void transfer(map<int, CArray<T,1>>& dataIn, CArray<T,1>& dataOut)
      {
        transfer(1,dataIn,dataOut) ;
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

      // hook for transfering mask in grid connector, maybe find an other way to doing that...
      void transfer_or(int rank,  CGathererConnector** connectors, int nConnectors, const bool* input, bool* output)
      {
        auto& connector = connector_[rank] ; // probably costly, find a better way to avoid the map
        auto& mask = mask_[rank] ; 
        int srcSize = mask.size() ;
      
        if (nConnectors==0)
        {
          for(int i=0, j=0; i<srcSize; i++)
            if (mask[i]) 
            {
              *(output+connector[j]) |= *(input + i) ;
              j++ ;
            }

        }
        else
       {
          int srcSliceSize = (*(connectors-1))->getSrcSliceSize(rank, connectors-1, nConnectors-1) ;
          int dstSliceSize = (*(connectors-1))->getDstSliceSize(connectors-1, nConnectors-1) ;

          const bool* in = input ; 
          for(int i=0,j=0;i<srcSize;i++) 
          {
            if (mask[i]) 
            {
              (*(connectors-1))->transfer_or(rank, connectors-1, nConnectors-1, in, output+connector[j]*dstSliceSize) ; // the multiplication must be avoid in further optimization
              j++ ;
            }
            in += srcSliceSize ;
          }
        }

      }



      template<typename T>
      void transfer(map<int, CArray<T,1>>& dataIn, CArray<T,1>& dataOut, T missingValue)
      {
        transfer(1, 1, dataIn, dataOut, missingValue);
      }
      
      template<typename T>
      void transfer(int sizeT, map<int, CArray<T,1>>& dataIn, CArray<T,1>& dataOut, T missingValue)
      {
         transfer(1, sizeT, dataIn, dataOut, missingValue) ;
      }

      template<typename T>
      void transfer(int repeat , int sizeT, map<int, CArray<T,1>>& dataIn, CArray<T,1>& dataOut, T missingValue)
      {
        dataOut.resize(repeat*dstSize_*sizeT) ;
        dataOut=missingValue ;
        transfer(repeat, sizeT, dataIn, dataOut) ;
      }

      template<typename T>
      void transfer(CEventServer& event, int sizeT, CArray<T,1>& dataOut)
      {
        map<int, CArray<T,1>> dataIn ;
        for (auto& subEvent : event.subEvents) 
        {
          auto& data = dataIn[subEvent.rank]; 
          (*subEvent.buffer) >> data ;
        }
        transfer(1, sizeT, dataIn, dataOut) ;
      }
      
      template<typename T>
      void transfer(CEventServer& event, CArray<T,1>& dataOut)
      {
        transfer(event, 1, dataOut) ;
      }

      template<typename T>
      void transfer(CEventServer& event, int sizeT, CArray<T,1>& dataOut, T missingValue)
      {
        map<int, CArray<T,1>> dataIn ;
        for (auto& subEvent : event.subEvents) 
        {
          auto& data = dataIn[subEvent.rank]; 
          (*subEvent.buffer) >> data ;
        }
        transfer(1, sizeT, dataIn, dataOut, missingValue) ;
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
        transfer(1, 1, dataIn, dataOut, missingValue) ;
      }

    int getSrcSliceSize(int rank, CGathererConnector** connectors, int nConnectors) 
    { if (nConnectors==0) return srcSize_[rank] ; else return srcSize_[rank] * (*(connectors-1))->getSrcSliceSize(rank, connectors-1,nConnectors-1) ; }

    int getDstSliceSize(CGathererConnector** connectors, int nConnectors) 
    { if (nConnectors==0) return dstSize_ ; else return dstSize_ * (*(connectors-1))->getDstSliceSize(connectors-1,nConnectors-1) ; }
  
    int getDstSize(void) {return dstSize_ ;}
  } ;

}

#endif
