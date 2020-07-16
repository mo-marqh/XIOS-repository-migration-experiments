#ifndef __LOCAL_CONNECTOR_HPP__
#define __LOCAL_CONNECTOR_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "local_view.hpp"



namespace xios
{
  
  class CLocalConnector
  {
    
    private:
      CLocalView* srcView_;
      CLocalView* dstView_;
      int srcSize_ ;
      int dstSize_ ;
      vector<int> connector_ ;
      vector<bool> mask_ ;

    public:
    
      CLocalConnector(CLocalView* srcView, CLocalView* dstView) : srcView_(srcView), dstView_(dstView), 
                                                                  srcSize_(srcView->getSize()), dstSize_(dstView->getSize()) {}
      void computeConnector(void);
      int getSrcSize(void) {return srcSize_ ;}
      int getDstSize(void) {return dstSize_ ; }

      template<typename T> void transfer(const CArray<T,1>& input, CArray<T,1>& output)
      {
        int size=mask_.size() ;
        output.resize(size) ;
        for(int i=0,j=0;i<size;i++) 
        {
          if (mask_[i]) 
          {
            output(i)=input(connector_[j]) ;
            j++ ;
          }
        }
      }

      template<typename T> void transfer(const CArray<T,1>& input, CArray<T,1>& output, T missingValue)
      {
        int size=mask_.size() ;
        output.resize(size) ;
        for(int i=0,j=0;i<size;i++) 
        {
          if (mask_[i]) 
          {
            output(i)=input(connector_[j]) ;
            j++ ;
          }
          else output(i)=missingValue ;
        }
      }

      template<typename T> void transfer(CLocalConnector** connectors, int nConnectors, const T* input, T* output)
      {

        int size=mask_.size() ;
        if (nConnectors==0)
        {
          for(int i=0,j=0;i<size;i++) 
            if (mask_[i]) 
            {
              *(output+i)=*(input+connector_[j]) ;
              j++ ;
            }
        }
        else
        {
          int srcSliceSize = (*(connectors-1))->getSrcSliceSize(connectors-1, nConnectors-1) ;
          int dstSliceSize = (*(connectors-1))->getDstSliceSize(connectors-1, nConnectors-1) ;

          T* out = output ; 
          for(int i=0,j=0;i<size;i++) 
          {
            if (mask_[i]) 
            {
              (*(connectors-1))->transfer(connectors-1, nConnectors-1, input+connector_[j]*srcSliceSize, out) ; // the multiplication must be avoid in further optimization
              j++ ;
            }
            out += dstSliceSize ;
          }

        }
      }

      template<typename T> void transfer(CLocalConnector** connectors, int nConnectors, const T* input, T* output, T missingValue)
      {
        int size=mask_.size() ;
        if (nConnectors==0)
        {
          for(int i=0,j=0;i<size;i++) 
            if (mask_[i]) 
            {
              *(output+i)=*(input+connector_[j]) ;
              j++ ;
            }
            else *(output+i)=missingValue ;
        }
        else
        {
          int srcSliceSize = (*(connectors-1))->getSrcSliceSize(connectors-1, nConnectors-1) ;
          int dstSliceSize = (*(connectors-1))->getDstSliceSize(connectors-1, nConnectors-1) ;

          T* out = output ; 
          for(int i=0,j=0;i<size;i++) 
          {
            if (mask_[i]) 
            {
              (*(connectors-1))->transfer(connectors-1, nConnectors-1, input+connector_[j]*srcSliceSize, out, missingValue) ; // the multiplication must be avoid in further optimization
              j++ ;
            }
            else for (int j=0 ; j<dstSliceSize ; j++) *(out+j) = missingValue ;
            out += dstSliceSize ;
          }
        }
      }

      int getSrcSliceSize(CLocalConnector** connectors, int nConnectors) 
      { if (nConnectors==0) return srcSize_ ; else return srcSize_ * (*(connectors-1))->getSrcSliceSize(connectors-1,nConnectors-1) ; }

      int getDstSliceSize(CLocalConnector** connectors, int nConnectors) 
      { if (nConnectors==0) return dstSize_ ; else return dstSize_ * (*(connectors-1))->getDstSliceSize(connectors-1,nConnectors-1) ; }

  };

}

#endif