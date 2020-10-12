#ifndef __GRID_LOCAL_CONNECTOR_HPP__
#define __GRID_LOCAL_CONNECTOR_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "local_connector.hpp"



namespace xios
{

  class CGridLocalElements;

  class CGridLocalConnector
  {

    private:  
      std::vector<CLocalConnector*> elementsConnector_ ;
      int srcSize_ ;
      int dstSize_ ;
      vector<bool> mask_ ;

    public:
      
      CGridLocalConnector(const std::vector<CLocalConnector*>& elementsConnector)  ;
      CGridLocalConnector(CGridLocalElements* parent, CElementView::type srcType, CElementView::type dstType, bool withMask=false) ;
      int getSrcSize(void) { return srcSize_ ;}
      int getDstSize(void) { return dstSize_ ;}

      void computeMask(void) ;
      bool computeMask_done_=false ;

      
      template<typename T, int Nin, int Nout> 
      void transfer(const CArray<T,Nin>& input, CArray<T,Nout>& output)
      {
        int n = elementsConnector_.size()-1 ;
        CLocalConnector** connector = elementsConnector_.data() + n ;
        elementsConnector_[n]->transfer(connector, n, input.dataFirst(), output.dataFirst()) ;
      }
      
      template<typename T, int Nin, int Nout> 
      void transfer(const CArray<T,Nin>& input, CArray<T,Nout>& output, T missingValue)
      {
        int n = elementsConnector_.size()-1 ;
        CLocalConnector** connector = elementsConnector_.data() + n ;
        elementsConnector_[n]->transfer(connector, n, input.dataFirst(), output.dataFirst(), missingValue) ;
        if (!computeMask_done_) computeMask() ;
        if (!mask_.empty()) 
        {
          T* out = output.dataFirst() ;
          for(auto mask : mask_) { if (!mask) *out=missingValue ; out++;}
        }
      }

  } ;

}

#endif