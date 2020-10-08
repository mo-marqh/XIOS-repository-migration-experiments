#ifndef __GRID_MASK_CONNECTOR_HPP__
#define __GRID_MASK_CONNECTOR_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "local_view.hpp"
#include "distributed_view.hpp"

namespace xios
{
 
  class CGridMaskConnector
  {

    public:

      CGridMaskConnector(vector<CLocalView*>& views) : views_(views) {}
      void computeConnector(CArray<bool,1>& mask) ;
     
      CArray<bool,1>& getElementMask(int pos) { return elementsMask_[pos] ;} 
      vector<CArray<bool,1>>& getElementsMask(void) ;
    
    private:
      vector<CLocalView*> views_ ;
      vector<CArray<bool,1>> elementsMask_ ; 

      vector<int> size_ ;
      vector<int> index_ ;
      int nViews_ ;
    
    private:
  
    void recursiveInternal(int level, bool*& mask)
    {
      if (level==0)
      {
        for(int i=0; i < size_[level] ; i++)
        {
          for(int j=0 ; j<nViews_ ; j++) elementsMask_[j](index_[j]) |= *mask ;
          index_[level]++ ;
          mask++ ;
        }
        index_[level] = 0 ;
      }
      else
      {  
        for(int i=0; i<size_[level] ; i++) 
        {
          recursiveInternal(level-1, mask) ;
          index_[level]++ ;
        }
        index_[level] = 0 ;
      }
    }

  } ;

}

#endif