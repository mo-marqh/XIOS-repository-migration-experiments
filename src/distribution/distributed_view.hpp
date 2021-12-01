#ifndef __DISTRIBUTED_VIEW_HPP__
#define __DISTRIBUTED_VIEW_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "element_view.hpp"

namespace xios
{
  class CDistributedElement;

  class CDistributedView : public CElementView
  {
    public:

      CDistributedView(shared_ptr<CDistributedElement> parent, CElementView::type type, const std::map<int,CArray<int,1>>& indexView) ;
      CDistributedView(shared_ptr<CDistributedElement> parent, CElementView::type type, const std::map<int,CArray<bool,1>>& maskView) ;
      
      std::map<int,CArray<int,1>>&  getIndex(void) { return index_ ;}
      std::map<int,CArray<size_t,1>>&  getGlobalIndex(void) { return globalIndex_ ;}
      std::map<int, int>& getSize(void) { return size_ ;}
      std::map<int, int>& getLocalSize(void) { return localSize_ ;}
      size_t getGlobalSize(void) { return globalSize_ ;}

      void getGlobalIndexView(map<int,CArray<size_t,1>>& globalIndexView)
      {
        for(auto& it : globalIndex_)
        {
          int rank=it.first ;
          auto& globalIndex = it.second;
          auto& globalIndView = globalIndexView[rank] ;
          auto& index = index_[rank] ;
          auto& size = size_[rank] ;
          auto& localSize = localSize_[rank] ;
          globalIndView.resize(size) ;
          int pos=0 ;
          for(int i=0 ; i<size ; i++)
          {
            if (index(i)>=0 && index(i)<localSize) 
            {
              globalIndView(i) = globalIndex(index(i)) ;
              pos++ ;
            }
          }
          if (pos==0) globalIndView.resize(pos) ;
          else globalIndView.resizeAndPreserve(pos) ;
      }
    }   

    void getGlobalIndex(int rank, vector<size_t>& globalIndex, size_t sliceIndex, size_t* sliceSize, shared_ptr<CDistributedView>* view, int pos)
    {
      // using map can be expensive, find an otherway later
      auto& globalInd=globalIndex_[rank] ;
      int localSize=globalInd.numElements() ;
      auto& index=index_[rank] ;
      int size=index.numElements() ;
              
      if (pos==0)
      {
        for(int i=0;i<size;i++)
          if (index(i)>=0 && index(i)<localSize) globalIndex.push_back(sliceIndex + globalInd(index(i))) ;
      }
      else 
      {
        for(int i=0;i<size;i++)
          if (index(i)>=0 && index(i)<localSize) 
            view[pos-1]->getGlobalIndex(rank, globalIndex, sliceIndex + globalInd(index(i))*sliceSize[pos], sliceSize, view , pos-1) ;
      }
    }


    protected:

      std::map<int,CArray<int,1>> index_ ;
      std::map<int, int> size_;

      std::map<int,CArray<size_t,1>>& globalIndex_ ; 
      size_t& globalSize_ ;
      std::map<int, int>& localSize_ ;
    
  } ;

}

#endif