#include "element_view.hpp"
#include "element.hpp"
#include "distributed_view.hpp"

namespace xios
{

  CDistributedView::CDistributedView(shared_ptr<CDistributedElement> parent, CElementView::type, const std::map<int,CArray<int,1>>& indexView) 
                                    : globalIndex_(parent->globalIndex_), globalSize_(parent->globalSize_), localSize_(parent->localSize_) 
  {
    for(auto& index : indexView) 
    {
      index_[index.first].reference(index.second.copy()) ;
      size_[index.first] = index.second.numElements() ;
    }
  }

  CDistributedView::CDistributedView(shared_ptr<CDistributedElement> parent, CElementView::type, const std::map<int,CArray<bool,1>>& maskView) 
                                    : globalIndex_(parent->globalIndex_), globalSize_(parent->globalSize_), localSize_(parent->localSize_) 
  {
    for(auto& it : maskView) 
    {
      int rank = it.first ;
      auto& mask = it.second ;
      int size = mask.numElements() ;
      auto& index = index_[rank] ; 
      index.resize(size) ;
      int pos=0 ;
      for(int i=0 ; i < size ; i++)
        if (mask(i)) { index(pos) = i ; pos++ ; }
      if (pos==0) index.resize(pos) ;
      else index.resizeAndPreserve(pos) ;
      size_[rank] = pos ;
    }
  } 
}