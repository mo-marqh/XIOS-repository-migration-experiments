#include "element_view.hpp"
#include "element.hpp"
#include "distributed_view.hpp"

namespace xios
{

  CDistributedView::CDistributedView(CDistributedElement* parent, CElementView::type, const std::map<int,CArray<int,1>>& indexView) 
                                    : globalIndex_(parent->globalIndex_), globalSize_(parent->globalSize_), localSize_(parent->localSize_) 
  {
    for(auto index : indexView) 
    {
      index_[index.first].reference(index.second.copy()) ;
      size_[index.first] = index.second.numElements() ;
    }
  }


} 