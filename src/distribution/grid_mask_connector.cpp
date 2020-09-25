#include "grid_mask_connector.hpp"

namespace xios
{
  void CGridMaskConnector::computeConnector(CArray<bool,1>& mask)
  {
    nViews_ = views_.size() ;
    for(auto& view : views_) 
    {
      size_.push_back(view->getLocalSize()) ;
      elementsMask_.push_back(CArray<bool,1>(view->getLocalSize())) ;
      elementsMask_.back() = false ;
    }
    index_.resize(nViews_,0) ;
    bool* maskPtr = mask.dataFirst() ; 
    recursiveInternal(nViews_-1, maskPtr) ;
  }
  
}