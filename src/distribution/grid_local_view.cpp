#include "grid_local_view.hpp"
#include "grid_elements.hpp"

namespace xios
{
  CGridLocalView::CGridLocalView(shared_ptr<CGridLocalElements> parent, CElementView::type type) : localMask_(parent->getLocalMask())
  {
    size_ = 1 ;
    for(auto element : parent->getElements()) 
    {
      views_.push_back(element->getView(type)) ;
      size_ *= element->getView(type)->getSize() ;
    }

  }

}