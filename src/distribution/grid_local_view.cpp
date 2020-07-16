#include "grid_local_view.hpp"
#include "grid_elements.hpp"

namespace xios
{
  CGridLocalView::CGridLocalView(CGridLocalElements* parent, CElementView::type type) : localMask_(parent->getLocalMask())
  {
    for(auto element : parent->getElements()) views_.push_back(element->getView(type)) ;
  }

}