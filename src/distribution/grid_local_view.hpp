#ifndef __GRID_LOCAL_VIEW_HPP__
#define __GRID_LOCAL_VIEW_HPP__

#include "local_view.hpp"
#include "element.hpp"

namespace xios
{
  class CGridLocalElements;

  class CGridLocalView
  {
    private:
      std::vector<CLocalView*> views_ ;
      std::vector<bool>& localMask_ ;
    
    public:
      CGridLocalView(CGridLocalElements* parent, CElementView::type type) ;
      std::vector<CLocalView*>& getViews(void) {return views_ ;}
  } ;
}


#endif