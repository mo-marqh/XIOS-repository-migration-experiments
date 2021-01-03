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
      int size_ ;
    
    public:
      CGridLocalView(CGridLocalElements* parent, CElementView::type type) ;
      std::vector<CLocalView*>& getViews(void) {return views_ ;}
      CLocalView* getView(int i) {return views_[i] ;}
      int getSize() { return size_ ;}
  } ;
}


#endif