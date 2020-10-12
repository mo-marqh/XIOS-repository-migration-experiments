#ifndef __GRID_ELEMENTS_HPP__
#define __GRID_ELEMENTS_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "element.hpp"

namespace xios
{
  class CGridLocalView ;
  class CGridLocalConnector ;

  class CGridLocalElements  
  {
    private:
      std::vector<CLocalElement*> elements_ ;
      std::vector<CGridLocalView*> views_= std::vector<CGridLocalView*>(CElementView::numViewType_) ;
      std::map<pair<CElementView::type,CElementView::type>, CGridLocalConnector*> connectors_ ;
      vector<bool> localMask_ ;
  
    public:
      CGridLocalElements(vector<CLocalElement*> elements) : elements_(elements) {}
      CGridLocalElements(vector<CLocalElement*> elements, vector<bool>& localMask) : elements_(elements), localMask_(localMask) {}

      bool hasLocalMask() { return !localMask_.empty() ;}
      vector<bool>& getLocalMask(void) { return localMask_ ;}

      std::vector<CLocalElement*>& getElements(void) { return elements_ ; }
      CGridLocalView* getView(CElementView::type type) ;
      CGridLocalConnector* getConnector(CElementView::type srcType, CElementView::type dstType, bool withMask=false) ;
  } ;
}

#endif