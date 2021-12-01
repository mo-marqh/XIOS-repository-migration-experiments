#ifndef __GRID_ELEMENTS_HPP__
#define __GRID_ELEMENTS_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "element.hpp"

namespace xios
{
  class CGridLocalView ;
  class CGridLocalConnector ;

  class CGridLocalElements : public std::enable_shared_from_this<CGridLocalElements>
  {
    private:
      std::vector<shared_ptr<CLocalElement>> elements_ ;
      std::vector<shared_ptr<CGridLocalView>> views_= std::vector<shared_ptr<CGridLocalView>>(CElementView::numViewType_) ;
      std::map<pair<CElementView::type,CElementView::type>, shared_ptr<CGridLocalConnector>> connectors_ ;
      vector<bool> localMask_ ;
  
    public:
      CGridLocalElements(vector<shared_ptr<CLocalElement> > elements) : elements_(elements) {}
      CGridLocalElements(vector<shared_ptr<CLocalElement>> elements, vector<bool>& localMask) : elements_(elements), localMask_(localMask) {}

      bool hasLocalMask() { return !localMask_.empty() ;}
      vector<bool>& getLocalMask(void) { return localMask_ ;}

      std::vector<shared_ptr<CLocalElement>>& getElements(void) { return elements_ ; }
      shared_ptr<CGridLocalView> getView(CElementView::type type) ;
      shared_ptr<CGridLocalConnector> getConnector(CElementView::type srcType, CElementView::type dstType, bool withMask=false) ;
  } ;
}

#endif