#include "grid_elements.hpp"
#include "grid_local_view.hpp"
#include "grid_local_connector.hpp"

namespace xios
{

  CGridLocalView* CGridLocalElements::getView(CElementView::type type)
  { 
    if (views_[type]==nullptr) views_[type] = new CGridLocalView(this, type) ;
    return views_[type] ;
  }
  
  CGridLocalConnector* CGridLocalElements::getConnector(CElementView::type srcType, CElementView::type dstType, bool withMask)
  {
    auto newPair = pair<CElementView::type,CElementView::type>(srcType,dstType);
    auto it = connectors_.find(newPair) ;
    if (it==connectors_.end()) 
    {
      auto insertPair=pair<pair<CElementView::type,CElementView::type>, CGridLocalConnector*>(newPair,new CGridLocalConnector(this, srcType, dstType, withMask)) ;
      it=connectors_.insert(insertPair).first ;
    }
    return it->second ;
  }
}