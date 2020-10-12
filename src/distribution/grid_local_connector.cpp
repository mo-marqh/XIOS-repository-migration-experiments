#include "grid_local_connector.hpp"
#include "grid_elements.hpp"
#include "element.hpp" 
#include "grid_local_view.hpp"
#include "local_view.hpp"

namespace xios
{
 
  CGridLocalConnector::CGridLocalConnector(const std::vector<CLocalConnector*>& elementsConnector) : elementsConnector_(elementsConnector)
  {
    srcSize_=1 ;
    for(auto connector : elementsConnector_) srcSize_*=connector->getSrcSize() ;
    dstSize_=1 ;
    for(auto connector : elementsConnector_) dstSize_*=connector->getDstSize() ;
  } 

  CGridLocalConnector::CGridLocalConnector(CGridLocalElements* parent, CElementView::type srcType, CElementView::type dstType, bool withMask)
  {
    CGridLocalView* srcView=parent->getView(srcType) ;
    CGridLocalView* dstView=parent->getView(dstType) ;

    vector<CLocalView*> srcViews = srcView->getViews() ; 
    vector<CLocalView*> dstViews = dstView->getViews() ;
    
    vector<CLocalElement*>& elements = parent->getElements();
    for(auto element : elements) elementsConnector_.push_back(element->getConnector(srcType, dstType)) ;
    srcSize_=1 ;
    for(auto connector : elementsConnector_) srcSize_*=connector->getSrcSize() ;
    dstSize_=1 ;
    for(auto connector : elementsConnector_) dstSize_*=connector->getDstSize() ;

    if (parent->hasLocalMask() && withMask)
    {
      vector<CLocalConnector*> elementsConnector ;
      for(auto element : elements) elementsConnector.push_back(element->getConnector(CElementView::FULL, dstType)) ;
      CGridLocalConnector localToDst(elementsConnector) ;
      CArray<bool,1> maskIn(localToDst.getSrcSize()) ;
      CArray<bool,1> maskOut1(localToDst.getDstSize()) ;
      CArray<bool,1> maskOut2(localToDst.getDstSize()) ;
      maskIn=true ;
      localToDst.transfer(maskIn,maskOut1,false) ;
      auto& localMask = parent->getLocalMask() ;
      for(int i=0 ; i < maskIn.numElements() ; i++) maskIn(i)=localMask[i] ;
      localToDst.transfer(maskIn,maskOut2,false) ;
      mask_.assign(dstSize_,true) ;
      for(int i=0;i<dstSize_;i++) if (maskOut1(i)==true && maskOut2(i)==false) mask_[i]=false ;
    } 
  }

  void CGridLocalConnector::computeMask(void)
  {


  }

}