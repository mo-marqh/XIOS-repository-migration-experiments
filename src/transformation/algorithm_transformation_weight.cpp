#include "algorithm_transformation_weight.hpp"
#include "context.hpp"

namespace xios
{


  void CAlgorithmTransformationWeight::computeAlgorithm(CLocalView* srcView, CLocalView* dstView)
 {
    this->computeRecvElement(srcView, dstView) ;
    weightTransformConnector_ = new  CWeightTransformConnector( recvElement_->getView(CElementView::FULL), dstView, transformationMapping_, transformationWeight_) ; 
  }
 

  void CAlgorithmTransformationWeight::apply(int dimBefore, int dimAfter, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
  {
    CArray<double,1> dataOutTmp ;
    transformConnector_->transfer(dimBefore, dimAfter, dataIn, dataOutTmp) ;
    weightTransformConnector_ -> transfer(dimBefore, dimAfter, dataOutTmp, dataOut) ;
  }

  void CAlgorithmTransformationWeight::computeRecvElement(CLocalView* srcView, CLocalView* dstView)
  {
    auto& srcMap = transformationMapping_ ;
    set<size_t> srcIndex ;
    for(auto& it : srcMap)
      for(size_t index : it.second) srcIndex.insert(index) ;

    CArray<size_t,1> srcArrayIndex(srcIndex.size()) ;
    int i=0 ;
    for(size_t index : srcIndex) { srcArrayIndex(i) = index ; i++ ;}
    recvElement_ = new CLocalElement(CContext::getCurrent()->getIntraCommRank(), srcView->getGlobalSize(), srcArrayIndex) ;
    recvElement_->addFullView() ;
  }
}