#include "algorithm_transformation_weight.hpp"
#include "context.hpp"

namespace xios
{


  void CAlgorithmTransformationWeight::computeAlgorithm(shared_ptr<CLocalView> srcView, shared_ptr<CLocalView> dstView, bool detectMissingValue, bool renormalize)
  {
    this->computeRecvElement(srcView, dstView) ;
    weightTransformConnector_ = make_shared<CWeightTransformConnector>(recvElement_->getView(CElementView::FULL), dstView, transformationMapping_, transformationWeight_, detectMissingValue, renormalize) ; 
  }
 

  void CAlgorithmTransformationWeight::apply(int dimBefore, int dimAfter, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
  {
    weightTransformConnector_ -> transfer(dimBefore, dimAfter, dataIn, dataOut) ;
  }

  void CAlgorithmTransformationWeight::computeRecvElement(shared_ptr<CLocalView> srcView, shared_ptr<CLocalView> dstView)
  {
    auto& srcMap = transformationMapping_ ;
    set<size_t> srcIndex ;
    for(auto& it : srcMap)
      for(size_t index : it.second) srcIndex.insert(index) ;

    CArray<size_t,1> srcArrayIndex(srcIndex.size()) ;
    int i=0 ;
    for(size_t index : srcIndex) { srcArrayIndex(i) = index ; i++ ;}
    recvElement_ = make_shared<CLocalElement>(getContext()->getIntraCommRank(), srcView->getGlobalSize(), srcArrayIndex) ;
    recvElement_->addFullView() ;
  }
}