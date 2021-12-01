#include "algorithm_transformation_reduce.hpp"
#include "context.hpp"

namespace xios
{


  void CAlgorithmTransformationReduce::computeAlgorithm(shared_ptr<CLocalView> srcView, shared_ptr<CLocalView> dstView)
 {
    this->computeRecvElement(srcView, dstView) ;
    reduceTransformConnector_ = make_shared<CReduceTransformConnector>(recvElement_->getView(CElementView::FULL), dstView, operator_, transformationMapping_, detectMissingValue_) ; 
  }
 

  void CAlgorithmTransformationReduce::apply(int dimBefore, int dimAfter, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
  {
    reduceTransformConnector_ -> transfer(dimBefore, dimAfter, dataIn, dataOut) ;
  }
  
  void CAlgorithmTransformationReduce::computeRecvElement(shared_ptr<CLocalView> srcView, shared_ptr<CLocalView> dstView)
  {
    auto& srcMap = transformationMapping_ ;
    set<size_t> srcIndex ;
    for(auto& it : srcMap)
      for(size_t index : it.second) srcIndex.insert(index) ;

    CArray<size_t,1> srcArrayIndex(srcIndex.size()) ;
    int i=0 ;
    for(size_t index : srcIndex) { srcArrayIndex(i) = index ; i++ ;}
    recvElement_ = make_shared<CLocalElement>(CContext::getCurrent()->getIntraCommRank(), srcView->getGlobalSize(), srcArrayIndex) ;
    recvElement_->addFullView() ;
  }
}