#include "algorithm_transformation_transfer.hpp"
#include "array_new.hpp"
#include "local_view.hpp"
#include "transform_connector.hpp"
#include "context.hpp"

namespace xios
{
 
 void CAlgorithmTransformationTransfer::computeAlgorithm(CLocalView* srcView, CLocalView* dstView)
 {
   set<size_t> srcIndex ;
   for(auto& it : transformationMapping_) srcIndex.insert(it.second) ;

    CArray<size_t,1> srcArrayIndex(srcIndex.size()) ;
    int i=0 ;
    for(size_t index : srcIndex) { srcArrayIndex(i) = index ; i++ ;}
    CLocalElement recvElement(CContext::getCurrent()->getIntraCommRank(), srcView->getGlobalSize(), srcArrayIndex) ;
    recvElement.addFullView() ;

    transformConnector_ = new CTransformConnector(srcView, recvElement.getView(CElementView::FULL), CContext::getCurrent()->getIntraComm())  ;
    transformConnector_->computeConnector() ;
    transferTransformConnector_ = new  CTransferTransformConnector( recvElement.getView(CElementView::FULL), dstView, transformationMapping_) ; 
  }
 

  void CAlgorithmTransformationTransfer::apply(int dimBefore, int dimAfter, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
  {
    CArray<double,1> dataOutTmp ;
    transformConnector_->transfer(dimBefore, dimAfter, dataIn, dataOutTmp) ;  
    transferTransformConnector_ -> transfer(dimBefore, dimAfter, dataOutTmp, dataOut) ; 
  }
  
  void CAlgorithmTransformationTransfer::computeRecvElement(CLocalView* srcView, CLocalView* dstView)
  {
    set<size_t> srcIndex ;
    for(auto& it : transformationMapping_) srcIndex.insert(it.second) ;

    CArray<size_t,1> srcArrayIndex(srcIndex.size()) ;
    int i=0 ;
    for(size_t index : srcIndex) { srcArrayIndex(i) = index ; i++ ;}
    recvElement_ = new CLocalElement(CContext::getCurrent()->getIntraCommRank(), srcView->getGlobalSize(), srcArrayIndex) ;
    recvElement_->addFullView() ;
  }

}