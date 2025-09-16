#include "algorithm_transformation_transfer.hpp"
#include "array_new.hpp"
#include "local_view.hpp"
#include "transform_connector.hpp"
#include "context.hpp"

namespace xios
{
 
  void CAlgorithmTransformationTransfer::computeAlgorithm(shared_ptr<CLocalView> srcView, shared_ptr<CLocalView> dstView)
  {
   this->computeRecvElement(srcView, dstView) ;
   transferTransformConnector_ = make_shared<CTransferTransformConnector>( recvElement_->getView(CElementView::FULL), dstView, transformationMapping_) ; 
  }
 

  void CAlgorithmTransformationTransfer::apply(int dimBefore, int dimAfter, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
  {
    transferTransformConnector_ -> transfer(dimBefore, dimAfter, dataIn, dataOut) ; 
  }
  
  void CAlgorithmTransformationTransfer::computeRecvElement(shared_ptr<CLocalView> srcView, shared_ptr<CLocalView> dstView)
  {
    set<size_t> srcIndex ;
    for(auto& it : transformationMapping_) srcIndex.insert(it.second) ;

    CArray<size_t,1> srcArrayIndex(srcIndex.size()) ;
    int i=0 ;
    for(size_t index : srcIndex) { srcArrayIndex(i) = index ; i++ ;}
    recvElement_ = make_shared<CLocalElement>(getContext()->getIntraCommRank(), srcView->getGlobalSize(), srcArrayIndex) ;
    recvElement_->addFullView() ;
  }

}