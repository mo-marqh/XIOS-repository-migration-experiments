#include "transfer_transform_connector.hpp"

namespace xios
{

  CTransferTransformConnector::CTransferTransformConnector(CLocalView* srcView, CLocalView* dstView, unordered_map<int, int>& indexMap) 
    : srcView_(srcView), dstView_(dstView)
  {
    computeConnector(indexMap) ;
  }

  void CTransferTransformConnector::computeConnector(unordered_map<int, int>& indexMap)
  {
    CArray<size_t,1> dstGlobalIndex ;
    CArray<size_t,1> srcGlobalIndex ;
    dstView_->getGlobalIndexView(dstGlobalIndex) ;
    srcView_->getGlobalIndexView(srcGlobalIndex) ;
    unordered_map<size_t,int> srcMapIndex ;
    srcSize_ = srcGlobalIndex.numElements() ;
    dstSize_ = dstGlobalIndex.numElements() ;
    mask_.resize(dstSize_) ;

    for(int i=0;i<srcSize_;i++) srcMapIndex[srcGlobalIndex(i)]=i ;
    for(int i=0; i<dstSize_;i++) 
    {
      if (indexMap.count(dstGlobalIndex(i))!=0)
      {
        int index  = indexMap[dstGlobalIndex(i)] ;
        connector_.push_back(srcMapIndex[index]) ;
        mask_[i]=true ;
      }
      else mask_[i]=false ;
    }
  }

}