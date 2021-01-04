#include "reduce_transform_connector.hpp"
#include "exception.hpp"

namespace xios
{

  CReduceTransformConnector::CReduceTransformConnector(CLocalView* srcView, CLocalView* dstView, EReduction op, 
                                                       unordered_map<int, std::vector<int>>& indexMap, bool detectMissingValue)
   : srcView_(srcView), dstView_(dstView), detectMissingValue_(detectMissingValue)
  {
    switch(op)
    {
      case EReduction::sum :
        transfer_=&CReduceTransformConnector::transferSum ;
        break ;
      case EReduction::min :
        transfer_=&CReduceTransformConnector::transferMin ;
        break ;
      case EReduction::max :
        transfer_=&CReduceTransformConnector::transferMax ;
        break ;
     case EReduction::average :
        transfer_=&CReduceTransformConnector::transferAverage ;
        break ;
      default :
       ERROR("CReduceTransformConnector::CReduceTransformConnector",
             <<"reduction operator "<<(int)op<<" is not defined for this operation") ;
       break ;
    }

    computeConnector(indexMap) ;
  }

  void CReduceTransformConnector::computeConnector(unordered_map<int, std::vector<int>>& indexMap)
  {
    CArray<size_t,1> dstGlobalIndex ;
    CArray<size_t,1> srcGlobalIndex ;
    dstView_->getGlobalIndexView(dstGlobalIndex) ;
    srcView_->getGlobalIndexView(srcGlobalIndex) ;
    unordered_map<size_t,int> srcMapIndex ;
    srcSize_ = srcGlobalIndex.numElements() ;
    dstSize_ = dstGlobalIndex.numElements() ;

    for(int i=0;i<srcSize_;i++) srcMapIndex[srcGlobalIndex(i)]=i ;
    for(int i=0; i< dstSize_;i++) 
    {
      if (indexMap.count(dstGlobalIndex(i))!=0)
      {
        auto& vectIndex  = indexMap[dstGlobalIndex(i)] ;
        nSrc_.push_back(vectIndex.size()) ;
        for(int j=0; j<vectIndex.size();j++) connector_.push_back(srcMapIndex[vectIndex[j]]) ;
      }
      else nSrc_.push_back(0) ;
    }
  }

}