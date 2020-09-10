#include "gatherer_connector.hpp"

namespace xios
{
  void CGathererConnector::computeConnector(void)
  {
    dstSize_=dstView_->getSize() ;
    auto& srcGlobalIndex = srcView_->getGlobalIndex() ;
    auto& srcIndex = srcView_->getIndex() ;
    for(auto& rankIndex : srcIndex) srcSize_[rankIndex.first] = rankIndex.second.numElements();
    auto& dstGlobalIndex = dstView_->getGlobalIndex() ;
    auto& dstIndex = dstView_->getIndex() ;
    dstSize_=dstIndex.numElements() ;

    unordered_map<size_t,int> mapGlobalLocalIndex ;
    int globalIndexSize=dstGlobalIndex.size() ;
    //for(auto& ind : dstIndex) mapGlobalLocalIndex[dstGlobalIndex(ind)] = ind ;
    for(int i=0; i<dstSize_ ; i++) if (dstIndex(i)>=0 && dstIndex(i)<globalIndexSize) mapGlobalLocalIndex[dstGlobalIndex(dstIndex(i))] = i ;

    for(auto& rankIndex : srcIndex)
    {
      int rank=rankIndex.first ;
      auto& index=rankIndex.second ;
      int indexSize = index.numElements() ;
      auto& globalIndex = srcGlobalIndex[rank] ;
      int globalIndexSize=globalIndex.numElements() ;
      auto& connector = connector_[rank] ;
      auto& mask = mask_[rank] ;
      for(int ind=0; ind<indexSize ; ind++)
      {
        if (index(ind)>=0 && index(ind)<globalIndexSize)
        {
           auto it=mapGlobalLocalIndex.find(globalIndex(index(ind))) ;
           if (it != mapGlobalLocalIndex.end()) 
           {
             connector.push_back(it->second) ;
             mask.push_back(true) ;
           }
           else mask.push_back(false) ;
        }
        else mask.push_back(false) ;
      } 
    } 
  }

  
}