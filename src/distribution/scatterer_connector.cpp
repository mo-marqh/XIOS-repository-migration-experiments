#include "scatterer_connector.hpp"


namespace xios
{

  void CScattererConnector::computeConnector(void)
  {
    auto& srcGlobalIndex = srcView_->getGlobalIndex() ;
    auto& srcIndex = srcView_->getIndex() ;
    srcSize_=srcIndex.numElements() ;
    auto srcSize = srcView_->getSize() ;
    auto srcLocalSize = srcView_->getLocalSize() ;
    
    auto& dstIndex = dstView_->getIndex() ;
    for(auto& rankIndex : dstIndex) dstSize_[rankIndex.first]=rankIndex.second.numElements() ;
    auto& dstGlobalIndex = dstView_->getGlobalIndex() ;

    unordered_map<size_t,int> mapGlobalLocalIndex ;
    int globalIndexSize=srcGlobalIndex.size() ;
    
    for(int i=0 ; i<srcSize ; i++) 
      if (srcIndex(i) >=0 && srcIndex(i) <srcLocalSize) mapGlobalLocalIndex[srcGlobalIndex(srcIndex(i))] = i ;
    
    for(auto& rankIndex : dstIndex)
    {
      int rank = rankIndex.first ;
      auto& index = rankIndex.second ;
      int size = index.numElements() ;
      auto& globalIndex = dstGlobalIndex[rank] ;
      int localSize = globalIndex.numElements() ;
      auto& connector = connector_[rank] ;
      auto& mask = mask_[rank] ;
      mask.resize(size) ;

      for(int i=0 ; i<size ; i++) 
      {
        if (index(i) >= 0  && index(i) < localSize)
        {
          const auto& it = mapGlobalLocalIndex.find(globalIndex(index(i))) ;
          if (it != mapGlobalLocalIndex.end()) 
          {
            connector.push_back(it->second) ;
            mask[i] = true ;
          }
          else mask[i]=false ;
        }
        else mask[i]=false ;
      }
    }

   // compute the number of senders for a remote destination view
    vector<int> ranks(remoteCommSize_,0) ;
    for(auto& rank : connector_) ranks[rank.first] = 1 ;
    MPI_Allreduce(MPI_IN_PLACE,ranks.data(),remoteCommSize_,MPI_INT,MPI_SUM,localComm_) ;
    for(auto& rank : connector_) nbSenders_[rank.first] = ranks[rank.first] ;

  }

}