#include "grid_remote_connector.hpp"
#include "client_client_dht_template.hpp"



namespace xios
{
  
  CGridRemoteConnector::CGridRemoteConnector(vector<CLocalView*>& srcView, vector<CDistributedView*>& dstView, MPI_Comm localComm) 
                       : srcView_(srcView), dstView_(dstView), localComm_(localComm) 
  {}

  void CGridRemoteConnector::computeConnector(void)
  {
    computeGenericMethod() ;
  }

  void CGridRemoteConnector::computeGenericMethod(void)
  {
    // generic method, every element can be distributed
    int nDst = dstView_.size() ;
    vector<size_t> dstSliceSize(nDst) ;
    dstSliceSize[0] = 1 ;  
    for(int i=1; i<nDst; i++)  dstSliceSize[i] = dstView_[i-1]->getGlobalSize()*dstSliceSize[i-1] ;
  
    CClientClientDHTTemplate<int>::Index2VectorInfoTypeMap dataInfo ;

    CClientClientDHTTemplate<size_t>::Index2VectorInfoTypeMap info ; // info map
    for(int pos=0; pos<nDst; pos++)
    {
      size_t sliceSize=dstSliceSize[pos] ;
      map<int,CArray<size_t,1>> globalIndexView ;
      dstView_[pos]->getGlobalIndexView(globalIndexView) ;
      
      CClientClientDHTTemplate<size_t>::Index2VectorInfoTypeMap lastInfo(info) ;

      if (pos>0)
      {
        CArray<size_t,1> ranks(globalIndexView.size()) ;
        auto it=globalIndexView.begin() ;
        for(int i=0 ; i<ranks.numElements();i++,it++) ranks(i)=it->first ;
        CClientClientDHTTemplate<size_t> dataRanks(info, localComm_) ;
        dataRanks.computeIndexInfoMapping(ranks) ;
        lastInfo = dataRanks.getInfoIndexMap() ;
      }
      
      info.clear() ;
      for(auto& it : globalIndexView)
      {
        int rank = it.first ;
        auto& globalIndex = it.second ;
        auto& inf = info[rank] ;
        if (pos==0) for(int i=0;i<globalIndex.numElements();i++) inf.push_back(globalIndex(i)) ;
        else
        {
          auto& lastGlobalIndex = lastInfo[rank] ;
          for(size_t lastGlobalInd : lastGlobalIndex)
          {
            for(int i=0;i<globalIndex.numElements();i++) inf.push_back(globalIndex(i)*sliceSize+lastGlobalInd) ;
          }
        } 
      }

      if (pos==nDst-1)
      {
         for(auto& it : info)
         {
           int rank=it.first ;
           auto& globalIndex = it.second ;
           for(auto globalInd : globalIndex) dataInfo[globalInd].push_back(rank) ;
         }
      } 
    }

    CClientClientDHTTemplate<int> dataRanks(dataInfo, localComm_) ;
/*
    CClientClientDHTTemplate<int>::Index2VectorInfoTypeMap info ; // info map
        
// generate list of global index for dst view, and insert it into DHT map
    int nDst = dstView_.size() ;
    vector<size_t> dstGlobalIndex ;
    vector<size_t> dstSliceSize(nDst) ;
    dstSliceSize[nDst-1] = 1 ;  
    for(int i=nDst-2; i>=0; i--)  dstSliceSize[i] = dstView_[i+1]->getGlobalSize()*dstSliceSize[i+1] ;
    for(auto& ranks : dstView_[0]->getLocalSize())
    {
      dstGlobalIndex.clear() ;
      int rank=ranks.first ;
      size_t sliceIndex=0 ;
      dstView_[nDst-1]->getGlobalIndex(rank, dstGlobalIndex, sliceIndex, dstSliceSize.data(), dstView_.data(), nDst-1) ;
      for(auto globalIndex : dstGlobalIndex) info[globalIndex].push_back(rank) ; // insert into DHT
    }
    
    CClientClientDHTTemplate<int> dataRanks(info, localComm_) ;
*/    
    // generate list of global index for src view
    int nSrc = srcView_.size() ;
    vector<size_t> srcSliceSize(nSrc) ;
//    srcSliceSize[nSrc-1] = 1 ;
//    for(int i=nSrc-2; i>=0; i--)  srcSliceSize[i] = srcView_[i+1]->getGlobalSize()*srcSliceSize[i+1] ;
   
    srcSliceSize[0] = 1 ;  
    for(int i=1; i<nSrc; i++)  srcSliceSize[i] = srcView_[i-1]->getGlobalSize()*srcSliceSize[i-1] ;

    vector<size_t> srcGlobalIndex ;
    size_t sliceIndex=0 ;
    srcView_[nSrc-1]->getGlobalIndex(srcGlobalIndex, sliceIndex, srcSliceSize.data(), srcView_.data(), nSrc-1) ;
    
    CArray<size_t,1> srcGlobalIndexArray(srcGlobalIndex.data(), shape(srcGlobalIndex.size()),neverDeleteData) ;
    dataRanks.computeIndexInfoMapping(srcGlobalIndexArray) ;
    const auto& returnInfo = dataRanks.getInfoIndexMap() ;

    vector<map<int, set<size_t>>> elements(nSrc) ; // internal representation of elements composing the grid

//    srcSliceSize[nSrc-1] = srcView_[nSrc-1]->getGlobalSize() ;
//    for(int i=nSrc-2 ; i>=0 ; i--) srcSliceSize[i] = srcView_[i]->getGlobalSize()*srcSliceSize[i+1] ;

    for(auto& indRanks : returnInfo)
    {
      size_t gridIndexGlo=indRanks.first ;
      auto& ranks = indRanks.second ;
      for(int i=nSrc-1; i>=0; i--)
      {
        auto& element = elements[i] ;
        size_t localIndGlo = gridIndexGlo / srcSliceSize[i] ;
        gridIndexGlo = gridIndexGlo % srcSliceSize[i] ;
        for(int rank : ranks) element[rank].insert(localIndGlo) ;
      }
    }

    elements_.resize(nSrc) ;
    for(int i=0 ; i<nSrc; i++)
    {
      auto& element=elements[i] ;
      for(auto& rankInd : element)
      {
        int rank=rankInd.first ;
        set<size_t>& indGlo = rankInd.second ;
        CArray<size_t,1>& indGloArray = elements_[i][rank] ;
        indGloArray.resize(indGlo.size()) ;
        int j=0 ;
        for (auto index : indGlo) { indGloArray(j) = index ; j++; }
      }
    }
  }

}