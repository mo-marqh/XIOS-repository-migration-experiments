#include "remote_connector.hpp"
#include "client_client_dht_template.hpp"


namespace xios
{

  void CRemoteConnector::computeConnector(void)
  {
    CClientClientDHTTemplate<int>::Index2VectorInfoTypeMap info ;
    for(auto& dstIndex : dstView_->getIndex())
    {
      int rank=dstIndex.first ;
      auto& indexList=dstIndex.second ;
      auto& globalIndex = dstView_->getGlobalIndex()[rank] ;
      for(int i=0;i<indexList.numElements();i++) info[globalIndex(indexList(i))].push_back(rank) ;  
    }
    CClientClientDHTTemplate<int> dataRanks(info, localComm_) ;
    
    set<size_t> setGlobalIndex ; // all global index from src
    auto& srcIndex = srcView_->getIndex() ;
    auto& globalIndex = srcView_->getGlobalIndex() ;
    int globalIndexSize = globalIndex.numElements() ;
    int indexSize = srcIndex.numElements() ;
    for(int i=0;i<indexSize;i++) 
    {
      if (srcIndex(i)>=0 & srcIndex(i)<globalIndexSize) setGlobalIndex.insert(globalIndex(srcIndex(i))) ;
    }
    
    CArray<size_t,1> srcGlobalIndex(setGlobalIndex.size()) ;
    int i=0 ;
    for(auto& globalIndex : setGlobalIndex) 
    {
      srcGlobalIndex(i) = globalIndex ;
      i++ ;
    }

    dataRanks.computeIndexInfoMapping(srcGlobalIndex) ;
    const auto& returnInfo = dataRanks.getInfoIndexMap() ;

    // so we have the info where to send data : rank and local index. 
    // create the interconnector :
    map<int,vector<size_t>> element ;
    for(int i=0;i<indexSize;i++)
    {
      if (srcIndex(i)>=0 & srcIndex(i)<globalIndexSize)
      {
        size_t indGlo = globalIndex(srcIndex(i)) ;
        // don't forget that a single point can be sent multiple time (redondency ?)
        for(auto& rank : returnInfo.at(indGlo))
        {
          connector_[rank].push_back(i) ;
          element[rank].push_back(indGlo) ;
        }
      }
    }

    for(auto rankIndex : element)
    {
      CArray<size_t,1> arrayTmp(rankIndex.second.data(), shape(rankIndex.second.size()), duplicateData) ;
      element_[rankIndex.first].reference(arrayTmp) ;
    }
    
    // compute the number of senders for a remote destination view
    int commSize ;
    MPI_Comm_size(localComm_, &commSize) ;
    vector<int> ranks(commSize,0) ;
    for(auto& rank : connector_) ranks[rank.first] = 1 ;
    MPI_Allreduce(MPI_IN_PLACE,ranks.data(),commSize,MPI_INT,MPI_SUM,localComm_) ;
    for(auto& rank : connector_) nbSenders_[rank.first] = ranks[rank.first] ;
  }





}
