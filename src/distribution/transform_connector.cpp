#include "transform_connector.hpp"
#include "element.hpp"
#include "scatterer_connector.hpp"
#include "gatherer_connector.hpp"
#include "client_client_dht_template.hpp"


namespace xios
{
  void CTransformConnector::computeConnector(void)
  {
     CClientClientDHTTemplate<int>::Index2VectorInfoTypeMap info ;
     
     // first, insert destination global index into DHT
     int rank ;
     MPI_Comm_rank(localComm_, &rank) ;
     CArray<size_t,1> globalIndex ;
     dstView_->getGlobalIndexView(globalIndex) ;
     for(int i=0;i<globalIndex.numElements();i++) info[globalIndex(i)].push_back(rank) ;  
     CClientClientDHTTemplate<int> dataRanks(info, localComm_) ;

    // get global src index from src view.
    set<size_t> setGlobalIndex ; // all global index from src
    srcView_->getGlobalIndexView(globalIndex) ;
    for(int i=0;i<globalIndex.numElements();i++) setGlobalIndex.insert(globalIndex((i))) ;
    
    CArray<size_t,1> srcGlobalIndex(setGlobalIndex.size()) ;
    int i=0 ;
    for(auto& globalIndex : setGlobalIndex) 
    {
      srcGlobalIndex(i) = globalIndex ;
      i++ ;
    }
   
    dataRanks.computeIndexInfoMapping(srcGlobalIndex) ;
    const auto& returnInfo = dataRanks.getInfoIndexMap() ;

    map<int,vector<size_t>> vectorIndex ;
    for(auto& rankIndGlo : returnInfo)
    {
      size_t indGlo = rankIndGlo.first ;
      auto& listRank = rankIndGlo.second ;
      for (auto& rank : listRank)  vectorIndex[rank].push_back(indGlo);
    }

    // convert vectorIndex into array    
    map<int,CArray<size_t,1>> dstArrayIndex ;
    for(auto& rankIndGlo : vectorIndex)
    {
      int rank = rankIndGlo.first ;
      auto& indexVector = rankIndGlo.second ;
      auto& arrayIndex = dstArrayIndex[rank] ;
      CArray<size_t,1> arrayTmp(indexVector.data(), shape(indexVector.size()), duplicateData) ;
      dstArrayIndex[rank].reference(arrayTmp) ;
    }

    // distributed element : where to send data
    auto dstElement = make_shared<CDistributedElement>(srcView_->getGlobalSize(), dstArrayIndex) ;
    dstElement->addFullView() ;
    
    // create scatterer connector
    int commSize ;
    MPI_Comm_size(localComm_, &commSize) ;
    scattererConnector_ = make_shared<CScattererConnector>(srcView_, dstElement->getView(CElementView::FULL), localComm_, commSize ) ;
    scattererConnector_->computeConnector() ;

    // how much sender ?
    vector<int> nbSenders(commSize) ;
    int nbSender ;
    for(auto& it : dstArrayIndex) nbSenders[it.first]=1 ;
    vector<int> recvCounts(commSize,1) ;
    MPI_Reduce_scatter(nbSenders.data(), &nbSender, recvCounts.data(), MPI_INT, MPI_SUM, localComm_) ;
    
    // transfer global index
    map<int,CArray<size_t,1>> remoteArrayIndex ;
    
    vector<MPI_Request> sendReq ;
    for(auto& it : dstArrayIndex)
    {
      MPI_Request req ;
      MPI_Isend(it.second.dataFirst(), it.second.numElements(), MPI_SIZE_T, it.first,0, localComm_,&req) ;
      sendReq.push_back(req) ;
    }

    for(int i=0; i<nbSender; i++) 
    {
      int size ;
      MPI_Status status ;
      MPI_Probe(MPI_ANY_SOURCE, 0, localComm_, &status ) ;
      MPI_Get_count(&status, MPI_SIZE_T, &size) ;
      vector<size_t> recvBuff(size) ;
      MPI_Recv(recvBuff.data(), size, MPI_SIZE_T, status.MPI_SOURCE,0,localComm_,&status) ;
      CArray<size_t,1> arrayTmp(recvBuff.data(), shape(recvBuff.size()), duplicateData) ;
      remoteArrayIndex[status.MPI_SOURCE].reference(arrayTmp) ;
      recvRankSize_[status.MPI_SOURCE] = size ;
    }
    vector<MPI_Status> sendStatus(sendReq.size()) ;
    MPI_Waitall(sendReq.size(),sendReq.data(),sendStatus.data()) ;

    auto remoteElement = make_shared<CDistributedElement>(dstView_->getGlobalSize(), remoteArrayIndex) ;
    remoteElement->addFullView() ;
    gathererConnector_= make_shared<CGathererConnector>(remoteElement->getView(CElementView::FULL),dstView_) ;
    gathererConnector_->computeConnector() ;

  }
}