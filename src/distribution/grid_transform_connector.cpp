#include "grid_transform_connector.hpp"
#include "element.hpp"
#include "scatterer_connector.hpp"
#include "gatherer_connector.hpp"
#include "grid_remote_connector.hpp"


namespace xios
{
  void CGridTransformConnector::computeConnector(bool eliminateRedundant)
  {
    int commSize ;
    int commRank ;
    MPI_Comm_size(localComm_, &commSize) ;
    MPI_Comm_rank(localComm_, &commRank) ;
    int nElements = srcViews_.size() ;

    auto remoteConnector = make_shared<CGridRemoteConnector>(srcViews_, remoteViews_, localComm_, commSize) ;  
    remoteConnector->computeConnector(eliminateRedundant) ; 
    
    vector<shared_ptr<CDistributedElement>> sendElements(nElements) ;
    scattererConnector_.resize(nElements) ;
    gathererConnector_.resize(nElements) ;

    for(int i=0;i<nElements;i++)
    {
      sendElements[i] = make_shared<CDistributedElement>(srcViews_[i]->getGlobalSize(), remoteConnector->getDistributedGlobalIndex(i)) ;
      sendElements[i]->addFullView() ;
      scattererConnector_[i] = make_shared<CScattererConnector>(srcViews_[i], sendElements[i]->getView(CElementView::FULL), localComm_, commSize) ;
      scattererConnector_[i]->computeConnector() ;
      std::map<int, CArray<size_t,1>>& sendIndex = sendElements[i]->getGlobalIndex() ;

      // how much sender ?
      vector<int> nbSenders(commSize) ;
      int nbSender ;
      for(auto& it : sendIndex) nbSenders[it.first]=1 ;
      vector<int> recvCounts(commSize,1) ;
      MPI_Reduce_scatter(nbSenders.data(), &nbSender, recvCounts.data(), MPI_INT, MPI_SUM, localComm_) ;
    
      // transfer global index
      // send Index
      vector<MPI_Request> sendReq ;
      for(auto& it : sendIndex)
      {
        MPI_Request req ;
        MPI_Isend(it.second.dataFirst(), it.second.numElements(), MPI_SIZE_T, it.first,0, localComm_, &req) ;
        sendReq.push_back(req) ;
      }

      // receive index
      map<int,CArray<size_t,1>> recvIndex ;
    
      for(int j=0; j<nbSender; j++) 
      {
        int size ;
        MPI_Status status ;
        MPI_Probe(MPI_ANY_SOURCE, 0, localComm_, &status ) ;
        MPI_Get_count(&status, MPI_SIZE_T, &size) ;
        vector<size_t> recvBuff(size) ;
        MPI_Recv(recvBuff.data(), size, MPI_SIZE_T, status.MPI_SOURCE,0, localComm_,&status) ;
        if (size!=0) {
            CArray<size_t,1> arrayTmp(recvBuff.data(), shape(recvBuff.size()), duplicateData) ;
            recvIndex[status.MPI_SOURCE].reference(arrayTmp) ;
        }
        else {
            CArray<size_t,1> arrayTmp(0) ;
            recvIndex[status.MPI_SOURCE].reference(arrayTmp) ;
        }
        if (recvRankSize_.count(status.MPI_SOURCE)==0) recvRankSize_[status.MPI_SOURCE] = size ; 
        else recvRankSize_[status.MPI_SOURCE] *= size ; 
      }
      vector<MPI_Status> sendStatus(sendReq.size()) ;
      MPI_Waitall(sendReq.size(),sendReq.data(),sendStatus.data()) ;
      MPI_Barrier( localComm_ );
      
      // create gatherer connector

      auto recvElement = make_shared<CDistributedElement>(remoteViews_[i]->getGlobalSize(), recvIndex) ;
      recvElement->addFullView() ;
      gathererConnector_[i] = make_shared<CGathererConnector>(recvElement->getView(CElementView::FULL), remoteViews_[i]) ;
      gathererConnector_[i]->computeConnector() ;
    }

    gridScattererConnector_ = make_shared<CGridScattererConnector>(scattererConnector_) ;
    gridGathererConnector_  = make_shared<CGridGathererConnector>(gathererConnector_) ;
  }

}
