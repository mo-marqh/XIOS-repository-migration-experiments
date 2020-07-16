#include "element.hpp"
#include "distributed_view.hpp"
#include "local_view.hpp"
#include "local_connector.hpp"
#include "context_client.hpp"
#include "context_server.hpp"
#include "mpi.hpp"


namespace xios
{

  CDistributedElement::CDistributedElement(int globalSize, const map<int, CArray<size_t,1>>& globalIndex)
  {
    globalSize_= globalSize ;
    for(auto index : globalIndex) 
    {
      globalIndex_[index.first].reference(index.second.copy()) ;
      localSize_[index.first] = index.second.numElements() ;
    }
  }

  CDistributedElement::CDistributedElement(CEventServer& event) 
  {
    recvFromClient(event) ;
  }
  
  void CDistributedElement::addView(CElementView::type type, std::map<int, CArray<int,1>>& indexView)
  {
    views_[type] = new CDistributedView(this, type, indexView) ;
  } 

  void CDistributedElement::addFullView(void)
  {
    if (views_[CElementView::FULL]!=nullptr) return ;

    std::map<int, CArray<int,1>> indexView ;
    for(auto rank : globalIndex_)
    {
      auto& index = indexView[rank.first] ;
      int size=rank.second.numElements() ;
      index.resize(size) ;
      for(int i=0;i<size;i++) index(i)=i ;
    }
    addView(CElementView::FULL, indexView) ;
  } 

  void CDistributedElement::sendToServer(CContextClient* client, CEventClient& event, const CMessage& messageHeader)
  {
    int remoteSize = client->getRemoteSize() ;
    vector<int> nbSenders(remoteSize,0) ;
    for(auto rank : globalIndex_) nbSenders[rank.first]=1 ; 
    MPI_Allreduce(MPI_IN_PLACE, nbSenders.data(), remoteSize, MPI_INT, MPI_SUM, client->getIntraComm()) ;

    list<CMessage> messages;
    for(auto ranksData : globalIndex_)
    {
      int rank = ranksData.first ;
      auto& data = ranksData.second ;

      messages.push_back(CMessage(messageHeader));
      messages.back()<<globalSize_<<data;
      event.push(rank, nbSenders[rank], messages.back());
    } 
    client->sendEvent(event) ; 
  }

  void CDistributedElement::recvFromClient(CEventServer& event)
  {
    globalIndex_.clear();
    for (auto& subEvent : event.subEvents)
    {      
      CBufferIn* buffer = subEvent.buffer;
      int rank=subEvent.rank ;
      *buffer>>globalSize_ ;
      *buffer >> globalIndex_[rank];
    }
    localSize_.clear() ;
    for(auto& globalIndex : globalIndex_) localSize_[globalIndex.first] = globalIndex.second.numElements() ;  
  }
 


  CLocalElement::CLocalElement(int localRank, size_t globalSize, CArray<size_t,1>& globalIndex) 
                             : CDistributedElement(globalSize, {{localRank, globalIndex}}),
                               globalIndex_(CDistributedElement::globalIndex_[localRank]), localSize_(CDistributedElement::localSize_[localRank]), localRank_(localRank)
  {
   
  }   

  CLocalElement::CLocalElement(int localRank, CEventServer& event) : globalIndex_(CDistributedElement::globalIndex_[localRank]), localSize_(CDistributedElement::localSize_[localRank]), localRank_(localRank) 
  {
    recvFromClient(localRank, event) ;
  }

  void CLocalElement::recvFromClient(int localRank, CEventServer& event)
  {
    set<size_t> globalIndex ;

    for (auto& subEvent : event.subEvents)
    {      
      CBufferIn* buffer = subEvent.buffer;
      int rank=subEvent.rank ;
      CArray<size_t,1> indGlo ;
      *buffer >> globalSize_>> indGlo;
      globalIndex.insert(indGlo.dataFirst(), indGlo.dataFirst()+indGlo.numElements()) ;
    }

    localSize_ = globalIndex.size() ;
    globalIndex_.resize(localSize_) ;
    int i=0 ;
    for(auto& ind : globalIndex) { globalIndex_(i)=ind ; i++; }
  }

  void CLocalElement::addView(CElementView::type type, CArray<int,1>& indexView)
  {
    views_[type] = new CLocalView(this, type, indexView) ;
  } 

  void CLocalElement::addFullView(void)
  {
    if (views_[CElementView::FULL]!=nullptr) return ;

    CArray<int,1> indexView(localSize_) ;
    for(int i=0;i<localSize_;i++) indexView(i)=i ;
    addView(CElementView::FULL, indexView) ;
  } 

  CLocalConnector* CLocalElement::getConnector(CElementView::type srcType, CElementView::type dstType) 
  { 
    auto newPair = pair<CElementView::type,CElementView::type>(srcType,dstType);
    auto it = connectors_.find(newPair) ;
    if (it==connectors_.end()) 
    {
      auto insertPair=pair<pair<CElementView::type,CElementView::type>, CLocalConnector*>(newPair,new CLocalConnector(getView(srcType),getView(dstType))) ;
      it=connectors_.insert(insertPair).first ;
      it->second->computeConnector() ;
    }
    return it->second ;
  }

}