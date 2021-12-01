#include "local_view.hpp"
#include "element.hpp"
#include "array_new.hpp"
#include "remote_connector.hpp"

namespace xios
{
  CLocalView::CLocalView(shared_ptr<CLocalElement> parent, CElementView::type type, const CArray<int,1>& indexView) 
                        : CDistributedView( parent, type, {{  parent->localRank_,  indexView }} ),
                          localRank_(parent->localRank_),
                          globalIndex_(parent->globalIndex_), size_(CDistributedView::size_[parent->localRank_]),
                          index_(CDistributedView::index_[parent->localRank_]), localSize_(CDistributedView::localSize_[parent->localRank_]) 
  {

  }

  CLocalView::CLocalView(shared_ptr<CLocalElement> parent, CElementView::type type, const CArray<bool,1>& maskView) 
                        : CDistributedView( parent, type, {{  parent->localRank_,  maskView }} ),
                          localRank_(parent->localRank_),
                          globalIndex_(parent->globalIndex_), size_(CDistributedView::size_[parent->localRank_]),
                          index_(CDistributedView::index_[parent->localRank_]), localSize_(CDistributedView::localSize_[parent->localRank_]) 
  {

  }

  void CLocalView::sendRemoteElement(shared_ptr<CRemoteConnector> connector, CContextClient* client, CEventClient& event, const CMessage& messageHeader)
  {
    int n = index_.numElements() ;
    int nglo=globalIndex_.numElements() ;
    CArray<size_t,1> ind(n) ;
    for(int i=0; i<n;i++)
    {
      if (index_(i)>=0 && index_(i)<nglo) ind(i) = globalIndex_(index_(i)) ;
      else ind(i)=index_(i) ;
    }
    CMessage message(messageHeader) ;
    message<<globalSize_ ;
    connector->transferToServer(ind, client, event, message) ;
  }


}