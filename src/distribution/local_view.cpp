#include "local_view.hpp"
#include "element.hpp"
#include "array_new.hpp"
#include "remote_connector.hpp"
#include "grid_transform_connector.hpp"

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
  
  void CLocalView::createWithoutRedundancyFullViewConnector( int globalSize, MPI_Comm comm_file, shared_ptr<CGridTransformConnector>& gridTransformConnector, CArray<size_t,1>& globalIndex )
  {
    int comm_file_rank(0);
    MPI_Comm_rank( comm_file, &comm_file_rank );
    int comm_file_size(1);
    MPI_Comm_size( comm_file, &comm_file_size );

    vector<shared_ptr<CLocalView>> srcViews;
    shared_ptr<CLocalView> srcView = shared_from_this();
    srcViews.push_back( srcView );
          
    // Compute a without redundancy element FULL view to enable a consistent hash computation
    vector<shared_ptr<CLocalView>> remoteViews;
    shared_ptr<CLocalView> remoteView;
    // define the remote view without redundancy (naive distribution of the remote view)
    int localSize = globalSize/comm_file_size;
    if ( (comm_file_rank==comm_file_size-1) && (localSize*comm_file_size != globalSize ) )
      localSize += globalSize-localSize*comm_file_size;
    globalIndex.resize( localSize );
    CArray<int,1> index( localSize );
    for (int iloc=0; iloc<localSize ; iloc++ )
      {
        globalIndex(iloc) = comm_file_rank*(globalSize/comm_file_size) + iloc;
        index(iloc) = iloc;
      }
    shared_ptr<CLocalElement> localElement = make_shared<CLocalElement>(comm_file_rank, globalSize, globalIndex) ;
    localElement->addView(CElementView::FULL, index) ;
    remoteView = localElement->getView(CElementView::FULL) ;
    remoteViews.push_back( remoteView );
        
    // Compute the connector between current and without redundancy FULL views
    gridTransformConnector = make_shared<CGridTransformConnector>(srcViews, remoteViews, comm_file ) ;
    gridTransformConnector->computeConnector(true) ; // eliminateRedondant = true

  }

}
