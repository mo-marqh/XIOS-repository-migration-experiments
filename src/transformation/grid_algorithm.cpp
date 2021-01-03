#include "grid_algorithm.hpp"
#include "grid_elements.hpp"
#include "grid_local_view.hpp"
#include "grid.hpp"
#include "context.hpp"

namespace xios
{

  CGridAlgorithm::CGridAlgorithm(CGrid* gridSrc, CGrid* gridDst, int pos,  CGenericAlgorithmTransformation* algo)
  : gridSrc_(gridSrc), gridDst_(gridDst), pos_(pos), algorithm_(algo)
  {
    this->computeAlgorithm() ;
  }

  void CGridAlgorithm::computeAlgorithm(void)
  {
    CGridLocalElements* gridSrcElements = gridSrc_->getGridLocalElements() ;
    CGridLocalElements* gridDstElements = gridDst_->getGridLocalElements() ;
    
    CGridLocalView* srcView = gridSrcElements->getView(CElementView::WORKFLOW) ;
    CGridLocalView* dstView = gridDstElements->getView(CElementView::WORKFLOW) ;
    MPI_Comm comm = CContext::getCurrent()->getIntraComm() ;
    int commSize = CContext::getCurrent()->getIntraCommSize() ;
    int commRank = CContext::getCurrent()->getIntraCommRank() ;
    
    auto& elements =  gridSrcElements->getElements() ;
    int nElements = elements.size() ;
    vector<CLocalElement*> remoteElements(nElements) ;
    vector<CLocalView*> remoteViews(nElements) ;
    for(int i=0;i<nElements;i++)
    {
      if (i==pos_) remoteElements[i] = algorithm_->getRecvElement() ;
      else
      { 
        CArray<size_t,1> globalIndexView ;
        srcView->getView(i)->getGlobalIndexView(globalIndexView) ;
        remoteElements[i] = new CLocalElement(commRank, srcView->getView(i)->getGlobalSize(),globalIndexView) ;
        remoteElements[i]->addFullView() ;
        if (i>pos_) dimBefore_ *= srcView->getView(i)->getSize() ;
        else dimAfter_ *= srcView->getView(i)->getSize() ;
          
      }
      remoteViews[i] = remoteElements[i] -> getView(CElementView::FULL);
    }

    gridTransformConnector_ = new CGridTransformConnector(srcView->getViews(), remoteViews, comm ) ;
 
  }

  void CGridAlgorithm::apply(const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
  {
    CArray<double,1> dataOutTmp ;
    gridTransformConnector_->transfer(dataIn, dataOutTmp) ;
    algorithm_->apply(dimBefore_, dimAfter_, dataOutTmp, dataOut) ;
  }


}