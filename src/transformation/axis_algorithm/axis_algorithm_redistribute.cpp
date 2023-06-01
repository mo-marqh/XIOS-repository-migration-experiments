/*!
   \file axis_algorithm_redistribute.cpp
   \brief Algorithm for redistribute an axis.
 */
#include "axis_algorithm_redistribute.hpp"
#include "redistribute_axis.hpp"
#include "axis.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "context.hpp"

namespace xios
{

  shared_ptr<CGenericAlgorithmTransformation> CAxisAlgorithmRedistribute::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                               CTransformation<CAxis>* transformation,
                                                               int elementPositionInGrid,
                                                               std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                               std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                               std::map<int, int>& elementPositionInGridSrc2DomainPosition, 
                                                               std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                               std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                               std::map<int, int>& elementPositionInGridDst2DomainPosition)
  TRY
  {
    std::vector<CAxis*> axisListDestP = gridDst->getAxis();
    std::vector<CAxis*> axisListSrcP  = gridSrc->getAxis();

    CRedistributeAxis* redistributeAxis = dynamic_cast<CRedistributeAxis*> (transformation);
    int axisDstIndex = elementPositionInGridDst2AxisPosition[elementPositionInGrid];
    int axisSrcIndex = elementPositionInGridSrc2AxisPosition[elementPositionInGrid];

    return make_shared<CAxisAlgorithmRedistribute>(isSource, axisListDestP[axisDstIndex], axisListSrcP[axisSrcIndex], redistributeAxis);
  }
  CATCH

  bool CAxisAlgorithmRedistribute::dummyRegistered_ = CAxisAlgorithmRedistribute::registerTrans();
  bool CAxisAlgorithmRedistribute::registerTrans()
  TRY
  {
    return CGridTransformationFactory<CAxis>::registerTransformation(TRANS_REDISTRIBUTE_AXIS, create);
  }
  CATCH

  CAxisAlgorithmRedistribute::CAxisAlgorithmRedistribute(bool isSource, CAxis* axisDestination, CAxis* axisSource, CRedistributeAxis* redistributeAxis)
  : CAlgorithmTransformationTransfer(isSource)
  TRY
  {
    CContext* context = CContext::getCurrent(); 

    axisDestination->n_glo.reset();
    axisDestination->index.reset();
    axisDestination->n.reset();
    axisDestination->begin.reset();

    axisDestination->mask.reset();
    axisDestination->data_index.reset();
    axisDestination->data_n.reset();
    axisDestination->data_begin.reset();

    axisDestination->value.reset();
    axisDestination->label.reset();
    axisDestination->bounds.reset();

    redistributeAxis->checkValid(axisSource);

    auto& index = redistributeAxis-> index ;
    int n_glo = axisSource -> n_glo ;
    axisDestination->n_glo = n_glo ;
    
    

    // define the global index of the new domain

    CArray<size_t,1> globalIndex ;

    if (redistributeAxis->type == CRedistributeAxis::type_attr::index)
    {   
      globalIndex.resize(index.numElements()) ;
      globalIndex=index ;
    }
    else
    {
      
      auto distType = CRedistributeAxis::type_attr::column ;

      if (redistributeAxis->type == CRedistributeAxis::type_attr::bands) distType = CRedistributeAxis::type_attr::column ;
      else  distType=redistributeAxis->type;

      int size, start ;
      if (distType==CRedistributeAxis::type_attr::column) // Bands distribution to send to file server
      {
        int nbClient = context->getIntraCommSize() ;
        int rankClient = context->getIntraCommRank() ;
      
        size = n_glo/nbClient ;
        if (rankClient < n_glo % nbClient)
        {
         size++ ;
         start = rankClient*size ;
        }
        else start = (n_glo % nbClient)*(size+1) + (rankClient-(n_glo % nbClient)) * size ;

      }
      else if (distType==CRedistributeAxis::type_attr::full) // domain is not distributed ie all servers get the same local domain
      {
        size=n_glo ; start=0 ;
      }
      else if (distType==CRedistributeAxis::type_attr::root) // domain is not distributed ie all servers get the same local domain
      {
        if (context->getIntraCommRank()==0) { size=n_glo ; start=0 ; }
        else { size=0 ; start=0 ; }
      }

      globalIndex.resize(size) ;
      size_t ind ;
      ind=0 ;
      for(int i=start; i<start+size; i++)
      {
       globalIndex(ind) =  i ;
       ind++ ;
      }
    }
    
    axisDestination->index.resize(globalIndex.numElements()) ;
    axisDestination->index = globalIndex ;
    // apply the transformation to attributes

    auto& transMap = this->transformationMapping_;
    for(int i=0; i<globalIndex.numElements(); i++) transMap[globalIndex(i)]=globalIndex(i) ;

    auto elementDst=shared_ptr<CLocalElement>(new CLocalElement(context->getIntraCommRank(), n_glo, globalIndex)) ;
    elementDst->addFullView() ;

    auto transformConnector = make_shared<CTransformConnector>(axisSource->getLocalView(CElementView::FULL), elementDst->getView(CElementView::FULL),context->getIntraComm()) ;
    transformConnector->computeConnector() ;
    
    if (axisSource->hasValue()) transformConnector->transfer(axisSource->value, axisDestination->value) ;
    
    
    
    if (axisSource->hasBounds())
    {
      CArray<double,1> boundsSrc(axisSource->bounds.dataFirst(),shape(axisSource->bounds.numElements()),neverDeleteData) ;
      CArray<double,1> boundsDst ; 
      transformConnector->transfer(2, boundsSrc, boundsDst) ;
      axisDestination->bounds.resize(2,globalIndex.numElements()) ;
      axisDestination->bounds = CArray<double,2>(boundsDst.dataFirst(), shape(2,globalIndex.numElements()),neverDeleteData) ;
    }
    
    if (axisSource->hasLabel())
    {
//ym transferTransformConnector.transfer(axisSrc->label, axisDestination->label) ;  -> probably not supported now, see later
    }

    // transfer the mask
    auto transformMask = make_shared<CTransformConnector>(axisSource->getLocalView(CElementView::WORKFLOW), elementDst->getView(CElementView::FULL),context->getIntraComm()) ;
    transformMask->computeConnector() ;

    CArray<bool,1> workflow(axisSource->getLocalView(CElementView::WORKFLOW)->getSize()) ;
    axisDestination->mask.resize(axisSource->getLocalView(CElementView::FULL)->getSize()) ;
    workflow=true ;
    
    transformMask->transfer(workflow,axisDestination->mask,false) ;   

    axisDestination->checkAttributes() ;
    this->computeAlgorithm(axisSource->getLocalView(CElementView::WORKFLOW), axisDestination->getLocalView(CElementView::WORKFLOW)) ;
  }
  CATCH


}
