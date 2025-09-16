/*!
   \file scalar_algorithm_redistribute.cpp
   \brief Algorithm for redistribute a scalar.
 */
#include "scalar_algorithm_redistribute.hpp"
#include "redistribute_scalar.hpp"
#include "scalar.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "context.hpp"

namespace xios
{

  shared_ptr<CGenericAlgorithmTransformation> CScalarAlgorithmRedistribute::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                               CTransformation<CScalar>* transformation,
                                                               int elementPositionInGrid,
                                                               std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                               std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                               std::map<int, int>& elementPositionInGridSrc2DomainPosition, 
                                                               std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                               std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                               std::map<int, int>& elementPositionInGridDst2DomainPosition)
  TRY
  {
    std::vector<CScalar*> scalarListDestP = gridDst->getScalars();
    std::vector<CScalar*> scalarListSrcP  = gridSrc->getScalars();

    CRedistributeScalar* redistributeScalar = dynamic_cast<CRedistributeScalar*> (transformation);
    int scalarDstIndex = elementPositionInGridDst2ScalarPosition[elementPositionInGrid];
    int scalarSrcIndex = elementPositionInGridSrc2ScalarPosition[elementPositionInGrid];

    return make_shared<CScalarAlgorithmRedistribute>(isSource, scalarListDestP[scalarDstIndex], scalarListSrcP[scalarSrcIndex], redistributeScalar);
  }
  CATCH

  bool CScalarAlgorithmRedistribute::dummyRegistered_ = CScalarAlgorithmRedistribute::registerTrans();
  bool CScalarAlgorithmRedistribute::registerTrans()
  TRY
  {
    return CGridTransformationFactory<CScalar>::registerTransformation(TRANS_REDISTRIBUTE_SCALAR, create);
  }
  CATCH

  CScalarAlgorithmRedistribute::CScalarAlgorithmRedistribute(bool isSource, CScalar* scalarDestination, CScalar* scalarSource, CRedistributeScalar* redistributeScalar)
  : CAlgorithmTransformationTransfer(redistributeScalar->getContext(), isSource)
  TRY
  {

    CContext* context = getContext(); 

    scalarDestination->n.reset();
    scalarDestination->mask.reset();
    scalarDestination->value.reset();
    scalarDestination->bounds.reset();

    redistributeScalar->checkValid(scalarSource);

    // define the global index of the new domain

    CArray<size_t,1> globalIndex ;

    if (    redistributeScalar->type == CRedistributeScalar::type_attr::bands 
         || redistributeScalar->type == CRedistributeScalar::type_attr::column 
         || redistributeScalar->type == CRedistributeScalar::type_attr::root )
    {
      if (context->getIntraCommRank()==0)
      {
        scalarDestination->n = 1;
        globalIndex.resize(1) ;
        globalIndex(0)=0 ;
      }
      else
      {
        scalarDestination->n = 0;
        globalIndex.resize(0) ;
      }
    }
    else if (redistributeScalar->type == CRedistributeScalar::type_attr::full)
    {
      scalarDestination->n = 1;
      globalIndex.resize(1) ;
      globalIndex(0)=0 ;
    }
    
    auto& transMap = this->transformationMapping_;
    for(int i=0; i<globalIndex.numElements(); i++) transMap[globalIndex(i)]=globalIndex(i) ;

    auto elementDst=shared_ptr<CLocalElement>(new CLocalElement(context->getIntraCommRank(), 1, globalIndex)) ;
    elementDst->addFullView() ;

    auto transformConnector = make_shared<CTransformConnector>(scalarSource->getLocalView(CElementView::FULL), elementDst->getView(CElementView::FULL),context->getIntraComm()) ;
    transformConnector->computeConnector() ;

    CArray<double,1> valSrc, valDst ;
    valSrc.resize(scalarSource->getLocalView(CElementView::FULL)->getSize()) ;

    if (scalarSource->hasBounds()) 
    {
      transformConnector->transfer(2, scalarSource->bounds, scalarDestination->bounds) ;
    }
    
    if (scalarSource->hasLabel())
    {
//ym transferTransformConnector.transfer(axisSrc->label, axisDestination->label) ;  -> probably not supported now, see later
    }

    // transfer the mask
    auto transformMask = make_shared<CTransformConnector>(scalarSource->getLocalView(CElementView::WORKFLOW), elementDst->getView(CElementView::FULL),context->getIntraComm()) ;
    transformMask->computeConnector() ;

    CArray<bool,1> workflow(scalarSource->getLocalView(CElementView::WORKFLOW)->getSize()) ;
    CArray<bool,1> mask ;
    mask.resize(scalarSource->getLocalView(CElementView::FULL)->getSize()) ;
    workflow=true ;
    
    transformMask->transfer(workflow, mask, false) ;   
    if (mask.numElements()>0)
    {
      scalarDestination->mask = mask(0) ;
    }

    scalarDestination->checkAttributes() ;
    this->computeAlgorithm(scalarSource->getLocalView(CElementView::WORKFLOW), scalarDestination->getLocalView(CElementView::WORKFLOW)) ;

    valDst.resize(scalarDestination->getLocalView(CElementView::FULL)->getSize()) ;
    if (scalarSource->hasValue())
    {
      if (valSrc.numElements()>0)  valSrc(0)=scalarSource->value ;
      transformConnector->transfer(valSrc, valDst) ;
      if (valDst.numElements()>0)  scalarDestination->value = valDst(0) ;
    }
  }
  CATCH


}
