/*!
   \file domain_algorithm_redistribute.cpp
   \brief Algorithm for redistribute a domain.
 */
#include "domain_algorithm_redistribute.hpp"
#include "redistribute_domain.hpp"
#include "domain.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "context.hpp"

namespace xios
{

  shared_ptr<CGenericAlgorithmTransformation> CDomainAlgorithmRedistribute::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                               CTransformation<CDomain>* transformation,
                                                               int elementPositionInGrid,
                                                               std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                               std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                               std::map<int, int>& elementPositionInGridSrc2DomainPosition, 
                                                               std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                               std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                               std::map<int, int>& elementPositionInGridDst2DomainPosition)
  TRY
  {
    std::vector<CDomain*> domainListDestP = gridDst->getDomains();
    std::vector<CDomain*> domainListSrcP  = gridSrc->getDomains();

    CRedistributeDomain* redistributeDomain = dynamic_cast<CRedistributeDomain*> (transformation);
    int domainDstIndex = elementPositionInGridDst2DomainPosition[elementPositionInGrid];
    int domainSrcIndex = elementPositionInGridSrc2DomainPosition[elementPositionInGrid];

    return make_shared<CDomainAlgorithmRedistribute>(isSource, domainListDestP[domainDstIndex], domainListSrcP[domainSrcIndex], redistributeDomain);
  }
  CATCH

  bool CDomainAlgorithmRedistribute::dummyRegistered_ = CDomainAlgorithmRedistribute::registerTrans();
  bool CDomainAlgorithmRedistribute::registerTrans()
  TRY
  {
    return CGridTransformationFactory<CDomain>::registerTransformation(TRANS_REDISTRIBUTE_DOMAIN, create);
  }
  CATCH

  CDomainAlgorithmRedistribute::CDomainAlgorithmRedistribute(bool isSource, CDomain* domainDestination, CDomain* domainSource, CRedistributeDomain* redistributeDomain)
  : CAlgorithmTransformationTransfer(redistributeDomain->getContext(), isSource)
  TRY
  {
     CContext* context = getContext(); 
   // Reset geometrical attributes to avoid incompatible (user/domainSource) attributs
   //   attributs will be defined using domainSource and/or transformation attributs
    domainDestination->type.reset();
    domainDestination->ni_glo.reset();
    domainDestination->nj_glo.reset();
   
    domainDestination->i_index.reset();   // defined using domainSource->getLocalElement()
    domainDestination->j_index.reset();   // "
    domainDestination->ibegin.reset();    // will be computed in domainDestination->checkDomain() (from checkAttributes())
    domainDestination->ni.reset();        // "
    domainDestination->jbegin.reset();    // "
    domainDestination->nj.reset();        // "
   
    domainDestination->mask_1d.reset();   // defined scanning domainSource->getFullView() & domainSource->getWorkflowView() differencies
    domainDestination->mask_2d.reset();   //   in all case domainDestination->mask_1d used as reference  

    // domainDestination->data_* attributes will be computed in :
    domainDestination->data_dim.reset();     // domainDestination->checkDomainData() (from checkAttributes())
    domainDestination->data_ni.reset();
    domainDestination->data_nj.reset();
    domainDestination->data_ibegin.reset();
    domainDestination->data_jbegin.reset();
    domainDestination->data_i_index.reset(); // domainDestination->checkCompression() (from checkAttributes())
    domainDestination->data_j_index.reset();

    // Next attributes will be set using domainSource->attributes 
    domainDestination->lonvalue_1d.reset();
    domainDestination->latvalue_1d.reset();
    domainDestination->lonvalue_2d.reset();
    domainDestination->latvalue_2d.reset();
    domainDestination->nvertex.reset();
    domainDestination->bounds_lon_1d.reset();
    domainDestination->bounds_lat_1d.reset();
    domainDestination->bounds_lon_2d.reset();
    domainDestination->bounds_lat_2d.reset();
    domainDestination->area.reset();
    domainDestination->radius.reset();

    auto& index = redistributeDomain-> index ;
    
    domainDestination->type = domainSource->type ;
    domainDestination-> ni_glo = domainSource-> ni_glo ;
    domainDestination-> nj_glo = domainSource-> nj_glo ;
    
    CArray<size_t,1> globalIndex ;
    int start_i, start_j ;
    int size_i, size_j ;
    int ni_glo = domainDestination-> ni_glo ;
    int nj_glo = domainDestination-> nj_glo ;
    
       
    if (redistributeDomain->type == CRedistributeDomain::type_attr::index)
    {   
      globalIndex.resize(index.numElements()) ;
      globalIndex=index ;
    }
    else
    {
      
      auto distType = CRedistributeDomain::type_attr::column ;
      
      if (redistributeDomain->type == CRedistributeDomain::type_attr::bands && domainDestination->type == CDomain::type_attr::unstructured) 
        distType = CRedistributeDomain::type_attr::column ;
      else  distType=redistributeDomain->type;

      if (distType==CRedistributeDomain::type_attr::bands) // Bands distribution to send to file server
      {
        int nbClient = context->getIntraCommSize() ;
        int rankClient = context->getIntraCommRank() ;
      
        int size = nj_glo/nbClient ;
        int start ;
        if (rankClient < nj_glo % nbClient)
        {
         size++ ;
         start = rankClient*size ;
        }
        else start = (nj_glo % nbClient)*(size+1) + (rankClient-(nj_glo % nbClient)) * size ;

        size_i=ni_glo ; start_i=0 ;
        size_j=size ; start_j=start ;
      }
      else if (distType==CRedistributeDomain::type_attr::column) // Bands distribution to send to file server
      {
        int nbClient = context->getIntraCommSize() ;
        int rankClient = context->getIntraCommRank() ;
      
        int size = ni_glo/nbClient ;
        int start ;

        if (rankClient < ni_glo % nbClient)
        {
         size++ ;
         start = rankClient*size ;
        }
        else start = (ni_glo % nbClient)*(size+1) + (rankClient-(ni_glo % nbClient)) * size ;

        size_i=size ; start_i=start ;
        size_j=nj_glo ; start_j=0 ;
      }
      else if (distType==CRedistributeDomain::type_attr::full) // domain is not distributed ie all servers get the same local domain
      {
        size_i=ni_glo ; start_i=0 ;
        size_j=nj_glo ; start_j=0 ;
      }
      else if (distType==CRedistributeDomain::type_attr::root) // domain is not distributed ie all servers get the same local domain
      {
        if (context->getIntraCommRank()==0)
        {
          size_i=ni_glo ; start_i=0 ;
          size_j=nj_glo ; start_j=0 ;
        }
        else
        {
          size_i=0 ; start_i=0 ;
          size_j=0 ; start_j=0 ;
        }  
      }
      globalIndex.resize(size_i*size_j) ;
      size_t ind ;
      ind=0 ;
      for(int j=start_j;j<start_j+size_j;j++)
        for(int i=start_i;i<start_i+size_i;i++)
        {
         globalIndex(ind) =  j*ni_glo+i ;
         ind++ ;
        }
    }


    //domainDestination->ni = globalIndex.numElements() ;
    //domainDestination->nj = 1 ;
    domainDestination->i_index.resize(globalIndex.numElements()) ;
    domainDestination->i_index = globalIndex ;
    domainDestination->j_index.resize(globalIndex.numElements()) ;
    domainDestination->j_index = 0 ;
    if (!domainSource->nvertex.isEmpty()) domainDestination->nvertex=domainSource->nvertex ;
    if (!domainSource->radius.isEmpty()) domainDestination->radius = domainSource->radius ;
    

    auto& transMap = this->transformationMapping_;
    for(int i=0; i<globalIndex.numElements(); i++) transMap[globalIndex(i)]=globalIndex(i) ;
    
    auto elementDst=shared_ptr<CLocalElement>(new CLocalElement(context->getIntraCommRank(),domainDestination-> ni_glo*domainDestination->nj_glo, globalIndex)) ;
    elementDst->addFullView() ;

    auto transformConnector = make_shared<CTransformConnector>(domainSource->getLocalView(CElementView::FULL), elementDst->getView(CElementView::FULL),context->getIntraComm()) ;
    transformConnector->computeConnector() ;

    if (domainSource->hasLonLat)
    {
      transformConnector->transfer(domainSource->lonvalue, domainDestination->lonvalue_1d, 0.) ; // 0. -> hole
      transformConnector->transfer(domainSource->latvalue, domainDestination->latvalue_1d, 0.) ; // 0. -> hole
    }

    if (domainSource->hasBounds)
    {
      int nv ;
      if ( (domainDestination->type == CDomain::type_attr::rectilinear) && (!domainSource->nvertex.isEmpty()) ) nv=domainSource->nvertex ;
      else if (domainDestination->type == CDomain::type_attr::rectilinear) nv=2 ;
      else if (domainDestination->type == CDomain::type_attr::curvilinear) nv=4 ;
      else if (domainDestination->type == CDomain::type_attr::unstructured) nv=domainDestination->nvertex ;

      { // bounds_lon
        CArray<double,1> boundsSrc(domainSource->bounds_lonvalue.dataFirst(),shape(domainSource->bounds_lonvalue.numElements()),neverDeleteData) ;
        CArray<double,1> boundsDst ; 
        transformConnector->transfer(nv, boundsSrc, boundsDst, 0.) ; // 0. -> hole
        domainDestination->bounds_lon_1d.resize(nv,globalIndex.numElements()) ;
        domainDestination->bounds_lon_1d = CArray<double,2>(boundsDst.dataFirst(), shape(nv,globalIndex.numElements()),neverDeleteData) ;
      }
      {  // bounds_lat
        CArray<double,1> boundsSrc(domainSource->bounds_latvalue.dataFirst(),shape(domainSource->bounds_latvalue.numElements()),neverDeleteData) ;
        CArray<double,1> boundsDst ; 
        transformConnector->transfer(nv, boundsSrc, boundsDst, 0.) ; // 0. -> hole
        domainDestination->bounds_lat_1d.resize(nv,globalIndex.numElements()) ;
        domainDestination->bounds_lat_1d = CArray<double,2>(boundsDst.dataFirst(), shape(nv,globalIndex.numElements()),neverDeleteData) ;
      }
    }
    
    if (domainSource->hasArea) transformConnector->transfer(domainSource->areavalue, domainDestination->area_1d) ; 

    // transfer the mask
     auto transformMask = make_shared<CTransformConnector>(domainSource->getLocalView(CElementView::WORKFLOW), elementDst->getView(CElementView::FULL),context->getIntraComm()) ;
    transformMask->computeConnector() ;

    CArray<bool,1> workflow(domainSource->getLocalView(CElementView::WORKFLOW)->getSize()) ;
    domainDestination->mask_1d.resize(domainSource->getLocalView(CElementView::FULL)->getSize()) ;
    workflow=true ;
    
    transformMask->transfer(workflow,domainDestination->mask_1d,false) ;   
/*
    CArray<int,1> workflow(domainSource->getLocalView(CElementView::WORKFLOW)->getSize()) ;
    CArray<int,1> mask(domainSource->getLocalView(CElementView::FULL)->getSize()) ;
    workflow=1 ;
    
    transformMask->transfer(workflow,mask,0) ;
    domainDestination->mask_1d.resize(mask.numElements()) ;
    for(int i=0 ; i<mask.numElements() ; i++) 
      if (mask(i)==1) domainDestination->mask_1d(i)=true ;
      else domainDestination->mask_1d(i)=false ;
*/  

    domainDestination->checkAttributes() ;
    this->computeAlgorithm(domainSource->getLocalView(CElementView::WORKFLOW), domainDestination->getLocalView(CElementView::WORKFLOW)) ;
  }
  CATCH


}
