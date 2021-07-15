/*!
   \file domain_algorithm_reorder.cpp
   \brief Algorithm for reorder a domain.
 */
#include "domain_algorithm_reorder.hpp"
#include "reorder_domain.hpp"
#include "domain.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"

namespace xios {
CGenericAlgorithmTransformation* CDomainAlgorithmReorder::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
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

  CReorderDomain* reorderDomain = dynamic_cast<CReorderDomain*> (transformation);
  int domainDstIndex = elementPositionInGridDst2DomainPosition[elementPositionInGrid];
  int domainSrcIndex = elementPositionInGridSrc2DomainPosition[elementPositionInGrid];

  return (new CDomainAlgorithmReorder(isSource, domainListDestP[domainDstIndex], domainListSrcP[domainSrcIndex], reorderDomain));
}
CATCH

bool CDomainAlgorithmReorder::dummyRegistered_ = CDomainAlgorithmReorder::registerTrans();
bool CDomainAlgorithmReorder::registerTrans()
TRY
{
  return CGridTransformationFactory<CDomain>::registerTransformation(TRANS_REORDER_DOMAIN, create);
}
CATCH

CDomainAlgorithmReorder::CDomainAlgorithmReorder(bool isSource, CDomain* domainDestination, CDomain* domainSource, CReorderDomain* reorderDomain)
: CAlgorithmTransformationNoDataModification(isSource)
TRY
{
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

 
   // Set attributes for this transformation
   domainDestination->type.setValue( domainSource->type );
   
   // Keep a 2D point of view for this transformation which is intrinsically 2D
   domainDestination->ni_glo = domainSource->ni_glo;
   domainDestination->nj_glo = domainSource->nj_glo;
   // Set attributes required to define domainDestination->localElement_ and associated views, full and workflow)
   CArray<size_t,1> sourceGlobalIdx = domainSource->getLocalElement()->getGlobalIndex();
   int indexSize = sourceGlobalIdx.numElements();
   domainDestination->i_index.resize( indexSize );
   domainDestination->j_index.resize( indexSize );
   for (size_t i = 0; i < indexSize ; ++i) {
     domainDestination->i_index(i) = sourceGlobalIdx(i)%domainSource->ni_glo;
     domainDestination->j_index(i) = sourceGlobalIdx(i)/domainSource->ni_glo;
   }
   // else
   //   - domainDestination->ni_glo = domainSource->ni_glo * domainSource->nj_glo;
   //   - domainDestination->nj_glo = 1;
   //   - domainDestination->i_index = sourceGlobalIdx;
   //   - domainDestination->j_index = 0;

   // set mask_1d to enable domainMask computing (in checkMask()) 
   CArray<int,1> sourceWorkflowIdx = domainSource->getLocalView(CElementView::WORKFLOW)->getIndex();
   CArray<int,1> sourceFullIdx     = domainSource->getLocalView(CElementView::FULL    )->getIndex();
   domainDestination->mask_1d.resize( indexSize );
   int countMasked(0); // countMasked will store the offset index between full and workflow views
   for (size_t i = 0; i < indexSize ; ++i) {
     if ( sourceFullIdx(i)==sourceWorkflowIdx(i-countMasked) ) {
       domainDestination->mask_1d(i) = 1;
     }
     else {
       domainDestination->mask_1d(i) = 0;
       countMasked++;
     }
   }

   
   // Set lon/lat values
   if (!domainSource->lonvalue_1d.isEmpty() )
   {
     domainDestination->latvalue_1d.resize( domainSource->latvalue_1d.numElements() );
     domainDestination->lonvalue_1d.resize( domainSource->lonvalue_1d.numElements() );
     domainDestination->latvalue_1d = domainSource->latvalue_1d;
     domainDestination->lonvalue_1d = domainSource->lonvalue_1d;
   }
   else if (!domainSource->lonvalue_2d.isEmpty() )
   {
     domainDestination->latvalue_2d.resize( domainSource->latvalue_2d.numElements() );
     domainDestination->lonvalue_2d.resize( domainSource->lonvalue_2d.numElements() );
     domainDestination->latvalue_2d = domainSource->latvalue_2d;
     domainDestination->lonvalue_2d = domainSource->lonvalue_2d;
   }
   // Set bounds_lon/lat values
   if (!domainSource->nvertex.isEmpty() )
     domainDestination->nvertex = domainSource->nvertex;
   if (!domainSource->bounds_lon_1d.isEmpty() )
   {
     domainDestination->bounds_lon_1d.resize( domainSource->bounds_lon_1d.numElements() );
     domainDestination->bounds_lat_1d.resize( domainSource->bounds_lat_1d.numElements() );
     domainDestination->bounds_lon_1d = domainSource->bounds_lon_1d;
     domainDestination->bounds_lat_1d = domainSource->bounds_lat_1d;
   }
   else if (!domainSource->bounds_lon_2d.isEmpty() )
   {
     domainDestination->bounds_lon_2d.resize( domainSource->bounds_lon_2d.numElements() );
     domainDestination->bounds_lat_2d.resize( domainSource->bounds_lat_2d.numElements() );
     domainDestination->bounds_lon_2d = domainSource->bounds_lon_2d;
     domainDestination->bounds_lat_2d = domainSource->bounds_lat_2d;
   }
   // set area
   if (!domainSource->area.isEmpty() )
   {
     domainDestination->area.resize( domainSource->area.numElements() );
     domainDestination->area = domainSource->area;    
   }
   if (!domainSource->radius.isEmpty() )
     domainDestination->radius = domainSource->radius;

   
  reorderDomain->checkValid(domainSource);
  // domainDestination->checkAttributes() will be operated at the end of the transformation definition to define correctly domainDestination views

  if (domainSource->type !=  CDomain::type_attr::rectilinear)
  {
      ERROR("CDomainAlgorithmReorder::CDomainAlgorithmReorder(CDomain* domainDestination, CDomain* domainSource, CReorderDomain* reorderDomain)",
           << "Domain destination is not rectilinear. This filter work only for rectilinear domain and destination domain with < id = "
           <<domainDestination->getId() <<" > is of type "<<domainDestination->type<<std::endl);
  }
  
  if (domainDestination == domainSource)
  {
    ERROR("CDomainAlgorithmReorder::CDomainAlgorithmReorder(CDomain* domainDestination, CDomain* domainSource, CReorderDomain* reorderDomain)",
           << "Domain source and domain destination are the same. Please make sure domain destination refers to domain source" << std::endl
           << "Domain source " <<domainSource->getId() << std::endl
           << "Domain destination " <<domainDestination->getId() << std::endl);
  }
  
  if (!reorderDomain->invert_lat.isEmpty() && reorderDomain->invert_lat.getValue() )
  {
    CArray<int,1>& j_index=domainDestination->j_index ;
    int nglo = j_index.numElements() ;
    int nj_glo =domainDestination->nj_glo ;
    for (size_t i = 0; i < nglo ; ++i)
    {
      j_index(i)=(nj_glo-1)-j_index(i) ;
    }
  }

  if (!reorderDomain->shift_lon_fraction.isEmpty())
  {
    int ni_glo =domainDestination->ni_glo ;
    int  offset = ni_glo*reorderDomain->shift_lon_fraction ;
    CArray<int,1>& i_index=domainDestination->i_index ;
    int nglo = i_index.numElements() ;
    for (size_t i = 0; i < nglo ; ++i)
    {
      i_index(i)=  (i_index(i)+offset+ni_glo)%ni_glo ;
    }
  }

  if (!reorderDomain->min_lon.isEmpty() && !reorderDomain->max_lon.isEmpty())
  {
    double min_lon=reorderDomain->min_lon ;
    double max_lon=reorderDomain->max_lon ;
    double delta=max_lon-min_lon ;
    
    if (!domainDestination->lonvalue_1d.isEmpty() )
    {
      CArray<double,1>& lon=domainDestination->lonvalue_1d ;
      for (int i=0;i<lon.numElements();++i)
      {
        while  (lon(i) > max_lon) lon(i)=lon(i)-delta ;
        while  (lon(i) < min_lon) lon(i)=lon(i)+delta ;
      }
    }

    if (!domainDestination->bounds_lon_1d.isEmpty() )
    {
      CArray<double,2>& bounds_lon=domainDestination->bounds_lon_1d ;
      for (int i=0;i<bounds_lon.extent(0);++i)
      {
        while  (bounds_lon(0,i) > max_lon) bounds_lon(0,i)=bounds_lon(0,i)-delta ;
        while  (bounds_lon(1,i) > max_lon) bounds_lon(1,i)=bounds_lon(1,i)-delta ;

        while  (bounds_lon(0,i) < min_lon) bounds_lon(0,i)=bounds_lon(0,i)+delta ;
        while  (bounds_lon(1,i) < min_lon) bounds_lon(1,i)=bounds_lon(1,i)+delta ;
      }
    }
  }

  domainDestination->checkAttributes() ;
}
CATCH


}
