/*!
   \file axis_algorithm_reduce_domain.cpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for reduce a domain to an axis
 */
#include "axis_algorithm_reduce_domain.hpp"
#include "reduce_domain_to_axis.hpp"
#include "axis.hpp"
#include "domain.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "reduction.hpp"

namespace xios {
shared_ptr<CGenericAlgorithmTransformation> CAxisAlgorithmReduceDomain::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
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
  std::vector<CDomain*> domainListSrcP = gridSrc->getDomains();

  CReduceDomainToAxis* reduceDomain = dynamic_cast<CReduceDomainToAxis*> (transformation);
  int axisDstIndex   = elementPositionInGridDst2AxisPosition[elementPositionInGrid];
  int domainSrcIndex = elementPositionInGridSrc2DomainPosition[elementPositionInGrid];

  return make_shared<CAxisAlgorithmReduceDomain>(isSource, axisListDestP[axisDstIndex], domainListSrcP[domainSrcIndex], reduceDomain);
}
CATCH

bool CAxisAlgorithmReduceDomain::dummyRegistered_ = CAxisAlgorithmReduceDomain::registerTrans();
bool CAxisAlgorithmReduceDomain::registerTrans()
TRY
{
  return CGridTransformationFactory<CAxis>::registerTransformation(TRANS_REDUCE_DOMAIN_TO_AXIS, create);
}
CATCH


CAxisAlgorithmReduceDomain::CAxisAlgorithmReduceDomain(bool isSource, CAxis* axisDestination, CDomain* domainSource, CReduceDomainToAxis* algo)
 : CAlgorithmTransformationReduce(algo->getContext(), isSource), domainSrc_(domainSource), axisDest_(axisDestination)
TRY
{
  switch (algo->operation)
  {
    case CReduceDomainToAxis::operation_attr::sum:
       operator_ = EReduction::sum;
      break;
    case CReduceDomainToAxis::operation_attr::min:
       operator_ = EReduction::min;
      break;
    case CReduceDomainToAxis::operation_attr::max:
       operator_ = EReduction::max;
      break;
    case CReduceDomainToAxis::operation_attr::average:
       operator_ = EReduction::average;
      break;
    default:
        ERROR("CAxisAlgorithmReduceDomain::CAxisAlgorithmReduceDomain(CAxis* axisDestination, CDomain* domainSource, CReduceDomainToAxis* algo)",
         << "Operation is wrongly defined. Supported operations: sum, min, max, average." << std::endl
         << "Domain source " <<domainSource->getId() << std::endl
         << "Axis destination " << axisDestination->getId());

  }
  
  algo->checkValid(axisDestination, domainSource);
  dir_ = (CReduceDomainToAxis::direction_attr::iDir == algo->direction)  ? iDir : jDir;

  bool local = false;
  if (!algo->local.isEmpty()) local=algo->local ;
  
  size_t nj_glo = domainSource->nj_glo ;
  size_t ni_glo = domainSource->ni_glo ;

  bool validAxis = axisDestination->checkGeometricAttributes(false) ;
  if (validAxis && !local) 
  {
    
    axisDestination->checkAttributes() ; 
    if (dir_==jDir)
    {
        if (axisDestination->n_glo != domainSource->nj_glo)
          ERROR("CAxisAlgorithmReduceDomain::CAxisAlgorithmReduceDomain(bool isSource, CAxis* axisDestination, CDomain* domainSource, CReduceDomainToAxis* algo)",
            << "Extract domain along j, axis destination should have n_glo equal to nj_glo of domain source"
            << "Domain source " <<domainSource->getId() << " has nj_glo " << domainSource->nj_glo << std::endl
            << "Axis destination " << axisDestination->getId() << " has n_glo " << axisDestination->n_glo)
    }
    else
    {
      if (axisDestination->n_glo != domainSource->ni_glo)
          ERROR("CAxisAlgorithmReduceDomain::CAxisAlgorithmReduceDomain(bool isSource, CAxis* axisDestination, CDomain* domainSource, CReduceDomainToAxis* algo)",
            << "Extract domain along j, axis destination should have n_glo equal to ni_glo of domain source"
            << "Domain source " <<domainSource->getId() << " has ni_glo " << domainSource->ni_glo << std::endl
            << "Axis destination " << axisDestination->getId() << " has n_glo " << axisDestination->n_glo);
    }
  }
  else
  {
    // create axis destination from source domain

    axisDestination->resetGeometricAttributes();
    if (dir_== jDir)
    {
      axisDestination->n_glo = domainSource->nj_glo ;
      
      CArray<size_t, 1> srcGlobalIndex ;
      set<size_t> indexFullView;
      set<size_t> indexWorkflowView;
      domainSource->getLocalView(CElementView::FULL)->getGlobalIndexView(srcGlobalIndex) ;
      for(int i=0; i<srcGlobalIndex.numElements(); i++) indexFullView.insert(srcGlobalIndex(i)/ni_glo) ;
      domainSource->getLocalView(CElementView::WORKFLOW)->getGlobalIndexView(srcGlobalIndex) ;
      for(int i=0; i<srcGlobalIndex.numElements(); i++) indexWorkflowView.insert(srcGlobalIndex(i)/ni_glo) ;

      axisDestination-> n = indexFullView.size() ;
      axisDestination-> index.resize(axisDestination-> n) ;
      axisDestination-> mask.resize(axisDestination-> n) ;
      
      map<size_t,int> globalToLocalIndex ;
      auto it=indexFullView.begin();
      for(int i=0; it!=indexFullView.end(); ++i,++it) 
      {
        axisDestination->index(i) = *it ;
        if (indexWorkflowView.count(*it)==0) axisDestination->mask(i) = false ;
        else axisDestination->mask(i) = true ;
        globalToLocalIndex[*it] = i ;
      }
      if (domainSource->hasLonLat && domainSource->type == CDomain::type_attr::rectilinear)
      {
        axisDestination->value.resize(axisDestination-> n) ;
        axisDestination->axis_type.setValue(CAxis::axis_type_attr::Y) ;
        if (domainSource->hasBounds) axisDestination-> bounds.resize(axisDestination-> n, 2) ;

        domainSource->getLocalView(CElementView::FULL)->getGlobalIndexView(srcGlobalIndex) ;
        for(int i=0; i<srcGlobalIndex.numElements(); i++) 
        {
          axisDestination->value(globalToLocalIndex[srcGlobalIndex(i)/ni_glo]) = domainSource->latvalue(i) ;
          if (domainSource->hasBounds) 
          {
            axisDestination->bounds(globalToLocalIndex[srcGlobalIndex(i)/ni_glo],0) = domainSource->bounds_latvalue(i,0) ;
            axisDestination->bounds(globalToLocalIndex[srcGlobalIndex(i)/ni_glo],1) = domainSource->bounds_latvalue(i,1) ;
          }
        } 
      }
      
    }
    else // dir_== iDir
    {
      axisDestination->n_glo = domainSource->ni_glo ;
      
      CArray<size_t, 1> srcGlobalIndex ;
      set<size_t> indexFullView;
      set<size_t> indexWorkflowView;
      domainSource->getLocalView(CElementView::FULL)->getGlobalIndexView(srcGlobalIndex) ;
      for(int i=0; i<srcGlobalIndex.numElements(); i++) indexFullView.insert(srcGlobalIndex(i)%ni_glo) ;
      domainSource->getLocalView(CElementView::WORKFLOW)->getGlobalIndexView(srcGlobalIndex) ;
      for(int i=0; i<srcGlobalIndex.numElements(); i++) indexWorkflowView.insert(srcGlobalIndex(i)%ni_glo) ;

      axisDestination-> n = indexFullView.size() ;
      axisDestination-> index.resize(axisDestination-> n) ;
      axisDestination-> mask.resize(axisDestination-> n) ;
      map<size_t,int> globalToLocalIndex ;
      auto it=indexFullView.begin();
      for(int i=0; it!=indexFullView.end(); ++i,++it) 
      {
        axisDestination->index(i) = *it ;
        if (indexWorkflowView.count(*it)==0) axisDestination->mask(i) = false ;
        else axisDestination->mask(i) = true ;
        globalToLocalIndex[*it] = i ;
      }
    
      if (domainSource->hasLonLat && domainSource->type == CDomain::type_attr::rectilinear)
      {
        axisDestination-> value.resize(axisDestination-> n) ;
        axisDestination-> axis_type.setValue(CAxis::axis_type_attr::X) ;
        if (domainSource->hasBounds) axisDestination->bounds.resize(axisDestination-> n, 2) ;

        domainSource->getLocalView(CElementView::FULL)->getGlobalIndexView(srcGlobalIndex) ;
        for(int i=0; i<srcGlobalIndex.numElements(); i++) 
        {
          axisDestination->value(globalToLocalIndex[srcGlobalIndex(i)%ni_glo]) = domainSource->lonvalue(i) ;
          if (domainSource->hasBounds) 
          {
            axisDestination->bounds(globalToLocalIndex[srcGlobalIndex(i)%ni_glo],0) = domainSource->bounds_lonvalue(i,0) ;
            axisDestination->bounds(globalToLocalIndex[srcGlobalIndex(i)%ni_glo],1) = domainSource->bounds_lonvalue(i,1) ;
          }
        } 
      }
    
    }
   
    axisDestination->checkAttributes() ;   
  }
  
  // compute needed index for tranformation

  TransformationIndexMap& transMap = transformationMapping_;

  CArray<size_t, 1> srcGlobalIndex ;
  domainSource->getLocalView(CElementView::WORKFLOW)->getGlobalIndexView(srcGlobalIndex) ;
  CArray<size_t, 1> dstGlobalIndex ;
  axisDestination->getLocalView(CElementView::WORKFLOW)->getGlobalIndexView(dstGlobalIndex) ;
  
  if (local)
  {
    
    set<size_t> dstGlobalIndexSet ;
    for(int i=0; i<dstGlobalIndex.numElements() ; i++) dstGlobalIndexSet.insert(dstGlobalIndex(i)) ; // for mask

    size_t dstInd ; 
    for(int i=0; i<srcGlobalIndex.numElements(); i++) 
    {
      if (dir_== jDir) dstInd=srcGlobalIndex(i)/ni_glo ;
      else dstInd=srcGlobalIndex(i)%ni_glo ;
      if (dstGlobalIndexSet.count(dstInd)!=0) transMap[dstInd].push_back(srcGlobalIndex(i)) ; 
    }
  }
  else
  {
    for(int i=0; i<dstGlobalIndex.numElements() ;i++)
    {
      if (dir_== jDir) 
        for(size_t j=0; j<ni_glo;j++) transMap[dstGlobalIndex(i)].push_back(dstGlobalIndex(i)*ni_glo+j) ;
      else
        for(size_t j=0; j<nj_glo;j++) transMap[dstGlobalIndex(i)].push_back(ni_glo*j+dstGlobalIndex(i)) ;
    }
  }

/*


  TransformationIndexMap& transMap = transformationMapping_;

  CArray<int,1>& axisDstIndex = axisDest_->index;
  int ni_glo = domainSrc_->ni_glo, nj_glo = domainSrc_->nj_glo;
  if (iDir == dir_)
  {
    if (local)
    {
      const CArray<int, 1>& i_index = domainSrc_-> i_index.getValue() ;
      const CArray<int, 1>& j_index = domainSrc_-> j_index.getValue() ;
      const CArray<bool,1>& localMask = domainSrc_-> localMask ;
      int nbDomainIdx = i_index.numElements();
      
      for (int idxDomain = 0; idxDomain < nbDomainIdx; ++idxDomain)
      {
        if (localMask(idxDomain))
        { 
          transMap[j_index(idxDomain)].push_back(j_index(idxDomain)* ni_glo + i_index(idxDomain));
        }
      }
    }
    else
    {
      int nbAxisIdx = axisDstIndex.numElements();
      for (int idxAxis = 0; idxAxis < nbAxisIdx; ++idxAxis)
      {
        int globalAxisIdx = axisDstIndex(idxAxis);
        transMap[globalAxisIdx].resize(ni_glo);
        for (int idx = 0; idx < ni_glo; ++idx)
        {
          transMap[globalAxisIdx][idx] = globalAxisIdx * ni_glo + idx;
        }
      }
    }
  }
  else if (jDir == dir_)
  {
    int nbAxisIdx = axisDstIndex.numElements();
    if (local)
    {
      const CArray<int, 1>& i_index = domainSrc_-> i_index.getValue() ;
      const CArray<int, 1>& j_index = domainSrc_-> j_index.getValue() ;
      const CArray<bool,1>& localMask = domainSrc_-> localMask ;
      int nbDomainIdx = i_index.numElements();
      
      for (int idxDomain = 0; idxDomain < nbDomainIdx; ++idxDomain)
      {
        if (localMask(idxDomain))
        { 
          transMap[i_index(idxDomain)].push_back(j_index(idxDomain)* ni_glo + i_index(idxDomain));
        }
      }
    }
    else
    {
      for (int idxAxis = 0; idxAxis < nbAxisIdx; ++idxAxis)
      {
        int globalAxisIdx = axisDstIndex(idxAxis);
        transMap[globalAxisIdx].resize(nj_glo);
        for (int idx = 0; idx < nj_glo; ++idx)
        {
          transMap[globalAxisIdx][idx] = globalAxisIdx + ni_glo*idx;
        }
      }
    }
  }
  else
  {}

  axisDestination->checkAttributes() ;
*/
  this->computeAlgorithm(domainSource->getLocalView(CElementView::WORKFLOW), axisDestination->getLocalView(CElementView::WORKFLOW)) ;

}
CATCH


CAxisAlgorithmReduceDomain::~CAxisAlgorithmReduceDomain()
TRY
{
}
CATCH


}
