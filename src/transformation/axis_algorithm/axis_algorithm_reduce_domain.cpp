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
 : CAlgorithmTransformationReduce(isSource), domainSrc_(domainSource), axisDest_(axisDestination)
TRY
{
  algo->checkValid(axisDestination, domainSource);
  axisDestination->checkAttributes() ;
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

  dir_ = (CReduceDomainToAxis::direction_attr::iDir == algo->direction)  ? iDir : jDir;
  bool local = algo->local ;

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
  this->computeAlgorithm(domainSource->getLocalView(CElementView::WORKFLOW), axisDestination->getLocalView(CElementView::WORKFLOW)) ;

}
CATCH


CAxisAlgorithmReduceDomain::~CAxisAlgorithmReduceDomain()
TRY
{
}
CATCH


}
