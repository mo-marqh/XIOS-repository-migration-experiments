/*!
   \file scalar_algorithm_reduce_domain.cpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for reduce a domain to a scalar
 */
#include "scalar_algorithm_reduce_domain.hpp"
#include "domain.hpp"
#include "scalar.hpp"
#include "reduce_domain_to_scalar.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"

namespace xios {
shared_ptr<CGenericAlgorithmTransformation> CScalarAlgorithmReduceDomain::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
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
  std::vector<CDomain*> domainListSrcP  = gridSrc->getDomains();

  CReduceDomainToScalar* reduceDomain = dynamic_cast<CReduceDomainToScalar*> (transformation);
  int scalarDstIndex = elementPositionInGridDst2ScalarPosition[elementPositionInGrid];
  int domainSrcIndex = elementPositionInGridSrc2DomainPosition[elementPositionInGrid];

  return make_shared<CScalarAlgorithmReduceDomain>(isSource, scalarListDestP[scalarDstIndex], domainListSrcP[domainSrcIndex], reduceDomain);
}
CATCH

bool CScalarAlgorithmReduceDomain::dummyRegistered_ = CScalarAlgorithmReduceDomain::registerTrans();
bool CScalarAlgorithmReduceDomain::registerTrans()
TRY
{
  return CGridTransformationFactory<CScalar>::registerTransformation(TRANS_REDUCE_DOMAIN_TO_SCALAR, create);
}
CATCH

CScalarAlgorithmReduceDomain::CScalarAlgorithmReduceDomain(bool isSource, CScalar* scalarDestination, CDomain* domainSource, CReduceDomainToScalar* algo)
 : CAlgorithmTransformationReduce(isSource), domainSrc_(domainSource)
TRY
{
  algo->checkValid(scalarDestination, domainSource);
  
  StdString op;
  switch (algo->operation)
  {
    case CReduceDomainToScalar::operation_attr::sum:
      operator_ = EReduction::sum;
      break;
    case CReduceDomainToScalar::operation_attr::min:
      operator_ = EReduction::min;
      break;
    case CReduceDomainToScalar::operation_attr::max:
      operator_ = EReduction::max;
      break;
    case CReduceDomainToScalar::operation_attr::average:
      operator_ = EReduction::average;
      break;
    default:
        ERROR("CScalarAlgorithmReduceDomain::CScalarAlgorithmReduceDomain(CDomain* domainDestination, CDomain* domainSource, CReduceDomainToScalar* algo)",
         << "Operation must be defined."
         << "Domain source " <<domainSource->getId() << std::endl
         << "Scalar destination " << scalarDestination->getId());

  }

  TransformationIndexMap& transMap = this->transformationMapping_;

  int ni_glo = domainSrc_->ni_glo ;
  int nj_glo = domainSrc_->nj_glo ;
  int nbDomainIdx ;
  
  bool  local = algo->local ;
  
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
          transMap[0].push_back(j_index(idxDomain)* ni_glo + i_index(idxDomain));
        }
      }
  }
  else
  {  
    nbDomainIdx = ni_glo * nj_glo;
    for (int idxDomain = 0; idxDomain < nbDomainIdx; ++idxDomain) transMap[0].push_back(idxDomain);    
  }
  
  scalarDestination->checkAttributes() ;
  this->computeAlgorithm(domainSource->getLocalView(CElementView::WORKFLOW), scalarDestination->getLocalView(CElementView::WORKFLOW)) ;
}
CATCH



CScalarAlgorithmReduceDomain::~CScalarAlgorithmReduceDomain()
TRY
{
}
CATCH


}
