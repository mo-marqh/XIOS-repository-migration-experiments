/*!
   \file axis_algorithm_reduce_domain.cpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for extract a domain to an axis
 */
#include "axis_algorithm_extract_domain.hpp"
#include "extract_domain_to_axis.hpp"
#include "axis.hpp"
#include "domain.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "reduction.hpp"

namespace xios {
CGenericAlgorithmTransformation* CAxisAlgorithmExtractDomain::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
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

  CExtractDomainToAxis* extractDomain = dynamic_cast<CExtractDomainToAxis*> (transformation);
  int axisDstIndex = elementPositionInGridDst2AxisPosition[elementPositionInGrid];
  int domainSrcIndex = elementPositionInGridSrc2DomainPosition[elementPositionInGrid];

  return (new CAxisAlgorithmExtractDomain(isSource, axisListDestP[axisDstIndex], domainListSrcP[domainSrcIndex], extractDomain));
}
CATCH

bool CAxisAlgorithmExtractDomain::dummyRegistered_ = CAxisAlgorithmExtractDomain::registerTrans();
bool CAxisAlgorithmExtractDomain::registerTrans()
TRY
{
  return CGridTransformationFactory<CAxis>::registerTransformation(TRANS_EXTRACT_DOMAIN_TO_AXIS, create);
}
CATCH


CAxisAlgorithmExtractDomain::CAxisAlgorithmExtractDomain(bool isSource, CAxis* axisDestination, CDomain* domainSource, CExtractDomainToAxis* algo)
 : CAlgorithmTransformationTransfer(isSource), pos_(-1), axisDest_(axisDestination), domainSrc_(domainSource)
TRY
{
  algo->checkValid(axisDestination, domainSource);
  StdString op = "extract";

  switch (algo->direction)
  {
    case CExtractDomainToAxis::direction_attr::jDir:
      dir_ = jDir;
      break;
    case CExtractDomainToAxis::direction_attr::iDir:
      dir_ = iDir;
      break;
    default:
      break;
  }

  pos_ = algo->position;

  auto& transMap = this->transformationMapping_;

  CArray<int,1>& axisDstIndex = axisDest_->index;
  int ni_glo = domainSrc_->ni_glo, nj_glo = domainSrc_->nj_glo;
  if (jDir == dir_)
  {
    int nbAxisIdx = axisDstIndex.numElements();
    for (int idxAxis = 0; idxAxis < nbAxisIdx; ++idxAxis)
    {
      int globalAxisIdx = axisDstIndex(idxAxis);
      transMap[globalAxisIdx] = globalAxisIdx * ni_glo + pos_;
    }
  }
  else if (iDir == dir_)
  {
    int nbAxisIdx = axisDstIndex.numElements();
    for (int idxAxis = 0; idxAxis < nbAxisIdx; ++idxAxis)
    {
      int globalAxisIdx = axisDstIndex(idxAxis);
      transMap[globalAxisIdx] = globalAxisIdx + ni_glo * pos_;
    }
  }
  else
  {}

  axisDestination->checkAttributes() ;
  this->computeAlgorithm(domainSource->getLocalView(CElementView::WORKFLOW), axisDestination->getLocalView(CElementView::WORKFLOW)) ;
}
CATCH


CAxisAlgorithmExtractDomain::~CAxisAlgorithmExtractDomain()
TRY
{
}
CATCH

}
