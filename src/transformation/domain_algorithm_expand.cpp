/*!
   \file domain_algorithm_expand.cpp
   \author Ha NGUYEN
   \since 08 Aug 2016
   \date 08 Aug 2016

   \brief Algorithm for expand on an domain.
 */
#include "domain_algorithm_expand.hpp"
#include "expand_domain.hpp"
#include "mesh.hpp"
#include "domain.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"

namespace xios {
CGenericAlgorithmTransformation* CDomainAlgorithmExpand::create(CGrid* gridDst, CGrid* gridSrc,
                                                               CTransformation<CDomain>* transformation,
                                                               int elementPositionInGrid,
                                                               std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                               std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                               std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                               std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                               std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                               std::map<int, int>& elementPositionInGridDst2DomainPosition)
{
  std::vector<CDomain*> domainListDestP = gridDst->getDomains();
  std::vector<CDomain*> domainListSrcP  = gridSrc->getDomains();

  CExpandDomain* expandDomain = dynamic_cast<CExpandDomain*> (transformation);
  int domainDstIndex = elementPositionInGridDst2AxisPosition[elementPositionInGrid];
  int domainSrcIndex = elementPositionInGridSrc2AxisPosition[elementPositionInGrid];

  return (new CDomainAlgorithmExpand(domainListDestP[domainDstIndex], domainListSrcP[domainSrcIndex], expandDomain));
}

bool CDomainAlgorithmExpand::registerTrans()
{
  CGridTransformationFactory<CDomain>::registerTransformation(TRANS_EXPAND_DOMAIN, create);
}

CDomainAlgorithmExpand::CDomainAlgorithmExpand(CDomain* domainDestination,
                                               CDomain* domainSource,
                                               CExpandDomain* expandDomain)
: CDomainAlgorithmTransformation(domainDestination, domainSource)
{
  this->type_ = (ELEMENT_MODIFICATION_WITH_DATA);
  expandDomain->checkValid(domainDestination);
}

/*!
  Compute the index mapping between domain on grid source and one on grid destination
*/
void CDomainAlgorithmExpand::computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs)
{

}

}
