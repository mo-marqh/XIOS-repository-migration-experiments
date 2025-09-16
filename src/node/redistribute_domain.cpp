#include "redistribute_domain.hpp"
#include "domain_algorithm_redistribute.hpp"
#include "type.hpp"
#include "field.hpp"

namespace xios
{

  /// ////////////////////// Définitions ////////////////////// ///

  CRedistributeDomain::CRedistributeDomain(CContext* context)
    : CObjectTemplate<CRedistributeDomain>(context), CRedistributeDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CRedistributeDomain::CRedistributeDomain(CContext* context, const StdString & id)
    : CObjectTemplate<CRedistributeDomain>(context, id), CRedistributeDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CRedistributeDomain::~CRedistributeDomain(void)
  {}

  CTransformation<CDomain>* CRedistributeDomain::create(CContext* context, const StdString& id, xml::CXMLNode* node)
  {
    CRedistributeDomain* redistributeDomain = CRedistributeDomainGroup::get(context, "redistribute_domain_definition")->createChild(id);
    if (node) redistributeDomain->parse(*node);
    return static_cast<CTransformation<CDomain>*>(redistributeDomain);
  }

  bool CRedistributeDomain::registerTrans()
  {
    return registerTransformation(TRANS_REDISTRIBUTE_DOMAIN, {create, getTransformation});
  }

  bool CRedistributeDomain::_dummyRegistered = CRedistributeDomain::registerTrans();

  //----------------------------------------------------------------

  StdString CRedistributeDomain::GetName(void)    { return StdString("redistribute_domain"); }
  StdString CRedistributeDomain::GetDefName(void) { return StdString("redistribute_domain"); }
  ENodeType CRedistributeDomain::GetType(void)    { return eRedistributeDomain; }

  void CRedistributeDomain::checkValid(CDomain* domainSrc)
  {

  }

  shared_ptr<CGenericAlgorithmTransformation> CRedistributeDomain::createAlgorithm(bool isSource,
                                                        CGrid* gridDst, CGrid* gridSrc,
                                                        int elementPositionInGrid,
                                                        std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                        std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridDst2DomainPosition)
  {
    return CDomainAlgorithmRedistribute::create(isSource, gridDst,  gridSrc, this, elementPositionInGrid, 
                elementPositionInGridSrc2ScalarPosition, elementPositionInGridSrc2AxisPosition, elementPositionInGridSrc2DomainPosition,
                elementPositionInGridDst2ScalarPosition, elementPositionInGridDst2AxisPosition, elementPositionInGridDst2DomainPosition);
  }
}
