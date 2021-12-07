#include "reorder_domain.hpp"
#include "domain_algorithm_reorder.hpp"
#include "type.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CReorderDomain::CReorderDomain(void)
    : CObjectTemplate<CReorderDomain>(), CReorderDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CReorderDomain::CReorderDomain(const StdString & id)
    : CObjectTemplate<CReorderDomain>(id), CReorderDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CReorderDomain::~CReorderDomain(void)
  {}

  CTransformation<CDomain>* CReorderDomain::create(const StdString& id, xml::CXMLNode* node)
  {
    CReorderDomain* reorderDomain = CReorderDomainGroup::get("reorder_domain_definition")->createChild(id);
    if (node) reorderDomain->parse(*node);
    return static_cast<CTransformation<CDomain>*>(reorderDomain);
  }

  bool CReorderDomain::_dummyRegistered = CReorderDomain::registerTrans();
  bool CReorderDomain::registerTrans()
  {
    return registerTransformation(TRANS_REORDER_DOMAIN, {create, getTransformation});
  }

  //----------------------------------------------------------------

  StdString CReorderDomain::GetName(void)    { return StdString("reorder_domain"); }
  StdString CReorderDomain::GetDefName(void) { return StdString("reorder_domain"); }
  ENodeType CReorderDomain::GetType(void)    { return eReorderDomain; }

  void CReorderDomain::checkValid(CDomain* domainSrc)
  {
    
  }

  shared_ptr<CGenericAlgorithmTransformation> CReorderDomain::createAlgorithm(bool isSource,
                                                        CGrid* gridDst, CGrid* gridSrc,
                                                        int elementPositionInGrid,
                                                        std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                        std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridDst2DomainPosition)
  {
    return CDomainAlgorithmReorder::create(isSource, gridDst,  gridSrc, this, elementPositionInGrid,
                       elementPositionInGridSrc2ScalarPosition, elementPositionInGridSrc2AxisPosition, elementPositionInGridSrc2DomainPosition,
                       elementPositionInGridDst2ScalarPosition, elementPositionInGridDst2AxisPosition, elementPositionInGridDst2DomainPosition);
  }
}
