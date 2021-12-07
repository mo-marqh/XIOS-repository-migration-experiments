#include "reduce_domain_to_scalar.hpp"
#include "scalar_algorithm_reduce_domain.hpp"
#include "type.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CReduceDomainToScalar::CReduceDomainToScalar(void)
    : CObjectTemplate<CReduceDomainToScalar>(), CReduceDomainToScalarAttributes(), CTransformation<CScalar>()
  { /* Ne rien faire de plus */ }

  CReduceDomainToScalar::CReduceDomainToScalar(const StdString & id)
    : CObjectTemplate<CReduceDomainToScalar>(id), CReduceDomainToScalarAttributes(), CTransformation<CScalar>()
  { /* Ne rien faire de plus */ }

  CReduceDomainToScalar::~CReduceDomainToScalar(void)
  {}

  CTransformation<CScalar>* CReduceDomainToScalar::create(const StdString& id, xml::CXMLNode* node)
  {
    CReduceDomainToScalar* reduceDomain = CReduceDomainToScalarGroup::get("reduce_domain_to_scalar_definition")->createChild(id);
    if (node) reduceDomain->parse(*node);
    return static_cast<CTransformation<CScalar>*>(reduceDomain);
  }

  bool CReduceDomainToScalar::registerTrans()
  {
    return registerTransformation(TRANS_REDUCE_DOMAIN_TO_SCALAR, {create, getTransformation});
  }

  bool CReduceDomainToScalar::_dummyRegistered = CReduceDomainToScalar::registerTrans();

  //----------------------------------------------------------------

  StdString CReduceDomainToScalar::GetName(void)    { return StdString("reduce_domain_to_scalar"); }
  StdString CReduceDomainToScalar::GetDefName(void) { return StdString("reduce_domain_to_scalar"); }
  ENodeType CReduceDomainToScalar::GetType(void)    { return eReduceDomainToScalar; }

  void CReduceDomainToScalar::checkValid(CScalar* scalarDst, CDomain* domainSrc)
  {
    if (this->local.isEmpty()) local=false ;
  }

  shared_ptr<CGenericAlgorithmTransformation> CReduceDomainToScalar::createAlgorithm(bool isSource,
                                                        CGrid* gridDst, CGrid* gridSrc,
                                                        int elementPositionInGrid,
                                                        std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                        std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridDst2DomainPosition)
  {
    return CScalarAlgorithmReduceDomain::create(isSource, gridDst,  gridSrc, this, elementPositionInGrid,
                       elementPositionInGridSrc2ScalarPosition, elementPositionInGridSrc2AxisPosition, elementPositionInGridSrc2DomainPosition,
                       elementPositionInGridDst2ScalarPosition, elementPositionInGridDst2AxisPosition, elementPositionInGridDst2DomainPosition) ;
  }
}
