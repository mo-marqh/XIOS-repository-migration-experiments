#include "compute_connectivity_domain.hpp"
#include "domain_algorithm_compute_connectivity.hpp"
#include "type.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CComputeConnectivityDomain::CComputeConnectivityDomain(void)
    : CObjectTemplate<CComputeConnectivityDomain>(), CComputeConnectivityDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CComputeConnectivityDomain::CComputeConnectivityDomain(const StdString & id)
    : CObjectTemplate<CComputeConnectivityDomain>(id), CComputeConnectivityDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CComputeConnectivityDomain::~CComputeConnectivityDomain(void)
  {}

  CTransformation<CDomain>* CComputeConnectivityDomain::create(const StdString& id, xml::CXMLNode* node)
  {
    CComputeConnectivityDomain* compute_connectivityDomain = CComputeConnectivityDomainGroup::get("compute_connectivity_domain_definition")->createChild(id);
    if (node) compute_connectivityDomain->parse(*node);
    return static_cast<CTransformation<CDomain>*>(compute_connectivityDomain);
  }

  bool CComputeConnectivityDomain::_dummyRegistered = CComputeConnectivityDomain::registerTrans();
  bool CComputeConnectivityDomain::registerTrans()
  {
    return registerTransformation(TRANS_COMPUTE_CONNECTIVITY_DOMAIN, {create, getTransformation});
  }

  //----------------------------------------------------------------

  StdString CComputeConnectivityDomain::GetName(void)    { return StdString("compute_connectivity_domain"); }
  StdString CComputeConnectivityDomain::GetDefName(void) { return StdString("compute_connectivity_domain"); }
  ENodeType CComputeConnectivityDomain::GetType(void)    { return eComputeConnectivityDomain; }

  void CComputeConnectivityDomain::checkValid(CDomain* domainDst)
  {
    if (CDomain::type_attr::unstructured != domainDst->type)
    {
      ERROR("CComputeConnectivityDomain::checkValid(CDomain* domainDst)",
            << "Domain connectivity computation is only supported for unstructured" << std::endl
            << "Check type of domain destination, id = " << domainDst->getId());
    }

    if (type.isEmpty()) type.setValue(CComputeConnectivityDomain::type_attr::edge);
    if (n_neighbor_max.isEmpty()) n_neighbor_max.setValue(0);
    if (n_neighbor.isEmpty()) n_neighbor.resize(domainDst->i_index.numElements());
    if (local_neighbor.isEmpty()) local_neighbor.resize(1,1);
  }

  shared_ptr<CGenericAlgorithmTransformation> CComputeConnectivityDomain::createAlgorithm(bool isSource,
                                                        CGrid* gridDst, CGrid* gridSrc,
                                                        int elementPositionInGrid,
                                                        std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                        std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridDst2DomainPosition)
  {
    return CDomainAlgorithmComputeConnectivity::create(isSource, gridDst,  gridSrc, this, elementPositionInGrid,
                       elementPositionInGridSrc2ScalarPosition, elementPositionInGridSrc2AxisPosition, elementPositionInGridSrc2DomainPosition,
                       elementPositionInGridDst2ScalarPosition, elementPositionInGridDst2AxisPosition, elementPositionInGridDst2DomainPosition) ;
  }
}
