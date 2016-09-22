#include "compute_connectivity_domain.hpp"
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
    registerTransformation(TRANS_COMPUTE_CONNECTIVITY_DOMAIN, CComputeConnectivityDomain::create);
  }

  //----------------------------------------------------------------

  StdString CComputeConnectivityDomain::GetName(void)    { return StdString("compute_connectivity_domain"); }
  StdString CComputeConnectivityDomain::GetDefName(void) { return StdString("compute_connectivity_domain"); }
  ENodeType CComputeConnectivityDomain::GetType(void)    { return eComputeConnectivityDomain; }

  void CComputeConnectivityDomain::checkValid(CDomain* domainDst)
  {
    if (type.isEmpty())
      ERROR("CComputeConnectivityDomain::checkValid(CDomain* domainDst)",
       << "Connectivity type is not defined. Chose 'node' or 'edge' for the type."
       << "Domain " <<domainDst->getId() << std::endl
       << "Connectivity object " << this->getId());

    if (n_neighbor_max.isEmpty()) n_neighbor_max.setValue(0);
    if (n_neighbor.isEmpty()) n_neighbor.resize(domainDst->i_index.numElements());
    if (local_neighbor.isEmpty()) local_neighbor.resize(1,1);
  }

}
