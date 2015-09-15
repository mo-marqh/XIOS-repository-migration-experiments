#include "generate_rectilinear_domain.hpp"
#include "type.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CGenerateRectilinearDomain::CGenerateRectilinearDomain(void)
    : CObjectTemplate<CGenerateRectilinearDomain>(), CGenerateRectilinearDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CGenerateRectilinearDomain::CGenerateRectilinearDomain(const StdString & id)
    : CObjectTemplate<CGenerateRectilinearDomain>(id), CGenerateRectilinearDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CGenerateRectilinearDomain::~CGenerateRectilinearDomain(void)
  {}

  //----------------------------------------------------------------

  StdString CGenerateRectilinearDomain::GetName(void)    { return StdString("generate_rectilinear_domain"); }
  StdString CGenerateRectilinearDomain::GetDefName(void) { return StdString("generate_rectilinear_domain"); }
  ENodeType CGenerateRectilinearDomain::GetType(void)    { return eGenerateRectilinearDomain; }

  void CGenerateRectilinearDomain::checkValid(CDomain* domainSrc)
  {
  }

}
