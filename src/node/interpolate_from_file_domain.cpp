#include "interpolate_from_file_domain.hpp"
#include "type.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CInterpolateFromFileDomain::CInterpolateFromFileDomain(void)
    : CObjectTemplate<CInterpolateFromFileDomain>(), CInterpolateFromFileDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CInterpolateFromFileDomain::CInterpolateFromFileDomain(const StdString & id)
    : CObjectTemplate<CInterpolateFromFileDomain>(id), CInterpolateFromFileDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CInterpolateFromFileDomain::~CInterpolateFromFileDomain(void)
  {}

  //----------------------------------------------------------------

  StdString CInterpolateFromFileDomain::GetName(void)    { return StdString("interpolate_from_file_domain"); }
  StdString CInterpolateFromFileDomain::GetDefName(void) { return StdString("interpolate_from_file_domain"); }
  ENodeType CInterpolateFromFileDomain::GetType(void)    { return eInterpolateFromFileDomain; }

  void CInterpolateFromFileDomain::checkValid(CDomain* domainSrc)
  {
  }

}
