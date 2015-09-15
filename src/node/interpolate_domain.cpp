#include "interpolate_domain.hpp"
#include "type.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CInterpolateDomain::CInterpolateDomain(void)
    : CObjectTemplate<CInterpolateDomain>(), CInterpolateDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CInterpolateDomain::CInterpolateDomain(const StdString & id)
    : CObjectTemplate<CInterpolateDomain>(id), CInterpolateDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CInterpolateDomain::~CInterpolateDomain(void)
  {}

  //----------------------------------------------------------------

  StdString CInterpolateDomain::GetName(void)    { return StdString("interpolate_domain"); }
  StdString CInterpolateDomain::GetDefName(void) { return StdString("interpolate_domain"); }
  ENodeType CInterpolateDomain::GetType(void)    { return eInterpolateDomain; }

  void CInterpolateDomain::checkValid(CDomain* domainSrc)
  {
    int order = 2;
    if (!this->order.isEmpty()) order = this->order.getValue();
    else this->order.setValue(order);
    if (order < 1)
    {
       ERROR("void CInterpolateDomain::checkValid(CDomain* domainSrc)",
             << "Interpolation order is less than 1, it should be greater than 0."
             << "Please define a correct one") ;
    }
  }

}
