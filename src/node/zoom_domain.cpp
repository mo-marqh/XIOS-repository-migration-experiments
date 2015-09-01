#include "zoom_domain.hpp"
#include "type.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CZoomDomain::CZoomDomain(void)
    : CObjectTemplate<CZoomDomain>(), CZoomDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CZoomDomain::CZoomDomain(const StdString & id)
    : CObjectTemplate<CZoomDomain>(id), CZoomDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CZoomDomain::~CZoomDomain(void)
  {}

  //----------------------------------------------------------------

  StdString CZoomDomain::GetName(void)    { return StdString("zoom_domain"); }
  StdString CZoomDomain::GetDefName(void) { return StdString("zoom_domain"); }
  ENodeType CZoomDomain::GetType(void)    { return eZoomDomain; }

  void CZoomDomain::checkValid(CDomain* domainSrc)
  {
    int ni_glo = domainSrc->ni_glo.getValue();
    int nj_glo = domainSrc->nj_glo.getValue();

    // Résolution et vérification des données globales de zoom.
    if (!this->zoom_ni.isEmpty() || !this->zoom_nj.isEmpty() ||
        !this->zoom_ibegin.isEmpty() || !this->zoom_jbegin.isEmpty())
    {
       if (this->zoom_ni.isEmpty()     || this->zoom_nj.isEmpty() ||
           this->zoom_ibegin.isEmpty() || this->zoom_jbegin.isEmpty())
       {
         ERROR("CZoomDomain::checkValid(CDomain* domainSrc)",
               << "If one of zoom attributes is defined then all zoom attributes must be defined.") ;
       }
       else
       {
          int zoom_iend = zoom_ibegin + zoom_ni - 1;
          int zoom_jend = zoom_jbegin + zoom_nj - 1;

          if (zoom_ibegin < 0  || zoom_jbegin < 0 || zoom_iend > ni_glo - 1 || zoom_jend > nj_glo - 1)
            ERROR("CZoomDomain::checkValid(CDomain* domainSrc)",
                  << "Zoom is wrongly defined, "
                  << "please check the values : 'zoom_ni' (" << zoom_ni.getValue() << "), 'zoom_nj' (" << zoom_nj.getValue() << "), "
                  << "'zoom_ibegin' (" << zoom_ibegin.getValue() << "), 'zoom_jbegin' (" << zoom_jbegin.getValue() << ")");
       }
    }
    else
    {
       zoom_ni = ni_glo;
       zoom_nj = nj_glo;
       zoom_ibegin = 0;
       zoom_jbegin = 0;
    }
  }

}
