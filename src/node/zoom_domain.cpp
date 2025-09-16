#include "zoom_domain.hpp"
#include "domain_algorithm_zoom.hpp"
#include "type.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CZoomDomain::CZoomDomain(CContext* context)
    : CObjectTemplate<CZoomDomain>(context), CZoomDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CZoomDomain::CZoomDomain(CContext* context, const StdString & id)
    : CObjectTemplate<CZoomDomain>(context, id), CZoomDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CZoomDomain::~CZoomDomain(void)
  {}

  CTransformation<CDomain>* CZoomDomain::create(CContext* context, const StdString& id, xml::CXMLNode* node)
  {
    CZoomDomain* zoomDomain = CZoomDomainGroup::get(context, "zoom_domain_definition")->createChild(id);
    if (node) zoomDomain->parse(*node);
    return static_cast<CTransformation<CDomain>*>(zoomDomain);
  }

  bool CZoomDomain::_dummyRegistered = CZoomDomain::registerTrans();
  bool CZoomDomain::registerTrans()
  {
    return registerTransformation(TRANS_ZOOM_DOMAIN, {create, getTransformation});
  }

  //----------------------------------------------------------------

  StdString CZoomDomain::GetName(void)    { return StdString("zoom_domain"); }
  StdString CZoomDomain::GetDefName(void) { return StdString("zoom_domain"); }
  ENodeType CZoomDomain::GetType(void)    { return eZoomDomain; }

  void CZoomDomain::checkValid(CDomain* domainSrc)
  {
    int ni_glo = domainSrc->ni_glo.getValue();
    int nj_glo = domainSrc->nj_glo.getValue();

    // Résolution et vérification des données globales de zoom.
    if (!this->ni.isEmpty() || !this->nj.isEmpty() ||
        !this->ibegin.isEmpty() || !this->jbegin.isEmpty())
    {
       if (this->ni.isEmpty()     || this->nj.isEmpty() ||
           this->ibegin.isEmpty() || this->jbegin.isEmpty())
       {
         ERROR("CZoomDomain::checkValid(CDomain* domainSrc)",
               << "If one of zoom attributes is defined then all zoom attributes must be defined.") ;
       }
       else
       {
          int iend = ibegin + ni - 1;
          int jend = jbegin + nj - 1;

          if (ibegin < 0  || jbegin < 0 || iend > ni_glo - 1 || jend > nj_glo - 1)
            ERROR("CZoomDomain::checkValid(CDomain* domainSrc)",
                  << "Zoom is wrongly defined, "
                  << "please check the values : 'ni' (" << ni.getValue() << "), 'nj' (" << nj.getValue() << "), "
                  << "'ibegin' (" << ibegin.getValue() << "), 'jbegin' (" << jbegin.getValue() << ")");
       }
    }
    else
    {
       ni = ni_glo;
       nj = nj_glo;
       ibegin = 0;
       jbegin = 0;
    }
  }

  shared_ptr<CGenericAlgorithmTransformation> CZoomDomain::createAlgorithm(bool isSource,
                                                        CGrid* gridDst, CGrid* gridSrc,
                                                        int elementPositionInGrid,
                                                        std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                        std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridDst2DomainPosition)
  {
    return CDomainAlgorithmZoom::create(isSource, gridDst,  gridSrc, this, elementPositionInGrid,
                       elementPositionInGridSrc2ScalarPosition, elementPositionInGridSrc2AxisPosition, elementPositionInGridSrc2DomainPosition,
                       elementPositionInGridDst2ScalarPosition, elementPositionInGridDst2AxisPosition, elementPositionInGridDst2DomainPosition);
  }
}
