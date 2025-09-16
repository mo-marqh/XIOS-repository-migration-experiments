#ifndef __XIOS_CZoomDomain__
#define __XIOS_CZoomDomain__

/// xios headers ///
#include "xios_spl.hpp"
#include "attribute_enum.hpp"
#include "attribute_enum_impl.hpp"
#include "attribute_array.hpp"
#include "declare_attribute.hpp"
#include "object_template.hpp"
#include "group_factory.hpp"
#include "declare_group.hpp"
#include "transformation.hpp"
#include "domain.hpp"

namespace xios {
  /// ////////////////////// Déclarations ////////////////////// ///
  class CZoomDomainGroup;
  class CZoomDomainAttributes;
  class CZoomDomain;
  class CDomain;
  class CGenericAlgorithmTransformation ;
  class CGrid;
  class CContext ;
  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CZoomDomain)
#include "zoom_domain_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CZoomDomain)

  ///--------------------------------------------------------------
  /*!
    \class CZoomDomain
    This class describes zoom_domain in xml file.
  */
  class CZoomDomain
    : public CObjectTemplate<CZoomDomain>
    , public CZoomDomainAttributes
    , public CTransformation<CDomain>
  {
    public :
      typedef CObjectTemplate<CZoomDomain> SuperClass;
      typedef CZoomDomainAttributes SuperClassAttribute;
      typedef CZoomDomain MyClass ;
      typedef CTransformation<CDomain> SuperTransform ;

    public :
      /// Constructeurs ///
      CZoomDomain(CContext* context);
      explicit CZoomDomain(CContext* context, const StdString& id);

      /// Destructeur ///
      virtual ~CZoomDomain(void);

      virtual void checkValid(CDomain* axisDest);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_ZOOM_DOMAIN ;}
      virtual void inheritFrom(SuperTransform* srcTransform) { solveDescInheritance(true, this->SuperClass::get(context_, (MyClass*)srcTransform)) ;}
      static CTransformation<CDomain>* getTransformation(CContext* context, const StdString& id) { return SuperClass::get(context, id);}
      virtual shared_ptr<CGenericAlgorithmTransformation> createAlgorithm(bool isSource,
                                                               CGrid* gridDst, CGrid* gridSrc,
                                                               int elementPositionInGrid,
                                                               std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                               std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                               std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                               std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                               std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                               std::map<int, int>& elementPositionInGridDst2DomainPosition)  ;
    private:
      static bool registerTrans();
      static CTransformation<CDomain>* create(CContext* context, const StdString& id, xml::CXMLNode* node);
     
      static bool _dummyRegistered;
  }; // class CZoomDomain

  DECLARE_GROUP(CZoomDomain);
} // namespace xios

#endif // __XIOS_CZoomDomain__
