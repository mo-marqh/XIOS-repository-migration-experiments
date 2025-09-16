#ifndef __XIOS_CReorderDomain__
#define __XIOS_CReorderDomain__

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
  class CReorderDomainGroup;
  class CReorderDomainAttributes;
  class CReorderDomain;
  class CDomain;
  class CGenericAlgorithmTransformation ;
  class CGrid;
  class CContext ;
  ///--------------------------------------------------------------

  // Declare/Define CReorderDomainAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CReorderDomain)
#include "reorder_domain_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CReorderDomain)

  ///--------------------------------------------------------------
  /*!
    \class CReorderDomain
    This class describes reorder_domain in xml file.
  */
  class CReorderDomain
    : public CObjectTemplate<CReorderDomain>
    , public CReorderDomainAttributes
    , public CTransformation<CDomain>
  {
    public :
      typedef CObjectTemplate<CReorderDomain> SuperClass;
      typedef CReorderDomainAttributes SuperClassAttribute;
      typedef CReorderDomain MyClass ;
      typedef CTransformation<CDomain> SuperTransform ;

    public :
      /// Constructeurs ///
      CReorderDomain(CContext* context);
      explicit CReorderDomain(CContext* context, const StdString& id);

      /// Destructeur ///
      virtual ~CReorderDomain(void);

      virtual void checkValid(CDomain* domainSrc);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_REORDER_DOMAIN ;}
      static CTransformation<CDomain>* getTransformation(CContext* context, const StdString& id) { return SuperClass::get(context, id);}
      virtual void inheritFrom(SuperTransform* srcTransform) { solveDescInheritance(true, this->SuperClass::get(context_, (MyClass*)srcTransform)) ;}
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
  }; // class CReorderDomain

  DECLARE_GROUP(CReorderDomain);
} // namespace xios

#endif // __XIOS_CReorderDomain__
