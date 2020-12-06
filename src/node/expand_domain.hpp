#ifndef __XIOS_CExpandDomain__
#define __XIOS_CExpandDomain__

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
  class CExpandDomainGroup;
  class CExpandDomainAttributes;
  class CExpandDomain;
  class CDomain;

  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CExpandDomain)
#include "expand_domain_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CExpandDomain)

  ///--------------------------------------------------------------
  /*!
    \class CExpandDomain
    This class describes expand_domain in xml file.
  */
  class CExpandDomain
    : public CObjectTemplate<CExpandDomain>
    , public CExpandDomainAttributes
    , public CTransformation<CDomain>
  {
    public :
      typedef CObjectTemplate<CExpandDomain> SuperClass;
      typedef CExpandDomainAttributes SuperClassAttribute;
      typedef CExpandDomain MyClass ;
      typedef CTransformation<CDomain> SuperTransform ;

    public :
      /// Constructeurs ///
      CExpandDomain(void);
      explicit CExpandDomain(const StdString& id);

      /// Destructeur ///
      virtual ~CExpandDomain(void);

      virtual void checkValid(CDomain* domain);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_EXPAND_DOMAIN ;}
      static CTransformation<CDomain>* getTransformation(const StdString& id) { return SuperClass::get(id);}
      virtual void inheritFrom(SuperTransform* srcTransform) { solveDescInheritance(true, this->SuperClass::get((MyClass*)srcTransform)) ;}
    private:
      static bool registerTrans();
      static CTransformation<CDomain>* create(const StdString& id, xml::CXMLNode* node);
      static bool _dummyRegistered;
  }; // class CExpandDomain

  DECLARE_GROUP(CExpandDomain);
} // namespace xios

#endif // __XIOS_CExpandDomain__
