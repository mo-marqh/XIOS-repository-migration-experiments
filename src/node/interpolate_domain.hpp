#ifndef __XIOS_CInterpolateDomain__
#define __XIOS_CInterpolateDomain__

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
  class CInterpolateDomainGroup;
  class CInterpolateDomainAttributes;
  class CInterpolateDomain;
  class CDomain;
  class CGenericAlgorithmTransformation ;
  class CGrid;
  class CContext ;
  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CInterpolateDomain)
#include "interpolate_domain_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CInterpolateDomain)

  ///--------------------------------------------------------------
  /*!
    \class CInterpolateDomain
    This class describes interpolate_from_file_domain in xml file.
  */
  class CInterpolateDomain
    : public CObjectTemplate<CInterpolateDomain>
    , public CInterpolateDomainAttributes
    , public CTransformation<CDomain>
  {
    public :
      typedef CObjectTemplate<CInterpolateDomain> SuperClass;
      typedef CInterpolateDomainAttributes SuperClassAttribute;
      typedef CInterpolateDomain MyClass ;
      typedef CTransformation<CDomain> SuperTransform ;

    public :
      /// Constructeurs ///
      CInterpolateDomain(CContext* context);
      explicit CInterpolateDomain(CContext* context, const StdString& id);

      /// Destructeur ///
      virtual ~CInterpolateDomain(void);

      virtual void checkValid(CDomain* domainSource);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_INTERPOLATE_DOMAIN ;}
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
  }; // class CInterpolateDomain

  DECLARE_GROUP(CInterpolateDomain);
} // namespace xios

#endif // __XIOS_CInterpolateDomain__
