#ifndef __XIOS_CRedistributeDomain__
#define __XIOS_CRedistributeDomain__

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
  class CRedistributeDomainGroup;
  class CRedistributeDomainAttributes;
  class CRedistributeDomain;
  class CDomain;
  class CGenericAlgorithmTransformation ;
  class CGrid;
  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CRedistributeDomain)
#include "redistribute_domain_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CRedistributeDomain)

  ///--------------------------------------------------------------
  /*!
    \class CRedistributeDomain
    This class describes redistribute_from_file_domain in xml file.
  */
  class CRedistributeDomain
    : public CObjectTemplate<CRedistributeDomain>
    , public CRedistributeDomainAttributes
    , public CTransformation<CDomain>
  {
    public :
      typedef CObjectTemplate<CRedistributeDomain> SuperClass;
      typedef CRedistributeDomainAttributes SuperClassAttribute;
      typedef CRedistributeDomain MyClass ;
      typedef CTransformation<CDomain> SuperTransform ;

    public :
      /// Constructeurs ///
      CRedistributeDomain(void);
      explicit CRedistributeDomain(const StdString& id);

      /// Destructeur ///
      virtual ~CRedistributeDomain(void);

      virtual void checkValid(CDomain* domainSource);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_REDISTRIBUTE_DOMAIN ;}
      static CTransformation<CDomain>* getTransformation(const StdString& id) { return SuperClass::get(id);}
      virtual void inheritFrom(SuperTransform* srcTransform) { solveDescInheritance(true, this->SuperClass::get((MyClass*)srcTransform)) ;}
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
      static CTransformation<CDomain>* create(const StdString& id, xml::CXMLNode* node);
      static bool _dummyRegistered;
  }; // class CRedistributeDomain

  DECLARE_GROUP(CRedistributeDomain);
} // namespace xios

#endif // __XIOS_CRedistributeDomain__