#ifndef __XIOS_CExtractDomain__
#define __XIOS_CExtractDomain__

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
  class CExtractDomainGroup;
  class CExtractDomainAttributes;
  class CExtractDomain;
  class CDomain;
  class CGenericAlgorithmTransformation ;
  class CGrid;
  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CExtractDomain)
#include "extract_domain_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CExtractDomain)

  ///--------------------------------------------------------------
  /*!
    \class CExtractDomain
    This class describes extract_domain in xml file.
  */
  class CExtractDomain
    : public CObjectTemplate<CExtractDomain>
    , public CExtractDomainAttributes
    , public CTransformation<CDomain>
  {
    public :
      typedef CObjectTemplate<CExtractDomain> SuperClass;
      typedef CExtractDomainAttributes SuperClassAttribute;
      typedef CExtractDomain MyClass ;
      typedef CTransformation<CDomain> SuperTransform ;

    public :
      /// Constructeurs ///
      CExtractDomain(void);
      explicit CExtractDomain(const StdString& id);

      /// Destructeur ///
      virtual ~CExtractDomain(void);

      virtual void checkValid(CDomain* axisDest);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_EXTRACT_DOMAIN ;}
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
  }; // class CExtractDomain

  DECLARE_GROUP(CExtractDomain);
} // namespace xios

#endif // __XIOS_CExtractDomain__
