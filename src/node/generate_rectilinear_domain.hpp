#ifndef __XIOS_CGenerateRectilinearDomain__
#define __XIOS_CGenerateRectilinearDomain__

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
  class CGenerateRectilinearDomainGroup;
  class CGenerateRectilinearDomainAttributes;
  class CGenerateRectilinearDomain;
  class CDomain;
  class CGenericAlgorithmTransformation ;
  class CGrid;
  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CGenerateRectilinearDomain)
#include "generate_rectilinear_domain_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CGenerateRectilinearDomain)

  ///--------------------------------------------------------------
  /*!
    \class CGenerateRectilinearDomain
    This class describes zoom_domain in xml file.
  */
  class CGenerateRectilinearDomain
    : public CObjectTemplate<CGenerateRectilinearDomain>
    , public CGenerateRectilinearDomainAttributes
    , public CTransformation<CDomain>
  {
    public :
      typedef CObjectTemplate<CGenerateRectilinearDomain> SuperClass;
      typedef CGenerateRectilinearDomainAttributes SuperClassAttribute;
      typedef CGenerateRectilinearDomain MyClass ;
      typedef CTransformation<CDomain> SuperTransform ;

    public :
      /// Constructeurs ///
      CGenerateRectilinearDomain(void);
      explicit CGenerateRectilinearDomain(const StdString& id);

      /// Destructeur ///
      virtual ~CGenerateRectilinearDomain(void);

      virtual void checkValid(CDomain* axisDest);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_GENERATE_RECTILINEAR_DOMAIN ;}
      static CTransformation<CDomain>* getTransformation(const StdString& id) { return SuperClass::get(id);}
      virtual void inheritFrom(SuperTransform* srcTransform) { solveDescInheritance(true, this->SuperClass::get((MyClass*)srcTransform)) ;}
      virtual CGenericAlgorithmTransformation* createAlgorithm(bool isSource,
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
  }; // class CGenerateRectilinearDomain

  DECLARE_GROUP(CGenerateRectilinearDomain);
} // namespace xios

#endif // __XIOS_CGenerateRectilinearDomain__
