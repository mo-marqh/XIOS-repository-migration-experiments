#ifndef __XIOS_CComputeConnectivityDomain__
#define __XIOS_CComputeConnectivityDomain__

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
  class CComputeConnectivityDomainGroup;
  class CComputeConnectivityDomainAttributes;
  class CComputeConnectivityDomain;
  class CDomain;
  class CGenericAlgorithmTransformation ;
  class CGrid;

  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CComputeConnectivityDomain)
#include "compute_connectivity_domain_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CComputeConnectivityDomain)

  ///--------------------------------------------------------------
  /*!
    \class CComputeConnectivityDomain
    This class describes zoom_domain in xml file.
  */
  class CComputeConnectivityDomain
    : public CObjectTemplate<CComputeConnectivityDomain>
    , public CComputeConnectivityDomainAttributes
    , public CTransformation<CDomain>
  {
    public :
      typedef CObjectTemplate<CComputeConnectivityDomain> SuperClass;
      typedef CComputeConnectivityDomainAttributes SuperClassAttribute;
      typedef CComputeConnectivityDomain MyClass ;
      typedef CTransformation<CDomain> SuperTransform ;

    public :
      /// Constructeurs ///
      CComputeConnectivityDomain(void);
      explicit CComputeConnectivityDomain(const StdString& id);

      /// Destructeur ///
      virtual ~CComputeConnectivityDomain(void);

      virtual void checkValid(CDomain* domain);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_COMPUTE_CONNECTIVITY_DOMAIN ;}
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
    public:
   }; // class CComputeConnectivityDomain

  DECLARE_GROUP(CComputeConnectivityDomain);
} // namespace xios

#endif // __XIOS_CComputeConnectivityDomain__
