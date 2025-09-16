#ifndef __XIOS_CReduceDomainToAxis__
#define __XIOS_CReduceDomainToAxis__

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

namespace xios {
  /// ////////////////////// Déclarations ////////////////////// ///
  class CReduceDomainToAxisGroup;
  class CReduceDomainToAxisAttributes;
  class CReduceDomainToAxis;
  class CAxis;
  class CDomain;
  class CGenericAlgorithmTransformation ;
  class CGrid;
  class CContext ;
  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CReduceDomainToAxis)
#include "reduce_domain_to_axis_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CReduceDomainToAxis)

  ///--------------------------------------------------------------
  /*!
    \class CReduceDomainToAxis
    This class describes reduce_domain in xml file.
  */
  class CReduceDomainToAxis
    : public CObjectTemplate<CReduceDomainToAxis>
    , public CReduceDomainToAxisAttributes
    , public CTransformation<CAxis>
  {
    public :
      typedef CObjectTemplate<CReduceDomainToAxis> SuperClass;
      typedef CReduceDomainToAxisAttributes SuperClassAttribute;
      typedef CReduceDomainToAxis MyClass ;
     typedef CTransformation<CAxis> SuperTransform ;

    public :
      /// Constructeurs ///
      CReduceDomainToAxis(CContext* context);
      explicit CReduceDomainToAxis(CContext* context, const StdString& id);

      /// Destructeur ///
      virtual ~CReduceDomainToAxis(void);

      virtual void checkValid(CAxis* axisDst, CDomain* domainSrc);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_REDUCE_DOMAIN_TO_AXIS ;}
      static CTransformation<CAxis>* getTransformation(CContext* context, const StdString& id) { return SuperClass::get(context, id);}
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
      static CTransformation<CAxis>* create(CContext* context, const StdString& id, xml::CXMLNode* node);
      static bool _dummyRegistered;
  }; // class CReduceDomainToAxis

  DECLARE_GROUP(CReduceDomainToAxis);
} // namespace xios

#endif // __XIOS_CReduceDomainToAxis__
