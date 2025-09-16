#ifndef __XIOS_CReduceAxisToAxis__
#define __XIOS_CReduceAxisToAxis__

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
  class CReduceAxisToAxisGroup;
  class CReduceAxisToAxisAttributes;
  class CReduceAxisToAxis;
  class CAxis;
  class CGenericAlgorithmTransformation ;
  class CGrid;
  class CContext ;
  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CReduceAxisToAxis)
#include "reduce_axis_to_axis_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CReduceAxisToAxis)

  ///--------------------------------------------------------------
  /*!
    \class CReduceAxisToAxis
    This class describes reduce_domain in xml file.
  */
  class CReduceAxisToAxis
    : public CObjectTemplate<CReduceAxisToAxis>
    , public CReduceAxisToAxisAttributes
    , public CTransformation<CAxis>
  {
    public :
      typedef CObjectTemplate<CReduceAxisToAxis> SuperClass;
      typedef CReduceAxisToAxisAttributes SuperClassAttribute;
      typedef CReduceAxisToAxis MyClass ;
      typedef CTransformation<CAxis> SuperTransform ;

    public :
      /// Constructeurs ///
      CReduceAxisToAxis(CContext* context);
      explicit CReduceAxisToAxis(CContext* context, const StdString& id);

      /// Destructeur ///
      virtual ~CReduceAxisToAxis(void);

      virtual void checkValid(CAxis* axisDst, CAxis* axisSrc);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_REDUCE_AXIS_TO_AXIS ;}
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
  }; // class CReduceAxisToAxis

  DECLARE_GROUP(CReduceAxisToAxis);
} // namespace xios

#endif // __XIOS_CReduceAxisToAxis__
