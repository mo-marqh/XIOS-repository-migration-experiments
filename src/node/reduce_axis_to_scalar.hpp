#ifndef __XIOS_CReduceAxisToScalar__
#define __XIOS_CReduceAxisToScalar__

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
  class CReduceAxisToScalarGroup;
  class CReduceAxisToScalarAttributes;
  class CReduceAxisToScalar;
  class CScalar;
  class CGenericAlgorithmTransformation ;
  class CGrid;
  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CReduceAxisToScalar)
#include "reduce_axis_to_scalar_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CReduceAxisToScalar)

  ///--------------------------------------------------------------
  /*!
    \class CReduceAxisToScalar
    This class describes reduce_axis in xml file.
  */
  class CReduceAxisToScalar
    : public CObjectTemplate<CReduceAxisToScalar>
    , public CReduceAxisToScalarAttributes
    , public CTransformation<CScalar>
  {
    public :
      typedef CObjectTemplate<CReduceAxisToScalar> SuperClass;
      typedef CReduceAxisToScalarAttributes SuperClassAttribute;
      typedef CReduceAxisToScalar MyClass ;
      typedef CTransformation<CScalar> SuperTransform ;

    public :
      /// Constructeurs ///
      CReduceAxisToScalar(void);
      explicit CReduceAxisToScalar(const StdString& id);

      /// Destructeur ///
      virtual ~CReduceAxisToScalar(void);

      virtual void checkValid(CScalar* scalarDst);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_REDUCE_AXIS_TO_SCALAR ;}
      static CTransformation<CScalar>* getTransformation(const StdString& id) { return SuperClass::get(id);}
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
      static CTransformation<CScalar>* create(const StdString& id, xml::CXMLNode* node);
      static bool _dummyRegistered;
  }; // class CReduceAxisToScalar

  DECLARE_GROUP(CReduceAxisToScalar);
} // namespace xios

#endif // __XIOS_CReduceAxisToScalar__
