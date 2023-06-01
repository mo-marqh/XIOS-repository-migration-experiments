#ifndef __XIOS_CRedistributeScalar__
#define __XIOS_CRedistributeScalar__

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
#include "scalar.hpp"

namespace xios {
  /// ////////////////////// Déclarations ////////////////////// ///
  class CRedistributeScalarGroup;
  class CRedistributeScalarAttributes;
  class CRedistributeScalar;
  class CScalar;
  class CGenericAlgorithmTransformation ;
  class CGrid;
  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CRedistributeScalar)
#include "redistribute_scalar_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CRedistributeScalar)

  ///--------------------------------------------------------------
  /*!
    \class CRedistributeScalar
    This class describes redistribute_from_file_scalar in xml file.
  */
  class CRedistributeScalar
    : public CObjectTemplate<CRedistributeScalar>
    , public CRedistributeScalarAttributes
    , public CTransformation<CScalar>
  {
    public :
      typedef CObjectTemplate<CRedistributeScalar> SuperClass;
      typedef CRedistributeScalarAttributes SuperClassAttribute;
      typedef CRedistributeScalar MyClass ;
      typedef CTransformation<CScalar> SuperTransform ;

    public :
      /// Constructeurs ///
      CRedistributeScalar(void);
      explicit CRedistributeScalar(const StdString& id);

      /// Destructeur ///
      virtual ~CRedistributeScalar(void);

      virtual void checkValid(CScalar* scalarSource);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_REDISTRIBUTE_SCALAR ;}
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
  }; // class CRedistributeScalar

  DECLARE_GROUP(CRedistributeScalar);
} // namespace xios

#endif // __XIOS_CRedistributeScalar__