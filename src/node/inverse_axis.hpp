#ifndef __XIOS_CInverseAxis__
#define __XIOS_CInverseAxis__

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
  class CInverseAxisGroup;
  class CInverseAxisAttributes;
  class CInverseAxis;
  class CAxis;
  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CInverseAxis)
#include "inverse_axis_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CInverseAxis)

  ///--------------------------------------------------------------
  /*!
    \class CInverseAxis
    This class describes inverse_axis in xml file.
  */
  class CInverseAxis
    : public CObjectTemplate<CInverseAxis>
    , public CInverseAxisAttributes
    , public CTransformation<CAxis>
  {
    public :
      typedef CObjectTemplate<CInverseAxis> SuperClass;
      typedef CInverseAxisAttributes SuperClassAttribute;
      typedef CInverseAxis MyClass ;
      typedef CTransformation<CAxis> SuperTransform ;

    public :
      /// Constructeurs ///
      CInverseAxis(void);
      explicit CInverseAxis(const StdString& id);

      /// Destructeur ///
      virtual ~CInverseAxis(void);

      virtual void checkValid(CAxis* axisDest);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_INVERSE_AXIS ;}
      static CTransformation<CAxis>* getTransformation(const StdString& id) { return SuperClass::get(id);}
      virtual void inheritFrom(SuperTransform* srcTransform) { solveDescInheritance(true, this->SuperClass::get((MyClass*)srcTransform)) ;}
    private:
      static bool registerTrans();
      static CTransformation<CAxis>* create(const StdString& id, xml::CXMLNode* node);
      static bool _dummyRegistered;

  }; // class CInverseAxis

  DECLARE_GROUP(CInverseAxis);
} // namespace xios

#endif // __XIOS_CInverseAxis__
