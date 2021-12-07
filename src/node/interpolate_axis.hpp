#ifndef __XIOS_CInterpolateAxis__
#define __XIOS_CInterpolateAxis__

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
#include "axis.hpp"

namespace xios {
  /// ////////////////////// Déclarations ////////////////////// ///
  class CInterpolateAxisGroup;
  class CInterpolateAxisAttributes;
  class CInterpolateAxis;
  class CAxis;
  class CGenericAlgorithmTransformation ;
  class CGrid;
  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CInterpolateAxis)
#include "interpolate_axis_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CInterpolateAxis)

  ///--------------------------------------------------------------
  /*!
    \class CInterpolateAxis
    This class describes interpolate_axis in xml file.
  */
  class CInterpolateAxis
    : public CObjectTemplate<CInterpolateAxis>
    , public CInterpolateAxisAttributes
    , public CTransformation<CAxis>
  {
    public :
      typedef CObjectTemplate<CInterpolateAxis> SuperClass;
      typedef CInterpolateAxisAttributes SuperClassAttribute;
      typedef CInterpolateAxis MyClass ;
      typedef CTransformation<CAxis> SuperTransform ;

    public :
      /// Constructeurs ///
      CInterpolateAxis(void);
      explicit CInterpolateAxis(const StdString& id);

      /// Destructeur ///
      virtual ~CInterpolateAxis(void);

      virtual void checkValid(CAxis* axisDest);

      std::vector<StdString> checkAuxInputs_();

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_INTERPOLATE_AXIS ;}
      static CTransformation<CAxis>* getTransformation(const StdString& id) { return SuperClass::get(id);}
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
      static CTransformation<CAxis>* create(const StdString& id, xml::CXMLNode* node);
      static bool _dummyRegistered;
  }; // class CInterpolateAxis

  DECLARE_GROUP(CInterpolateAxis);
} // namespace xios

#endif // __XIOS_CInterpolateAxis__
