#ifndef __XIOS_CRedistributeAxis__
#define __XIOS_CRedistributeAxis__

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

namespace xios 
{
  /// ////////////////////// Déclarations ////////////////////// ///
  class CRedistributeAxisGroup;
  class CRedistributeAxisAttributes;
  class CRedistributeAxis;
  class CAxis;
  class CGenericAlgorithmTransformation ;
  class CGrid;
  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CRedistributeAxis)
#include "redistribute_axis_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CRedistributeAxis)

  ///--------------------------------------------------------------
  /*!
    \class CInterpolateAxis
    This class describes interpolate_axis in xml file.
  */
  class CRedistributeAxis
    : public CObjectTemplate<CRedistributeAxis>
    , public CRedistributeAxisAttributes
    , public CTransformation<CAxis>
  {
    public :
      typedef CObjectTemplate<CRedistributeAxis> SuperClass;
      typedef CRedistributeAxisAttributes SuperClassAttribute;
      typedef CRedistributeAxis MyClass ;
      typedef CTransformation<CAxis> SuperTransform ;

    public :
      /// Constructeurs ///
      CRedistributeAxis(void);
      explicit CRedistributeAxis(const StdString& id);

      /// Destructeur ///
      virtual ~CRedistributeAxis(void);

      virtual void checkValid(CAxis* axisDest);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_REDISTRIBUTE_AXIS ;}
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
  }; // class CRedistributeAxis

  DECLARE_GROUP(CRedistributeAxis);
} // namespace xios

#endif // __XIOS_CRedistributeAxis__