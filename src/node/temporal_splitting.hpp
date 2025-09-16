#ifndef __XIOS_CTemporalSplitting__
#define __XIOS_CTemporalSplitting__

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
  class CTemporalSplittingGroup;
  class CTemporalSplittingAttributes;
  class CTemporalSplitting;
  class CAxis;
  class CScalar;
  class CGenericAlgorithmTransformation ;
  class CGrid;
  class CContext ;
  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CTemporalSplitting)
#include "temporal_splitting.conf"
  END_DECLARE_ATTRIBUTE_MAP(CTemporalSplitting)

  ///--------------------------------------------------------------
  /*!
    \class CExtractDomainToAxis
    This class describes reduce_domain in xml file.
  */
  class CTemporalSplitting
    : public CObjectTemplate<CTemporalSplitting>
    , public CTemporalSplittingAttributes
    , public CTransformation<CAxis>
  {
    public :
      typedef CObjectTemplate<CTemporalSplitting> SuperClass;
      typedef CTemporalSplittingAttributes SuperClassAttribute;
      typedef CTemporalSplitting MyClass ;
      typedef CTransformation<CAxis> SuperTransform ;

    public :
      /// Constructeurs ///
      CTemporalSplitting(CContext* context);
      explicit CTemporalSplitting(CContext* context, const StdString& id);

      /// Destructeur ///
      virtual ~CTemporalSplitting(void);

      virtual void checkValid(CAxis* axisDst, CScalar* scalarSrc);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
      const string& getId(void) { return this->SuperClass::getId();}
      ETranformationType getTransformationType(void) { return TRANS_TEMPORAL_SPLITTING ;}
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
  }; // class CTemporalSplitting

  DECLARE_GROUP(CTemporalSplitting);
} // namespace xios

#endif // __XIOS_CTemporalSplitting__
