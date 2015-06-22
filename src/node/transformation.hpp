#ifndef __XMLIO_CTransformation__
#define __XMLIO_CTransformation__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "attribute_enum.hpp"
#include "attribute_enum_impl.hpp"
#include "attribute_array.hpp"
#include "declare_attribute.hpp"
#include "object_template.hpp"
#include "group_factory.hpp"
#include "declare_group.hpp"

namespace xios {

  /// ////////////////////// Déclarations ////////////////////// ///
  class CTransformationGroup;
  class CTransformationAttributes;
  class CTransformation;
  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CTransformation)
#include "transformation_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CTransformation)

  ///--------------------------------------------------------------
  /*!
    \class CTransformation
    This class describes transformation in xml file.
  */
  class CTransformation
    : public CObjectTemplate<CTransformation>
    , public CTransformationAttributes
  {
    public :
      enum TransformationId
      {
        TRANS_ID_ZOOM, TRANS_ID_INVERSE
      };
      typedef CObjectTemplate<CTransformation> SuperClass;
      typedef CTransformationAttributes SuperClassAttribute;

    public :
      /// Constructeurs ///
      CTransformation(void);
      explicit CTransformation(const StdString& id);

      /// Destructeur ///
      virtual ~CTransformation(void);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
    private:
      static std::vector<StdString> TransformationTypes;
      static std::vector<TransformationId> TransformationTypeIds;

    public :
      void checkAttributes();

    private:
      void checkAttributesType(TransformationId& transType);
      void checkAttributesZoomType();
      void checkAttributesInverseType();

    private:
  }; // class CTransformation

  DECLARE_GROUP(CTransformation);
} // namespace xios

#endif // __XMLIO_CTransformation__
