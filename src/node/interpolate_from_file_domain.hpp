#ifndef __XIOS_CInterpolateFromFileDomain__
#define __XIOS_CInterpolateFromFileDomain__

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
  class CInterpolateFromFileDomainGroup;
  class CInterpolateFromFileDomainAttributes;
  class CInterpolateFromFileDomain;
  class CDomain;

  ///--------------------------------------------------------------

  // Declare/Define CFileAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CInterpolateFromFileDomain)
#include "interpolate_from_file_domain_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CInterpolateFromFileDomain)

  ///--------------------------------------------------------------
  /*!
    \class CInterpolateFromFileDomain
    This class describes interpolate_from_file_domain in xml file.
  */
  class CInterpolateFromFileDomain
    : public CObjectTemplate<CInterpolateFromFileDomain>
    , public CInterpolateFromFileDomainAttributes
    , public CTransformation<CDomain>
  {
    public :
      typedef CObjectTemplate<CInterpolateFromFileDomain> SuperClass;
      typedef CInterpolateFromFileDomainAttributes SuperClassAttribute;

    public :
      /// Constructeurs ///
      CInterpolateFromFileDomain(void);
      explicit CInterpolateFromFileDomain(const StdString& id);

      /// Destructeur ///
      virtual ~CInterpolateFromFileDomain(void);

      virtual void checkValid(CDomain* axisDest);

      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);
    private:

  }; // class CInterpolateFromFileDomain

  DECLARE_GROUP(CInterpolateFromFileDomain);
} // namespace xios

#endif // __XIOS_CInterpolateFromFileDomain__
