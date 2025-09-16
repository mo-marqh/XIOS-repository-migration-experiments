#ifndef __XIOS_CServiceNode__
#define __XIOS_CServiceNode__

#include "xios_spl.hpp"
#include "attribute_enum.hpp"
#include "attribute_enum_impl.hpp"
#include "attribute_array.hpp"
#include "declare_attribute.hpp"
#include "object_template.hpp"
#include "group_factory.hpp"
#include "declare_group.hpp"


namespace xios
{
  /// ////////////////////// DÃ©clarations ////////////////////// ///

  class CServiceNodeGroup;
  class CServiceNodeAttributes;
  class CServiceNode;
  class CContext ;
  ///--------------------------------------------------------------

  // Declare/Define CVarAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CServiceNode)
#include "service_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CServiceNode)

  ///--------------------------------------------------------------

  class CServiceNode: public CObjectTemplate<CServiceNode>
               , public CServiceNodeAttributes
  {
      friend class CCServiceNodeGroup;

      /// typedef ///
      typedef CObjectTemplate<CServiceNode>   SuperClass;
      typedef CServiceNodeAttributes SuperClassAttribute;

    public :

      typedef CServiceNodeAttributes RelAttributes;
      typedef CServiceNodeGroup      RelGroup;

      /// Constructeurs ///
      CServiceNode(CContext* context) ;
      explicit CServiceNode(CContext* context, const StdString & id);
      CServiceNode(const CServiceNode & var);       // Not implemented yet.
      CServiceNode(const CServiceNode * const var); // Not implemented yet.
   
      /// Destructeur ///
      virtual ~CServiceNode(void);
     
    public :
      /// Accesseurs statiques ///
      static StdString GetName(void)     { return StdString("service"); }
      static StdString GetDefName(void)  { return StdString("service"); }
      static ENodeType GetType(void)     { return eServiceNode; }
    public:
      virtual void parse(xml::CXMLNode & node);
      void allocateRessources(const string& poolId) ;
      std::vector<CServiceNode*> servicesOnto_;

  }; // class CServiceNode
  ///--------------------------------------------------------------

  // Declare/Define CServiceNodeGroup and CServiceNodeDefinition
  DECLARE_GROUP(CServiceNode);
} // namespace xios

#endif // __XIOS_CServiceNode__
