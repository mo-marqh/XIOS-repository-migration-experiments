#ifndef __XIOS_CPoolNode__
#define __XIOS_CPoolNode__

#include "xios_spl.hpp"
#include "attribute_enum.hpp"
#include "attribute_enum_impl.hpp"
#include "attribute_array.hpp"
#include "declare_attribute.hpp"
#include "object_template.hpp"
#include "group_factory.hpp"
#include "declare_group.hpp"
#include "service_node.hpp"


namespace xios
{
  /// ////////////////////// Déclarations ////////////////////// ///

  class CPoolNodeGroup;
  class CPoolNodeAttributes;
  class CPoolNode;
  ///--------------------------------------------------------------

  // Declare/Define CVarAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CPoolNode)
#include "pool_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CPoolNode)

  ///--------------------------------------------------------------

  class CPoolNode: public CObjectTemplate<CPoolNode>
               , public CPoolNodeAttributes
  {
      friend class CCPoolNodeGroup;

      /// typedef ///
      typedef CObjectTemplate<CPoolNode>   SuperClass;
      typedef CPoolNodeAttributes SuperClassAttribute;

    public :

      typedef CPoolNodeAttributes RelAttributes;
      typedef CPoolNodeGroup      RelGroup;

      /// Constructeurs ///
      CPoolNode(void) ;
      explicit CPoolNode(const StdString & id);
      CPoolNode(const CPoolNode & var);       // Not implemented yet.
      CPoolNode(const CPoolNode * const var); // Not implemented yet.
    
      /// Destructeur ///
      virtual ~CPoolNode(void);
     
    public :
      /// Accesseurs statiques ///
      static StdString GetName(void)     { return StdString("pool"); }
      static StdString GetDefName(void)  { return StdString("pool"); }
      static ENodeType GetType(void)     { return ePoolNode; }
    public:
      virtual void parse(xml::CXMLNode & node);
      void allocateRessources(void) ;

    private:
      std::vector<CServiceNode*> getAllServiceNodes(void) const {return this->vServiceNodeGroup->getAllChildren();}
      CServiceNodeGroup* getVirtualServiceNodeGroup(void) const {return this->vServiceNodeGroup; }
      void setVirtualServiceNodeGroup(CServiceNodeGroup* newVServiceNodeGroup) { this->vServiceNodeGroup = newVServiceNodeGroup; }
      CServiceNodeGroup* vServiceNodeGroup;

  }; // class CPoolNode
  ///--------------------------------------------------------------

  // Declare/Define CPoolNodeGroup and CPoolNodeDefinition
  DECLARE_GROUP(CPoolNode);
} // namespace xios

#endif // __XIOS_CPoolNode__

