#include "pool_node.hpp"

namespace xios
{
  
  CPoolNode::CPoolNode(void) : CObjectTemplate<CPoolNode>(), CPoolNodeAttributes()
  { /* Ne rien faire de plus */ }

  CPoolNode::CPoolNode(const StdString & id) : CObjectTemplate<CPoolNode>(id), CPoolNodeAttributes()
  { /* Ne rien faire de plus */ }

  CPoolNode::~CPoolNode(void)
  { /* Ne rien faire de plus */ }


  void CPoolNode::parse(xml::CXMLNode & node)
  {
    SuperClass::parse(node);
  }


}

