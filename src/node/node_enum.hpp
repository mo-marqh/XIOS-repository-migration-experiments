#ifndef __XMLIO_NODE_ENUM__
#define __XMLIO_NODE_ENUM__

//#define DECLARE_NODE(Name_, name_)     ,e##Name_, g##Name_
//#define DECLARE_NODE_PAR(Name_, name_) ,e##Name_, g##Name_

namespace xios
{
   namespace tree
   {
      /// ////////////////////// Définitions ////////////////////// ///
      typedef enum _node_type
      {
         Unknown = 0,
         eAxis,gAxis,
         eDomain,gDomain,
         eField,gField,
         eFile,gFile,
         eGrid,gGrid,
         eVariable,gVariable,
         eContext,gContext

//#include "node_type.conf"

      } ENodeType;

   } // namespace tree
} // namespace xios

#endif // __XMLIO_NODE_ENUM__
