#ifndef __XMLIO_CIndent__
#define __XMLIO_CIndent__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "xml_node.hpp"

namespace xmlioserver
{
   /// ////////////////////// Déclarations ////////////////////// ///
   class CIndent
   {
      public :

         /// Méthodes statiques ///
         static StdOStream & NIndent  (StdOStream & out);
         static StdOStream & IncIndent(StdOStream & out);
         static StdOStream & DecEndl  (StdOStream & out);

      private :

         /// Propriétés  statiques ///
         static unsigned int Indent;
         static StdString    Increm;
         static bool         WithLine;

   }; // class CIndent

    ///--------------------------------------------------------------
    
   class CIndentedXml
   {
      public :

         /// Méthode statique ///
         static StdString Indented(const StdString & content);

   }; // class CIndentedXml

    ///--------------------------------------------------------------

} // namespace xmlioserver

   /// ////////////////////// Macros ////////////////////// ///
   
#define NIndent   CIndent::NIndent
#define IncIndent CIndent::IncIndent
#define DecEndl   CIndent::DecEndl

#endif // __XMLIO_CIndent__
