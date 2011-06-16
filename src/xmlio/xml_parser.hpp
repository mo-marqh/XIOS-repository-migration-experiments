#ifndef __XMLIO_CXMLParser__
#define __XMLIO_CXMLParser__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "xml_node.hpp"
#include "context.hpp"

namespace xmlioserver
{
   namespace xml
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CXMLParser
      {
         public :

            static void ParseFile(const StdString & filename);
            static void ParseString(const StdString & xmlContent);
            static void ParseStream(StdIStream & stream);

      }; //class CXMLParser

   }// namespace xml
} // namespace xmlioserver

#endif // __XMLIO_CXMLParser__
