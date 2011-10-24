#ifndef __XMLIO_CTreeManager__
#define __XMLIO_CTreeManager__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "node_type.hpp"
#include "xml_parser.hpp"
#include "indent.hpp"

#undef  DECLARE_ATTRIBUTE
#define DECLARE_ATTRIBUTE(type, name)  , type * name = NULL

namespace xmlioserver
{
   namespace tree
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CTreeManager
      {
         public :

            /// Mutateurs ///
            static void SetCurrentContextId(const StdString & id);
            static boost::shared_ptr<CContext> CreateContext(const StdString & id = StdString(""));
            
            template<typename ObjType, typename AttType> // Pas encore implémenté
               static void SetAttribute(ObjType & obj, const StdString & attname, const AttType & attvalue);
            template<typename ObjType, typename AttType> // Pas encore implémenté
               static void SetAttribute(const StdString & objname, const StdString & attname, const AttType & attvalue);

            /// Sortie ///
            static void PrintTreeToFile(const StdString & path);
            static void PrintTreeToString(StdString & content);
            static void PrintTreeToStream(StdOStream & out);

            /// Parsing ///
            static void ParseFile  (const StdString & filename);
            static void ParseString(const StdString & xmlContent);
            static void ParseStream(StdIStream & stream);
            
            /// Binaire ///
            static void ToBinary  (StdOStream & os);
            static void FromBinary(StdIStream & is);
            static void FromBinary(StdString & str);
            
            static void DomainsToBinary  (StdOStream & os);
            static void DomainsFromBinary(StdIStream & is);
            static void DomainsFromBinary(StdString & str);

      }; // class CTreeManager

   } // namespace tree
} // namespace xmlioserver

#endif // __XMLIO_CTreeManager__

