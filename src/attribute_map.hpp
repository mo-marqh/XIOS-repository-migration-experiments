#ifndef __XIOS_CAttributeMap__
#define __XIOS_CAttributeMap__

/// XIOS headers ///
#include "xios_spl.hpp"
#include "attribute.hpp"
#include "attribute_template.hpp"
#include "exception.hpp"
#include "xml_node.hpp"

namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///
      class CAttributeMap
         : public xios_map<StdString, CAttribute*>
      {
            typedef xios_map<StdString, CAttribute*> SuperClassMap;

         public :

            /// Tests ///
            inline bool hasAttribute(const StdString & key) const;

            /// Accesseurs ///
            CAttribute * operator[](const StdString & key);

            /// Mutateurs ///
            void setAttribute(const StdString & key, CAttribute * const attr);

            void setAttributes(const xml::THashAttributes & attributes);
            void setAttributes(const CAttributeMap * const _parent, bool apply=true);
            void duplicateAttributes(const CAttributeMap* const _parent);
            void clearAllAttributes(void);

            void clearAttribute(const StdString& key);

            /// Destructeur ///
            virtual ~CAttributeMap(void);

            /// Flux ///
            // Debug only //
            // friend StdOStream & operator << (StdOStream & os, const CAttributeMap & attributmap);

            /// Autre ///
            virtual StdString toString(void) const;
            virtual void fromString(const StdString & str);

//            virtual void toBinary  (StdOStream & os) const;
//            virtual void fromBinary(StdIStream & is);
            virtual void generateCInterface(ostream& oss, const string& className) ;
            virtual void generateFortran2003Interface(ostream& oss, const string& className) ;
            virtual void generateFortranInterface_hdl_(ostream& oss, const string& className) ;
            virtual void generateFortranInterface_hdl(ostream& oss, const string& className) ;
            virtual void generateFortranInterface_id(ostream& oss, const string& className) ;
            virtual void generateFortranInterfaceGet_hdl_(ostream& oss, const string& className) ;
            virtual void generateFortranInterfaceGet_hdl(ostream& oss, const string& className) ;
            virtual void generateFortranInterfaceGet_id(ostream& oss, const string& className) ;
            virtual void generateFortranInterfaceIsDefined_hdl_(ostream& oss, const string& className) ;
            virtual void generateFortranInterfaceIsDefined_hdl(ostream& oss, const string& className) ;
            virtual void generateFortranInterfaceIsDefined_id(ostream& oss, const string& className) ;

         protected :

            /// Constructeurs ///
            CAttributeMap(void);
            CAttributeMap(const xios_map<StdString, CAttribute*> & umap);       // Never implemented.
            CAttributeMap(const xios_map<StdString, CAttribute*> * const umap); // Not implemented.

            /// Propriété statique ///
            static CAttributeMap * Current;

      };  // class CAttributeMap

} // namespace xios

#endif // __XIOS_CAttributeMap__
