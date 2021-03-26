#ifndef __XIOS_CAttributeTemplate__
#define __XIOS_CAttributeTemplate__

/// boost headers ///
#include <boost/lexical_cast.hpp>

/// XIOS headers ///
#include "xios_spl.hpp"
#include "exception.hpp"
#include "attribute.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "type.hpp"
#include "tv_data_display.h"

#ifdef __GNUC__
#include <typeinfo>
#include <cxxabi.h>
#endif

namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///
  /*!
    \class CAttributeTemplate
    The class implements attribute of some basic types
  */
       template <class T>
         class CAttributeTemplate : public CAttribute, public CType<T>
      {
            typedef CAttribute SuperClass;

         public :

            /// Typedef ///
            typedef T ValueType;

            /// Constructeurs ///
            explicit CAttributeTemplate(const StdString & id);
            CAttributeTemplate(const StdString & id,
                               xios_map<StdString, CAttribute*> & umap);
            CAttributeTemplate(const StdString & id, const ValueType & value);
            CAttributeTemplate(const StdString & id, const ValueType & value,
                               xios_map<StdString, CAttribute*> & umap);
//            CAttributeTemplate(const CAttribute & attribut) throw (CException);
//            CAttributeTemplate(const CAttribute * const attribut); // Not implemented.

          public :

            /// Accesseur ///
            ValueType getValue(void) const;
//            ValueType* getRef(void) ;

            /// Mutateurs ///
            void setValue(const ValueType & value);

            void set(const CAttribute& attr) ;
            void set(const CAttributeTemplate& attr) ;
            void reset(void) ;
            void checkEmpty(void) const;


            void setInheritedValue(const CAttributeTemplate& attr );
            void setInheritedValue(const CAttribute& attr );
            T getInheritedValue(void) const ;
            bool hasInheritedValue(void) const;
            
            bool isEqual(const CAttribute& attr );

            /// Destructeur ///
            virtual ~CAttributeTemplate(void) { }

            /// Operateur ///
            CAttributeTemplate& operator=(const ValueType & value);

            /// Autre ///
            virtual StdString toString(void) const { return _toString();}
            virtual void fromString(const StdString & str) { if (str==resetInheritanceStr) { reset(); _canInherite=false ;}  else _fromString(str);}
//            virtual CAttributeTemplate* clone() const {}
//            virtual void toBinary  (StdOStream & os) const;
//            virtual void fromBinary(StdIStream & is);
            virtual StdString dump(void) const { return _dump();}

            virtual bool toBuffer  (CBufferOut& buffer) const { return _toBuffer(buffer);}
            virtual bool fromBuffer(CBufferIn& buffer) { return _fromBuffer(buffer); }
//            virtual size_t size(void) const;
            virtual void generateCInterface(ostream& oss,const string& className) ;
            virtual void generateFortran2003Interface(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceDeclaration_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceBody_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceDeclaration(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceGetDeclaration_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceGetBody_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceGetDeclaration(ostream& oss,const string& className) ;
//            virtual void generateFortranInterfaceIsDefinedDeclaration_(ostream& oss,const string& className) ;
//            virtual void generateFortranInterfaceIsDefinedBody_(ostream& oss,const string& className) ;
//            virtual void generateFortranInterfaceIsDefinedDeclaration(ostream& oss,const string& className) ;

           static int show_TV_ttf_display_type ( const CAttributeTemplate<T>* attr)
           {
             int status ;
             if (attr->isEmpty()) 
             {
               status = TV_ttf_add_row("State", TV_ttf_type_ascii_string,"(empty)") ;
               if (status != TV_ttf_ec_ok) return TV_ttf_format_raw ;
               else return TV_ttf_format_ok_elide ;
             }
             else 
             {
               char tname[128] ;
               char bname[128] = "ValueType" ;
#ifdef __GNUC__
               size_t size = sizeof(bname) ;
               abi::__cxa_demangle(typeid(T).name(), bname, &size, &status) ;
               if (status !=0) return TV_ttf_format_raw ;
#endif
               snprintf (tname, sizeof(tname), "%s", bname);
               if (typeid(T)==typeid(string))
                 status = TV_ttf_add_row("values", TV_ttf_type_ascii_string, ((string*)(attr->ptrValue))->c_str() );
               else status = TV_ttf_add_row("values", tname, attr->ptrValue) ;
               if (status != TV_ttf_ec_ok) return TV_ttf_format_raw ;
               else return TV_ttf_format_ok_elide ;
             }
           }
      
           static int TV_ttf_display_type ( const CAttributeTemplate<T>* attr )
           {
             return show_TV_ttf_display_type (attr) ;
           }


         protected :

            /// Constructeurs ///
//            CAttributeTemplate(void); // Not implemented.
         private :
          bool isEqual_(const CAttributeTemplate& attr);
          StdString _toString(void) const;
          StdString _dump(void) const;
          void _fromString(const StdString & str);
          bool _toBuffer  (CBufferOut& buffer) const;
          bool _fromBuffer(CBufferIn& buffer) ;

          CType<T> inheritedValue ;
      }; // class CAttribute

#define macrotype(_TYPE_)\
  template<> int CAttributeTemplate<_TYPE_>::TV_ttf_display_type( const CAttributeTemplate<_TYPE_>* attr );

macrotype(double)
macrotype(int)
macrotype(bool)
macrotype(string)
//macrotype(CDate)
//macrotype(CDuration)
#undef macrotype

   template <class T>  void FromBinary(StdIStream & is, T & obj);


} // namespace xios

#endif // __XIOS_CAttributeTemplate__
