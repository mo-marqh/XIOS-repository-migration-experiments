#ifndef __XIOS_ATTRIBUTE_ARRAY__
#define __XIOS_ATTRIBUTE_ARRAY__

/// XIOS headers ///
#include "xios_spl.hpp"
#include "exception.hpp"
#include "attribute.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "array_new.hpp"

namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///
      /*!
        \class CAttributeArray
        This class implements the attribute representing array of value
      */
      template <typename T_numtype, int N_rank>
         class CAttributeArray : public CAttribute, public CArray<T_numtype, N_rank>
      {
        public :
            using CArray<T_numtype,N_rank>::operator = ;

            /// Constructeurs ///
            explicit CAttributeArray(const StdString & id);
            CAttributeArray(const StdString & id, xios_map<StdString, CAttribute*> & umap);
            CAttributeArray(const StdString & id, const CArray<T_numtype, N_rank>& value);
            CAttributeArray(const StdString & id, const CArray<T_numtype, N_rank>& value,
                           xios_map<StdString, CAttribute*> & umap);

            /// Accesseur ///
            CArray<T_numtype, N_rank> getValue(void) const;

            /// Mutateurs ///
            void setValue(const CArray<T_numtype, N_rank>& value);
            void set(const CAttribute& attr) ;
            void set(const CAttributeArray& attr) ;
            void reset(void) ;
            void setInheritedValue(const CAttributeArray& attr );
            void setInheritedValue(const CAttribute& attr );
            CArray<T_numtype, N_rank> getInheritedValue(void) const ;
            bool hasInheritedValue(void) const;
                        
            bool isEqual(const CAttribute& attr);

            /// Destructeur ///
            virtual ~CAttributeArray(void) { }


            /// Autre ///
            virtual string toString(void) const { return _toString();}
            virtual void fromString(const StdString & str) { if (str==resetInheritanceStr) { reset(); _canInherite=false ;}  else _fromString(str);}
            virtual bool toBuffer  (CBufferOut& buffer) const { return _toBuffer(buffer);}
            virtual bool fromBuffer(CBufferIn& buffer) { return _fromBuffer(buffer); }
            virtual string dump(void) const { return _dump();}

            virtual void generateCInterface(ostream& oss,const string& className) ;
            virtual void generateFortran2003Interface(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceDeclaration_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceBody_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceDeclaration(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceGetDeclaration_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceGetBody_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceGetDeclaration(ostream& oss,const string& className) ;
 
 static int show_TV_ttf_display_type ( const CAttributeArray<T_numtype,N_rank>* array )
      {
        int status ;
        if (array->isEmpty()) 
        {
          status = TV_ttf_add_row("State", TV_ttf_type_ascii_string,"(empty)") ;
          if (status != TV_ttf_ec_ok) return TV_ttf_format_raw ;
          else return TV_ttf_format_ok_elide ;
        }
        else 
        {
          char tname[128] ;
          char bname[128] = "value_type" ;
#ifdef __GNUC__
         size_t size = sizeof(bname) ;
         abi::__cxa_demangle(typeid(T_numtype).name(), bname, &size, &status) ;
         if (status !=0) return TV_ttf_format_raw ;
#endif
          int dim = array->dimensions() ;
          if (dim==1) snprintf (tname, sizeof(tname), "%s[%d]", bname, array->extent(0));
          if (dim==2) snprintf (tname, sizeof(tname), "%s[%d][%d]", bname, array->extent(1), array->extent(0));
          if (dim==3) snprintf (tname, sizeof(tname), "%s[%d][%d][%d]", bname, array->extent(2), array->extent(1), array->extent(3));
          if (dim==4) snprintf (tname, sizeof(tname), "%s[%d][%d][%d][%d]", bname, array->extent(0), array->extent(1), array->extent(2), array->extent(3));
          if (dim==5) snprintf (tname, sizeof(tname), "%s[%d][%d][%d][%d][%d]", bname, array->extent(4), array->extent(3), array->extent(2), array->extent(1)
                                                                                      ,array->extent(0));
          if (dim==6) snprintf (tname, sizeof(tname), "%s[%d][%d][%d][%d][%d][%d]", bname, array->extent(5), array->extent(4), array->extent(3), array->extent(2)
                                                                                      ,array->extent(1),array->extent(0));
          if (dim==7) snprintf (tname, sizeof(tname), "%s[%d][%d][%d][%d][%d][%d][%d]", bname, array->extent(6), array->extent(5), array->extent(4), array->extent(3)
                                                                                      ,array->extent(2),array->extent(1),array->extent(0));
          status = TV_ttf_add_row("array_values", tname, array->dataFirst()) ;
          if (status != TV_ttf_ec_ok) return TV_ttf_format_raw ;
          else return TV_ttf_format_ok ;
        }
      }
      
            
            static int TV_ttf_display_type ( const  CAttributeArray<T_numtype,N_rank>* array )
           {
             return CAttributeArray<T_numtype,N_rank>::show_TV_ttf_display_type (array) ;
           }

         private :
          bool isEqual_(const CAttributeArray& attr);
          CArray<T_numtype, N_rank> inheritedValue ;
          StdString _toString(void) const;
          StdString _dump(void) const;
          void _fromString(const StdString & str);
          bool _toBuffer  (CBufferOut& buffer) const;
          bool _fromBuffer(CBufferIn& buffer) ;
      }; // class CAttributeEnum


#define macrotyperank(_TYPE_,_RANK_)\
  template<> int CAttributeArray<_TYPE_,_RANK_>::TV_ttf_display_type(const CAttributeArray<_TYPE_,_RANK_>* array );

#define macrotype(_TYPE_)\
macrotyperank(_TYPE_,1)\
macrotyperank(_TYPE_,2)\
macrotyperank(_TYPE_,3)\
macrotyperank(_TYPE_,4)\
macrotyperank(_TYPE_,5)\
macrotyperank(_TYPE_,6)\
macrotyperank(_TYPE_,7)

macrotype(double)
macrotype(int)
macrotype(bool)
macrotype(size_t)
macrotype(float)
macrotype(string)

#undef macrotyperank
#undef macrotype
} // namespace xios

#endif // __XIOS_ATTRIBUTE_ARRAY__
