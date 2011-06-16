#ifndef __XMLIO_CAttributeTemplate__
#define __XMLIO_CAttributeTemplate__

/// boost headers ///
#include <boost/lexical_cast.hpp>

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "array.hpp"
#include "attribute.hpp"

namespace xmlioserver
{
   namespace tree
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      template <class T>
         class CAttributeTemplate : public CAttribute
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
            CAttributeTemplate(const CAttribute & attribut) throw (CException);
            CAttributeTemplate(const CAttribute * const attribut); // Not implemented.

          public :

            /// Accesseur ///
            inline ValueType getValue(void) const;

            /// Mutateurs ///
            inline void setValue(const ValueType & value);

            /// Destructeur ///
            virtual ~CAttributeTemplate(void);

            /// Operateur ///
            ValueType operator=(const ValueType & value);

            /// Autre ///
            virtual StdString toString(void) const;
            virtual void fromString(const StdString & str);

            virtual void toBinary  (StdOStream & os) const;
            virtual void fromBinary(StdIStream & is);            

         protected :

            /// Constructeurs ///
            CAttributeTemplate(void); // Not implemented.

      }; // class CAttribute    
      
   } // namespace tree
   
   template <class T>  void FromBinary(StdIStream & is, T & obj);
   
} // namespace xmlioserver

#endif // __XMLIO_CAttributeTemplate__
