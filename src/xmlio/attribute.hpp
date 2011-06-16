#ifndef __XMLIO_CAttribute__
#define __XMLIO_CAttribute__

/// boost headers ///
#include <boost/any.hpp>

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "object.hpp"

namespace xmlioserver
{
   namespace tree
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CAttribute : public CObject
      {
            typedef CObject SuperClass;

         public :

            /// Constructeurs ///
            explicit CAttribute(const StdString & id);
            CAttribute(const CAttribute & attribut);
            CAttribute(const CAttribute * const attribut); // Not implemented.

            /// Accesseurs ///
            const StdString & getName(void) const;
            const boost::any & getAnyValue(void) const;
            template <typename T> inline T getValue(void) const;

            /// Mutateurs ///
            template <typename T> inline void setValue(const T & value);
            void setAnyValue(const boost::any & value);
            void clear(void);

            /// Test ///
            bool isEmpty(void) const;
            template <typename T> inline bool isType(void);

            /// Destructeur ///
            virtual ~CAttribute(void);

            /// Autres ///
            virtual StdString toString(void) const = 0;
            virtual void fromString(const StdString & str) = 0;

            virtual void toBinary  (StdOStream & os) const = 0;
            virtual void fromBinary(StdIStream & is) = 0;

         protected :

            /// Constructeurs ///
            CAttribute(void);  // Not implemented.

         private :

            /// Propriété ///
            boost::any value;

      }; // class CAttribute

      /// ////////////////////// Définitions ////////////////////// ///
      template <typename T>
         T CAttribute::getValue(void) const
      { 
         return (boost::any_cast<T>(this->value)); 
      }

      template <typename T>
         void CAttribute::setValue(const T & value)
      { 
         this->value = value; 
      }

      template<typename T>
         bool CAttribute::isType(void)
      { 
         return (this->value.type() == typeid(T)); 
      }

   } // namespace tree
} // namespace xmlioserver

#endif // __XMLIO_CAttribute__
