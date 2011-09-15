#ifndef __XMLIO_CAttributeTemplate_impl__
#define __XMLIO_CAttributeTemplate_impl__

#include "array.hpp"

namespace xmlioserver
{
   namespace tree
   {
      /// ////////////////////// Définitions ////////////////////// ///
      template <class T>
         CAttributeTemplate<T>::CAttributeTemplate(const StdString & id)
         : CAttribute(id)
      { /* Ne rien faire de plus */ }

      template <class T>
         CAttributeTemplate<T>::CAttributeTemplate(const StdString & id, const T & value)
         : CAttribute(id)
      {
         this->setValue(value);
      }

      template <class T>
         CAttributeTemplate<T>::CAttributeTemplate(const CAttribute & attribut)
         throw (CException)
         : CAttribute(attribut)
      {
         if (!attribut.isEmpty() && !attribut.isType<T>())
            ERROR("CAttributeTemplate", << "Invalid instantiation !");
      }

      template <class T>
         CAttributeTemplate<T>::CAttributeTemplate(const StdString & id,
                              xios_map<StdString, CAttribute*> & umap)
         : CAttribute(id)
      {
         umap.insert(umap.end(), std::make_pair(id, this));
      }

      template <class T>
         CAttributeTemplate<T>::CAttributeTemplate
            (const StdString & id, const T & value,
             xios_map<StdString, CAttribute*> & umap)
         : CAttribute(id)
      {
         this->setValue(value);
         umap.insert(umap.end(), std::make_pair(id, this));
      }

      template <class T>
         CAttributeTemplate<T>::~CAttributeTemplate(void)
      { 
         this->clear();
      }

      ///--------------------------------------------------------------

      template <class T>
         T CAttributeTemplate<T>::getValue(void) const
      {
         if (SuperClass::isEmpty())
         {
            ERROR("T CAttributeTemplate<T>::getValue(void) const",
                  << "[ id = " << this->getId() << "]"
                  << " L'attribut est requis mais n'est pas défini !");
          }
         return (SuperClass::getValue<T>());
      }

      template <class T>
         void CAttributeTemplate<T>::setValue(const T & value)
      {
         SuperClass::setValue<T>(value);
      }

      //---------------------------------------------------------------

      template <class T>
         T CAttributeTemplate<T>::operator=(const T & value)
      {
         this->setValue(value);
         return (this->getValue());
      }

      //---------------------------------------------------------------

      template <class T>
         StdString CAttributeTemplate<T>::toString(void) const
      {
         StdOStringStream oss;
         if (!this->isEmpty() && this->hasId())
            oss << this->getName() << "=\"" << this->getValue() << "\"";
         return (oss.str());
      }

      template <class T>
         void CAttributeTemplate<T>::fromString(const StdString & str)
      {
         ERROR("CAttributeTemplate<T>::fromString(const StdString & str)",
               << "[ str = " << str << " ] Not implemented yet !");
      }

      //---------------------------------------------------------------

      template <class T>
         void CAttributeTemplate<T>::toBinary (StdOStream & os) const
      {
         this->getValue()->toBinary(os);
      }

      template <class T>
         void CAttributeTemplate<T>::fromBinary(StdIStream & is)
      {
         T value;
         FromBinary(is, value);
         this->setValue(value);
      }

      //---------------------------------------------------------------

      /** Spécialisations des templates pour la fonction [toString] **/

      template <>
         StdString CAttributeTemplate<bool>::toString(void) const;

      //---------------------------------------------------------------

      /** Spécialisations des templates pour la fonction [fromString] **/

      template <> // Chaîne de caractères.
         void CAttributeTemplate<StdString>::fromString(const StdString & str);

      template <> // Entier
         void CAttributeTemplate<int>::fromString(const StdString & str);

      template <> // Booléen
         void CAttributeTemplate<bool>::fromString(const StdString & str);

      template <> // Double
         void CAttributeTemplate<double>::fromString(const StdString & str);

      template<> // Tableau
         void CAttributeTemplate<ARRAY(double, 1)>::fromString(const StdString & str);

      //---------------------------------------------------------------

      /** Spécialisations des templates pour la fonction [toBinary] **/

      template <> // Chaîne de caractères.
         void CAttributeTemplate<StdString>::toBinary (StdOStream & os) const;

      template <> // Entier
         void CAttributeTemplate<int>::toBinary(StdOStream & os) const;

      template <> // Booléen
         void CAttributeTemplate<bool>::toBinary(StdOStream & os) const;
         
      template <> // Double
         void CAttributeTemplate<double>::toBinary(StdOStream & os) const;

      //---------------------------------------------------------------

      /** Spécialisations des templates pour la fonction [fromBinary] **/

      template <> // Chaîne de caractères.
         void CAttributeTemplate<StdString>::fromBinary(StdIStream & is);

      template <> // Entier
         void CAttributeTemplate<int>::fromBinary(StdIStream & is);

      template <> // Booléen
         void CAttributeTemplate<bool>::fromBinary(StdIStream & is);
         
      template <> // Double
         void CAttributeTemplate<double>::fromBinary(StdIStream & is);

      ///--------------------------------------------------------------
   } // namespace tree
} // namespace xmlioserver

#endif // __XMLIO_CAttributeTemplate_impl__
