#ifndef __XIOS_CAttributeTemplate_impl__
#define __XIOS_CAttributeTemplate_impl__

#include "type.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "generate_interface.hpp"
#include "attribute_template.hpp"


namespace xios
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
/*
      template <class T>
         CAttributeTemplate<T>::CAttributeTemplate(const CAttribute & attribut)
         throw (CException)
         : CAttribute(attribut)
      {
         if (!attribut.isEmpty() && !attribut.isType<T>())
            ERROR("CAttributeTemplate", << "Invalid instantiation !");
      }
*/
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
/*
      template <class T>
      CAttributeTemplate<T>::~CAttributeTemplate(void)
      {
//         this->CType<T>::reset() ;
//         this->clear();
      }
*/
      ///--------------------------------------------------------------
      template <class T>
      void CAttributeTemplate<T>::reset(void)
      {
        CType<T>::reset() ;
        inheritedValue.reset() ;
      }

      template <class T>
      void CAttributeTemplate<T>::checkEmpty(void) const
      {
        if (CType<T>::empty)
        {
          StdString msg("On checking attribute with id=");
          msg.append(this->getId());
          msg.append(" : ");
          msg.append("data is not initialized\n");
          ERROR("template <typename T> void CType<T>::checkEmpty(void) const", << msg);
        }
      }


      template <class T>
         T CAttributeTemplate<T>::getValue(void) const
      {
        return CType<T>::get() ;

/*
        if (SuperClass::isEmpty())
        {
           ERROR("T CAttributeTemplate<T>::getValue(void) const",
                 << "[ id = " << this->getId() << "]"
                 << " L'attribut est requis mais n'est pas défini !");
         }
        return (SuperClass::getValue<T>());
*/
      }

/*
      template <class T>
         T* CAttributeTemplate<T>::getRef(void)
      {
         if (SuperClass::isEmpty())
         {
            ERROR("T CAttributeTemplate<T>::getValue(void) const",
                  << "[ id = " << this->getId() << "]"
                  << " L'attribut est requis mais n'est pas défini !");
          }
         return (SuperClass::getRef<T>());
      }
*/

      template <class T>
         void CAttributeTemplate<T>::setValue(const T & value)
      {
         CType<T>::set(value) ;
//         SuperClass::setValue<T>(value);
      }

    template <class T>
    void CAttributeTemplate<T>::set(const CAttribute& attr)
    {
      this->set(dynamic_cast<const CAttributeTemplate<T>& >(attr)) ;
    }

   template <class T>
    void CAttributeTemplate<T>::set(const CAttributeTemplate& attr)
    {
      CType<T>::set(attr) ;
    }

    template <class T>
    void CAttributeTemplate<T>::setInheritedValue(const CAttribute& attr)
    {
      this->setInheritedValue(dynamic_cast<const CAttributeTemplate<T>& >(attr)) ;
    }

    template <class T>
    void CAttributeTemplate<T>::setInheritedValue(const CAttributeTemplate& attr)
    {
      if (this->isEmpty() && _canInherite && attr.hasInheritedValue()) inheritedValue.set(attr.getInheritedValue()) ;
    }

    template <class T>
    T CAttributeTemplate<T>::getInheritedValue(void) const
    {
      if (this->isEmpty()) return inheritedValue.get() ;
      else return getValue() ;
    }

    template <class T>
    bool CAttributeTemplate<T>::hasInheritedValue(void) const
    {
      return !this->isEmpty() || !inheritedValue.isEmpty() ;
    }

    template <class T>
    bool CAttributeTemplate<T>::isEqual(const CAttribute& attr)
    {
      const CAttributeTemplate<T>& tmp = dynamic_cast<const CAttributeTemplate<T>& >(attr);
      return this->isEqual_(tmp);
    }

    template <class T>
    bool CAttributeTemplate<T>::isEqual_(const CAttributeTemplate& attr)
    {
      if (this->isEmpty() && attr.isEmpty()) return true ;
      if (!this->isEmpty() && !attr.isEmpty())  return (this->getValue() == attr.getValue());
      else return false;
    }

    template <class T>
    size_t CAttributeTemplate<T>::computeHash(void)
    {
      // Basic hash computation through string
      return std::hash<string>{}( toString() );
    }

      //---------------------------------------------------------------

      template <class T>
         CAttributeTemplate<T>& CAttributeTemplate<T>::operator=(const T & value)
      {
         this->setValue(value);
//         return (this->getValue());
         return *this;
      }

      //---------------------------------------------------------------

      template <class T>
         StdString CAttributeTemplate<T>::_toString(void) const
      {
         StdOStringStream oss;
         if (!CType<T>::isEmpty() && this->hasId())
            oss << this->getName() << "=\"" << CType<T>::toString() << "\"";
         return (oss.str());
      }

      template <class T>
         void CAttributeTemplate<T>::_fromString(const StdString & str)
      {
        CType<T>::fromString(str) ;
      }

      //---------------------------------------------------------------

      template <class T>
         StdString CAttributeTemplate<T>::_dump(void) const
      {
         StdOStringStream oss;
         if (!CType<T>::isEmpty() && this->hasId())
            oss << this->getName() << "=\"" << CType<T>::dump() << "\"";
         return (oss.str());
      }
      
      template <class T>
         StdString CAttributeTemplate<T>::_dumpGraph(void) const
      {
         StdOStringStream oss;
         if (!CType<T>::isEmpty() && this->hasId())
            oss << this->getName() << "=" << CType<T>::dump() << "</br>";
         return (oss.str());
      }


      //---------------------------------------------------------------
/*
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
*/
      template <class T>
         bool CAttributeTemplate<T>::_toBuffer (CBufferOut& buffer) const
      {
         return CType<T>::toBuffer(buffer) ;
      }

      template <class T>
      bool CAttributeTemplate<T>::_fromBuffer(CBufferIn& buffer)
      {
        return CType<T>::fromBuffer(buffer) ;
      }

      template <typename T>
      void CAttributeTemplate<T>::generateCInterface(ostream& oss,const string& className)
      {
        CInterface::AttributeCInterface<T>(oss, className, this->getName());
      }

      template <typename T>
      void CAttributeTemplate<T>::generateFortran2003Interface(ostream& oss,const string& className)
      {
        CInterface::AttributeFortran2003Interface<T>(oss, className, this->getName());
      }

      template <typename T>
      void CAttributeTemplate<T>::generateFortranInterfaceDeclaration_(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceDeclaration<T>(oss, className, this->getName() + "_");
      }

      template <typename T>
      void CAttributeTemplate<T>::generateFortranInterfaceBody_(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceBody<T>(oss, className, this->getName());
      }

      template <typename T>
      void CAttributeTemplate<T>::generateFortranInterfaceDeclaration(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceDeclaration<T>(oss, className, this->getName());
      }

      template <typename T>
      void CAttributeTemplate<T>::generateFortranInterfaceGetDeclaration_(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceGetDeclaration<T>(oss, className, this->getName() + "_");
      }


      template <typename T>
      void CAttributeTemplate<T>::generateFortranInterfaceGetBody_(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceGetBody<T>(oss, className, this->getName());
      }

      template <typename T>
      void CAttributeTemplate<T>::generateFortranInterfaceGetDeclaration(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceGetDeclaration<T>(oss, className, this->getName());
      }


/*
      //---------------------------------------------------------------

      // Spécialisations des templates pour la fonction [toString]

      template <>
         StdString CAttributeTemplate<bool>::toString(void) const;

      //---------------------------------------------------------------

      // Spécialisations des templates pour la fonction [fromString]

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

      // Spécialisations des templates pour la fonction [toBinary] //

      template <> // Chaîne de caractères.
         void CAttributeTemplate<StdString>::toBinary (StdOStream & os) const;

      template <> // Entier
         void CAttributeTemplate<int>::toBinary(StdOStream & os) const;

      template <> // Booléen
         void CAttributeTemplate<bool>::toBinary(StdOStream & os) const;

      template <> // Double
         void CAttributeTemplate<double>::toBinary(StdOStream & os) const;

      //---------------------------------------------------------------

      // Spécialisations des templates pour la fonction [fromBinary] //

      template <> // Chaîne de caractères.
         void CAttributeTemplate<StdString>::fromBinary(StdIStream & is);

      template <> // Entier
         void CAttributeTemplate<int>::fromBinary(StdIStream & is);

      template <> // Booléen
         void CAttributeTemplate<bool>::fromBinary(StdIStream & is);

      template <> // Double
         void CAttributeTemplate<double>::fromBinary(StdIStream & is);

      ///--------------------------------------------------------------
*/
} // namespace xios

#endif // __XIOS_CAttributeTemplate_impl__
