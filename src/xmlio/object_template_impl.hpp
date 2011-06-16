#ifndef __XMLIO_CObjectTemplate_impl__
#define __XMLIO_CObjectTemplate_impl__

namespace xmlioserver
{
   /// ////////////////////// Définitions ////////////////////// ///
   template <class T>
      xios_map<StdString,
      xios_map<StdString,
      boost::shared_ptr<T> > > CObjectTemplate<T>::AllMapObj;

   template <class T>
      xios_map<StdString,
      std::vector<boost::shared_ptr<T> > > CObjectTemplate<T>::AllVectObj;

   template <class T>
      CObjectTemplate<T>::CObjectTemplate(void)
         : tree::CAttributeMap()
         , CObject()
   { /* Ne rien faire de plus */ }

   template <class T>
      CObjectTemplate<T>::CObjectTemplate(const StdString & id)
         : tree::CAttributeMap()
         , CObject(id)
   { /* Ne rien faire de plus */ }

   template <class T>
      CObjectTemplate<T>::CObjectTemplate
         (const CObjectTemplate<T> & object, bool withAttrList, bool withId)
         : tree::CAttributeMap()
         , CObject()
   {
      if (object.hasId() && withId)
         this->setId(object.getId());
      ERROR("CObjectTemplate<T> construtor 3", << "Not completly implemented yet !");
   }
   
   template <class T>
      CObjectTemplate<T>::~CObjectTemplate(void)
   { /* Ne rien faire de plus */ }

   ///--------------------------------------------------------------

   template <class T>
      std::vector<boost::shared_ptr<T> > &
         CObjectTemplate<T>::GetAllVectobject(const StdString & contextId)
   { 
      return (CObjectTemplate<T>::AllVectObj[contextId]); 
   }
   
   //---------------------------------------------------------------
   
   template <class T>
      StdString CObjectTemplate<T>::toString(void) const
   {
      StdOStringStream oss;
      oss << "<" << T::GetName();
      if (this->hasId())
         oss << " id=\"" << this->getId() << "\"";
      oss << " " << SuperClassMap::toString() << "/>";
      return (oss.str());
   }

   template <class T>
      void CObjectTemplate<T>::fromString(const StdString & str)
   { 
      ERROR("CObjectTemplate<T>::fromString(str)",
            << "[ str = " << str << "] Not implemented yet !"); 
   }
   
   //---------------------------------------------------------------
   
   template <class T>
      void CObjectTemplate<T>::toBinary(StdOStream & os) const
   {
      SuperClassMap::toBinary(os);    
   }
      
   template <class T>
      void CObjectTemplate<T>::fromBinary(StdIStream & is)
   {
      SuperClassMap::fromBinary(is); 
   }
   
   //---------------------------------------------------------------

   template <class T>
      void CObjectTemplate<T>::parse(xml::CXMLNode & node)
   {
      xml::THashAttributes attributes = node.getAttributes();
      CAttributeMap::setAttributes(attributes);
   }

   //---------------------------------------------------------------

   template <class T>
      tree::ENodeType CObjectTemplate<T>::getType(void) const
   {
      return (T::GetType());
   }
   
   //---------------------------------------------------------------

   template <class T>
      bool CObjectTemplate<T>::hasChild(void) const
   { 
      return (false); 
   }

   //---------------------------------------------------------------

   template <class T>
      void CObjectTemplate<T>::solveDescInheritance(const CAttributeMap * const parent)
   { 
      SuperClassMap::setAttributes(parent); 
   }

   //---------------------------------------------------------------

   template <class T>
      void CObjectTemplate<T>::ClearAllAttributes(void)
   {
      std::vector<boost::shared_ptr<T> > & avect =
         CObjectTemplate<T>::GetAllVectobject(CObjectFactory::GetCurrentContextId());
      typename std::vector<boost::shared_ptr<T> >::iterator
            it = avect.begin(), end = avect.end();

      for (;it != end; it++)
      {
         CAttributeMap & amap = **it;
         amap.clearAllAttributes();
      }
   }

   ///--------------------------------------------------------------

} // namespace xmlioserver

#endif // __XMLIO_CObjectTemplate_impl__
