#ifndef __XMLIO_CObjectTemplate_impl__
#define __XMLIO_CObjectTemplate_impl__

#include "object_factory.hpp"
#include "context.hpp"
#include "transfert_parameters.hpp"
#include "buffer_in.hpp"
#include "attribute.hpp"
#include "event_client.hpp"
#include "context_client.hpp"
#include "object_template.hpp"

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
      xios_map<StdString,long int> CObjectTemplate<T>::GenId;

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

   template <class T>
   void CObjectTemplate<T>::sendAttributToServer(const string& id)
   {
      CAttributeMap & attrMap = *this;
      CAttribute* attr=attrMap[id] ;
      sendAttributToServer(*attr) ;
   }

   template <class T>
   void CObjectTemplate<T>::sendAttributToServer(tree::CAttribute& attr)
   {
     shared_ptr<CContext> context=CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId()) ;
     
    if (!context->hasServer)
    {
       CContextClient* client=context->client ;

       CEventClient event(getType(),EVENT_ID_SEND_ATTRIBUTE) ;   
       if (client->isServerLeader())
       {
         CMessage msg ;
         msg<<this->getId() ;
         msg<<attr.getName() ;
         msg<<attr ;
         event.push(client->getServerLeader(),1,msg) ;
         client->sendEvent(event) ;
       }
       else client->sendEvent(event) ;
    }
      
   }
   
   template <class T>
   void CObjectTemplate<T>::recvAttributFromClient(CEventServer& event)
   {
      
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id,attrId;
      *buffer>>id ;
      CAttributeMap & attrMap = *get(id);
      *buffer>>attrId ;
      CAttribute* attr=attrMap[attrId] ;
      info(50)<<"attribut recu "<<attrId<<"  " ;
      if (attr->isEmpty()) info(50)<<"--> empty"<<endl ;
      else info(50) /*<attr->getValue()*/<<endl ;
      *buffer>>*attr ;
       info(50)<<"attribut recu "<<attrId<<"  " ;
      if (attr->isEmpty()) info(50)<<"--> empty"<<endl ;
      else info(50) /*attr->getValue()*/<<endl ;
  }

   template <class T>
   bool CObjectTemplate<T>::dispatchEvent(CEventServer& event)
   {
      switch(event.type)
      {
         case EVENT_ID_SEND_ATTRIBUTE :
           recvAttributFromClient(event) ;
           return true ;
           break ;
       
         default :
         return false ;
//           ERROR("void CObjectTemplate<T>::recvEvent(CEventServer& event)",
//                 <<"Unknown Event") ;
      }
   }
   
   template <typename T>
   bool CObjectTemplate<T>::has(const string & id)
   {
     return CObjectFactory::HasObject<T>(id) ;
   }

   template <typename T>
   boost::shared_ptr<T> CObjectTemplate<T>::get(const string & id)
   {
     return CObjectFactory::GetObject<T>(id) ;
   }

   template <typename T>
   boost::shared_ptr<T> CObjectTemplate<T>::create(const string & id)
   {
     return CObjectFactory::CreateObject<T>(id) ;
   }   ///--------------------------------------------------------------

  template <typename T>
  boost::shared_ptr<T> CObjectTemplate<T>::get(void)
  {
    return CObjectFactory::GetObject<T>((T*)this) ;
  }
  

  
} // namespace xmlioserver

#endif // __XMLIO_CObjectTemplate_impl__
