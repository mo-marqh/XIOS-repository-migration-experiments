#ifndef __XIOS_CGroupTemplate_impl__
#define __XIOS_CGroupTemplate_impl__

#include "xios_spl.hpp"
#include "event_server.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "context.hpp"
#include "event_client.hpp"
#include "context_client.hpp"
#include "message.hpp"
#include "type.hpp"
#include "type_util.hpp"
#include <boost_extract.hpp>

namespace xios
{

   /// ////////////////////// Définitions ////////////////////// ///

   template <class U, class V, class W>
      CGroupTemplate<U, V, W>::CGroupTemplate(CContext* context)
         : CObjectTemplate<V>(context) //, V()
         , childMap(), childList()
         , groupMap(), groupList()
   { /* Ne rien faire de plus */ }

   template <class U, class V, class W>
      CGroupTemplate<U, V, W>::CGroupTemplate(CContext* context, const StdString & id)
         : CObjectTemplate<V>(context, id) //, V()
         , childMap(), childList()
         , groupMap(), groupList()
   { /* Ne rien faire de plus */ }

   template <class U, class V, class W>
      CGroupTemplate<U, V, W>::~CGroupTemplate(void)
   { /* Ne rien faire de plus */ }
   
   template <class U, class V, class W>
      StdString CGroupTemplate<U, V, W>::toString(void) const
   {
      StdOStringStream oss;
      StdString name = (this->getId().compare(V::GetDefName()) != 0)
                     ? V::GetName() : V::GetDefName();

      oss << "<" << name << " ";
      if (this->hasId() && (this->getId().compare(V::GetDefName()) != 0))
         oss << " id=\"" << this->getId() << "\" ";
         
      if (this->hasChild())
      {
         oss << SuperClassAttribute::toString() << ">" << std::endl;
         
         typename std::vector<V*>::const_iterator 
            itg = this->groupList.begin(), endg = this->groupList.end();
         typename std::vector<U*>::const_iterator 
            itc = this->childList.begin(), endc = this->childList.end();
            
         for (; itg != endg; itg++)
         { 
            V* group = *itg;
            oss << *group << std::endl;
         }
            
         for (; itc != endc; itc++)
         { 
            U* child = *itc;
            oss << *child << std::endl;
         }
            
         oss << "</" << name << " >";
      }
      else
      {
         oss << SuperClassAttribute::toString() << "/>";
      }
      return (oss.str());
   }

   template <class U, class V, class W>
      void CGroupTemplate<U, V, W>::fromString(const StdString & str)
   { 
      ERROR("CGroupTemplate<U, V, W>::toString(void)",
            << "[ str = " << str << "] Not implemented yet !");
   }

   //---------------------------------------------------------------

   template <class U, class V, class W>
      StdString CGroupTemplate<U, V, W>::GetName(void)
   { 
      return (U::GetName().append("_group")); 
   }

   template <class U, class V, class W>
      StdString CGroupTemplate<U, V, W>::GetDefName(void)
   { 
      return (U::GetName().append("_definition")); 
   }
   
   //---------------------------------------------------------------   

   template <class U, class V, class W>
      const xios_map<StdString, U*>&
         CGroupTemplate<U, V, W>::getChildMap(void) const
   { 
      return (this->childMap); 
   }

   //---------------------------------------------------------------

   template <class U, class V, class W>
      const xios_map<StdString, V*>&
         CGroupTemplate<U, V, W>::getGroupMap(void) const
   { 
      return (this->groupMap);
   }

   //---------------------------------------------------------------

   template <class U, class V, class W>
      bool CGroupTemplate<U, V, W>::hasChild(void) const
   { 
      return ((groupList.size() + childList.size()) > 0); 
   }

   //---------------------------------------------------------------

   template <class U, class V, class W>
      void CGroupTemplate<U, V, W>::solveDescInheritance(bool apply, const CAttributeMap * const parent)
   {
      if (parent != NULL)
         SuperClassAttribute::setAttributes(parent, apply);
         
      typename std::vector<U*>::const_iterator 
         itc = this->childList.begin(), endc = this->childList.end();
      typename std::vector<V*>::const_iterator 
         itg = this->groupList.begin(), endg = this->groupList.end();
             
      for (; itc != endc; itc++)
      { 
         U* child = *itc;
         child->solveDescInheritance(apply,this);
      }
            
      for (; itg != endg; itg++)
      { 
         V* group = *itg;
         if (apply) group->solveRefInheritance();
         group->solveDescInheritance(apply,this);
      }
   }

   //---------------------------------------------------------------

   template <class U, class V, class W>
      void CGroupTemplate<U, V, W>::getAllChildren(std::vector<U*>& allc) const
   {
      allc.insert (allc.end(), childList.begin(), childList.end());
      typename std::vector<V*>::const_iterator 
         itg = this->groupList.begin(), endg = this->groupList.end();
         
      for (; itg != endg; itg++)
      { 
         V* group = *itg;
         group->getAllChildren(allc);
      }
   }

   //---------------------------------------------------------------

   template <class U, class V, class W>
      std::vector<U*> CGroupTemplate<U, V, W>::getAllChildren(void) const
   { 
      std::vector<U*> allc;
      this->getAllChildren(allc);
      return (allc);
   }

   //---------------------------------------------------------------

   template <class U, class V, class W>
      void CGroupTemplate<U, V, W>::solveRefInheritance(void)
   { /* Ne rien faire de plus */ }
   

   ///--------------------------------------------------------------

  
   template <class U, class V, class W>
   U* CGroupTemplate<U, V, W>::createChild(const string& id) 
  {
    return CGroupFactory::CreateChild<V>(CGroupTemplate<U, V, W>::SuperClass::context_, this->getShared(), id).get() ;
  }

   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::addChild(U* child) 
  {
    return CGroupFactory::AddChild<V>(this->getShared(),child->getShared()) ;
  }
  
   template <class U, class V, class W>
   V* CGroupTemplate<U, V, W>::createChildGroup(const string& id) 
  {
    return CGroupFactory::CreateGroup<V>(CGroupTemplate<U, V, W>::SuperClass::context_, this->getShared(), id).get() ;
  }

   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::addChildGroup(V* childGroup) 
  {
    return CGroupFactory::AddGroup<V>(this->getShared(), childGroup->getShared()) ;
  }

   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::sendCreateChild(const string& id, CContextClient* client, const string& objectId)
   {

    CEventClient event(this->getType(),EVENT_ID_CREATE_CHILD) ;
    if (client->isServerLeader())
    {
      CMessage msg ;
      if (objectId.empty()) msg << this->getId();
      else msg << objectId;
      msg<<id ;
      const std::list<int>& ranks = client->getRanksServerLeader();
      for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
       event.push(*itRank,1,msg) ;
      client->sendEvent(event) ;
    }
    else client->sendEvent(event) ;
   }

   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::sendCreateChildGroup(const string& id, CContextClient* client, const string& objectId)
   {
     CEventClient event(this->getType(),EVENT_ID_CREATE_CHILD_GROUP) ;
     if (client->isServerLeader())
     {
       CMessage msg ;
       if (objectId.empty()) msg << this->getId();
       else msg << objectId;
       msg<<id ;
       const std::list<int>& ranks = client->getRanksServerLeader();
       for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
         event.push(*itRank,1,msg) ;
       client->sendEvent(event) ;
     }
     else client->sendEvent(event) ;
   }   


   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::recvCreateChild(CContext* context, CEventServer& event)
   {
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id ;
      V::get(context, id)->recvCreateChild(*buffer) ;
   }
   
   
   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::recvCreateChild(CBufferIn& buffer)
   {
      string id ;
      buffer>>id ;
      createChild(id) ;
   }

   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::recvCreateChildGroup(CContext* context, CEventServer& event)
   {
      
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id ;
      V::get(context,id)->recvCreateChildGroup(*buffer) ;
   }
   
   
   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::recvCreateChildGroup(CBufferIn& buffer)
   {
      string id ;
      buffer>>id ;
      createChildGroup(id) ;
   }
   

   template <class U, class V, class W>
   bool CGroupTemplate<U, V, W>::dispatchEvent(CContext* context, CEventServer& event)
   {
      if (CObjectTemplate<V>::dispatchEvent(context, event)) return true ;
      else
      {
        switch(event.type)
        {
           case EVENT_ID_CREATE_CHILD :
             recvCreateChild(context, event) ;
             return true ;
             break ;
         
           case EVENT_ID_CREATE_CHILD_GROUP :
             recvCreateChildGroup(context, event) ;
             return true ;
             break ;       
         
           default :
           return false ;
        }
      }
   }

   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::parse(xml::CXMLNode & node)
   { 
      this->parse(node, true); 
   }
   
   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::parse(xml::CXMLNode & node, bool withAttr, const std::set<StdString>& parseContextList)
   {
     ERROR("void CGroupTemplate<U, V, W>::parse(xml::CXMLNode & node, bool withAttr, const std::set<StdString>& parseContextList)",
                     <<"must not be called by this kind of object : "<<GetName() ) ;
   }

   template <class U, class V, class W>
      void CGroupTemplate<U, V, W>::parse(xml::CXMLNode & node, bool withAttr)
   {

      StdString name = node.getElementName();
      xml::THashAttributes attributes = node.getAttributes();
      if (withAttr)
      {
         CGroupTemplate<U, V, W>::SuperClass::parse(node);
         if (attributes.end() != attributes.find("src")) xml::CXMLParser::ParseInclude(attributes["src"].c_str(), *this);
      }

      // PARSING POUR GESTION DES ENFANTS
           V* group_ptr = (this->hasId()) ? V::get(SuperClass::context_, this->getId()) : xios_polymorphic_downcast<V*>(this);

      if (!(node.goToChildElement()))
      {
         if (this->hasId())
         {
            DEBUG(<< "L'objet de type \'" << V::GetName()
                  << "\' nommé \'" << this->getId()
                  << "\' ne contient pas d\'enfant !");
         }
      }
      else
      {
         do { // Parcours pour traitement.

            StdString name = node.getElementName();
            attributes.clear();
            attributes = node.getAttributes();

            if (name.compare(V::GetName()) == 0)
            {
               if (attributes.end() == attributes.find("id"))
                  CGroupFactory::CreateGroup(CGroupTemplate<U, V, W>::SuperClass::context_, group_ptr->getShared())->parse(node);
               else
                  CGroupFactory::CreateGroup(CGroupTemplate<U, V, W>::SuperClass::context_, group_ptr->getShared(), attributes["id"])->parse(node);
               continue;
            }

            if (name.compare(U::GetName()) == 0)
            {
               if (attributes.end() == attributes.find("id"))
                  CGroupFactory::CreateChild(CGroupTemplate<U, V, W>::SuperClass::context_, group_ptr->getShared())->parse(node);
               else
                  CGroupFactory::CreateChild(CGroupTemplate<U, V, W>::SuperClass::context_, group_ptr->getShared(), attributes["id"])->parse(node);
               continue;
            }

            DEBUG(<< "Dans le contexte \'" << getContext()->getId()
                  << "\', un objet de type \'" << V::GetName()
                  << "\' ne peut contenir qu'un objet de type \'" << V::GetName()
                  << "\' ou de type \'" << U::GetName()
                  << "\' (reçu : " << name << ") !");

         } while (node.goToNextElement());
         node.goToParentElement(); // Retour au parent
      }
   }
   
   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::parseChild(xml::CXMLNode & node)
   {


      // PARSING POUR GESTION DES ENFANTS
           V* group_ptr = (this->hasId()) 
         ? V::get(SuperClass::context_, this->getId())
         : xios_polymorphic_downcast<V*>(this);

          StdString name = node.getElementName();
          xml::THashAttributes attributes = node.getAttributes();

          if (name.compare(V::GetName()) == 0)
          {
             if (attributes.end() == attributes.find("id"))
                CGroupFactory::CreateGroup(CGroupTemplate<U, V, W>::SuperClass::context_, group_ptr->getShared())->parse(node);
             else
                CGroupFactory::CreateGroup(CGroupTemplate<U, V, W>::SuperClass::context_, group_ptr->getShared(), attributes["id"])->parse(node);
             return ;
          }
          else if (name.compare(U::GetName()) == 0)
          {
             if (attributes.end() == attributes.find("id"))
                CGroupFactory::CreateChild(CGroupTemplate<U, V, W>::SuperClass::context_, group_ptr->getShared())->parse(node);
             else
                CGroupFactory::CreateChild(CGroupTemplate<U, V, W>::SuperClass::context_, group_ptr->getShared(), attributes["id"])->parse(node);
             return ;
          }

          DEBUG(<< "Dans le contexte \'" << getContext()->getId()
                << "\', un objet de type \'" << V::GetName()
                << "\' ne peut contenir qu'un objet de type \'" << V::GetName()
                << "\' ou de type \'" << U::GetName()
                << "\' (reçu : " << name << ") !");

   }
} // namespace xios


#endif // __XIOS_CGroupTemplate_impl__
