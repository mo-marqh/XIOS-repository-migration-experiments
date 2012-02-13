#ifndef __XMLIO_CGroupTemplate__
#define __XMLIO_CGroupTemplate__

#include "xmlioserver_spl.hpp"
#include "declare_attribute.hpp"
#include "event_server.hpp"
#include "object_template.hpp"

namespace xmlioserver
{
   using namespace tree;

   /// ////////////////////// Déclarations ////////////////////// ///
   template <class U, class V, class W>
      class CGroupTemplate
         : public CObjectTemplate<V>, public virtual W
   {
         /// Friend ///
         friend class CGroupFactory;

         /// Typedef ///
         typedef U Child;
         typedef V Derived, Group;
         typedef W SuperClassAttribute;
         typedef CObjectTemplate<V> SuperClass;

      public :
      
         enum EEventId
         {
           EVENT_ID_CREATE_CHILD=200, EVENT_ID_CREATE_CHILD_GROUP
         } ;

         /// Spécifique ///
         DECLARE_ATTRIBUTE(StdString, group_ref)

         /// Accesseurs ///
         const xios_map<StdString,
                           boost::shared_ptr<Group> >& getGroupMap(void) const;
         const std::vector<boost::shared_ptr<Child> >& getChildList(void) const;

         void getAllChildren(std::vector<boost::shared_ptr<Child> > & allc) const;
         std::vector<boost::shared_ptr<Child> > getAllChildren(void) const;

         /// Autres ///
         virtual StdString toString(void) const;
         virtual void fromString(const StdString & str);
         
         virtual void toBinary  (StdOStream & os) const;
         virtual void fromBinary(StdIStream & is);

         virtual void parse(xml::CXMLNode & node);
         virtual void parse(xml::CXMLNode & node, bool withAttr);
         
         /// Test ///
         virtual bool hasChild(void) const;

         /// Accesseurs statiques ///
         static inline StdString GetName(void);
         static inline StdString GetDefName(void);

         /// Traitements ///
         virtual void solveDescInheritance(const CAttributeMap * const parent = 0);
         void solveRefInheritance(void);
//         static bool has(const string & id); 
//         static boost::shared_ptr<V> get(const string& id) ;
//         static boost::shared_ptr<V> create(const string& id=string("")) ;
         boost::shared_ptr<U> createChild(const string& id="") ; 
         boost::shared_ptr<V> createChildGroup(const string& id="") ; 
         static bool dispatchEvent(CEventServer& event) ;
         void sendCreateChild(const string& id="") ;
         void sendCreateChildGroup(const string& id="") ;
         static void recvCreateChild(CEventServer& event) ;
         void recvCreateChild(CBufferIn& buffer) ;
         static void recvCreateChildGroup(CEventServer& event) ;
         void recvCreateChildGroup(CBufferIn& buffer) ;
         
         /// Destructeur ///
         virtual ~CGroupTemplate(void);

      protected :

         /// Constructeurs ///
         CGroupTemplate(void);
         CGroupTemplate(const StdString & id);
         CGroupTemplate(const CGroupTemplate<U, V, W> & group,
                        bool withAttrList = true, bool withId = true); // Not implemented yet.
         CGroupTemplate(const CGroupTemplate<U, V, W> * const group);  // Not implemented yet.

      private :

         /// Propriétés ///
         xios_map<StdString,
                     boost::shared_ptr<Child> > childMap;
         std::vector<boost::shared_ptr<Child> > childList;

         xios_map<StdString,
                     boost::shared_ptr<Group> > groupMap;
         std::vector<boost::shared_ptr<Group> > groupList;

   }; // class CGroupTemplate
} // namespace xmlioserver

//#include "group_template_impl.hpp"

#endif // __XMLIO_CGroupTemplate__
