#ifndef __XMLIO_CGroupTemplate__
#define __XMLIO_CGroupTemplate__

#include "declare_attribute.hpp"

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
         inline void parse(xml::CXMLNode & node, bool withAttr);
         
         /// Test ///
         virtual bool hasChild(void) const;

         /// Accesseurs statiques ///
         static inline StdString GetName(void);
         static inline StdString GetDefName(void);

         /// Traitements ///
         virtual void solveDescInheritance(const CAttributeMap * const parent = 0);
         void solveRefInheritance(void);

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

#endif // __XMLIO_CGroupTemplate__
