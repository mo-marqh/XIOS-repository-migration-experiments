#ifndef __XMLIO_GROUP_TEMPLATE__
#define __XMLIO_GROUP_TEMPLATE__ 

namespace XMLIOSERVER
{
   template <class T, class U>
      class GroupTemplate : public ObjectTemplate<GroupTemplate<T, U> >, public U
   {
      public:
            
         GroupTemplate(void) : ObjectTemplate<GroupTemplate<T, U> >(),  U(), childList(), groupList()
         {/* Ne rien faire de plus */}               
         GroupTemplate(const string& _id) : ObjectTemplate<GroupTemplate<T, U> >(_id), U(), childList(), groupList()
         {/* Ne rien faire de plus */}            
      
         virtual const char* getName(void) const {return ("GroupTemplate"); }
                 
         /// Pour les groupes d'objets enfants ///
         
         GroupTemplate<T, U>& createGroup(const string _id) throw (XMLIOUndefinedValueException) 
         {
            GroupTemplate<T, U> &obj = GroupTemplate<T, U>::CreateObject(_id);
            groupList.addObject(&obj);
              
            return (GroupTemplate<T, U>::GetObject(_id));
         }
         
         GroupTemplate<T, U>& createGroup(void)
         {
            GroupTemplate<T, U>& obj = GroupTemplate<T, U>::CreateObject();
            groupList.addObject(&obj);
                     
            return (obj);
         }
         
         GroupTemplate<T, U>& getGroup(const string _id) throw (XMLIOUndefinedValueException) { return (*groupList[_id]); }
         bool hasGroup(const string _id) { return (groupList.hasMappedValue(_id)); }
         
         const StrHashMap<GroupTemplate<T, U>* >& getGroupList(void) { return (groupList); }
         
         size_t getNbGroup() const {return (groupList.getVectorSize()); }
         
         /// Pour les objets enfants ///
         
         T& createChild(const string _id) throw (XMLIOUndefinedValueException) 
         {
            T& obj = ObjectTemplate<T>::CreateObject(_id);
            childList.addObject(&obj);
            return (obj);
         }
         
         T& createChild(void)
         {
            T& obj = ObjectTemplate<T>::CreateObject();
            childList.addObject(&obj);            
            return (obj);
         }         
         
         T& getChild(const string _id) throw (XMLIOUndefinedValueException) { return (*childList[_id]); }
         bool hasChild(const string _id) { return (childList.hasMappedValue(_id)); }
         
         const StrHashMap<T*>& getCurrentListChild(void) { return (childList); }
         
         size_t getNbChild() const {return (childList.getVectorSize()); }
            
        
         virtual ~GroupTemplate()
         {
            for (unsigned int i = 0; i < childList.getVector().size(); i++)
               delete childList.getVector()[i];
            for (unsigned int i = 0; i < groupList.getVector().size(); i++)
               delete groupList.getVector()[i];
         }
         
      protected:
      
      private:
         StrHashMap<T> childList;
         StrHashMap<GroupTemplate<T, U> > groupList;

   }; // class GroupTemplate
   
   
   
}// namespace XMLIOSERVER


#endif // __XMLIO_GROUP_TEMPLATE__
