#ifndef GROUP_TEMPLATE_HPP
#define GROUP_TEMPLATE_HPP

#include "object_template.hpp"
#include "xmlio_std.hpp"

template<typename CObject>
class group  : public CObjectTemplate<group<CObject> >
{
  public :

  CObject * defaultAttribut ;
  
  unordered_map<string,CObject *>* childListId ;
  vector<CObject *>* childList ;
  unordered_map<string,group<CObject> *>* groupListId ;
  vector<group<CObject> *>* groupList ;
  
  group(void) : CObjectTemplate< group<CObject> >() 
  {
    defaultAttribut=new CObject ;
    childListId=new unordered_map<string,CObject *> ;
    childList=new vector<CObject *> ;
    groupListId=new unordered_map<string,group<CObject> *> ;
    groupList=new vector<group<CObject> *> ;
  }

  group(const string Id) : CObjectTemplate< group<CObject> >(Id) 
  {
    defaultAttribut=new CObject ;
    childListId=new unordered_map<string,CObject *> ;
    childList=new vector<CObject *> ;
    groupListId=new unordered_map<string,group<CObject> *> ;
    groupList=new vector<group<CObject>*> ;
  }

  CObject * getDefaultAttribut(void) {return defaultAttribut ;}
  
  bool foundChild(const string & Id) const 
  {
    if (childListId->find(Id)==childListId->end()) return false ;
    else return true ;
  }

  CObject * getChild(const string & Id)
  {
    typename unordered_map<string,CObject*>::iterator It ;

    It=childListId->find(Id) ;
    if (It==childListId->end())
    {
      error("group<object>::getChild(const string & Id)")<<"Child <<"<<Id<<">> does not exist"<<endl ;
      return 0 ;
    }
    else return It->second ;
  }

  CObject * getNewChild(void)
  {
    CObject * newChild=new CObject ;
    childList->push_back(newChild) ;
    return newChild ;
  }


  CObject* getNewChild(const string & Id)
  {
    CObject* newChild=new CObject(Id) ;
    
    pair<string,CObject *> value(Id,newChild) ;
    if (childListId->insert(value).second) 
    {
      childList->push_back(newChild) ;
      return newChild ;
    }
    else
    {
      error("group<object>::getNewChild(const string & Id)")<<"Child <<"<<Id<<">> is already present"<<endl ;
      return 0 ;
    }
  }


  CObject * getOrAppendChild(const string & Id)
  {
    if (foundChild(Id)) return getChild(Id) ;
    else return getNewChild(Id) ;
  }

  
  bool foundGroup(const string & Id) const
  {
    if (groupListId->find(Id)==groupListId->end()) return false ;
    else return true ;
  }
  
  group<CObject> * getGroup(const string & Id)
  {
    typename unordered_map<string,group<CObject>*>::iterator It ;

    It=groupListId->find(Id) ;
    if (It==groupListId->end())
    {
      error("group<object>::getGroup(const string & Id)")<<"Group <<"<<Id<<">> does not exist"<<endl ;
      return 0 ;
    }
    else return It->second ;
  }

  group<CObject>* getNewGroup(void)
  {
    group<CObject> * newGroup=new group<CObject> ;
    groupList->push_back(newGroup) ;
    return newGroup ;
  }


  group<CObject>* getNewGroup(const string & Id)
  {
    group<CObject>* newGroup=new group<CObject>(Id) ;
    
    pair<string,group<CObject>*> value(Id,newGroup) ;
    if (groupListId->insert(value).second)
    {
      groupList->push_back(newGroup) ;
      return newGroup ;
    }
    else
    {
      error("group<object>::getNewGroup(const string & Id)")<<"Group <<"<<Id<<">> is already present"<<endl ;
      return 0 ;
    }
  }


  group<CObject> * getOrAppendGroup(const string & Id)
  {
    if (foundGroup(Id)) return getGroup(Id) ;
    else return getNewGroup(Id) ;
  }

  static const char* getName(void) 
  {
    return (string("group_")+string(CObject::getName())).c_str() ;
  }


  ostream& Print(ostream& o)
  {
    typename vector<CObject *>::iterator it1 ;
    typename vector<group<CObject> *>::iterator it2 ;
    
    o<<"Object :"<<getName()<<" : " ;
    this->PrintId(o)<<inc_endl ;
    this->PrintAttribut(o)<<iendl ;
    o<<"Default Attribut"<<inc_endl ;
    defaultAttribut->Print(o)<<dec_endl ;
    o<<"Group "<<getName()<<IncIndent ;
    for(it2=groupList->begin();it2!=groupList->end();it2++) 
    {
      o<<iendl ;
      (*it2)->Print(o) ;
    }
    o<<DecIndent<<iendl ;
    o<<"Child "<<CObject::getName()<<IncIndent ;
    for(it1=childList->begin();it1!=childList->end();it1++) 
    {
      o<<iendl;
      (*it1)->Print(o) ;
    }
    o<<DecIndent<<DecIndent ;
    return o ;
  }
  
  void Print(void)
  {
    Print(StdOut) ;
  }

} ;



#endif
