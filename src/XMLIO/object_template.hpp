#ifndef OBJECT_TEMPLATE_HPP
#define OBJECT_TEMPLATE_HPP


#include "base_object.hpp"
#include "xmlio_std.hpp"

template <typename CObject>
class CObjectTemplate : public CBase_object
{
  public:
  
  static vector<CObject *> * List ;
  static unordered_map<string,CObject *>* listId ;

  CObjectTemplate(void) : CBase_object() {} ;
  CObjectTemplate(const string & Id) : CBase_object(Id) {} ;

  static void SwapContext(unordered_map<string,CObject *>* newListId,vector<CObject *> * newList)
  {
    listId=newListId ;
    List=newList ;
  }
  
  static bool found(const string & Id)
  {
    if (listId->find(Id)==listId->end()) return false ;
    else return true ;
  }
  
  static CObject* get(const string & Id)
  {
    typename unordered_map<string,CObject*>::iterator It ;

    It=listId->find(Id) ;
    if (It==listId->end())
    {
      error("CObjectTemplate<object>::get(const string & Id)")<<"Object <<"<<Id<<">> does not exist"<<endl ;
      return 0 ;
    }
    else return It->second ;
  }
      
  static CObject* getNew(void)
  {
    CObject * newObject=new CObject ;
    List->push_back(newObject) ;
    return newObject ;
  }
  
  static CObject* getNew(const string & Id)
  {
    CObject* newField=new CObject(Id) ;
    
    pair<string,CObject *> value(Id,newField) ;
    if (listId->insert(value).second) 
    {
      List->push_back(newField) ;
      return newField ;
    }
    else
    {
      error("CObjectTemplate::getNew(const string & Id)")<<"Object <<"<<Id<<">> is already present"<<endl ;
      return 0 ;
    }
  }
  
  static CObject* getOrAppend(const string & Id)
  {
    if (found(Id)) return get(Id) ;
    else return getNew(Id) ;
  }

  ostream & Print(ostream& o)
  {
    o<<"Object : "<<CObject::getName()<<" : " ; 
    PrintId(o)<<inc_endl ;
    PrintAttribut(o) ;
    o<<DecIndent ;
    return o ;
  }
  
  void Print(void)
  {
    Print(StdOut) ;
  }

   
} ;

template <typename CObject>
vector<CObject *>* CObjectTemplate<CObject>::List=new vector<CObject*>  ;

template <typename CObject>
unordered_map<string,CObject *>* CObjectTemplate<CObject>::listId=new unordered_map<string,CObject *>  ;
#endif
