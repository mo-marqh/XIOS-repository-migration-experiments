#ifndef BASE_OBJECT_HPP
#define BASE_OBJECT_HPP

#include "xmlio_std.hpp"


class CBase_object : public virtual CAttributRegistrar
{
 public :
  string id ;
  bool hasId ;

  CBase_object(void) : hasId(false) {}
  CBase_object(const string& Id) : id(Id), hasId(true) {}
  
  void setId(const string& Id)
  {
    id=Id ;
    hasId=true ;
  }

  ostream & PrintId(ostream& o)
  {
    if (hasId) o<<" ---> Id = "<<id ;
    else o<<" ---> No Id " ;
    return o ;
  }

} ;

#endif
