#ifndef ATTRIBUT_HPP
#define ATTRIBUT_HPP

#include "base_attribut.hpp"
#include "xmlio_std.hpp"

template <class Ctype>
class CAttribut : public CBaseAttribut
{
  public :
  
  bool hasValue ;
  Ctype value ;

  virtual const char * getName(void) const = 0 ;
  CAttribut(void) : hasValue(false) {} ;
  CAttribut(const Ctype& value_) : value(value_), hasValue(true) {} ;
  CAttribut(const CAttribut& attr) : hasValue(attr.hasValue) 
  {
     if (hasValue) value=attr.value ;
  } ;

  CAttribut& operator = (const CAttribut & attr)
  { 
     hasValue=attr.hasValue ;
     if (hasValue) value=attr.value ;
     return *this ; 
  } ;

  operator Ctype()
  {
    if (!hasValue) error("CAttribut& CAttribut<Ctype>::operator Ctype")<<"access to undefined value of attribut <<"
                                                                       <<this->getName()<<">>"<<endl;
    return value ;
  } ;

  virtual ostream& print(ostream & o) const
  {
    o<<"Attribut : "<<getName()<<"  ---->" ;
    if (hasValue) o<<" value = "<<value ;
    else o<<" undefined value" ;
    return o ;
  }

  virtual void setValue(const Ctype & value_)
  {
     hasValue=true ;
     value=value_ ;
  }

  virtual void getValue(Ctype & value_) const
  {
    if (!hasValue)  error("void CAttribut<Ctype>::getValue")<<"access to undefined value of attribut <<"
                                                               <<this->getName()<<">>"<<endl;
    value_=value ;
  }

} ;


  
#endif
