#ifndef FIELD_ATTRIBUT_HPP
#define FIELD_ATTRIBUT_HPP

#include "attribut_registrar.hpp"
#include "attribut.hpp"
#include "declare_attribut.hpp"

class CFieldAttribut : public virtual CAttributRegistrar
{
  public :
   
   DECLARE_ATTR(name,string) ; 
   DECLARE_ATTR(level,int) ;
   
  CFieldAttribut(void)
  {
    RegisterAttribut(name) ;
    RegisterAttribut(level) ;
  }
  
} ;

#endif
