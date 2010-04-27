#ifndef ATTRIBUT_REGISTRAR_HPP
#define ATTRIBUT_REGISTRAR_HPP

#include "base_attribut.hpp"
#include "xmlio_std.hpp"

class CAttributRegistrar
{
  public :
  
  
  void RegisterAttribut(CBaseAttribut & attribut) ;
  ostream & PrintAttribut(ostream & o) ;

  unordered_map<string,CBaseAttribut *> attrMap ;
  vector<CBaseAttribut *> attrVector ;

  bool hasAttribut(const string& att_name)
  {
    if (attrMap.find(att_name)!=attrMap.end()) return true ;
    else return false ;
  }
  
  template <class ValueType>
  void setAttribut(const string& att_name, ValueType value)
  {
    unordered_map<string,CBaseAttribut *>::iterator it ;
    
    it=attrMap.find(att_name) ;
    if (it!=attrMap.end()) (*it).second->setValue(value) ;
    else error("CAttributRegistrar::setAttribut<ValueType>")
         <<"Could not find <<"<<att_name<<">> attribut in registred list"<<endl ;
    
  }
  
} ;

inline void CAttributRegistrar::RegisterAttribut(CBaseAttribut& Attribut)
{
  attrMap.insert(make_pair(Attribut.getName(),&Attribut)) ;
  attrVector.push_back(&Attribut) ;
}

inline ostream & CAttributRegistrar::PrintAttribut(ostream& o)
{
  vector<CBaseAttribut *>::iterator it ;
  o<<"List of attribut"<<IncIndent ;
  for(it=attrVector.begin(); it!=attrVector.end();it++)
  {
    o<<iendl ;
    (*it)->print(o) ;
  }
  o<<DecIndent ;
  return o ;
}


#endif
