#ifndef BASE_ATTRIBUT_HPP
#define BASE_ATTRIBUT_HPP
#include "xmlio_std.hpp"

class CBaseAttribut
{
  public:
  
  virtual const char * getName(void) const = 0  ;
     
  virtual ostream & print(ostream& o) const = 0  ;
  
  friend ostream& operator <<(ostream& o,const CBaseAttribut& Attr)
  {
    return Attr.print(o) ;
  }

  
  virtual void setValue(const int & value)          { error_set() ; }
  virtual void setValue(const Array<int,1>& value)  { error_set() ; }
  virtual void setValue(const Array<int,2>& value)  { error_set() ; }
  virtual void setValue(const Array<int,3>& value)  { error_set() ; }
  
  virtual void setValue(const double & value)          { error_set() ; }
  virtual void setValue(const Array<double,1>& value)  { error_set() ; }
  virtual void setValue(const Array<double,2>& value)  { error_set() ; }
  virtual void setValue(const Array<double,3>& value)  { error_set() ; }
  
  virtual void setValue(const bool & value)          { error_set() ; }
  virtual void setValue(const Array<bool,1>& value)  { error_set() ; }
  virtual void setValue(const Array<bool,2>& value)  { error_set() ; }
  virtual void setValue(const Array<bool,3>& value)  { error_set() ; }
  
  virtual void setValue(const char * value)            { error_set() ; }
  virtual void setValue(const string & value)          { error_set() ; }
  virtual void setValue(const Array<string,1>& value)  { error_set() ; }
  virtual void setValue(const Array<string,2>& value)  { error_set() ; }
  virtual void setValue(const Array<string,3>& value)  { error_set() ; }
  
  virtual void setValue(const char & value)          { error_set() ; }
  virtual void setValue(const Array<char,1>& value)  { error_set() ; }
  virtual void setValue(const Array<char,2>& value)  { error_set() ; }
  virtual void setValue(const Array<char,3>& value)  { error_set() ; }


  virtual void getValue(int & value) const        { error_get() ; }
  virtual void getValue(Array<int,1>& value) const  { error_get() ; }
  virtual void getValue(Array<int,2>& value) const  { error_get() ; }
  virtual void getValue(Array<int,3>& value) const  { error_get() ; }
  
  virtual void getValue(double & value) const          { error_get() ; }
  virtual void getValue(Array<double,1>& value) const  { error_get() ; }
  virtual void getValue(Array<double,2>& value) const  { error_get() ; }
  virtual void getValue(Array<double,3>& value) const  { error_get() ; }
  
  virtual void getValue(bool & value) const          { error_get() ; }
  virtual void getValue(Array<bool,1>& value) const  { error_get() ; }
  virtual void getValue(Array<bool,2>& value) const  { error_get() ; }
  virtual void getValue(Array<bool,3>& value) const  { error_get() ; }
  
  virtual void getValue(char * value) const            { error_get() ; }
  virtual void getValue(string & value) const          { error_get() ; }
  virtual void getValue(Array<string,1>& value) const  { error_get() ; }
  virtual void getValue(Array<string,2>& value) const  { error_get() ; }
  virtual void getValue(Array<string,3>& value) const  { error_get() ; }
  
  virtual void getValue(char & value) const          { error_get() ; }
  virtual void getValue(Array<char,1>& value) const  { error_get() ; }
  virtual void getValue(Array<char,2>& value) const  { error_get() ; }
  virtual void getValue(Array<char,3>& value) const  { error_get() ; }

  static void error_set(void)
  {
     error("CBaseAttribut::set<type>")<<"Setting value type is incompatible"
                                      <<" with attribut type"<<endl ;
  }

  static void error_get(void)
  {
     error("CBaseAttribut::set<type>")<<"Getting value type is incompatible"
                                      <<" with attribut type"<<endl ;
  }
} ;

#endif 
  
