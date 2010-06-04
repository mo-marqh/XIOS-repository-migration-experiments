#ifndef __ATTRIBUT__
#define __ATTRIBUT__

#include "base_attribut.hpp"

using XMLIOSERVER::BaseAttribut;
using XMLIOSERVER::XMLIOUndefinedValueException;
using std::ostringstream;

namespace XMLIOSERVER
{
   class IStringStream_alt
   {
      public : 
         IStringStream_alt(const std::string& str) : iss(str)
         { /* Ne rien faire de plus */}
         
         istream& operator>> (std::string& s) {s.assign(this->iss.str()); return (this->iss);}
         istream& operator>> (int& s) { return (iss>>s); }
         istream& operator>> (bool& s)
         {
            if(!this->iss.str().compare(string(".TRUE."))) s =  true;
            else s = false;
            return (this->iss);
         }
         
      private : 
         istringstream iss;
         
   }; // class IStringStream_alt


   template <class Ctype>
      class Attribut : public BaseAttribut
   {
      public :
     
         bool hasValue ;
         Ctype value ;

         virtual const char * getName(void) const = 0 ;
      
         Attribut(void) : hasValue(false) {} ;
         Attribut(const Ctype& value_) : hasValue(true), value(value_) {} ;
         
         Attribut(const Attribut& attr) : hasValue(attr.hasValue) 
         {  if (hasValue) value=attr.value ; }

         Attribut& operator = (const Attribut & attr)
         { 
            hasValue=attr.hasValue ;
            if (hasValue) value=attr.value ;
            return *this ; 
         }
     
         virtual const char * getType(void) const = 0;
     
         virtual void setFromString(const std::string str) 
         {    
            IStringStream_alt iss(str); Ctype val;
            iss >> val;
            this->setValue(val);
         }   

         operator Ctype()
         {
            if (!hasValue)
            {
               ostringstream oss;
               oss << "CAttribut& CAttribut<Ctype>::operator Ctype , access to undefined value of attribut <<" << this->getName() <<">>";
                                                                          
               throw XMLIOUndefinedValueException(oss.str());
            }
            return value ;
         }

         virtual ostream& print(ostream & o) const
         {
            o<<"Attribut : "<<getName()<<"  ---->" ;
            if (hasValue) o<< boolalpha <<" value = "<< value ;
            else o<< "/" ;
            return o ;
         }

         virtual void setValue(const Ctype & value_)
         { hasValue=true ; value=value_ ; }

         virtual void getValue(Ctype & value_) const
         {
            if (!hasValue)
            {
               ostringstream oss;
               oss << "CAttribut& CAttribut<Ctype>::operator Ctype , access to undefined value of attribut <<" << this->getName() <<">>";
                                                                          
               throw XMLIOUndefinedValueException(oss.str());
            }
            value_=value ;
         }

   }; // class Attribut 
}; // namespace XMLIOSERVER
  
#endif //__ATTRIBUT__
