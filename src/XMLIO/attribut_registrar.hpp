#ifndef __ATTRIBUT_REGISTRAR__
#define __ATTRIBUT_REGISTRAR__

#include "attribut.hpp"

namespace XMLIOSERVER
{
   class AttributRegistrar
   {
      public :
        
         AttributRegistrar():attrList()
         {/* Ne rien faire de plus */}
      
         void RegisterAttribut(BaseAttribut* attribut){ attrList.addObject(attribut); }         
         StrHashMap<BaseAttribut>& getAttributList(void) { return (attrList); }         
         size_t getNbAttributes() const {return (attrList.getSize()); }
         bool hasAttribut(const string _id) { return (attrList.hasMappedValue(_id)); }
         
         BaseAttribut* getAttribut(const string _id) throw (XMLIOUndefinedValueException) { return (attrList[_id]); }
        
         void setSAttribut(const string& att_name, const std::string& value)
         {
            //std::cout << "Attribut :" <<  att_name<< ", " << value<< std::endl;

            if (hasAttribut(att_name))
            { getAttribut(att_name)->setFromString(value) ; }
            else
            {
               ostringstream oss;
               oss << "CAttributRegistrar::setAttribut<ValueType>, could not find <<" << att_name <<">> attribut in registred list" <<">>";
               throw XMLIOUndefinedValueException(oss.str());
            }         
         }
         
         friend ostream& operator<< (ostream& out, const AttributRegistrar& c) 
         { out << c.toString(); return (out);}   
               
      protected :
      
         string toString(void) const
         {
            ostringstream st("A réimplémenter");
            // A compléter
            return (st.str());
         }
         
      public :
         
        ~AttributRegistrar(void)
        {/* Ne rien faire de plus */}
        
      private :
         StrHashMap<BaseAttribut> attrList;
        
   }; // class AttributRegistrar  
}; // namespace XMLIOSERVER


#endif //__ATTRIBUT_REGISTRAR__
