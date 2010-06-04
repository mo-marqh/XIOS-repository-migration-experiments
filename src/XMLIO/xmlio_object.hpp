#ifndef __XMLIO_OBJECT__
#define __XMLIO_OBJECT__ 

// Classes utilisées issues de la STL
using std::pair;
using std::string;
using std::ostream;
using std::ostringstream;

// Classes XMLIOSERVER
using XMLIOSERVER::XMLIOException;
using XMLIOSERVER::XMLIOUndefinedValueException;

namespace XMLIOSERVER
{
   class AbstractObject 
   {
      public :
      
         AbstractObject(void) : IdDefined(false)
         {/* Ne rien faire de plus */}
         
         AbstractObject(const string& _id) : id(_id), IdDefined(true)
         {/* Ne rien faire de plus */}      
   
         const string& getId(void) throw (XMLIOUndefinedValueException)
         { if (!IdDefined) throw XMLIOUndefinedValueException("Appel de la méthode AbstractObject::getId invalide."); return (id); }   
         
         bool hasId(void) const { return(IdDefined); }
         void resetId(void) { IdDefined = false ;}
         void setId(const string& _id) { id = _id ; IdDefined = true ;}
         
         bool operator==(const AbstractObject& other)
         {
            // Si l'un ou l'autre des objets n'a pas d'identifiant, les objets de ne sont pas "égaux".
            if(!this->hasId() or !other.hasId()) return false;
            return (id.compare(other.id) == 0);
         }
         
         friend ostream& operator<< (ostream& out, const AbstractObject& c) 
         { out << c.toString(); return (out);}   
         
         virtual ~AbstractObject(void)
         {/* Ne rien faire de plus */}
      
      protected :
      
         virtual string toString(void) const
         {
            ostringstream st("[");
            st << "[" << this->getName();
            if (hasId()) st << " id=\"" <<  id << "\"";
            st << "]" ;
            return (st.str());
         }
         
         virtual const char* getName(void) const {return ("AbstractObject"); }
            
      private :
      
         string id ;
         bool IdDefined ;
      
   };// class AbstractObject
   
}// namespace XMLIOSERVER

#endif // __XMLIO_OBJECT__   
