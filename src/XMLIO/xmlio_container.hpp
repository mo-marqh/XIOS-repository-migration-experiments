#ifndef __XMLIO_CONTAINER__
#define __XMLIO_CONTAINER__ 

// Classes utilisées issues de Poco
using Poco::HashMap;
using Poco::Hash;

// Classes utilisées issues de la STL
using std::vector;
using std::string;
using std::pair;
using std::ostream;

// Classes XMLIOSERVER
using XMLIOSERVER::XMLIOUndefinedValueException;

namespace XMLIOSERVER
{
   template<class Key, class Mapped, class HashFunc = Hash<Key> >
      class ExHashMap
         : private HashMap<Key, Mapped*, Hash<Key> >
   {
      public :
         ExHashMap() :  HashMap<Key, Mapped*, Hash<Key> >(), _elemList()
         {/* Ne rien faire de plus */}
               
         Mapped* operator[] (const Key& kval) throw (XMLIOUndefinedValueException)
         { 
            if(!hasMappedValue(kval))
               throw XMLIOUndefinedValueException("Appel de la méthode ExHashMap::operator[] invalide.");
            return (find(kval)->second); 
         }
          
         bool hasMappedValue(const Key& kval) {return (find(kval) != this->end());}
          
         const vector<Mapped*> getVector(void) { return (this->_elemList); }         

         size_t getVectorSize(void) const {return (this->_elemList.size());}
         size_t getSize(void) const {return (this->size());}
         
         virtual ~ExHashMap()
         {/* Ne rien faire de plus */}
         
      protected :
      
         bool addValue(const Key& kval, Mapped* element)
         {
            pair<typename ExHashMap::Iterator,bool> p = this->insert(make_pair (kval,element));
            if(!p.second) return (false);
            return (this->addValue(element));
         }
         
         bool addValue(Mapped* element)
         {   this->_elemList.insert(this->_elemList.end(), element);   return (true);   }   
         
         
         void removeValue(const Key& kval) // Non testé
         { Mapped* element = find(kval)->second; removeValue(element); this->erase(kval); }
         
         void removeValue(const Mapped* element) // Non testé
         {
            for (int i = 0; i < this->_elemList.size(); i++)             
               if (*this->_elemList[i] == element )
                  this->_elemList.erase(this->_elemList.begin()+i);
                  
            delete element;
         }
         
      private :   
         vector<Mapped*> _elemList;
         
   }; // class ExHashMap
   
   /////////////////////////////////////////////////////////////
   
   template<class Mapped>
      class StrHashMap
         : public ExHashMap<string, Mapped, Hash<string> >
   {
      public :
      
         StrHashMap() :  ExHashMap<string, Mapped, Hash<string> >()   
         {/* Ne rien faire de plus */}   
         
         bool addObject(Mapped* element)
         { if(element->hasId()) return(addValue(element->getId(), element));   return(addValue(element)); }
                  
         bool removeObject(const string& kval)
         { 
            if(!ExHashMap<string, Mapped, Hash<string> >::hasMappedValue(kval)) return (false);
            ExHashMap<string, Mapped, Hash<string> >::removeValue(kval); return (true); 
         }
         
         virtual ~StrHashMap()   
         {/* Ne rien faire de plus */}
         
   }; // class StrHashMap
            
}; // namespace XMLIOSERVER

#endif // __XMLIO_CONTAINER__ 
