#ifndef __XMLIO_OBJECT_TEMPLATE__
#define __XMLIO_OBJECT_TEMPLATE__ 

// Classes utilisées issues de la STL
using std::pair;
using std::string;
using std::ostream;

using XMLIOSERVER::StrHashMap;
   
namespace XMLIOSERVER
{
   template <class T>
      class ObjectTemplate : public AbstractObject
   {
      public :
         
         static T& CreateObject(const string _id) throw (XMLIOUndefinedValueException) 
         {
            // Si l'identifiant est répertorié, on retourne l'élément existant.
            if(ObjectTemplate<T>::HasObject(_id))
               return (ObjectTemplate<T>::GetObject(_id));
                              
            // Ajout d'un nouvel objet si l'identifiant n'est pas répertorié.           
            ObjectTemplate<T>::AllListObj[CurrContext].addObject(new T(_id));
            
            return (ObjectTemplate<T>::GetObject(_id));
         }
         
         static T& CreateObject(void)
         {
            T* value = new T;
            ObjectTemplate<T>::AllListObj[CurrContext].addObject(value);
            return (*value);
         }
         
         static T& GetObject(const string _id) throw (XMLIOUndefinedValueException)
         { return (*ObjectTemplate<T>::AllListObj[CurrContext][_id]); }
         
         static bool HasObject(const string _id)// << Bug
         { return (ObjectTemplate<T>::AllListObj[CurrContext].hasMappedValue(_id)); }
         
         static const StrHashMap<T>& GetCurrentListObject(void) { return (AllListObj[CurrContext]); }
         static HashMap<string, StrHashMap<T> >& GetAllListObject(void) { return (AllListObj); }
                
         static void SetContext(const string& id){ ObjectTemplate<T>::CurrContext = id; } 
         
         static string& GetCurrentContextId(void) { return (CurrContext); } 
         
         virtual const char* getName(void) const {return ("ObjectTemplate"); }
         
         virtual ~ObjectTemplate(void)
         {/* Ne rien faire de plus */}
         
      protected :
      
         ObjectTemplate(void) : AbstractObject()
         {/* Ne rien faire de plus */}         
         ObjectTemplate(const string& _id) : AbstractObject(_id) 
         {/* Ne rien faire de plus */}
         
      private :
         
         static string CurrContext;
         static HashMap<string, StrHashMap<T> > AllListObj;
      
   };// class ObjectTemplate
   
   template <class T> string ObjectTemplate<T>::CurrContext ;
   template <class T> HashMap<string, StrHashMap<T> > ObjectTemplate<T>::AllListObj;
   
}// namespace XMLIOSERVER
   
#endif // __XMLIO_OBJECT_TEMPLATE__ 
