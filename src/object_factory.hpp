#ifndef __XMLIO_CObjectFactory__
#define __XMLIO_CObjectFactory__

/// boost headers ///
#include <boost/shared_ptr.hpp>

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "object_template.hpp"

namespace xmlioserver
{
   /// ////////////////////// Déclarations ////////////////////// ///
   class CObjectFactory
   {
      public :

         /// Mutateurs ///
         static void SetCurrentContextId(const StdString & context);

         /// Accesseurs ///
         static StdString & GetCurrentContextId(void);

         template <typename U>
            static inline boost::shared_ptr<U> GetObject(const StdString & id);

         template <typename U>
            static inline boost::shared_ptr<U> GetObject(const StdString& context,const StdString & id);

         template <typename U>
            static inline boost::shared_ptr<U> GetObject(const U * const object);

         template <typename U>
            static inline int GetObjectNum(void);
         template <typename U>
            static inline int GetObjectIdNum(void);

         template <typename U>
            static inline const std::vector<boost::shared_ptr<U> > &
               GetObjectVector(const StdString & context = CObjectFactory::GetCurrentContextId());

         /// Tests ///
         template <typename U>
            static inline bool HasObject(const StdString & id);

         template <typename U>
            static inline bool HasObject(const StdString& context,const StdString & id);

         /// Instanciateur ///
         template <typename U>
            static inline boost::shared_ptr<U> CreateObject(const StdString & id = StdString(""));

         template <typename U> static inline StdString GenUId(void) ; 

      private :

         /// Propriétés statiques ///
         static StdString CurrContext;

   }; // class CObjectFactory
} // namespace xmlioserver

#include "object_factory_impl.hpp"

#endif // __XMLIO_CObjectFactory__
