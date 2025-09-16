#ifndef __XIOS_CObjectFactory__
#define __XIOS_CObjectFactory__

#include <memory>

/// XIOS headers ///
#include "xios_spl.hpp"
#include "exception.hpp"
#include "object_template.hpp"

namespace xios
{
   /// ////////////////////// Déclarations ////////////////////// ///
   class CContext ;
   
   class CObjectFactory
   {
      public :

         /// Mutateurs ///
         static void SetCurrentContextId(const StdString & context);

         /// Accesseurs ///
         static StdString & GetCurrentContextId(void);
         static void clearCurrentContextId(void) {CurrContext.clear(); CurrContext.shrink_to_fit(); }

        template <typename U>
            static  std::shared_ptr<U> GetObject(const StdString& context,const StdString & id);

         template <typename U>
            static  std::shared_ptr<U> GetObject(const U * const object);

         template <typename U>
            static  std::shared_ptr<U> GetObject(const StdString& context, const U * const object);

         template <typename U>
            static  int GetObjectNum(const string& contextId);
         template <typename U>
            static  int GetObjectIdNum(const string& contextId);

         template <typename U>
            static  const std::vector<std::shared_ptr<U> >& GetObjectVector(const StdString & context );

         /// Tests ///

         template <typename U>
            static  bool HasObject(const StdString& context,const StdString & id);

         /// Instanciateur ///

         template <typename U>
            static  std::shared_ptr<U> CreateObject(const StdString& contextId, const StdString & id );

         template <typename U>
            static  std::shared_ptr<U> CreateObject(CContext* context, const StdString & id );

         template <typename U>
             static std::shared_ptr<U> CreateAlias(const StdString& contextId, const StdString& id, const StdString& alias) ;
         
         template <typename U> static const StdString GetUIdBase(const string& contextId);
         template <typename U> static StdString GenUId(const StdString& contextId);
         template <typename U> static bool IsGenUId(const string& contextId, const StdString& id);
         template <typename U> static void deleteContext(const StdString & context) ;
         template <typename U> static void deleteAllContexts(void) ;
         template <typename U> static void dumpObjects(void) ;
      private :

         /// Propriétés statiques ///
         static StdString CurrContext;

   }; // class CObjectFactory
} // namespace xios


#endif // __XIOS_CObjectFactory__
