#ifndef __XIOS_CObjectFactory_impl__
#define __XIOS_CObjectFactory_impl__

#include "object_factory.hpp"

namespace xios
{
   /// ////////////////////// Définitions ////////////////////// ///
   template <typename U>
       int CObjectFactory::GetObjectNum(const string& contextId)
   {
      return (U::AllVectObj[contextId].size());
   }

   template <typename U>
      int CObjectFactory::GetObjectIdNum(const string& contextId)
   {
      return (U::AllMapObj[contextId].size());
   }

   template <typename U>
      bool CObjectFactory::HasObject(const StdString & context, const StdString & id)
   {
      if (U::AllMapObj.find(context) == U::AllMapObj.end()) return false ;
      else return (U::AllMapObj[context].find(id) !=  U::AllMapObj[context].end());
   }

   template <typename U>
      std::shared_ptr<U> CObjectFactory::GetObject(const StdString & contextId, const U * const object)
   {
      std::vector<std::shared_ptr<U> > & vect =
                     U::AllVectObj[contextId];

      typename std::vector<std::shared_ptr<U> >::const_iterator
         it = vect.begin(), end = vect.end();

      for (; it != end; it++)
      {
         std::shared_ptr<U> ptr = *it;
         if (ptr.get() == object)
            return (ptr);
      }

      ERROR("CObjectFactory::GetObject(const StdString & contextId, const U * const object)",
               << "[type = " << U::GetName() << ", adress = " << object << "] "
               << "object was not found.");
      return (std::shared_ptr<U>()); // jamais atteint
   }

   template <typename U>
      std::shared_ptr<U> CObjectFactory::GetObject(const U * const object)
   {
      std::vector<std::shared_ptr<U> > & vect =
                     U::AllVectObj[object->getContext()->getId()];

      typename std::vector<std::shared_ptr<U> >::const_iterator
         it = vect.begin(), end = vect.end();

      for (; it != end; it++)
      {
         std::shared_ptr<U> ptr = *it;
         if (ptr.get() == object)
            return (ptr);
      }

      ERROR("CObjectFactory::GetObject(const U * const object)",
               << "[type = " << U::GetName() << ", adress = " << object << "] "
               << "object was not found.");
      return (std::shared_ptr<U>()); // jamais atteint
   }

   template <typename U>
      std::shared_ptr<U> CObjectFactory::GetObject(const StdString & context, const StdString & id)
   {
      if (!CObjectFactory::HasObject<U>(context,id))
         ERROR("CObjectFactory::GetObject(const StdString & id)",
               << "[ id = " << id << ", U = " << U::GetName() <<", context = "<<context<< " ] "
               << "object was not found.");
      return (U::AllMapObj[context][id]);
   }

   template <typename U>
   std::shared_ptr<U> CObjectFactory::CreateObject(const StdString& contextId, const StdString& id)
   {
      if (CObjectFactory::HasObject<U>(contextId, id))
      {
         return CObjectFactory::GetObject<U>(contextId, id);
      }
      else
      {
         std::shared_ptr<U> value(new U(id.empty() ? CObjectFactory::GenUId<U>(contextId) : id));

         U::AllVectObj[contextId].insert(U::AllVectObj[contextId].end(), value);
         U::AllMapObj[contextId].insert(std::make_pair(value->getId(), value));

         return value;
      }
   }

   template <typename U>
   std::shared_ptr<U> CObjectFactory::CreateObject(CContext* context, const StdString& id)
   {
      string contextId = (context == nullptr) ? id : context->getId() ;

      if (CObjectFactory::HasObject<U>(contextId, id))
      {
         return CObjectFactory::GetObject<U>(contextId, id);
      }
      else
      {
         std::shared_ptr<U> value(new U(context, id.empty() ? CObjectFactory::GenUId<U>(contextId) : id));

         U::AllVectObj[contextId].insert(U::AllVectObj[contextId].end(), value);
         U::AllMapObj[contextId].insert(std::make_pair(value->getId(), value));

         return value;
      }
   }


   template <typename U>
   std::shared_ptr<U> CObjectFactory::CreateAlias(const StdString& ContextId, const StdString& id, const StdString& alias)
   {

      if (CObjectFactory::HasObject<U>(ContextId, alias))
      {
         return CObjectFactory::GetObject<U>(ContextId, alias);
      }
      else
      {
        if (! CObjectFactory::HasObject<U>(ContextId, id))
        {
            ERROR("CObjectFactory::CreateAlias(const StdString& id, const StdString& alias)",
               << "[ id = " << id << " alias = "<<alias<<" ] object id doesn't exist"); 
        }
        else  
        {
          std::shared_ptr<U> value = CObjectFactory::GetObject<U>(ContextId, id);  
          U::AllMapObj[ContextId].insert(std::make_pair(alias, value));
          return value;
         }
      }
   }

   template <typename U>
      const std::vector<std::shared_ptr<U> >& CObjectFactory::GetObjectVector(const StdString & context)
   {
      return (U::AllVectObj[context]);
   }

   template <typename U>
   const StdString CObjectFactory::GetUIdBase(const string& contextId)
   {
      StdString base ; 
      base = contextId + "__" + U::GetName() + "_undef_id_";
      return base;
   }

   template <typename U>
   StdString CObjectFactory::GenUId(const StdString& contextId)
   {
      StdOStringStream oss;
      oss << GetUIdBase<U>(contextId) << U::GenId[contextId]++;
      return oss.str();
   }

   template <typename U>
   bool CObjectFactory::IsGenUId(const string& contextId, const StdString& id)
   {
      const StdString base = GetUIdBase<U>(contextId);
      return (id.size() > base.size() && id.compare(0, base.size(), base) == 0);
   }

   template <typename U> 
   void CObjectFactory::deleteContext(const StdString & context)
   {
     for (auto& v : U::AllVectObj[context]) v.reset() ;
     U::AllVectObj[context].clear() ;
     U::AllVectObj.erase(context) ; 
     for (auto& m : U::AllMapObj[context])  m.second.reset() ;
     U::AllMapObj[context].clear() ;
     U::AllMapObj.erase(context) ;

     U::GenId.erase(context) ;
   }

   template <typename U> 
   void CObjectFactory::deleteAllContexts(void)
   {
     list<StdString> contextList ;
     for(auto& context : U::AllMapObj) contextList.push_back(context.first) ;
     for(auto& context : contextList) deleteContext<U>(context) ;
   }

   
   template <typename U> 
   void CObjectFactory::dumpObjects(void)
   {
     for (auto& context : U::AllMapObj) 
      for(auto& m : context.second)
      {
        info(100)<<"Dump All Object"<<endl ;
        info(100)<<"Object from context "<<context.first<<" with id "<<m.first<<endl ; 
      }
   }
} // namespace xios

#endif // __XIOS_CObjectFactory_impl__
