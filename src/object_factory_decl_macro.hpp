#include "object_factory_impl.hpp"
#include "node_type.hpp"


#define macro(U) \
  template std::shared_ptr<U> CObjectFactory::GetObject<U>(const StdString& context,const StdString& id); \
  template std::shared_ptr<U> CObjectFactory::GetObject<U>(const StdString& contextId, const U* const object); \
  template int CObjectFactory::GetObjectNum<U>(const string& contextId); \
  template int CObjectFactory::GetObjectIdNum<U>(const string& contextId); \
  template const std::vector<std::shared_ptr<U> >& CObjectFactory::GetObjectVector<U>(const StdString& context ); \
  template bool CObjectFactory::HasObject<U>(const StdString& context,const StdString& id); \
  template std::shared_ptr<U> CObjectFactory::CreateObject<U>(CContext* context, const StdString& id ); \
  template std::shared_ptr<U> CObjectFactory::CreateAlias<U>(const StdString& contextId, const StdString& id, const StdString& alias ); \
  template const StdString CObjectFactory::GetUIdBase<U>(const string& contextId); \
  template StdString CObjectFactory::GenUId<U>(const string& contextId); \
  template bool CObjectFactory::IsGenUId<U>(const string& contextId, const StdString& id);\
  template void CObjectFactory::deleteContext<U>(const StdString & context) ;\
  template void CObjectFactory::deleteAllContexts<U>() ;\
  template void CObjectFactory::dumpObjects<U>(void) ;




