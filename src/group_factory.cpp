#include "group_factory.hpp"

namespace xmlioserver
{
   /// ////////////////////// Définitions ////////////////////// ///
   StdString CGroupFactory::CurrContext("");

   void CGroupFactory::SetCurrentContextId(const StdString & context)
   { 
      CGroupFactory::CurrContext = context;
   }

   StdString & CGroupFactory::GetCurrentContextId(void)
   { 
      return (CGroupFactory::CurrContext);
   }

} // namespace xmlioserver
