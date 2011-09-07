#include "variable.hpp"

#include "attribute_template_impl.hpp"
#include "object_template_impl.hpp"
#include "group_template_impl.hpp"

#include "object_factory.hpp"
#include "object_factory_impl.hpp"

namespace xmlioserver {
namespace tree {

   /// ////////////////////// Définitions ////////////////////// ///

   CVariable::CVariable(void)
      : CObjectTemplate<CVariable>()
      , CVariableAttributes()
   { /* Ne rien faire de plus */ }

   CVariable::CVariable(const StdString & id)
      : CObjectTemplate<CVariable>(id)
      , CVariableAttributes()
   { /* Ne rien faire de plus */ }

   CVariable::~CVariable(void)
   { /* Ne rien faire de plus */ }

   StdString CVariable::GetName(void)   { return (StdString("variable")); }
   StdString CVariable::GetDefName(void){ return (CVariable::GetName()); }
   ENodeType CVariable::GetType(void)   { return (eVariable); }

} // namespace tree
} // namespace xmlioserver
