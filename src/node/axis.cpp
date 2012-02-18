#include "axis.hpp"

#include "attribute_template_impl.hpp"
#include "object_template_impl.hpp"
#include "group_template_impl.hpp"
#include "transfert_parameters.hpp"

namespace xmlioserver {
namespace tree {
   
   /// ////////////////////// Définitions ////////////////////// ///

   CAxis::CAxis(void)
      : CObjectTemplate<CAxis>()
      , CAxisAttributes(), isChecked(false), relFiles()
   { /* Ne rien faire de plus */ }

   CAxis::CAxis(const StdString & id)
      : CObjectTemplate<CAxis>(id)
      , CAxisAttributes(), isChecked(false), relFiles()
   { /* Ne rien faire de plus */ }

   CAxis::~CAxis(void)
   { /* Ne rien faire de plus */ }

   ///---------------------------------------------------------------

   const std::set<StdString> & CAxis::getRelFiles(void) const
   {
      return (this->relFiles);
   }

   bool CAxis::IsWritten(const StdString & filename) const
   {
      return (this->relFiles.find(filename) != this->relFiles.end());
   }

   void CAxis::addRelFile(const StdString & filename)
   {
      this->relFiles.insert(filename);
   }

   //----------------------------------------------------------------

   StdString CAxis::GetName(void)   { return (StdString("axis")); }
   StdString CAxis::GetDefName(void){ return (CAxis::GetName()); }
   ENodeType CAxis::GetType(void)   { return (eAxis); }

   //----------------------------------------------------------------

   void CAxis::checkAttributes(void)
   {
      if (this->isChecked) return;
      StdSize size = this->size.getValue();
      StdSize true_size = value.getValue()->num_elements();
      if (size != true_size)
         ERROR("CAxis::checkAttributes(void)",
               << "Le tableau \'value\' a une taille différente de celle indiquée dans l'attribut \'size\'")

      this->isChecked = true;
   }

   ///---------------------------------------------------------------

} // namespace tree
} // namespace xmlioserver
