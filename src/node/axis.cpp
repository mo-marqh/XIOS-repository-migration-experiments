#include "axis.hpp"

#include "attribute_template_impl.hpp"
#include "object_template_impl.hpp"
#include "group_template_impl.hpp"

namespace xios {
   
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
      if (this->size.isEmpty())
         ERROR("CAxis::checkAttributes(void)",<< "Attribut <size> of the axis must be specified") ;
      StdSize size = this->size.getValue();
      
      StdSize zoom_begin,zoom_end, zoom_size ;
      
      zoom_begin = (this->zoom_begin.isEmpty()) ?  1 : this->zoom_begin.getValue() ;
      zoom_end = (this->zoom_end.isEmpty()) ?  size : this->zoom_end.getValue() ; 
      zoom_size = (this->zoom_size.isEmpty()) ?  size : this->zoom_size.getValue() ;
      
      if (this->zoom_begin.isEmpty()) zoom_begin=zoom_end-zoom_size+1 ;
      if (this->zoom_end.isEmpty()) zoom_end=zoom_begin+zoom_size-1 ;
      if (this->zoom_size.isEmpty()) zoom_size=zoom_end-zoom_begin+1 ;
      
      if ( (zoom_begin < 1) || (zoom_begin > size) || (zoom_end<1) || (zoom_end>size) || (zoom_size<1) || (zoom_size>size) || (zoom_begin>zoom_end))
        ERROR("CAxis::checkAttributes(void)",<< "One or more attribut of <zoom_begin>, <zoom_end>, <zoom_size>, are not well specified") ;
      this->zoom_begin.setValue(zoom_begin) ;
      this->zoom_end.setValue(zoom_end) ;
      this->zoom_size.setValue(zoom_size) ;
      
      StdSize true_size = value.getValue()->num_elements();
      if (size != true_size)
         ERROR("CAxis::checkAttributes(void)",
               << "Le tableau \'value\' a une taille différente de celle indiquée dans l'attribut \'size\'")

      this->isChecked = true;
   }

   ///---------------------------------------------------------------

} // namespace xios
