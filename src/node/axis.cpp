#include "axis.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "message.hpp"
#include "type.hpp"
#include "xmlioserver_spl.hpp"

namespace xios {

   /// ////////////////////// Définitions ////////////////////// ///

   CAxis::CAxis(void)
      : CObjectTemplate<CAxis>()
      , CAxisAttributes(), isChecked(false), relFiles(), baseRefObject()
   { /* Ne rien faire de plus */ }

   CAxis::CAxis(const StdString & id)
      : CObjectTemplate<CAxis>(id)
      , CAxisAttributes(), isChecked(false), relFiles(), baseRefObject()
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

      if (!this->ibegin.isEmpty())
      {
        StdSize ibegin = this->ibegin.getValue();
        if ((ibegin < 0) || (ibegin > size-1))
          ERROR("CAxis::checkAttributes(void)",<< "Attribut <ibegin> of the axis must be non-negative and smaller than size-1") ;
      }
      else this->ibegin.setValue(0);

      if (!this->ni.isEmpty())
      {
        StdSize ni = this->ni.getValue();
        if ((ni < 0) || (ni > size))
          ERROR("CAxis::checkAttributes(void)",<< "Attribut <ni> of the axis must be non-negative and smaller than size") ;
      }
      else this->ni.setValue(size);

      StdSize zoom_begin,zoom_end, zoom_size ;

//      zoom_begin = (this->zoom_begin.isEmpty()) ?  1 : this->zoom_begin.getValue() ;
//      zoom_end = (this->zoom_end.isEmpty()) ?  size : this->zoom_end.getValue() ;
//      zoom_size = (this->zoom_size.isEmpty()) ?  size : this->zoom_size.getValue() ;

      // Maybe index begins at 0 (zero)
      zoom_begin = (this->zoom_begin.isEmpty()) ?  0 : this->zoom_begin.getValue() ;
      zoom_end = (this->zoom_end.isEmpty()) ?  size-1 : this->zoom_end.getValue() ;
      zoom_size = (this->zoom_size.isEmpty()) ?  size : this->zoom_size.getValue() ;

      if (this->zoom_begin.isEmpty()) zoom_begin=zoom_end-zoom_size+1 ;
      if (this->zoom_end.isEmpty()) zoom_end=zoom_begin+zoom_size-1 ;
      if (this->zoom_size.isEmpty()) zoom_size=zoom_end-zoom_begin+1 ;
//
//      if ( (zoom_begin < 1) || (zoom_begin > size) || (zoom_end<1) || (zoom_end>size) || (zoom_size<1) || (zoom_size>size) || (zoom_begin>zoom_end))
//        ERROR("CAxis::checkAttributes(void)",<< "One or more attribut of <zoom_begin>, <zoom_end>, <zoom_size>, are not well specified") ;

      if ( (zoom_begin < 0) || (zoom_begin > size-1) || (zoom_end<1) || (zoom_end>size-1) || (zoom_size<1) || (zoom_size>size) || (zoom_begin>zoom_end))
        ERROR("CAxis::checkAttributes(void)",<< "One or more attribut of <zoom_begin>, <zoom_end>, <zoom_size>, are not well specified") ;

      this->zoom_begin.setValue(zoom_begin) ;
      this->zoom_end.setValue(zoom_end) ;
      this->zoom_size.setValue(zoom_size) ;

      StdSize true_size = value.numElements();
      if (size != true_size)
         ERROR("CAxis::checkAttributes(void)",
               << "The array \'value\' has a different size that the one defined by the \'size\' attribut")

      this->checkData();
      this->checkMask();
      this->isChecked = true;
   }

   void CAxis::checkData()
   {
      if (data_begin.isEmpty()) data_begin.setValue(0);
      if (!data_n.isEmpty() && data_n.getValue() <= 0)
      {
        ERROR("CAxis::checkData(void)",
              << "Data dimension is negative (data_n).") ;
      }
      else if (data_n.isEmpty())
        data_n.setValue(zoom_size.getValue());

      if (data_index.isEmpty())
      {
        int dn = data_n.getValue();
        data_index.resize(dn);
        for (int i = 0; i < dn; ++i) data_index(i) = (i+1);
      }
   }

   void CAxis::checkMask()
   {
      int begin_mask = 0,
          end_mask = ni.getValue()-1;

      if (!zoom_begin.isEmpty())
      {
         int zoom_end = zoom_begin.getValue() + zoom_size.getValue() - 1;

         begin_mask = std::max(ibegin.getValue(), zoom_begin.getValue());
         end_mask   = std::min(ibegin.getValue() + ni.getValue()-1, zoom_end);

         begin_mask -= ibegin.getValue();
         end_mask   -= ibegin.getValue();
      }


      if (!mask.isEmpty())
      {
         if (mask.extent(0) != ni)
            ERROR("CAxis::checkMask(void)",
                  <<"the mask has not the same size than the local axis"<<endl
                  <<"Local size is "<<ni<<"x"<<endl
                  <<"Mask size is "<<mask.extent(0)<<"x");
         for (int i = 0; i < ni; ++i)
         {
           if (i < begin_mask && i > end_mask)  mask(i) = false;
         }
      }
      else // (!mask.hasValue())
      { // Si aucun masque n'est défini,
        // on en crée un nouveau qui valide l'intégralité du domaine.
         mask.resize(ni) ;
         for (int i = 0; i < ni.getValue(); ++i)
         {
               if (i >= begin_mask && i <= end_mask)
                 mask(i) = true;
               else  mask(i) = false;
         }
      }
   }

   DEFINE_REF_FUNC(Axis,axis)

   ///---------------------------------------------------------------

} // namespace xios
