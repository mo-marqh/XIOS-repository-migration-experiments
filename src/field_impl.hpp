
#ifndef __FIELD_IMPL_HPP__
#define __FIELD_IMPL_HPP__

#include "xmlioserver_spl.hpp"
#include "field.hpp"
#include "context.hpp"
#include "grid.hpp"


namespace xios {

   template <StdSize N>
   void CField::setData(const ARRAY(double, N) _data)
   {
     const std::vector<boost::shared_ptr<CField> > & refField=getAllReference();
     std::vector<boost::shared_ptr<CField> >::const_iterator  it = refField.begin(), end = refField.end();
     
     for (; it != end; it++) (*it)->updateData(_data) ;
    }
    
   template <StdSize N>
      bool CField::updateData(const ARRAY(double, N) _data)
   {        
      shared_ptr<CContext> context=CContext::getCurrent();
      const CDate & currDate = context->getCalendar()->getCurrentDate();
      const CDate opeDate      = *last_operation + freq_operation;
      const CDate writeDate    = *last_Write     + freq_write;       

   
      info(50) << "CField::updateData " << currDate <<  " : send data to " << this->getBaseFieldId() << std::endl;
      info(50) << "Next operation "  << opeDate<<std::endl;

      if (opeDate <= currDate)
      {
         if (this->data->num_elements() != this->grid->storeIndex_client->num_elements())
         {
            this->data->resize(boost::extents[this->grid->storeIndex_client ->num_elements()]);
         }
            
         ARRAY_CREATE(input, double, 1, [this->data->num_elements()]);
         this->grid->inputField(_data, input);          
         (*this->foperation)(input);
         
         *last_operation = currDate;
         info(50) << "(*last_operation = currDate) : " << *last_operation << " = " << currDate << std::endl; 
      }
      
      if (writeDate < (currDate + freq_operation))
      {
         this->foperation->final();
         *last_Write = writeDate;
         info(50) << "(*last_Write = currDate) : " << *last_Write << " = " << currDate	<< std::endl;
         sendUpdateData() ;
         return (true);        
      }

      return (false);
   }

} // namespace xios

#endif
