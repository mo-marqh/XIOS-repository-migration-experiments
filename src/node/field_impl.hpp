
#ifndef __FIELD_IMPL_HPP__
#define __FIELD_IMPL_HPP__

#include "xios_spl.hpp"
#include "field.hpp"
#include "context.hpp"
#include "grid.hpp"
#include "timer.hpp"
#include "array_new.hpp"
#include "source_filter.hpp"
#include "store_filter.hpp"


namespace xios {

  template <int N>
  void CField::setData(const CArray<double, N>& _data)
  {
    if (clientSourceFilter)
      clientSourceFilter->streamData(CContext::getCurrent()->getCalendar()->getCurrentDate(), _data);
    else if (!field_ref.isEmpty() || !content.empty())
      ERROR("void CField::setData(const CArray<double, N>& _data)",
            << "Impossible to receive data from the model for a field [ id = " << getId() << " ] with a reference or an arithmetic operation.");

    /*if (hasInstantData)
    {
      grid->inputField(_data, instantData);
      for(list< pair<CField *,int> >::iterator it=fieldDependency.begin(); it!=fieldDependency.end(); ++it)  it->first->setSlot(it->second);
    }

    if (!hasExpression)
    {
      const std::vector<CField*>& refField=getAllReference();
      std::vector<CField*>::const_iterator  it = refField.begin(), end = refField.end(),
                                            itFilterSrc, iteFilterSrc;

      for (; it != end; it++)
      {
        const std::vector<CField*>& fieldFilterSources = (*it)->getFilterSources();
        if (!fieldFilterSources.empty())
        {
          itFilterSrc  = fieldFilterSources.begin();
          iteFilterSrc = fieldFilterSources.end();
          for (; itFilterSrc != iteFilterSrc; ++itFilterSrc)
          {
            (*itFilterSrc)->updateDataWithoutOperation(_data, (*itFilterSrc)->data);
            if ((*it)->filteredData.numElements() != (*it)->grid->storeIndex_client.numElements())
            {
               (*it)->filteredData.resize((*it)->grid->storeIndex_client.numElements());
            }
            (*it)->applyFilter((*itFilterSrc)->data, (*it)->filteredData);
          }
          if ((*it)->hasOutputFile || (*it)->hasFieldOut) (*it)->updateFilteredData((*it)->filteredData);
        }
        else
        {
          (*it)->setData(_data);
        }
      }
      if (hasOutputFile || hasFieldOut) updateData(_data);
    }*/
  }

  void CField::setDataFromExpression(const CArray<double, 1>& _data)
  {
    if (hasInstantData)
    {
      instantData=_data;
      for(list< pair<CField *,int> >::iterator it=fieldDependency.begin(); it!=fieldDependency.end(); ++it)  it->first->setSlot(it->second);
    }

    if (!hasExpression)
    {
      const std::vector<CField*>& refField=getAllReference();
      std::vector<CField*>::const_iterator  it = refField.begin(), end = refField.end();

      for (; it != end; it++) (*it)->setDataFromExpression(_data);
      if (hasOutputFile || hasFieldOut) updateDataFromExpression(_data);
    }
  }

   template<int N>
   void CField::updateDataWithoutOperation(const CArray<double, N>& _data, CArray<double,1>& updatedData)
   {
     if (updatedData.numElements() != this->grid->storeIndex_client.numElements())
     {
        updatedData.resize(this->grid->storeIndex_client.numElements());
        this->grid->inputField(_data, updatedData);
     }
   }

   template<int N>
   bool CField::updateFilteredData(CArray<double, N>& filteredData)
   {
      CContext* context=CContext::getCurrent();
      const CDate & currDate = context->getCalendar()->getCurrentDate();
      const CDate opeDate      = *last_operation + freq_operation;
      const CDate writeDate    = *last_Write     + freq_write;
      bool doOperation, doWrite;


      info(50) << "CField::updateData " << currDate <<  " : send data to " << this->getBaseFieldId() << std::endl;
      info(50) << "Next operation "  << opeDate<<std::endl;

      doOperation = (opeDate <= currDate);
      if (isOnceOperation)
        if (isFirstOperation) doOperation=true;
        else doOperation=false;

      if (doOperation)
      {
         if (this->data.numElements() != filteredData.numElements())
         {
            this->data.resize(filteredData.numElements());
         }

         (*this->foperation)(filteredData);

         *last_operation = currDate;
         info(50) << "(*last_operation = currDate) : " << *last_operation << " = " << currDate << std::endl;
      }

      doWrite = (writeDate < (currDate + freq_operation));
      if (isOnceOperation)
      {
        if(isFirstOperation)
        {
          doWrite=true;
          isFirstOperation=false;
        }
        else doWrite=false;
      }

      if (doWrite)
      {
         this->foperation->final();
         *last_Write = writeDate;
         if (hasOutputFile)
         {
           info(50) << "(*last_Write = currDate) : " << *last_Write << " = " << currDate << std::endl;
           CTimer::get("XIOS Send Data").resume();
           sendUpdateData();
           CTimer::get("XIOS Send Data").suspend();
         }

//         if (hasFieldOut)
//         {
//           fieldOut->setDataFromExpression(data);
//         }
         return (true);
      }

      return (false);
   }

   template <int N>
   bool CField::updateData(const CArray<double, N>& _data)
   {
      CContext* context=CContext::getCurrent();
      const CDate & currDate = context->getCalendar()->getCurrentDate();
      const CDate opeDate      = *last_operation + freq_operation;
      const CDate writeDate    = *last_Write     + freq_write;
      bool doOperation, doWrite;


      info(50) << "CField::updateData " << currDate <<  " : send data to " << this->getBaseFieldId() << std::endl;
      info(50) << "Next operation "  << opeDate<<std::endl;

      doOperation = (opeDate <= currDate);
      if (isOnceOperation)
        if (isFirstOperation) doOperation=true;
        else doOperation=false;

      if (doOperation)
      {
         if (this->data.numElements() != this->grid->storeIndex_client.numElements())
         {
            this->data.resize(this->grid->storeIndex_client.numElements());
         }

         CArray<double,1> input(data.numElements());
         this->grid->inputField(_data, input);
         (*this->foperation)(input);

         *last_operation = currDate;
         info(50) << "(*last_operation = currDate) : " << *last_operation << " = " << currDate << std::endl;
      }

      doWrite = (writeDate < (currDate + freq_operation));
      if (isOnceOperation)
      {
        if(isFirstOperation)
        {
          doWrite=true;
          isFirstOperation=false;
        }
        else doWrite=false;
      }

      if (doWrite)
      {
         this->foperation->final();
         *last_Write = writeDate;
         if (hasOutputFile)
         {
           info(50) << "(*last_Write = currDate) : " << *last_Write << " = " << currDate << std::endl;
           CTimer::get("XIOS Send Data").resume();
           sendUpdateData();
           CTimer::get("XIOS Send Data").suspend();
         }

         if (hasFieldOut)
         {
           fieldOut->setDataFromExpression(data);
         }
         return (true);
      }

      return (false);
   }

   bool CField::updateDataFromExpression(const CArray<double, 1>& _data)
   {
      CContext* context=CContext::getCurrent();
      const CDate & currDate = context->getCalendar()->getCurrentDate();
      const CDate opeDate      = *last_operation + freq_operation;
      const CDate writeDate    = *last_Write     + freq_write;
      bool doOperation, doWrite;


      info(50) << "CField::updateData " << currDate <<  " : send data to " << this->getBaseFieldId() << std::endl;
      info(50) << "Next operation "  << opeDate<<std::endl;

      doOperation = (opeDate <= currDate);
      if (isOnceOperation)
        if (isFirstOperation) doOperation=true;
        else doOperation=false;

      if (doOperation)
      {
         if (this->data.numElements() != this->grid->storeIndex_client.numElements())
         {
            this->data.resize(this->grid->storeIndex_client.numElements());
         }

        (*this->foperation)(_data);

         *last_operation = currDate;
         info(50) << "(*last_operation = currDate) : " << *last_operation << " = " << currDate << std::endl;
      }

      doWrite = (writeDate < (currDate + freq_operation));
      if (isOnceOperation)
      {
        if(isFirstOperation)
        {
          doWrite=true;
          isFirstOperation=false;
        }
        else doWrite=false;
      }

      if (doWrite)
      {
         this->foperation->final();
         *last_Write = writeDate;
         if (hasOutputFile)
         {
           info(50) << "(*last_Write = currDate) : " << *last_Write << " = " << currDate << std::endl;
           CTimer::get("XIOS Send Data").resume();
           sendUpdateData();
           CTimer::get("XIOS Send Data").suspend();
         }

         if (hasFieldOut)
         {
           fieldOut->setDataFromExpression(data);
         }
         return (true);
      }

      return (false);
   }

  template <int N>
  void CField::getData(CArray<double, N>& _data) const
  {
    if (storeFilter)
    {
      CDataPacket::StatusCode status = storeFilter->getData(CContext::getCurrent()->getCalendar()->getCurrentDate(), _data);

      if (status == CDataPacket::END_OF_STREAM)
        ERROR("void CField::getData(CArray<double, N>& _data) const",
              << "Impossible to access field data, all the records of the field [ id = " << getId() << " ] have been already read.");

      /*CContext* context = CContext::getCurrent();
      const CDate& currentDate = context->getCalendar()->getCurrentDate();

      while (isReadDataRequestPending)
        context->checkBuffersAndListen();

      if (isEOF)
        ERROR("void CField::getData(CArray<double, N>& _data) const",
              << "Impossible to access field data, all the records of the field [ id = " << getId() << " ] have been already read.");

      grid->outputField(instantData, _data);*/
    }
    else
    {
      ERROR("void CField::getData(CArray<double, N>& _data) const",
            << "Impossible to access field data, the field [ id = " << getId() << " ] does not have read access.");
    }
  }
} // namespace xios

#endif
