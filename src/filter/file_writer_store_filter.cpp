#include "file_writer_store_filter.hpp"
#include "exception.hpp"
#include "field.hpp"
#include "file.hpp"
#include "context.hpp"

namespace xios
{
  CFileWriterStoreFilter::CFileWriterStoreFilter(CGarbageCollector& gc, CField* field)
    : CInputPin(gc, 1)
    , field_(field)

  {
    CContext* context = CContext::getCurrent();

    if (!field) ERROR("CFileWriterStoreFilter::CFileWriterStoreFilter(CField* field)", "The field cannot be null.");
    file_ = field->getFileOut() ;
    grid_= field->getGrid() ;
    freqWrite_ = file_->output_freq ;
    lastWrite_ = context->getCalendar()->getInitDate();
    if (!file_->isEmptyZone() && (field->getGrid()->doGridHaveDataToWrite() || file_->type == CFile::type_attr::one_file))
         needToWrite_=true ;
    else needToWrite_=false;
    lastFileSplit_ = file_->getLastSplit() ;
    nstep_ = 0 ;
    if (!field->scale_factor.isEmpty()) { scaleFactor_ = field->scale_factor ; hasScaleFactor_ = true ; }
    if (!field->add_offset.isEmpty()) { addOffset_ = field->add_offset ; hasAddOffset_ = true ; }
    if (!field->prec.isEmpty() && field->prec == 2) hasRounding_ = true ;
    if (!field->default_value.isEmpty()) {hasDefaultValue_=true ; defaultValue_ = field->default_value ;}
  }

  void CFileWriterStoreFilter::onInputReady(std::vector<CDataPacketPtr> data)
  {
    const CDate currentWrite = lastWrite_ + freqWrite_;
    if (needToWrite_)
    {
      file_->checkWriteFile();
      if (file_->getLastSplit() != lastFileSplit_)
      {
        nstep_ = 0 ;
        lastFileSplit_ = file_->getLastSplit() ;
      }
      nstep_ = nstep_+1;
      
      CArray<double,1> dataIn = data[0]->data ;
      CArray<double,1> fieldData ;

      if (hasAddOffset_ || hasScaleFactor_ || hasRounding_) dataIn = data[0]->data ;
      else dataIn.reference(data[0]->data) ;

      if (hasAddOffset_)   dataIn = dataIn - addOffset_ ;
      if (hasScaleFactor_) dataIn = dataIn / scaleFactor_;
      if (hasRounding_)    dataIn = round(dataIn);

      if (hasDefaultValue_)
      {
        size_t nbData=dataIn.numElements() ;
        for (size_t idx = 0; idx < nbData; ++idx) if ( NumTraits<double>::isNan(dataIn(idx)) ) dataIn(idx)=defaultValue_ ;
      }

      if (field_->getUseCompressedOutput()) fieldData.reference(dataIn) ;
      else
      {
        fieldData.resize(grid_->getWorkflowToFullConnector()->getDstSize());
        if (hasDefaultValue_) grid_->getWorkflowToFullConnector()->transfer(dataIn, fieldData, defaultValue_ ) ;
        else grid_->getWorkflowToFullConnector()->transfer(dataIn, fieldData ) ;
      }  
      nstep_ = file_->getDataOutput()->writeFieldData(field_, fieldData, lastWrite_,currentWrite, nstep_);
    }

    lastWrite_ = currentWrite ;

  }

  bool CFileWriterStoreFilter::mustAutoTrigger() const
  {
    return true;
  }

  bool CFileWriterStoreFilter::isDataExpected(const CDate& date) const
  {
    return true;
  }
} // namespace xios
