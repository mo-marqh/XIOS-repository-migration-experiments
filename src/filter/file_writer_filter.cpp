#include "file_writer_filter.hpp"
#include "exception.hpp"
#include "field.hpp"
#include "utils.hpp"

namespace xios
{
  CFileWriterFilter::CFileWriterFilter(CGarbageCollector& gc, CField* field)
    : CInputPin(gc, 1)
    , field(field)
  {
    if (!field)
      ERROR("CFileWriterFilter::CFileWriterFilter(CField* field)",
            "The field cannot be null.");
  }

  void CFileWriterFilter::onInputReady(std::vector<CDataPacketPtr> data)
  {
    bool ignoreMissingValue = (!field->detect_missing_value.isEmpty() && 
                               !field->default_value.isEmpty() && 
                               field->detect_missing_value == true);
    if (ignoreMissingValue)
    {
      double missingValue = field->default_value;
      size_t nbData = data[0]->data.numElements();
      for (size_t idx = 0; idx < nbData; ++idx)
      {
        if (NumTraits<double>::isnan(data[0]->data(idx)))
          data[0]->data(idx) = missingValue;
      }
    }    

    field->sendUpdateData(data[0]->data);
  }
} // namespace xios
