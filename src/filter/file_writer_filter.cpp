#include "file_writer_filter.hpp"
#include "exception.hpp"
#include "field.hpp"
#include "utils.hpp"
#include "context_client.hpp"

namespace xios
{
  CFileWriterFilter::CFileWriterFilter(CGarbageCollector& gc, CField* field, CContextClient* client)
    : CInputPin(gc, 1)
    , field(field), client_(client)
  {
    if (!field)
      ERROR("CFileWriterFilter::CFileWriterFilter(CField* field)",
            "The field cannot be null.");
  }

  void CFileWriterFilter::onInputReady(std::vector<CDataPacketPtr> data)
  {
    const bool detectMissingValue = ( !field->default_value.isEmpty() &&
                               ( (!field->detect_missing_value.isEmpty() || field->detect_missing_value == true)
                                 || field->hasGridMask()) );

    CArray<double, 1> dataArray = (detectMissingValue) ? data[0]->data.copy() : data[0]->data;

    if (detectMissingValue)
    {
      const double missingValue = field->default_value;
      const size_t nbData = dataArray.numElements();
      for (size_t idx = 0; idx < nbData; ++idx)
      {
        if (NumTraits<double>::isNan(dataArray(idx)))
          dataArray(idx) = missingValue;
      }
    }

    field->sendUpdateData(dataArray, client_);
  }

  bool CFileWriterFilter::mustAutoTrigger() const
  {
    return true;
  }

  bool CFileWriterFilter::isDataExpected(const CDate& date) const
  {
    return true;
  }
} // namespace xios
