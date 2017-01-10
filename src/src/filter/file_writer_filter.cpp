#include "file_writer_filter.hpp"
#include "exception.hpp"
#include "field.hpp"

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
    field->sendUpdateData(data[0]->data);
  }
} // namespace xios
