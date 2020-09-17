#include "file_writer_store_filter.hpp"
#include "exception.hpp"
#include "field.hpp"

namespace xios
{
  CFileWriterStoreFilter::CFileWriterStoreFilter(CGarbageCollector& gc, CField* field)
    : CInputPin(gc, 1)
    , field(field)
  {
    if (!field)
      ERROR("CFileWriterStoreFilter::CFileWriterStoreFilter(CField* field)",
            "The field cannot be null.");
  }

  void CFileWriterStoreFilter::onInputReady(std::vector<CDataPacketPtr> data)
  {
    field->writeUpdateData(data[0]->data);
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
