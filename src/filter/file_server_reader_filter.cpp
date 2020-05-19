#include "file_server_reader_filter.hpp"
#include "grid.hpp"
#include "field.hpp"
#include "exception.hpp"
#include "calendar_util.hpp"
#include <limits> 

namespace xios
{
  CFileServerReaderFilter::CFileServerReaderFilter(CGarbageCollector& gc, CField* field) : COutputPin(gc), field_(field)
  {
 
  }

  void CFileServerReaderFilter::streamData(CDate date, const CArray<double, 1>& data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = date;
    packet->timestamp = date;
    packet->status = CDataPacket::NO_ERROR;
    packet->data.reference(data.copy()) ;
    onOutputReady(packet);
  }

  void CFileServerReaderFilter::signalEndOfStream(CDate date)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = date;
    packet->timestamp = date;
    packet->status = CDataPacket::END_OF_STREAM;
    onOutputReady(packet);
  }
} // namespace xios
