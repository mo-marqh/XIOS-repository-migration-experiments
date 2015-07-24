#include "source_filter.hpp"
#include "grid.hpp"
#include "exception.hpp"

namespace xios
{
  CSourceFilter::CSourceFilter(CGrid* grid)
    : grid(grid)
  {
    if (!grid)
      ERROR("CSourceFilter::CSourceFilter(CGrid* grid)",
            "Impossible to construct a source filter without providing a grid.");
  }

  template <int N>
  void CSourceFilter::streamData(CDate date, const CArray<double, N>& data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = date;
    packet->timestamp = date;
    packet->status = CDataPacket::NO_ERROR;

    packet->data.resize(grid->storeIndex_client.numElements());
    grid->inputField(data, packet->data);

    deliverOuput(packet);
  }

  template void CSourceFilter::streamData<1>(CDate date, const CArray<double, 1>& data);
  template void CSourceFilter::streamData<2>(CDate date, const CArray<double, 2>& data);
  template void CSourceFilter::streamData<3>(CDate date, const CArray<double, 3>& data);

  void CSourceFilter::streamDataFromServer(CDate date, const std::map<int, CArray<double, 1> >& data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = date;
    packet->timestamp = date;
    packet->status = CDataPacket::NO_ERROR;

    if (data.size() != grid->storeIndex_toSrv.size())
      ERROR("CSourceFilter::streamDataFromServer(CDate date, const std::map<int, CArray<double, 1> >& data)",
            << "Incoherent data received from servers,"
            << " expected " << grid->storeIndex_toSrv.size() << " chunks but " << data.size() << " were given.");

    packet->data.resize(grid->storeIndex_client.numElements());
    std::map<int, CArray<double, 1> >::const_iterator it, itEnd = data.end();
    for (it = data.begin(); it != itEnd; it++)
    {
      CArray<int,1>& index = grid->storeIndex_toSrv[it->first];

      for (int n = 0; n < index.numElements(); n++)
        packet->data(index(n)) = it->second(n);
    }

    deliverOuput(packet);
  }

  void CSourceFilter::signalEndOfStream(CDate date)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = date;
    packet->timestamp = date;
    packet->status = CDataPacket::END_OF_STREAM;
    deliverOuput(packet);
  }
} // namespace xios
