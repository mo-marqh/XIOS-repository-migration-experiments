#include "transform_filter.hpp"
#include "grid_algorithm.hpp"

namespace xios
{
  
  CTransformFilter::CTransformFilter( CGarbageCollector& gc, CGridAlgorithm* algo, bool detectMissingValues, double defaultValue) 
                                    : CFilter(gc, 1, this), algorithm_(algo), 
                                      detectMissingValues_(detectMissingValues), defaultValue_(defaultValue)
  {

  }

  CDataPacketPtr CTransformFilter::apply(std::vector<CDataPacketPtr> data)
  {
    // for now, no auxilliairy field
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    if (packet->status == CDataPacket::NO_ERROR) algorithm_->apply(data[0]->data, packet->data);
    return packet;
  }



}