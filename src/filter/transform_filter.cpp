#include "transform_filter.hpp"

namespace xios
{
  
  CTransformFilter::CTransformFilter( CGarbageCollector& gc, CGenericAlgorithmTransformation* algo, int dimBefore, int dimAfter, 
                                      bool detectMissingValues, double defaultValue) 
                                    : CFilter(gc, 1, this), algorithm_(algo), dimBefore_(dimBefore), dimAfter_(dimAfter), 
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

    if (packet->status == CDataPacket::NO_ERROR) algorithm_->apply(dimBefore_, dimAfter_, data[0]->data, packet->data);
    return packet;
  }



}