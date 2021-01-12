#include "transform_filter.hpp"
#include "grid_algorithm.hpp"

namespace xios
{
  
  CTransformFilter::CTransformFilter( CGarbageCollector& gc, int slots, CGridAlgorithm* algo, bool detectMissingValues, double defaultValue) 
                                    : CFilter(gc, slots, this), algorithm_(algo), 
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

    if (packet->status == CDataPacket::NO_ERROR) 
    {
      if (data.size()>1)
      {
        vector<CArray<double,1>> auxData(data.size()-1); 
        for(int i=0;i<data.size()-1 ;i++) auxData[i].reference(data[i+1]->data) ;
        algorithm_->apply(data[0]->data, auxData, packet->data);
      }
      else algorithm_->apply(data[0]->data, packet->data);
    }
    return packet;
  }



}