#include "temporal_transform_filter.hpp"

namespace xios
{

  CTemporalTransformFilter::CTemporalTransformFilter(CGarbageCollector& gc, int slots, CGridAlgorithm* algo, int nrecords, bool detectMissingValues, double defaultValue)
  : CTransformFilter(gc, slots, algo, detectMissingValues, defaultValue), nrecords_(nrecords)
  {

  }

  CDataPacketPtr CTemporalTransformFilter::apply(std::vector<CDataPacketPtr> data)
  {
    if (data[0]->status == CDataPacket::NO_ERROR)
    {
      if (record_==0) tmpData_.resize(nrecords_) ;
      algorithm_->apply(data[0]->data, tmpData_[record_]);
      record_++ ;
      if (record_==nrecords_)
      {
        size_t size=0 ;
        for(auto& it : tmpData_) size += it.numElements() ;
        // for now, no auxilliairy field
        CDataPacketPtr packet(new CDataPacket);
        packet->date = data[0]->date;
        packet->timestamp = data[0]->timestamp;
        packet->status = data[0]->status;
        packet->data.resize(size) ;
        double* out = packet->data.dataFirst() ;
        for(auto& it : tmpData_) 
        {
          size = it.numElements() ; 
          double* tmp = it.dataFirst() ;
          for(size_t i=0 ; i<size ; i++, out++, tmp++) *out=*tmp ;   
        }
        tmpData_.clear() ;
        record_=0 ;
        return packet ;
      }
      else return nullptr ;
    }
    else // error
    {
      CDataPacketPtr packet(new CDataPacket);
      packet->date = data[0]->date;
      packet->timestamp = data[0]->timestamp;
      packet->status = data[0]->status;
      return packet ;
    }    
  }
}