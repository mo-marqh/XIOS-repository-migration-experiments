#include "client_to_model_store_filter.hpp"
#include "context.hpp"
#include "grid.hpp"
#include "timer.hpp"
#include "tracer.hpp"
#include "workflow_graph.hpp"

namespace xios
{
  CClientToModelStoreFilter::CClientToModelStoreFilter(CGarbageCollector& gc, CField* field)
    : CInputPin(gc, 1)
    , gc_(gc), graphEnabled(false)
  {
    context_ = CContext::getCurrent() ;
    grid_ = field->getGrid() ;

    detectMissingValues_ = (!field->detect_missing_value.isEmpty() && !field->default_value.isEmpty() && field->detect_missing_value == true);
    hasMissingValue_ = !field->default_value.isEmpty() ;
    if (hasMissingValue_) missingValue_  = field->default_value ;
    detectMissingValues_ = (!field->detect_missing_value.isEmpty() && hasMissingValue_);
  }

  CConstDataPacketPtr CClientToModelStoreFilter::getPacket(Time timestamp)
  {
    CTimer timer("CStoreFilter::getPacket");
//    timer.resume(); 
    info(0)<<"ENTERING CStoreFilter::getPacket"<<std::endl ;
    traceOff() ;
//    timer.suspend();
    CConstDataPacketPtr packet;
    const double timeout = CXios::recvFieldTimeout;

    do
    {
      if (canBeTriggered()) trigger(timestamp);

      timer.resume();

      std::map<Time, CDataPacketPtr>::const_iterator it = packets_.find(timestamp);
      if (it != packets_.end()) packet = it->second;
      else  context_->eventLoop(); // if the packet is not available yet, check if it can be received

      timer.suspend();
    } while (!packet && timer.getCumulatedTime() < timeout);
//    timer.resume();
    traceOn() ;
//    timer.suspend();

    if (!packet)
    {
      std::map<Time, CDataPacketPtr>::const_iterator it ;
      info(0)<<"Impossible to get the packet with timestamp = " << timestamp<<std::endl<<"Available timestamp are : "<<std::endl ;
      for(it=packets_.begin();it!=packets_.end();++it) info(0)<<it->first<<"  ";
      info(0)<<std::endl ;
      ERROR("CConstDataPacketPtr CStoreFilter::getPacket(Time timestamp) const",
            << "Impossible to get the packet with timestamp = " << timestamp);
    }
    return packet;
  }

  template <int N>
  CDataPacket::StatusCode CClientToModelStoreFilter::getData(Time timestamp, CArray<double, N>& data)
  {
    CConstDataPacketPtr packet = getPacket(timestamp);

    if (packet->status == CDataPacket::NO_ERROR)
    {
      if ( data.numElements() != grid_->getWorkflowToModelConnector()->getDstSize() )
      {
	ERROR("CGridLocalConnector::transfer(...)",
	      << "Bad definition of grids size for grid (destination) " << grid_->getId()
	      << ", awaited size = " << data.numElements()
	      << ", while will generate data size = " << grid_->getWorkflowToModelConnector()->getDstSize()
	      );

      }
      if ( packet->data.numElements() != grid_->getWorkflowToModelConnector()->getSrcSize() )
      {
	ERROR("CGridLocalConnector::transfer(...)",
	      << "Bad definition of grids size for grid (source) " << grid_->getId()
	      << ", awaited size = " << grid_->getWorkflowToModelConnector()->getSrcSize()
	      << ", while received data size = " << packet->data.numElements()
	      );
      }
      if (hasMissingValue_) grid_->getWorkflowToModelConnector()->transfer(packet->data, data, missingValue_);
      else grid_->getWorkflowToModelConnector()->transfer(packet->data, data);
    }
    return packet->status;
  }

  template CDataPacket::StatusCode CClientToModelStoreFilter::getData<1>(Time timestamp, CArray<double, 1>& data);
  template CDataPacket::StatusCode CClientToModelStoreFilter::getData<2>(Time timestamp, CArray<double, 2>& data);
  template CDataPacket::StatusCode CClientToModelStoreFilter::getData<3>(Time timestamp, CArray<double, 3>& data);
  template CDataPacket::StatusCode CClientToModelStoreFilter::getData<4>(Time timestamp, CArray<double, 4>& data);
  template CDataPacket::StatusCode CClientToModelStoreFilter::getData<5>(Time timestamp, CArray<double, 5>& data);
  template CDataPacket::StatusCode CClientToModelStoreFilter::getData<6>(Time timestamp, CArray<double, 6>& data);
  template CDataPacket::StatusCode CClientToModelStoreFilter::getData<7>(Time timestamp, CArray<double, 7>& data);

  void CClientToModelStoreFilter::onInputReady(std::vector<CDataPacketPtr> data)
  {

    CDataPacketPtr packet;
    if (detectMissingValues_)
    {
      const size_t nbData = data[0]->data.numElements();

      packet = CDataPacketPtr(new CDataPacket);
      packet->date = data[0]->date;
      packet->timestamp = data[0]->timestamp;
      packet->status = data[0]->status;
      packet->data.resize(nbData);
      packet->data = data[0]->data;

      for (size_t idx = 0; idx < nbData; ++idx)
      {
        if (NumTraits<double>::isNan(packet->data(idx)))
          packet->data(idx) = missingValue_;
      }

    }

    else
    {
      packet = data[0];
    }

    if(this->graphEnabled)
    {
      this->graphPackage->filterId = CWorkflowGraph::getNodeSize();
      CWorkflowGraph::addNode("Client to Model Store filter", 5, true, 1, packet);
    }


    packets_.insert(std::make_pair(packet->timestamp, packet));
    // The packet is always destroyed by the garbage collector
    // so we register but never unregister
    gc_.registerObject(this, packet->timestamp);

  }

  bool CClientToModelStoreFilter::mustAutoTrigger() const
  {
    return false;
  }

  bool CClientToModelStoreFilter::isDataExpected(const CDate& date) const
  {
    return true;
  }

  void CClientToModelStoreFilter::invalidate(Time timestamp)
  {
    CInputPin::invalidate(timestamp);
    packets_.erase(packets_.begin(), packets_.lower_bound(timestamp));
  }
} // namespace xios
