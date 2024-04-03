#include "model_to_client_source_filter.hpp"
#include "grid.hpp"
#include "exception.hpp"
#include "calendar_util.hpp"
#include <limits> 
#include "workflow_graph.hpp"
#include "mem_checker.hpp"
#include "timer.hpp"

namespace xios
{
  extern CLogType logProfile ;
  CModelToClientSourceFilter::CModelToClientSourceFilter(CGarbageCollector& gc, CGrid* grid, bool hasMissingValue, double defaultValue)
    : COutputPin(gc)
    , grid_(grid)
    , hasMissingValue_(hasMissingValue), defaultValue_(defaultValue)
  {
    if (!grid)
      ERROR("CSourceFilter::CSourceFilter(CGrid* grid)",
            "Impossible to construct a source filter without providing a grid.");
  }

  template <int N>
  void CModelToClientSourceFilter::streamData(CDate date, const CArray<double, N>& data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = date;
    packet->timestamp = date;
    packet->status = CDataPacket::NO_ERROR;
    
    auto connector = grid_->getModelToWorkflowConnector() ;

    if (connector->getSrcSize() != data.numElements())
      ERROR("void CModelToClientSourceFilter::streamData(CDate date, const CArray<double, N>& data)",
            << "[ Awaiting data of size = " <<connector->getSrcSize() << ", "
            << "Received data size = "      << data.numElements() << " ] "
            << "The data array does not have the right size! "
            << "grid = " << grid_->getId())
    const double nanValue = std::numeric_limits<double>::quiet_NaN();
    packet->data.resize(connector->getDstSize()) ;
    if (info.isActive(logProfile)) CTimer::get("Model to client").resume();
    connector->transfer(data, packet->data, nanValue) ;
    if (info.isActive(logProfile)) CTimer::get("Model to client").suspend();

    CMemChecker::logMem( "CModelToClientSourceFilter::streamData" );

    if (hasMissingValue_)
    {
        const double nanValue = std::numeric_limits<double>::quiet_NaN();
        const size_t nbData = packet->data.numElements();
        for (size_t idx = 0; idx < nbData; ++idx)
          if (defaultValue_ == packet->data(idx))   packet->data(idx) = nanValue;
    }
    
    buildWorkflowGraph(packet);
    if (info.isActive(logProfile)) CTimer::get("Client workflow").resume();
    onOutputReady(packet);
    if (info.isActive(logProfile)) CTimer::get("Client workflow").suspend();
  }
  
  void CModelToClientSourceFilter::buildWorkflowGraph(CDataPacketPtr packet)
  {
    if(this->graphEnabled)
    {
      this->graphPackage->filterId = CWorkflowGraph::getNodeSize();
      if(!packet->graphPackage)
      {
        packet->graphPackage = new CGraphDataPackage;
      }
      packet->graphPackage->fromFilter = this->graphPackage->filterId;
      packet->graphPackage->currentField = this->graphPackage->inFields[0];
      CWorkflowGraph::addNode("Model to Client Source filter", 1, false, 0, packet);
    }
  }




  template void CModelToClientSourceFilter::streamData<1>(CDate date, const CArray<double, 1>& data);
  template void CModelToClientSourceFilter::streamData<2>(CDate date, const CArray<double, 2>& data);
  template void CModelToClientSourceFilter::streamData<3>(CDate date, const CArray<double, 3>& data);
  template void CModelToClientSourceFilter::streamData<4>(CDate date, const CArray<double, 4>& data);
  template void CModelToClientSourceFilter::streamData<5>(CDate date, const CArray<double, 5>& data);
  template void CModelToClientSourceFilter::streamData<6>(CDate date, const CArray<double, 6>& data);
  template void CModelToClientSourceFilter::streamData<7>(CDate date, const CArray<double, 7>& data);

} // namespace xios
