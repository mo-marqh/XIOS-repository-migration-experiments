#include "spatial_transform_filter.hpp"
#include "grid_transformation.hpp"
#include "context.hpp"
#include "context_client.hpp"

namespace xios
{
  CSpatialTransformFilter::CSpatialTransformFilter(CGarbageCollector& gc, CSpatialTransformFilterEngine* engine)
    : CFilter(gc, 1, engine)
  { /* Nothing to do */ }

  std::pair<boost::shared_ptr<CSpatialTransformFilter>, boost::shared_ptr<CSpatialTransformFilter> >
  CSpatialTransformFilter::buildFilterGraph(CGarbageCollector& gc, CGrid* srcGrid, CGrid* destGrid)
  {
    if (!srcGrid || !destGrid)
      ERROR("std::pair<boost::shared_ptr<CSpatialTransformFilter>, boost::shared_ptr<CSpatialTransformFilter> >"
            "buildFilterGraph(CGarbageCollector& gc, CGrid* srcGrid, CGrid* destGrid)",
            "Impossible to build the filter graph if either the source or the destination grid are null.");

    // TODO: Implement the real path finding algorithm after a solution has been found
    //       to support initializing grid transformations during parsing.
    CSpatialTransformFilterEngine* engine = CSpatialTransformFilterEngine::get(destGrid->getTransformations());
    boost::shared_ptr<CSpatialTransformFilter> filter(new CSpatialTransformFilter(gc, engine));

    return std::make_pair(filter, filter);
  }

  CSpatialTransformFilterEngine::CSpatialTransformFilterEngine(CGridTransformation* gridTransformation)
    : gridTransformation(gridTransformation)
  {
    if (!gridTransformation)
      ERROR("CSpatialTransformFilterEngine::CSpatialTransformFilterEngine(CGridTransformation* gridTransformation)",
            "Impossible to construct a spatial transform filter engine without a valid grid transformation.");
  }

  std::map<CGridTransformation*, boost::shared_ptr<CSpatialTransformFilterEngine> > CSpatialTransformFilterEngine::engines;

  CSpatialTransformFilterEngine* CSpatialTransformFilterEngine::get(CGridTransformation* gridTransformation)
  {
    if (!gridTransformation)
      ERROR("CSpatialTransformFilterEngine& CSpatialTransformFilterEngine::get(CGridTransformation* gridTransformation)",
            "Impossible to get the requested engine, the grid transformation is invalid.");

    std::map<CGridTransformation*, boost::shared_ptr<CSpatialTransformFilterEngine> >::iterator it = engines.find(gridTransformation);
    if (it == engines.end())
    {
      boost::shared_ptr<CSpatialTransformFilterEngine> engine(new CSpatialTransformFilterEngine(gridTransformation));
      it = engines.insert(std::make_pair(gridTransformation, engine)).first;
    }

    return it->second.get();
  }

  CDataPacketPtr CSpatialTransformFilterEngine::apply(std::vector<CDataPacketPtr> data)
  {
    CDataPacketPtr packet(new CDataPacket);
    packet->date = data[0]->date;
    packet->timestamp = data[0]->timestamp;
    packet->status = data[0]->status;

    if (packet->status == CDataPacket::NO_ERROR)
    {
      packet->data.resize(gridTransformation->getGridDestination()->storeIndex_client.numElements());
      apply(data[0]->data, packet->data);
    }

    return packet;
  }

  void CSpatialTransformFilterEngine::apply(const CArray<double, 1>& dataSrc, CArray<double,1>& dataDest)
  {
    CContextClient* client = CContext::getCurrent()->client;

    const std::map<int, CArray<int,1> >& localIndexToSend = gridTransformation->getLocalIndexToSendFromGridSource();
    const std::map<int, std::vector<std::vector<std::pair<int,double> > > >& localIndexToReceive = gridTransformation->getLocalIndexToReceiveOnGridDest();

    dataDest = 0.0;

    // Sending data from field sources to do transformations
    std::map<int, CArray<int,1> >::const_iterator itbSend = localIndexToSend.begin(), itSend,
                                                  iteSend = localIndexToSend.end();
    int sendBuffSize = 0;
    for (itSend = itbSend; itSend != iteSend; ++itSend) sendBuffSize = (sendBuffSize < itSend->second.numElements())
                                                                     ? itSend->second.numElements(): sendBuffSize;
    double* sendBuff;
    if (0 != sendBuffSize) sendBuff = new double[sendBuffSize];
    std::vector<MPI_Request> sendRequest;
    for (itSend = itbSend; itSend != iteSend; ++itSend)
    {
      int destRank = itSend->first;
      const CArray<int,1>& localIndex_p = itSend->second;
      int countSize = localIndex_p.numElements();
      for (int idx = 0; idx < countSize; ++idx)
      {
        sendBuff[idx] = dataSrc(localIndex_p(idx));
      }
      sendRequest.push_back(MPI_Request());
      MPI_Isend(sendBuff, countSize, MPI_DOUBLE, destRank, 12, client->intraComm, &sendRequest.back());
    }

    // Receiving data on destination fields
    std::map<int,std::vector<std::vector<std::pair<int,double> > > >::const_iterator itbRecv = localIndexToReceive.begin(), itRecv,
                                                                                     iteRecv = localIndexToReceive.end();
    int recvBuffSize = 0;
    for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv) recvBuffSize = (recvBuffSize < itRecv->second.size())
                                                                     ? itRecv->second.size() : recvBuffSize;
    double* recvBuff;
    if (0 != recvBuffSize) recvBuff = new double[recvBuffSize];
    for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv)
    {
      MPI_Status status;
      int srcRank = itRecv->first;
      int countSize = itRecv->second.size();
      MPI_Recv(recvBuff, recvBuffSize, MPI_DOUBLE, srcRank, 12, client->intraComm, &status);
      for (int idx = 0; idx < countSize; ++idx)
      {
        const std::vector<std::pair<int,double> >& localIndex_p = itRecv->second[idx];
        int numIndex = localIndex_p.size();
        for (int i = 0; i < numIndex; ++i)
        {
//        if (localIndex_p[i].first >= dataDest.numElements())
          dataDest(localIndex_p[i].first) += recvBuff[idx] * localIndex_p[i].second;
        }
      }
    }

    std::vector<MPI_Status> requestStatus(sendRequest.size());
    MPI_Wait(&sendRequest[0], &requestStatus[0]);
    if (0 != sendBuffSize) delete [] sendBuff;
    if (0 != recvBuffSize) delete [] recvBuff;
  }
} // namespace xios
