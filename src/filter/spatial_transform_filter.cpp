#include "spatial_transform_filter.hpp"
#include "grid_transformation.hpp"
#include "context.hpp"
#include "context_client.hpp"

namespace xios
{
  CSpatialTransformFilter::CSpatialTransformFilter(CGarbageCollector& gc, CSpatialTransformFilterEngine* engine, size_t inputSlotsCount)
    : CFilter(gc, inputSlotsCount, engine)
  { /* Nothing to do */ }

  std::pair<boost::shared_ptr<CSpatialTransformFilter>, boost::shared_ptr<CSpatialTransformFilter> >
  CSpatialTransformFilter::buildFilterGraph(CGarbageCollector& gc, CGrid* srcGrid, CGrid* destGrid)
  {
    if (!srcGrid || !destGrid)
      ERROR("std::pair<boost::shared_ptr<CSpatialTransformFilter>, boost::shared_ptr<CSpatialTransformFilter> >"
            "buildFilterGraph(CGarbageCollector& gc, CGrid* srcGrid, CGrid* destGrid)",
            "Impossible to build the filter graph if either the source or the destination grid are null.");

    boost::shared_ptr<CSpatialTransformFilter> firstFilter, lastFilter;
    // Note that this loop goes from the last transformation to the first transformation
    do
    {
      CGridTransformation* gridTransformation = destGrid->getTransformations();
      CSpatialTransformFilterEngine* engine = CSpatialTransformFilterEngine::get(destGrid->getTransformations());
      const std::vector<StdString>& auxInputs = gridTransformation->getAuxInputs();
      size_t inputCount = 1 + (auxInputs.empty() ? 0 : auxInputs.size());
      boost::shared_ptr<CSpatialTransformFilter> filter(new CSpatialTransformFilter(gc, engine, inputCount));

      if (!lastFilter)
        lastFilter = filter;
      else
        filter->connectOutput(firstFilter, 0);

      firstFilter = filter;
      for (size_t idx = 0; idx < auxInputs.size(); ++idx)
      {
        CField* fieldAuxInput = CField::get(auxInputs[idx]);
        fieldAuxInput->buildFilterGraph(gc, false);
        fieldAuxInput->getInstantDataFilter()->connectOutput(firstFilter,idx+1);
      }

      destGrid = gridTransformation->getGridSource();
    }
    while (destGrid != srcGrid);

    return std::make_pair(firstFilter, lastFilter);
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
      if (1 < data.size())  // Dynamical transformations
      {
        std::vector<CArray<double,1>* > dataAuxInputs(data.size()-1);
        for (size_t idx = 0; idx < dataAuxInputs.size(); ++idx) dataAuxInputs[idx] = &(data[idx+1]->data);
        gridTransformation->computeAll(dataAuxInputs);
      }
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
    int idxSendBuff = 0;
    std::vector<double*> sendBuff(localIndexToSend.size());
    for (itSend = itbSend; itSend != iteSend; ++itSend, ++idxSendBuff)
    {
      if (0 != itSend->second.numElements())
        sendBuff[idxSendBuff] = new double[itSend->second.numElements()];
    }

    idxSendBuff = 0;
    std::vector<MPI_Request> sendRequest;
    for (itSend = itbSend; itSend != iteSend; ++itSend, ++idxSendBuff)
    {
      int destRank = itSend->first;
      const CArray<int,1>& localIndex_p = itSend->second;
      int countSize = localIndex_p.numElements();
      for (int idx = 0; idx < countSize; ++idx)
      {
        sendBuff[idxSendBuff][idx] = dataSrc(localIndex_p(idx));
      }
      sendRequest.push_back(MPI_Request());
      MPI_Isend(sendBuff[idxSendBuff], countSize, MPI_DOUBLE, destRank, 12, client->intraComm, &sendRequest.back());
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
      int countBuff = 0;
      MPI_Get_count(&status, MPI_DOUBLE, &countBuff);
      if (countBuff != countSize)
        ERROR("CSpatialTransformFilterEngine::apply(const CArray<double, 1>& dataSrc, CArray<double,1>& dataDest)",
              "Incoherent between the received size and expected size");
      for (int idx = 0; idx < countSize; ++idx)
      {
        const std::vector<std::pair<int,double> >& localIndex_p = itRecv->second[idx];
        int numIndex = localIndex_p.size();
        for (int i = 0; i < numIndex; ++i)
        {
          dataDest(localIndex_p[i].first) += recvBuff[idx] * localIndex_p[i].second;
        }
      }
    }


    if (!sendRequest.empty()) MPI_Waitall(sendRequest.size(), &sendRequest[0], MPI_STATUSES_IGNORE);
    idxSendBuff = 0;
    for (itSend = itbSend; itSend != iteSend; ++itSend, ++idxSendBuff)
    {
      if (0 != itSend->second.numElements())
        delete [] sendBuff[idxSendBuff];
    }
    if (0 != recvBuffSize) delete [] recvBuff;
  }
} // namespace xios
