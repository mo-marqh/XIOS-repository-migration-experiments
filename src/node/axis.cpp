#include "axis.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "message.hpp"
#include "type.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "context_server.hpp"
#include "xios_spl.hpp"
#include "inverse_axis.hpp"
#include "zoom_axis.hpp"
#include "interpolate_axis.hpp"
#include "server_distribution_description.hpp"
#include "client_server_mapping_distributed.hpp"
#include "distribution_client.hpp"

namespace xios {

   /// ////////////////////// Définitions ////////////////////// ///

   CAxis::CAxis(void)
      : CObjectTemplate<CAxis>()
      , CAxisAttributes(), isChecked(false), relFiles(), areClientAttributesChecked_(false)
      , isDistributed_(false), hasBounds_(false), isCompressible_(false)
      , numberWrittenIndexes_(0), totalNumberWrittenIndexes_(0), offsetWrittenIndexes_(0)
      , transformationMap_(), global_zoom_begin(0), global_zoom_size(0)
   {
   }

   CAxis::CAxis(const StdString & id)
      : CObjectTemplate<CAxis>(id)
      , CAxisAttributes(), isChecked(false), relFiles(), areClientAttributesChecked_(false)
      , isDistributed_(false), hasBounds_(false), isCompressible_(false)
      , numberWrittenIndexes_(0), totalNumberWrittenIndexes_(0), offsetWrittenIndexes_(0)
      , transformationMap_(), global_zoom_begin(0), global_zoom_size(0)
   {
   }

   CAxis::~CAxis(void)
   { /* Ne rien faire de plus */ }

   ///---------------------------------------------------------------

   const std::set<StdString> & CAxis::getRelFiles(void) const
   {
      return (this->relFiles);
   }

   bool CAxis::IsWritten(const StdString & filename) const
   {
      return (this->relFiles.find(filename) != this->relFiles.end());
   }

   bool CAxis::isWrittenCompressed(const StdString& filename) const
   {
      return (this->relFilesCompressed.find(filename) != this->relFilesCompressed.end());
   }

   bool CAxis::isDistributed(void) const
   {
      return isDistributed_;
   }

   /*!
    * Test whether the data defined on the axis can be outputted in a compressed way.
    *
    * \return true if and only if a mask was defined for this axis
    */
   bool CAxis::isCompressible(void) const
   {
      return isCompressible_;
   }

   void CAxis::addRelFile(const StdString & filename)
   {
      this->relFiles.insert(filename);
   }

   void CAxis::addRelFileCompressed(const StdString& filename)
   {
      this->relFilesCompressed.insert(filename);
   }

   //----------------------------------------------------------------

   const std::vector<int>& CAxis::getIndexesToWrite(void) const
   {
     return indexesToWrite;
   }

   /*!
     Returns the number of indexes written by each server.
     \return the number of indexes written by each server
   */
   int CAxis::getNumberWrittenIndexes() const
   {
     return numberWrittenIndexes_;
   }

   /*!
     Returns the total number of indexes written by the servers.
     \return the total number of indexes written by the servers
   */
   int CAxis::getTotalNumberWrittenIndexes() const
   {
     return totalNumberWrittenIndexes_;
   }

   /*!
     Returns the offset of indexes written by each server.
     \return the offset of indexes written by each server
   */
   int CAxis::getOffsetWrittenIndexes() const
   {
     return offsetWrittenIndexes_;
   }

   //----------------------------------------------------------------

   /*!
    * Compute the minimum buffer size required to send the attributes to the server(s).
    *
    * \return A map associating the server rank with its minimum buffer size.
    */
   std::map<int, StdSize> CAxis::getAttributesBufferSize()
   {
     CContextClient* client = CContext::getCurrent()->client;

     std::map<int, StdSize> attributesSizes = getMinimumBufferSizeForAttributes();

     bool isNonDistributed = (n == n_glo);

     if (client->isServerLeader())
     {
       // size estimation for sendServerAttribut
       size_t size = 6 * sizeof(size_t);
       // size estimation for sendNonDistributedValue
       if (isNonDistributed)
         size = std::max(size, CArray<double,1>::size(n_glo) + (isCompressible_ ? CArray<int,1>::size(n_glo) : 0));
       size += CEventClient::headerSize + getId().size() + sizeof(size_t);

       const std::list<int>& ranks = client->getRanksServerLeader();
       for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
       {
         if (size > attributesSizes[*itRank])
           attributesSizes[*itRank] = size;
       }
     }

     if (!isNonDistributed)
     {
       // size estimation for sendDistributedValue
       std::map<int, std::vector<size_t> >::const_iterator it, ite = indSrv_.end();
       for (it = indSrv_.begin(); it != ite; ++it)
       {
         size_t sizeIndexEvent = CArray<int,1>::size(it->second.size());
         if (isCompressible_)
           sizeIndexEvent += CArray<int,1>::size(indWrittenSrv_[it->first].size());

         size_t sizeValEvent = CArray<double,1>::size(it->second.size());
         if (hasBounds_)
           sizeValEvent += CArray<double,2>::size(2 * it->second.size());

         size_t size = CEventClient::headerSize + getId().size() + sizeof(size_t) + std::max(sizeIndexEvent, sizeValEvent);
         if (size > attributesSizes[it->first])
           attributesSizes[it->first] = size;
       }
     }

     return attributesSizes;
   }

   //----------------------------------------------------------------

   StdString CAxis::GetName(void)   { return (StdString("axis")); }
   StdString CAxis::GetDefName(void){ return (CAxis::GetName()); }
   ENodeType CAxis::GetType(void)   { return (eAxis); }

   //----------------------------------------------------------------

   CAxis* CAxis::createAxis()
   {
     CAxis* axis = CAxisGroup::get("axis_definition")->createChild();
     return axis;
   }

   void CAxis::checkAttributes(void)
   {
      if (this->n_glo.isEmpty())
        ERROR("CAxis::checkAttributes(void)",
              << "[ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] "
              << "The axis is wrongly defined, attribute 'n_glo' must be specified");
      StdSize size = this->n_glo.getValue();

      isDistributed_ = !this->begin.isEmpty() || !this->n.isEmpty();

      if (!this->begin.isEmpty())
      {
        if (begin < 0 || begin > size - 1)
          ERROR("CAxis::checkAttributes(void)",
                << "[ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] "
                << "The axis is wrongly defined, attribute 'begin' (" << begin.getValue() << ") must be non-negative and smaller than size-1 (" << size - 1 << ").");
      }
      else this->begin.setValue(0);

      if (!this->n.isEmpty())
      {
        if (n < 0 || n > size)
          ERROR("CAxis::checkAttributes(void)",
                << "[ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] "
                << "The axis is wrongly defined, attribute 'n' (" << n.getValue() << ") must be non-negative and smaller than size (" << size << ").");
      }
      else this->n.setValue(size);

      StdSize true_size = value.numElements();
      if (this->n.getValue() != true_size)
        ERROR("CAxis::checkAttributes(void)",
              << "[ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] "
              << "The axis is wrongly defined, attribute 'value' has a different size (" << true_size << ") than the one defined by the \'size\' attribute (" << n.getValue() << ").");

      this->checkData();
      this->checkZoom();
      this->checkMask();
      this->checkBounds();
   }

   void CAxis::checkData()
   {
      if (data_begin.isEmpty()) data_begin.setValue(0);

      if (data_n.isEmpty())
      {
        data_n.setValue(n);
      }
      else if (data_n.getValue() < 0)
      {
        ERROR("CAxis::checkData(void)",
              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
              << "The data size should be strictly positive ('data_n' = " << data_n.getValue() << ").");
      }

      if (data_index.isEmpty())
      {
        data_index.resize(data_n);
        for (int i = 0; i < data_n; ++i) data_index(i) = i;
      }
   }

   void CAxis::checkZoom(void)
   {
     if (0 == global_zoom_size) global_zoom_size = this->n_glo.getValue();
   }

   void CAxis::checkMask()
   {
      if (!mask.isEmpty())
      {
         if (mask.extent(0) != n)
           ERROR("CAxis::checkMask(void)",
                 << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                 << "The mask does not have the same size as the local domain." << std::endl
                 << "Local size is " << n.getValue() << "." << std::endl
                 << "Mask size is " << mask.extent(0) << ".");
      }
      else // (mask.isEmpty())
      { // If no mask was defined, we create a default one without any masked point.
         mask.resize(n);
         for (int i = 0; i < n; ++i)
         {
           mask(i) = true;
         }
      }
   }

  void CAxis::checkBounds()
  {
    if (!bounds.isEmpty())
    {
      if (bounds.extent(0) != 2 || bounds.extent(1) != n)
        ERROR("CAxis::checkAttributes(void)",
              << "The bounds array of the axis [ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] must be of dimension 2 x axis size." << std::endl
              << "Axis size is " << n.getValue() << "." << std::endl
              << "Bounds size is "<< bounds.extent(0) << " x " << bounds.extent(1) << ".");
      hasBounds_ = true;
    }
    else hasBounds_ = false;
  }

  void CAxis::checkEligibilityForCompressedOutput()
  {
    // We don't check if the mask is valid here, just if a mask has been defined at this point.
    isCompressible_ = !mask.isEmpty();
  }

  bool CAxis::dispatchEvent(CEventServer& event)
   {
      if (SuperClass::dispatchEvent(event)) return true;
      else
      {
        switch(event.type)
        {
           case EVENT_ID_SERVER_ATTRIBUT :
             recvServerAttribut(event);
             return true;
             break;
           case EVENT_ID_INDEX:
            recvIndex(event);
            return true;
            break;
          case EVENT_ID_DISTRIBUTED_VALUE:
            recvDistributedValue(event);
            return true;
            break;
          case EVENT_ID_NON_DISTRIBUTED_VALUE:
            recvNonDistributedValue(event);
            return true;
            break;
           default :
             ERROR("bool CAxis::dispatchEvent(CEventServer& event)",
                    << "Unknown Event");
           return false;
         }
      }
   }

   void CAxis::checkAttributesOnClient()
   {
     if (this->areClientAttributesChecked_) return;

     this->checkAttributes();

     this->areClientAttributesChecked_ = true;
   }

   // Send all checked attributes to server
   void CAxis::sendCheckedAttributes(const std::vector<int>& globalDim, int orderPositionInGrid,
                                     CServerDistributionDescription::ServerDistributionType distType)
   {
     if (!this->areClientAttributesChecked_) checkAttributesOnClient();
     CContext* context = CContext::getCurrent();

     if (this->isChecked) return;
     if (context->hasClient)
     {
       sendServerAttribut(globalDim, orderPositionInGrid, distType);
       sendValue();
     }

     this->isChecked = true;
   }

  void CAxis::sendValue()
  {
     if (n.getValue() == n_glo.getValue())
     {
       sendNonDistributedValue();
     }
     else
     {
       computeConnectedServer();
       sendDistributedValue();
     }
  }

  void CAxis::computeConnectedServer()
  {
    CContext* context = CContext::getCurrent();
    CContextClient* client = context->client;
    int nbServer = client->serverSize;
    int range, clientSize = client->clientSize;

    size_t ni = this->n.getValue();
    size_t ibegin = this->begin.getValue();
    size_t zoom_end = global_zoom_begin+global_zoom_size-1;
    size_t nZoomCount = 0;
    for (size_t idx = 0; idx < ni; ++idx)
    {
      size_t globalIndex = ibegin + idx;

      if (globalIndex >= global_zoom_begin && globalIndex <= zoom_end) ++nZoomCount;
    }

    CArray<size_t,1> globalIndexAxis(ni);
    std::vector<size_t> globalAxisZoom(nZoomCount);
    nZoomCount = 0;
    for (size_t idx = 0; idx < ni; ++idx)
    {
      size_t globalIndex = ibegin + idx;
      globalIndexAxis(idx) = globalIndex;
      if (globalIndex >= global_zoom_begin && globalIndex <= zoom_end)
      {
        globalAxisZoom[nZoomCount] = globalIndex;
        ++nZoomCount;
      }
    }

    std::set<int> writtenInd;
    if (isCompressible_)
    {
      for (int idx = 0; idx < data_index.numElements(); ++idx)
      {
        int ind = CDistributionClient::getAxisIndex(data_index(idx), data_begin, ni);

        if (ind >= 0 && ind < ni && mask(ind))
        {
          ind += ibegin;
          if (ind >= global_zoom_begin && ind <= zoom_end)
            writtenInd.insert(ind);
        }
      }
    }

    std::vector<int> nGlobDomain(1);
    nGlobDomain[0] = n_glo.getValue();

    size_t globalSizeIndex = 1, indexBegin, indexEnd;
    for (int i = 0; i < nGlobDomain.size(); ++i) globalSizeIndex *= nGlobDomain[i];
    indexBegin = 0;
    for (int i = 0; i < clientSize; ++i)
    {
      range = globalSizeIndex / clientSize;
      if (i < (globalSizeIndex%clientSize)) ++range;
      if (i == client->clientRank) break;
      indexBegin += range;
    }
    indexEnd = indexBegin + range - 1;

    CServerDistributionDescription serverDescription(nGlobDomain);
    serverDescription.computeServerGlobalIndexInRange(nbServer, std::make_pair<size_t,size_t>(indexBegin, indexEnd), 0);
    CClientServerMapping* clientServerMap = new CClientServerMappingDistributed(serverDescription.getGlobalIndexRange(), client->intraComm);
    clientServerMap->computeServerIndexMapping(globalIndexAxis);
    const std::map<int, std::vector<size_t> >& globalIndexAxisOnServer = clientServerMap->getGlobalIndexOnServer();

    std::map<int, std::vector<size_t> >::const_iterator it = globalIndexAxisOnServer.begin(),
                                                       ite = globalIndexAxisOnServer.end();
    std::vector<size_t>::const_iterator itbVec = (globalAxisZoom).begin(),
                                        iteVec = (globalAxisZoom).end();
    indSrv_.clear();
    indWrittenSrv_.clear();
    for (; it != ite; ++it)
    {
      int rank = it->first;
      const std::vector<size_t>& globalIndexTmp = it->second;
      int nb = globalIndexTmp.size();

      for (int i = 0; i < nb; ++i)
      {
        if (std::binary_search(itbVec, iteVec, globalIndexTmp[i]))
        {
          indSrv_[rank].push_back(globalIndexTmp[i]);
        }

        if (writtenInd.count(globalIndexTmp[i]))
        {
          indWrittenSrv_[rank].push_back(globalIndexTmp[i]);
        }
      }
    }

    connectedServerRank_.clear();
    for (it = globalIndexAxisOnServer.begin(); it != ite; ++it) {
      connectedServerRank_.push_back(it->first);
    }

    if (!indSrv_.empty())
    {
      connectedServerRank_.clear();
      for (it = indSrv_.begin(); it != indSrv_.end(); ++it)
        connectedServerRank_.push_back(it->first);
    }
    nbConnectedClients_ = clientServerMap->computeConnectedClients(client->serverSize, client->clientSize, client->intraComm, connectedServerRank_);
    delete clientServerMap;
  }

  void CAxis::sendNonDistributedValue()
  {
    CContext* context = CContext::getCurrent();
    CContextClient* client = context->client;
    CEventClient event(getType(), EVENT_ID_NON_DISTRIBUTED_VALUE);

    int zoom_end = global_zoom_begin + global_zoom_size - 1;
    int nb = 0;
    for (size_t idx = 0; idx < n; ++idx)
    {
      size_t globalIndex = begin + idx;
      if (globalIndex >= global_zoom_begin && globalIndex <= zoom_end) ++nb;
    }

    int nbWritten = 0;
    if (isCompressible_)
    {
      for (int idx = 0; idx < data_index.numElements(); ++idx)
      {
        int ind = CDistributionClient::getAxisIndex(data_index(idx), data_begin, n);

        if (ind >= 0 && ind < n && mask(ind))
        {
          ind += begin;
          if (ind >= global_zoom_begin && ind <= zoom_end)
            ++nbWritten;
        }
      }
    }

    CArray<double,1> val(nb);
    nb = 0;
    for (size_t idx = 0; idx < n; ++idx)
    {
      size_t globalIndex = begin + idx;
      if (globalIndex >= global_zoom_begin && globalIndex <= zoom_end)
      {
        val(nb) = value(idx);
        ++nb;
      }
    }

    CArray<int, 1> writtenInd(nbWritten);
    nbWritten = 0;
    if (isCompressible_)
    {
      for (int idx = 0; idx < data_index.numElements(); ++idx)
      {
        int ind = CDistributionClient::getAxisIndex(data_index(idx), data_begin, n);

        if (ind >= 0 && ind < n && mask(ind))
        {
          ind += begin;
          if (ind >= global_zoom_begin && ind <= zoom_end)
          {
            writtenInd(nbWritten) = ind;
            ++nbWritten;
          }
        }
      }
    }

    if (client->isServerLeader())
    {
      std::list<CMessage> msgs;

      const std::list<int>& ranks = client->getRanksServerLeader();
      for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
      {
        msgs.push_back(CMessage());
        CMessage& msg = msgs.back();
        msg << this->getId();
        msg << val;
        if (isCompressible_)
          msg << writtenInd;
        event.push(*itRank, 1, msg);
      }
      client->sendEvent(event);
    }
    else client->sendEvent(event);
  }

  void CAxis::sendDistributedValue(void)
  {
    int ns, n, i, j, ind, nv, idx;
    CContext* context = CContext::getCurrent();
    CContextClient* client=context->client;

    // send value for each connected server
    CEventClient eventIndex(getType(), EVENT_ID_INDEX);
    CEventClient eventVal(getType(), EVENT_ID_DISTRIBUTED_VALUE);

    list<CMessage> list_msgsIndex, list_msgsVal;
    list<CArray<int,1> > list_indi;
    list<CArray<int,1> > list_writtenInd;
    list<CArray<double,1> > list_val;
    list<CArray<double,2> > list_bounds;

    std::map<int, std::vector<size_t> >::const_iterator it, iteMap;
    iteMap = indSrv_.end();
    for (int k = 0; k < connectedServerRank_.size(); ++k)
    {
      int nbData = 0;
      int rank = connectedServerRank_[k];
      it = indSrv_.find(rank);
      if (iteMap != it)
        nbData = it->second.size();

      list_indi.push_back(CArray<int,1>(nbData));
      list_val.push_back(CArray<double,1>(nbData));

      if (hasBounds_)
      {
        list_bounds.push_back(CArray<double,2>(2,nbData));
      }

      CArray<int,1>& indi = list_indi.back();
      CArray<double,1>& val = list_val.back();

      for (n = 0; n < nbData; ++n)
      {
        idx = static_cast<int>(it->second[n]);
        ind = idx - begin;

        val(n) = value(ind);
        indi(n) = idx;

        if (hasBounds_)
        {
          CArray<double,2>& boundsVal = list_bounds.back();
          boundsVal(0, n) = bounds(0,n);
          boundsVal(1, n) = bounds(1,n);
        }
      }

      list_msgsIndex.push_back(CMessage());
      list_msgsIndex.back() << this->getId() << list_indi.back();

      if (isCompressible_)
      {
        std::vector<int>& writtenIndSrc = indWrittenSrv_[rank];
        list_writtenInd.push_back(CArray<int,1>(writtenIndSrc.size()));
        CArray<int,1>& writtenInd = list_writtenInd.back();

        for (n = 0; n < writtenInd.numElements(); ++n)
          writtenInd(n) = writtenIndSrc[n];

        list_msgsIndex.back() << writtenInd;
      }

      list_msgsVal.push_back(CMessage());
      list_msgsVal.back() << this->getId() << list_val.back();

      if (hasBounds_)
      {
        list_msgsVal.back() << list_bounds.back();
      }

      eventIndex.push(rank, nbConnectedClients_[rank], list_msgsIndex.back());
      eventVal.push(rank, nbConnectedClients_[rank], list_msgsVal.back());
    }

    client->sendEvent(eventIndex);
    client->sendEvent(eventVal);
  }

  void CAxis::recvIndex(CEventServer& event)
  {
    CAxis* axis;

    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {
      CBufferIn* buffer = it->buffer;
      string axisId;
      *buffer >> axisId;
      axis = get(axisId);
      axis->recvIndex(it->rank, *buffer);
    }

    if (axis->isCompressible_)
    {
      std::sort(axis->indexesToWrite.begin(), axis->indexesToWrite.end());

      CContextServer* server = CContext::getCurrent()->server;
      axis->numberWrittenIndexes_ = axis->indexesToWrite.size();
      MPI_Allreduce(&axis->numberWrittenIndexes_, &axis->totalNumberWrittenIndexes_, 1, MPI_INT, MPI_SUM, server->intraComm);
      MPI_Scan(&axis->numberWrittenIndexes_, &axis->offsetWrittenIndexes_, 1, MPI_INT, MPI_SUM, server->intraComm);
      axis->offsetWrittenIndexes_ -= axis->numberWrittenIndexes_;
    }
  }

  void CAxis::recvIndex(int rank, CBufferIn& buffer)
  {
    buffer >> indiSrv_[rank];

    if (isCompressible_)
    {
      CArray<int, 1> writtenIndexes;
      buffer >> writtenIndexes;
      indexesToWrite.reserve(indexesToWrite.size() + writtenIndexes.numElements());
      for (int i = 0; i < writtenIndexes.numElements(); ++i)
        indexesToWrite.push_back(writtenIndexes(i));
    }
  }

  void CAxis::recvDistributedValue(CEventServer& event)
  {
    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {
      CBufferIn* buffer = it->buffer;
      string axisId;
      *buffer >> axisId;
      get(axisId)->recvDistributedValue(it->rank, *buffer);
    }
  }

  void CAxis::recvDistributedValue(int rank, CBufferIn& buffer)
  {
    CArray<int,1> &indi = indiSrv_[rank];
    CArray<double,1> val;
    CArray<double,2> boundsVal;

    buffer >> val;
    if (hasBounds_) buffer >> boundsVal;

    int i, j, ind_srv;
    for (int ind = 0; ind < indi.numElements(); ++ind)
    {
      i = indi(ind);
      ind_srv = i - zoom_begin_srv;
      value_srv(ind_srv) = val(ind);
      if (hasBounds_)
      {
        bound_srv(0,ind_srv) = boundsVal(0, ind);
        bound_srv(1,ind_srv) = boundsVal(1, ind);
      }
    }
  }

   void CAxis::recvNonDistributedValue(CEventServer& event)
  {
    CAxis* axis;

    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {
      CBufferIn* buffer = it->buffer;
      string axisId;
      *buffer >> axisId;
      axis = get(axisId);
      axis->recvNonDistributedValue(it->rank, *buffer);
    }

    if (axis->isCompressible_)
    {
      std::sort(axis->indexesToWrite.begin(), axis->indexesToWrite.end());

      axis->numberWrittenIndexes_ = axis->totalNumberWrittenIndexes_ = axis->indexesToWrite.size();
      axis->offsetWrittenIndexes_ = 0;
    }
  }

  void CAxis::recvNonDistributedValue(int rank, CBufferIn& buffer)
  {
    CArray<double,1> val;
    buffer >> val;

    for (int ind = 0; ind < val.numElements(); ++ind)
    {
      value_srv(ind) = val(ind);
      if (hasBounds_)
      {
        bound_srv(0,ind) = bounds(0,ind);
        bound_srv(1,ind) = bounds(1,ind);
      }
    }

    if (isCompressible_)
    {
      CArray<int, 1> writtenIndexes;
      buffer >> writtenIndexes;
      indexesToWrite.reserve(indexesToWrite.size() + writtenIndexes.numElements());
      for (int i = 0; i < writtenIndexes.numElements(); ++i)
        indexesToWrite.push_back(writtenIndexes(i));
    }
  }

  void CAxis::sendServerAttribut(const std::vector<int>& globalDim, int orderPositionInGrid,
                                 CServerDistributionDescription::ServerDistributionType distType)
  {
    CContext* context = CContext::getCurrent();
    CContextClient* client = context->client;

    CServerDistributionDescription serverDescription(globalDim);

    int nbServer = client->serverSize;

    serverDescription.computeServerDistribution(nbServer, false, distType);
    std::vector<std::vector<int> > serverIndexBegin = serverDescription.getServerIndexBegin();
    std::vector<std::vector<int> > serverDimensionSizes = serverDescription.getServerDimensionSizes();

    CEventClient event(getType(),EVENT_ID_SERVER_ATTRIBUT);
    if (client->isServerLeader())
    {
      std::list<CMessage> msgs;

      const std::list<int>& ranks = client->getRanksServerLeader();
      for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
      {
        // Use const int to ensure CMessage holds a copy of the value instead of just a reference
        const int begin = serverIndexBegin[*itRank][orderPositionInGrid];
        const int ni    = serverDimensionSizes[*itRank][orderPositionInGrid];
        const int end   = begin + ni - 1;

        msgs.push_back(CMessage());
        CMessage& msg = msgs.back();
        msg << this->getId();
        msg << ni << begin << end;
        msg << global_zoom_begin << global_zoom_size;
        msg << isCompressible_;

        event.push(*itRank,1,msg);
      }
      client->sendEvent(event);
    }
    else client->sendEvent(event);
  }

  void CAxis::recvServerAttribut(CEventServer& event)
  {
    CBufferIn* buffer = event.subEvents.begin()->buffer;
    string axisId;
    *buffer >> axisId;
    get(axisId)->recvServerAttribut(*buffer);
  }

  void CAxis::recvServerAttribut(CBufferIn& buffer)
  {
    int ni_srv, begin_srv, end_srv, global_zoom_begin_tmp, global_zoom_size_tmp;

    buffer >> ni_srv >> begin_srv >> end_srv;
    buffer >> global_zoom_begin_tmp >> global_zoom_size_tmp;
    buffer >> isCompressible_;
    global_zoom_begin = global_zoom_begin_tmp;
    global_zoom_size  = global_zoom_size_tmp;
    int global_zoom_end = global_zoom_begin + global_zoom_size - 1;

    zoom_begin_srv = global_zoom_begin > begin_srv ? global_zoom_begin : begin_srv ;
    zoom_end_srv   = global_zoom_end < end_srv ? global_zoom_end : end_srv ;
    zoom_size_srv  = zoom_end_srv - zoom_begin_srv + 1;

    if (zoom_size_srv<=0)
    {
      zoom_begin_srv = 0; zoom_end_srv = 0; zoom_size_srv = 0;
    }

    if (n_glo == n)
    {
      zoom_begin_srv = global_zoom_begin;
      zoom_end_srv   = global_zoom_end; //zoom_end;
      zoom_size_srv  = zoom_end_srv - zoom_begin_srv + 1;
    }
    value_srv.resize(zoom_size_srv);
    bound_srv.resize(2,zoom_size_srv);
  }

  bool CAxis::hasTransformation()
  {
    return (!transformationMap_.empty());
  }

  void CAxis::setTransformations(const TransMapTypes& axisTrans)
  {
    transformationMap_ = axisTrans;
  }

  CAxis::TransMapTypes CAxis::getAllTransformations(void)
  {
    return transformationMap_;
  }

  /*!
    Check the validity of all transformations applied on axis
  This functions is called AFTER all inherited attributes are solved
  */
  void CAxis::checkTransformations()
  {
    TransMapTypes::const_iterator itb = transformationMap_.begin(), it,
                                  ite = transformationMap_.end();
    for (it = itb; it != ite; ++it)
    {
      (it->second)->checkValid(this);
    }
  }

  /*!
   * Go through the hierarchy to find the axis from which the transformations must be inherited
   */
  void CAxis::solveInheritanceTransformation()
  {
    if (hasTransformation() || !hasDirectAxisReference())
      return;

    CAxis* axis = this;
    std::vector<CAxis*> refAxis;
    while (!axis->hasTransformation() && axis->hasDirectAxisReference())
    {
      refAxis.push_back(axis);
      axis = axis->getDirectAxisReference();
    }

    if (axis->hasTransformation())
      for (size_t i = 0; i < refAxis.size(); ++i)
        refAxis[i]->setTransformations(axis->getAllTransformations());

    // Try to inherit the id of the referenced object as the axis name
    // when no name was been defined and a defaut id is used.
    if (name.isEmpty())
    {
      static const std::string defId("__axis_undef_id_");
      const std::string& id = getId();
      if (id.size() > defId.size() && id.compare(0, defId.size(), defId) == 0)
        name = axis->getId();
    }
  }

  void CAxis::parse(xml::CXMLNode & node)
  {
    SuperClass::parse(node);

    if (node.goToChildElement())
    {
      StdString inverseAxisDefRoot("inverse_axis_definition");
      StdString inverse("inverse_axis");
      StdString zoomAxisDefRoot("zoom_axis_definition");
      StdString zoom("zoom_axis");
      StdString interpAxisDefRoot("interpolate_axis_definition");
      StdString interp("interpolate_axis");
      do
      {
        if (node.getElementName() == inverse) {
          CInverseAxis* tmp = (CInverseAxisGroup::get(inverseAxisDefRoot))->createChild();
          tmp->parse(node);
          transformationMap_.push_back(std::make_pair(TRANS_INVERSE_AXIS,tmp));
        } else if (node.getElementName() == zoom) {
          CZoomAxis* tmp = (CZoomAxisGroup::get(zoomAxisDefRoot))->createChild();
          tmp->parse(node);
          transformationMap_.push_back(std::make_pair(TRANS_ZOOM_AXIS,tmp));
        }
        else if (node.getElementName() == interp) {
          CInterpolateAxis* tmp = (CInterpolateAxisGroup::get(interpAxisDefRoot))->createChild();
          tmp->parse(node);
          transformationMap_.push_back(std::make_pair(TRANS_INTERPOLATE_AXIS,tmp));
        }
      } while (node.goToNextElement()) ;
      node.goToParentElement();
    }
  }

  DEFINE_REF_FUNC(Axis,axis)

   ///---------------------------------------------------------------

} // namespace xios
