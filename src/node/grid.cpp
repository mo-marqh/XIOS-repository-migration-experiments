
#include "grid.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "message.hpp"
#include <iostream>
#include "xios_spl.hpp"
#include "type.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "context_server.hpp"
#include "array_new.hpp"
#include "server_distribution_description.hpp"
#include "client_server_mapping_distributed.hpp"
#include "distribution_client.hpp"
#include "grid_transformation.hpp"
#include "grid_generate.hpp"

namespace xios {

   /// ////////////////////// Dfinitions ////////////////////// ///

   CGrid::CGrid(void)
      : CObjectTemplate<CGrid>(), CGridAttributes()
      , isChecked(false), isDomainAxisChecked(false)
      , vDomainGroup_(), vAxisGroup_(), axisList_(), isAxisListSet(false), isDomListSet(false)
      , clientDistribution_(0), isIndexSent(false) , serverDistribution_(0), clientServerMap_(0)
      , writtenDataSize_(0), numberWrittenIndexes_(0), totalNumberWrittenIndexes_(0), offsetWrittenIndexes_(0)
      , globalDim_(), connectedDataSize_(), connectedServerRank_(), isDataDistributed_(true), isCompressible_(false)
      , transformations_(0), isTransformed_(false)
      , axisPositionInGrid_(), positionDimensionDistributed_(1), hasDomainAxisBaseRef_(false)
   {
     setVirtualDomainGroup();
     setVirtualAxisGroup();
   }

   CGrid::CGrid(const StdString& id)
      : CObjectTemplate<CGrid>(id), CGridAttributes()
      , isChecked(false), isDomainAxisChecked(false)
      , vDomainGroup_(), vAxisGroup_(), axisList_(), isAxisListSet(false), isDomListSet(false)
      , clientDistribution_(0), isIndexSent(false) , serverDistribution_(0), clientServerMap_(0)
      , writtenDataSize_(0), numberWrittenIndexes_(0), totalNumberWrittenIndexes_(0), offsetWrittenIndexes_(0)
      , globalDim_(), connectedDataSize_(), connectedServerRank_(), isDataDistributed_(true), isCompressible_(false)
      , transformations_(0), isTransformed_(false)
      , axisPositionInGrid_(), positionDimensionDistributed_(1), hasDomainAxisBaseRef_(false)
   {
     setVirtualDomainGroup();
     setVirtualAxisGroup();
   }

   CGrid::~CGrid(void)
   {
    if (0 != clientDistribution_) delete clientDistribution_;
    if (0 != serverDistribution_) delete serverDistribution_;
    if (0 != clientServerMap_) delete clientServerMap_;
    if (0 != transformations_) delete transformations_;
   }

   ///---------------------------------------------------------------

   StdString CGrid::GetName(void)    { return StdString("grid"); }
   StdString CGrid::GetDefName(void) { return CGrid::GetName(); }
   ENodeType CGrid::GetType(void)    { return eGrid; }


   StdSize CGrid::getDimension(void) const
   {
      return globalDim_.size();
   }

   //---------------------------------------------------------------

   StdSize CGrid::getDataSize(void) const
   {
     StdSize retvalue = 1;
     if (!isScalarGrid())
     {
       std::vector<int> dataNindex = clientDistribution_->getDataNIndex();
       for (int i = 0; i < dataNindex.size(); ++i) retvalue *= dataNindex[i];
     }
     return retvalue;
   }

   /*!
    * Compute the minimum buffer size required to send the attributes to the server(s).
    *
    * \return A map associating the server rank with its minimum buffer size.
    */
   std::map<int, StdSize> CGrid::getAttributesBufferSize()
   {
     std::map<int, StdSize> attributesSizes = getMinimumBufferSizeForAttributes();

     // The grid indexes require a similar size as the actual data
     std::map<int, StdSize> dataSizes = getDataBufferSize();
     std::map<int, StdSize>::iterator it, itE = dataSizes.end();
     for (it = dataSizes.begin(); it != itE; ++it)
     {
       it->second += 2 * sizeof(bool);
       if (it->second > attributesSizes[it->first])
         attributesSizes[it->first] = it->second;
     }

     // Account for the axis attributes
     std::vector<CAxis*> axisList = getAxis();
     for (size_t i = 0; i < axisList.size(); ++i)
     {
       std::map<int, StdSize> axisAttBuffSize = axisList[i]->getAttributesBufferSize();
       for (it = axisAttBuffSize.begin(), itE = axisAttBuffSize.end(); it != itE; ++it)
       {
         if (it->second > attributesSizes[it->first])
           attributesSizes[it->first] = it->second;
       }
     }

     // Account for the domain attributes
     std::vector<CDomain*> domList = getDomains();
     for (size_t i = 0; i < domList.size(); ++i)
     {
       std::map<int, StdSize> domAttBuffSize = domList[i]->getAttributesBufferSize();
       for (it = domAttBuffSize.begin(), itE = domAttBuffSize.end(); it != itE; ++it)
       {
         if (it->second > attributesSizes[it->first])
           attributesSizes[it->first] = it->second;
       }
     }

     return attributesSizes;
   }

   /*!
    * Compute the minimum buffer size required to send the data to the server(s).
    *
    * \param id the id used to tag the data
    * \return A map associating the server rank with its minimum buffer size.
    */
   std::map<int, StdSize> CGrid::getDataBufferSize(const std::string& id /*= ""*/)
   {
     std::map<int, StdSize> dataSizes;
     // The record index is sometimes sent along with the data but we always
     // include it in the size calculation for the sake of simplicity
     const size_t extraSize = CEventClient::headerSize + (id.empty() ? getId() : id).size() + 2 * sizeof(size_t);

     std::map<int, size_t>::const_iterator itEnd = connectedDataSize_.end();
     for (size_t k = 0; k < connectedServerRank_.size(); ++k)
     {
       int rank = connectedServerRank_[k];
       std::map<int, size_t>::const_iterator it = connectedDataSize_.find(rank);
       size_t count = (it != itEnd) ? it->second : 0;

       dataSizes.insert(std::make_pair(rank, extraSize + CArray<double,1>::size(count)));
     }

     return dataSizes;
   }

   void CGrid::checkAttributesAfterTransformation()
   {
     setDomainList();
     std::vector<CDomain*> domListP = this->getDomains();
     if (!domListP.empty())
     {
       for (int i = 0; i < domListP.size(); ++i)
       {
         domListP[i]->checkAttributesOnClientAfterTransformation();
       }
     }
   }

   //---------------------------------------------------------------

   /*!
    * Test whether the data defined on the grid can be outputted in a compressed way.
    *
    * \return true if and only if a mask was defined for this grid
    */
   bool CGrid::isCompressible(void) const
   {
      return isCompressible_;
   }

   //---------------------------------------------------------------

   void CGrid::addRelFileCompressed(const StdString& filename)
   {
      this->relFilesCompressed.insert(filename);
   }

   bool CGrid::isWrittenCompressed(const StdString& filename) const
   {
      return (this->relFilesCompressed.find(filename) != this->relFilesCompressed.end());
   }

   //---------------------------------------------------------------

   void CGrid::solveDomainAxisRef(bool areAttributesChecked)
   {
     if (this->isDomainAxisChecked) return;

     this->solveAxisRef(areAttributesChecked);
     this->solveDomainRef(areAttributesChecked);
     computeGridGlobalDimension(getDomains(), getAxis(), axis_domain_order);
     this->isDomainAxisChecked = areAttributesChecked;
   }

   void CGrid::solveDomainAxisBaseRef()
   {
     if (this->hasDomainAxisBaseRef_) return;
     // Account for the axis attributes
     std::vector<CAxis*> axisList = getAxis();
     for (size_t i = 0; i < axisList.size(); ++i)
     {
       axisList[i]->setAttributesReference();
     }

     // Account for the domain attributes
     std::vector<CDomain*> domList = getDomains();
     for (size_t i = 0; i < domList.size(); ++i)
     {
       domList[i]->setAttributesReference();
     }
     this->hasDomainAxisBaseRef_ = true;
   }

   void CGrid::checkEligibilityForCompressedOutput()
   {
     // We don't check if the mask is valid here, just if a mask has been defined at this point.
     isCompressible_ = !mask1.isEmpty() || !mask2.isEmpty() || !mask3.isEmpty();
   }

   void CGrid::checkMaskIndex(bool doSendingIndex)
   {
     CContext* context = CContext::getCurrent();
     CContextClient* client=context->client;

     if (isScalarGrid())
     {
       if (context->hasClient)
          if (this->isChecked && doSendingIndex && !isIndexSent) { sendIndexScalarGrid(); this->isIndexSent = true; }

       if (this->isChecked) return;
       if (context->hasClient)
       {
          this->computeIndexScalarGrid();
       }

       this->isChecked = true;
       return;
     }

     if (context->hasClient)
      if (this->isChecked && doSendingIndex && !isIndexSent) { sendIndex(); this->isIndexSent = true; }

     if (this->isChecked) return;

     if (context->hasClient)
     {
        this->checkAttributesAfterTransformation();
        this->checkMask();
        this->computeIndex();
     }
     this->isChecked = true;
   }

   void CGrid::checkMask(void)
   {
      using namespace std;
      std::vector<CDomain*> domainP = this->getDomains();
      std::vector<CAxis*> axisP = this->getAxis();
      int dim = domainP.size() * 2 + axisP.size();

      std::vector<CArray<bool,1>* > domainMasks(domainP.size());
      for (int i = 0; i < domainMasks.size(); ++i) domainMasks[i] = &(domainP[i]->mask_1d);
      std::vector<CArray<bool,1>* > axisMasks(axisP.size());
      for (int i = 0; i < axisMasks.size(); ++i) axisMasks[i] = &(axisP[i]->mask);

      switch (dim) {
        case 1:
          checkGridMask(mask1, domainMasks, axisMasks, axis_domain_order);
          break;
        case 2:
          checkGridMask(mask2, domainMasks, axisMasks, axis_domain_order);
          break;
        case 3:
          checkGridMask(mask3, domainMasks, axisMasks, axis_domain_order);
          break;
//        case 4:
//          checkGridMask(mask4, domainMasks, axisMasks, axis_domain_order);
//          break;
//        case 5:
//          checkGridMask(mask5, domainMasks, axisMasks, axis_domain_order);
//          break;
//        case 6:
//          checkGridMask(mask6, domainMasks, axisMasks, axis_domain_order);
//          break;
//        case 7:
//          checkGridMask(mask7, domainMasks, axisMasks, axis_domain_order);
//          break;
        default:
          break;
      }
   }

   void CGrid::modifyMask(const CArray<int,1>& indexToModify)
   {
      using namespace std;
      std::vector<CDomain*> domainP = this->getDomains();
      std::vector<CAxis*> axisP = this->getAxis();
      int dim = domainP.size() * 2 + axisP.size();

      switch (dim) {
        case 1:
          modifyGridMask(mask1, indexToModify);
          break;
        case 2:
          modifyGridMask(mask2, indexToModify);
          break;
        case 3:
          modifyGridMask(mask3, indexToModify);
          break;

        default:
          break;
      }
   }

   //---------------------------------------------------------------

   void CGrid::solveDomainRef(bool sendAtt)
   {
      setDomainList();
      std::vector<CDomain*> domListP = this->getDomains();
      if (!domListP.empty())
      {
        for (int i = 0; i < domListP.size(); ++i)
        {
          if (sendAtt) domListP[i]->sendCheckedAttributes();
          else domListP[i]->checkAttributesOnClient();
        }
      }
   }

   //---------------------------------------------------------------

   void CGrid::solveAxisRef(bool sendAtt)
   {
      setAxisList();
      std::vector<CAxis*> axisListP = this->getAxis();
      if (!axisListP.empty())
      {
        int idx = 0;
        axisPositionInGrid_.resize(0);
        for (int i = 0; i < axis_domain_order.numElements(); ++i)
        {
          if (false == axis_domain_order(i))
          {
            axisPositionInGrid_.push_back(idx);
            ++idx;
          }
          else idx += 2;
        }

        for (int i = 0; i < axisListP.size(); ++i)
        {
          if (sendAtt)
            axisListP[i]->sendCheckedAttributes(globalDim_,axisPositionInGrid_[i]);
          else
            axisListP[i]->checkAttributesOnClient();
          ++idx;
        }
      }
   }

   std::vector<int> CGrid::getAxisPositionInGrid() const
   {
     return axisPositionInGrid_;
   }

   //---------------------------------------------------------------

   void CGrid::computeIndex(void)
   {
     CContext* context = CContext::getCurrent();
     CContextClient* client = context->client;

     // First of all, compute distribution on client side
     clientDistribution_ = new CDistributionClient(client->clientRank, this);
     // Get local data index on client
     storeIndex_client.resize(clientDistribution_->getLocalDataIndexOnClient().size());
     int nbStoreIndex = storeIndex_client.numElements();
     for (int idx = 0; idx < nbStoreIndex; ++idx) storeIndex_client(idx) = (clientDistribution_->getLocalDataIndexOnClient())[idx];
     isDataDistributed_= clientDistribution_->isDataDistributed();

     if (!doGridHaveDataDistributed())
     {
        if (0 == client->clientRank)
        {
          size_t ssize = clientDistribution_->getLocalDataIndexOnClient().size();
          for (int rank = 0; rank < client->serverSize; ++rank)
            connectedDataSize_[rank] = ssize;
        }
        return;
     }

     // Compute mapping between client and server
     size_t globalSizeIndex = 1, indexBegin, indexEnd;
     int range, clientSize = client->clientSize;
     for (int i = 0; i < globalDim_.size(); ++i) globalSizeIndex *= globalDim_[i];
     indexBegin = 0;
     for (int i = 0; i < clientSize; ++i)
     {
       range = globalSizeIndex / clientSize;
       if (i < (globalSizeIndex%clientSize)) ++range;
       if (i == client->clientRank) break;
       indexBegin += range;
     }
     indexEnd = indexBegin + range - 1;

     // Then compute distribution on server side
     CServerDistributionDescription serverDistributionDescription(globalDim_);
     serverDistributionDescription.computeServerGlobalIndexInRange(client->serverSize,
                                                                   std::make_pair<size_t,size_t>(indexBegin, indexEnd),
                                                                   positionDimensionDistributed_);

     // Finally, compute index mapping between client(s) and server(s)
     clientServerMap_ = new CClientServerMappingDistributed(serverDistributionDescription.getGlobalIndexRange(),
                                                            client->intraComm,
                                                            clientDistribution_->isDataDistributed());

     clientServerMap_->computeServerIndexMapping(clientDistribution_->getGlobalIndex());
     const std::map<int, std::vector<size_t> >& globalIndexOnServer = clientServerMap_->getGlobalIndexOnServer();

     const std::vector<size_t>& globalIndexSendToServer = clientDistribution_->getGlobalDataIndexSendToServer();
     std::map<int, std::vector<size_t> >::const_iterator iteGlobalMap, itbGlobalMap, itGlobalMap;
     itbGlobalMap = itGlobalMap = globalIndexOnServer.begin();
     iteGlobalMap = globalIndexOnServer.end();

     typedef XIOSBinarySearchWithIndex<size_t> BinarySearch;
     std::vector<int>::iterator itVec;
     int nbGlobalIndex = globalIndexSendToServer.size();
     for (; itGlobalMap != iteGlobalMap; ++itGlobalMap)
     {
       int serverRank = itGlobalMap->first;
       int indexSize = itGlobalMap->second.size();
       std::vector<int> permutIndex(indexSize);
       XIOSAlgorithms::fillInIndex(indexSize, permutIndex);
       XIOSAlgorithms::sortWithIndex<size_t, CVectorStorage>(itGlobalMap->second, permutIndex);
       BinarySearch binSearch(itGlobalMap->second);
       for (int i = 0; i < nbGlobalIndex; ++i)
       {
         if (binSearch.search(permutIndex.begin(), permutIndex.end(), globalIndexSendToServer[i], itVec))
         {
           if (connectedDataSize_.end() == connectedDataSize_.find(serverRank))
             connectedDataSize_[serverRank] = 1;
           else
             ++connectedDataSize_[serverRank];
         }
       }
     }

     connectedServerRank_.clear();
     for (std::map<int, std::vector<size_t> >::const_iterator it = globalIndexOnServer.begin(); it != globalIndexOnServer.end(); ++it) {
       connectedServerRank_.push_back(it->first);
     }

     nbSenders = clientServerMap_->computeConnectedClients(client->serverSize, client->clientSize, client->intraComm, connectedServerRank_);
   }

   //----------------------------------------------------------------

   CGrid* CGrid::createGrid(CDomain* domain)
   {
      std::vector<CDomain*> vecDom(1, domain);
      std::vector<CAxis*> vecAxis;

      return createGrid(vecDom, vecAxis);
   }

   CGrid* CGrid::createGrid(CDomain* domain, CAxis* axis)
   {
      std::vector<CDomain*> vecDom(1, domain);
      std::vector<CAxis*> vecAxis(1, axis);

      return createGrid(vecDom, vecAxis);
   }

   CGrid* CGrid::createGrid(const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                            const CArray<bool,1>& axisDomainOrder)
   {
      return createGrid(generateId(domains, axis, axisDomainOrder), domains, axis, axisDomainOrder);
   }

   CGrid* CGrid::createGrid(StdString id, const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                            const CArray<bool,1>& axisDomainOrder)
   {
      if (axisDomainOrder.numElements() > 0 && axisDomainOrder.numElements() != (domains.size() + axis.size()))
        ERROR("CGrid* CGrid::createGrid(...)",
              << "The size of axisDomainOrder (" << axisDomainOrder.numElements()
              << ") is not coherent with the number of elements (" << domains.size() + axis.size() <<").");

      CGrid* grid = CGridGroup::get("grid_definition")->createChild(id);
      grid->setDomainList(domains);
      grid->setAxisList(axis);

      // By default, domains are always the first elements of a grid
      if (0 == axisDomainOrder.numElements())
      {
        int size = domains.size() + axis.size();
        grid->axis_domain_order.resize(size);
        for (int i = 0; i < size; ++i)
        {
          if (i < domains.size()) grid->axis_domain_order(i) = true;
          else grid->axis_domain_order(i) = false;
        }
      }
      else
      {
        grid->axis_domain_order.resize(axisDomainOrder.numElements());
        grid->axis_domain_order = axisDomainOrder;
      }

      grid->solveDomainAxisRefInheritance(true);

      return grid;
   }

   StdString CGrid::generateId(const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                               const CArray<bool,1>& axisDomainOrder)
   {
      if (axisDomainOrder.numElements() > 0 && axisDomainOrder.numElements() != (domains.size() + axis.size()))
        ERROR("CGrid* CGrid::generateId(...)",
              << "The size of axisDomainOrder (" << axisDomainOrder.numElements()
              << ") is not coherent with the number of elements (" << domains.size() + axis.size() <<").");

      std::ostringstream id;

      if (domains.empty() && axis.empty())
        id << "__scalar_grid__";
      else
      {
        id << "__grid";

        if (0 == axisDomainOrder.numElements())
        {
          for (size_t i = 0; i < domains.size(); ++i) id << "_" << domains[i]->getId();
          for (size_t i = 0; i < axis.size(); ++i) id << "_" << axis[i]->getId();
        }
        else
        {
          size_t iDomain = 0, iAxis = 0;
          for (size_t i = 0; i < axisDomainOrder.numElements(); ++i)
          {
            if (axisDomainOrder(i))
              id << "_" << domains[iDomain++]->getId();
            else
              id << "_" << axis[iAxis++]->getId();
          }
        }

        id << "__";
      }

      return id.str();
   }

   //----------------------------------------------------------------

   CDomainGroup* CGrid::getVirtualDomainGroup() const
   {
     return this->vDomainGroup_;
   }

   CAxisGroup* CGrid::getVirtualAxisGroup() const
   {
     return this->vAxisGroup_;
   }

   void CGrid::outputField(int rank, const CArray<double, 1>& stored, double* field)
   {
     const CArray<size_t,1>& out_i = outIndexFromClient[rank];
     StdSize numElements = stored.numElements();
     for (StdSize n = 0; n < numElements; ++n)
     {
       field[out_i(n)] = stored(n);
     }
   }

   void CGrid::inputField(int rank, const double* const field, CArray<double,1>& stored)
   {
     const CArray<size_t,1>& out_i = outIndexFromClient[rank];
     StdSize numElements = stored.numElements();
     for (StdSize n = 0; n < numElements; ++n)
     {
       stored(n) = field[out_i(n)];
     }
   }

   void CGrid::outputCompressedField(int rank, const CArray<double,1>& stored, double* field)
   {
     const CArray<size_t,1>& out_i = compressedOutIndexFromClient[rank];
     StdSize numElements = stored.numElements();
     for (StdSize n = 0; n < numElements; ++n)
     {
       field[out_i(n)] = stored(n);
     }
   }

   //----------------------------------------------------------------

   void CGrid::storeField_arr(const double* const data, CArray<double, 1>& stored) const
   {
      const StdSize size = storeIndex_client.numElements();

      stored.resize(size);
      for(StdSize i = 0; i < size; i++) stored(i) = data[storeIndex_client(i)];
   }

   void CGrid::restoreField_arr(const CArray<double, 1>& stored, double* const data) const
   {
      const StdSize size = storeIndex_client.numElements();

      for(StdSize i = 0; i < size; i++) data[storeIndex_client(i)] = stored(i);
   }

  void CGrid::computeIndexScalarGrid()
  {
    CContext* context = CContext::getCurrent();
    CContextClient* client=context->client;

    storeIndex_client.resize(1);
    storeIndex_client[0] = 0;
    if (0 == client->clientRank)
    {
      for (int rank = 0; rank < client->serverSize; ++rank)
        connectedDataSize_[rank] = 1;
    }
    isDataDistributed_ = false;
  }

  void CGrid::computeCompressedIndex()
  {
    compressedOutIndexFromClient = outIndexFromClient;

    std::map<size_t, size_t> indexes;

    {
      std::map<int, CArray<size_t,1> >::const_iterator it = compressedOutIndexFromClient.begin();
      std::map<int, CArray<size_t,1> >::const_iterator itEnd = compressedOutIndexFromClient.end();
      for (; it != itEnd; ++it)
      {
        for (int i = 0; i < it->second.numElements(); ++i)
          indexes.insert(std::make_pair(it->second(i), 0));
      }
    }

    {
      std::map<size_t, size_t>::iterator it = indexes.begin();
      std::map<size_t, size_t>::iterator itEnd = indexes.end();
      for (size_t i = 0; it != itEnd; ++it, ++i)
        it->second = i;
    }

    {
      std::map<int, CArray<size_t,1> >::iterator it = compressedOutIndexFromClient.begin();
      std::map<int, CArray<size_t,1> >::iterator itEnd = compressedOutIndexFromClient.end();
      for (; it != itEnd; ++it)
      {
        for (int i = 0; i < it->second.numElements(); ++i)
          it->second(i) = indexes[it->second(i)];
      }
    }
  }

  void CGrid::sendIndexScalarGrid()
  {
    CContext* context = CContext::getCurrent();
    CContextClient* client = context->client;

    CEventClient event(getType(), EVENT_ID_INDEX);
    list<CMessage> listMsg;
    list<CArray<size_t,1> > listOutIndex;

    if (0 == client->clientRank)
    {
      for (int rank = 0; rank < client->serverSize; ++rank)
      {
        int nb = 1;
        storeIndex_toSrv.insert(std::make_pair(rank, CArray<int,1>(nb)));
        listOutIndex.push_back(CArray<size_t,1>(nb));

        CArray<int, 1>& outLocalIndexToServer = storeIndex_toSrv[rank];
        CArray<size_t, 1>& outGlobalIndexOnServer = listOutIndex.back();

        for (int k = 0; k < nb; ++k)
        {
          outGlobalIndexOnServer(k) = 0;
          outLocalIndexToServer(k)  = 0;
        }

        listMsg.push_back(CMessage());
        listMsg.back() << getId( )<< isDataDistributed_ << isCompressible_ << listOutIndex.back();

        event.push(rank, 1, listMsg.back());
      }

      client->sendEvent(event);
    }
    else
      client->sendEvent(event);
  }

  void CGrid::sendIndex(void)
  {
    CContext* context = CContext::getCurrent();
    CContextClient* client = context->client;

    CEventClient event(getType(), EVENT_ID_INDEX);
    int rank;
    list<CMessage> listMsg;
    list<CArray<size_t,1> > listOutIndex;
    const std::map<int, std::vector<size_t> >& globalIndexOnServer = clientServerMap_->getGlobalIndexOnServer();
    const std::vector<int>& localIndexSendToServer = clientDistribution_->getLocalDataIndexSendToServer();
    const std::vector<size_t>& globalIndexSendToServer = clientDistribution_->getGlobalDataIndexSendToServer();

    if (!doGridHaveDataDistributed())
    {
      if (0 == client->clientRank)
      {
        CArray<size_t,1> outGlobalIndexOnServer(globalIndexSendToServer.size());
        for (int idx = 0; idx < globalIndexSendToServer.size();++idx)
          outGlobalIndexOnServer(idx) = globalIndexSendToServer[idx];

        CArray<int,1> outLocalIndexToServer(localIndexSendToServer.size());
        for (int idx = 0; idx < localIndexSendToServer.size();++idx)
          outLocalIndexToServer(idx) = localIndexSendToServer[idx];

        for (rank = 0; rank < client->serverSize; ++rank)
        {
          storeIndex_toSrv.insert(std::make_pair(rank, CArray<int,1>(outLocalIndexToServer)));
          listOutIndex.push_back(CArray<size_t,1>(outGlobalIndexOnServer));

          listMsg.push_back(CMessage());
          listMsg.back() << getId() << isDataDistributed_ << isCompressible_ << listOutIndex.back();

          event.push(rank, 1, listMsg.back());
        }

        client->sendEvent(event);
      }
      else
        client->sendEvent(event);
    }
    else
    {
      std::map<int, std::vector<size_t> >::const_iterator iteGlobalMap, itbGlobalMap, itGlobalMap;
      itbGlobalMap = itGlobalMap = globalIndexOnServer.begin();
      iteGlobalMap = globalIndexOnServer.end();

      int nbGlobalIndex = globalIndexSendToServer.size();
      std::map<int,std::vector<int> >localIndexTmp;
      std::map<int,std::vector<size_t> > globalIndexTmp;

      typedef XIOSBinarySearchWithIndex<size_t> BinarySearch;
      std::vector<int>::iterator itVec;
      for (; itGlobalMap != iteGlobalMap; ++itGlobalMap)
      {
        int serverRank = itGlobalMap->first;
        int indexSize = itGlobalMap->second.size();
        std::vector<int> permutIndex(indexSize);
        XIOSAlgorithms::fillInIndex(indexSize, permutIndex);
        XIOSAlgorithms::sortWithIndex<size_t, CVectorStorage>(itGlobalMap->second, permutIndex);
        BinarySearch binSearch(itGlobalMap->second);

        for (int i = 0; i < nbGlobalIndex; ++i)
        {
          if (binSearch.search(permutIndex.begin(), permutIndex.end(), globalIndexSendToServer[i], itVec))
          {
            globalIndexTmp[serverRank].push_back(globalIndexSendToServer[i]);
            localIndexTmp[serverRank].push_back(localIndexSendToServer[i]);
          }
        }
      }

      for (int ns = 0; ns < connectedServerRank_.size(); ++ns)
      {
        rank = connectedServerRank_[ns];
        int nb = 0;
        if (globalIndexTmp.end() != globalIndexTmp.find(rank))
          nb = globalIndexTmp[rank].size();

        storeIndex_toSrv.insert(make_pair(rank, CArray<int,1>(nb)));
        listOutIndex.push_back(CArray<size_t,1>(nb));

        CArray<int, 1>& outLocalIndexToServer = storeIndex_toSrv[rank];
        CArray<size_t, 1>& outGlobalIndexOnServer = listOutIndex.back();

        for (int k = 0; k < nb; ++k)
        {
          outGlobalIndexOnServer(k) = globalIndexTmp[rank].at(k);
          outLocalIndexToServer(k)  = localIndexTmp[rank].at(k);
        }

        listMsg.push_back(CMessage());
        listMsg.back() << getId() << isDataDistributed_ << isCompressible_ << listOutIndex.back();

        event.push(rank, nbSenders[rank], listMsg.back());
      }

      client->sendEvent(event);
    }
  }

  void CGrid::recvIndex(CEventServer& event)
  {
    string gridId;
    vector<int> ranks;
    vector<CBufferIn*> buffers;

    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {
      ranks.push_back(it->rank);
      CBufferIn* buffer = it->buffer;
      *buffer >> gridId;
      buffers.push_back(buffer);
    }
    get(gridId)->recvIndex(ranks, buffers);
  }

  void CGrid::computeGridGlobalDimension(const std::vector<CDomain*>& domains,
                                         const std::vector<CAxis*>& axis,
                                         const CArray<bool,1>& axisDomainOrder)
  {
    globalDim_.resize(domains.size()*2+axis.size());
    int idx = 0, idxDomain = 0, idxAxis = 0;
    for (int i = 0; i < axisDomainOrder.numElements(); ++i)
    {
      if (axisDomainOrder(i))
      {
        if (!(domains[idxDomain]->type.isEmpty()) && (domains[idxDomain]->type==CDomain::type_attr::unstructured))
        {
          positionDimensionDistributed_ = idx;
        }
        else
        {
          positionDimensionDistributed_ = idx +1;
        }

        globalDim_[idx]   = domains[idxDomain]->ni_glo.getValue();
        globalDim_[idx+1] = domains[idxDomain]->nj_glo.getValue();

        ++idxDomain;
        idx += 2;
      }
      else
      {
        globalDim_[idx] = axis[idxAxis]->n_glo.getValue();
        ++idxAxis;
        ++idx;
      }
    }
  }

  std::vector<int> CGrid::getGlobalDimension()
  {
    return globalDim_;
  }

  bool CGrid::isScalarGrid() const
  {
    return (axisList_.empty() && domList_.empty());
  }

  /*!
    Verify whether one server need to write data
    There are some cases on which one server has nodata to write. For example, when we
    just only want to zoom on a domain.
  */
  bool CGrid::doGridHaveDataToWrite()
  {
     return (0 != writtenDataSize_);
  }

  /*!
    Return size of data which is written on each server
    Whatever dimension of a grid, data which are written on server must be presented as
    an one dimension array.
    \return size of data written on server
  */
  size_t CGrid::getWrittenDataSize() const
  {
    return writtenDataSize_;
  }

  /*!
    Returns the number of indexes written by each server.
    \return the number of indexes written by each server
  */
  int CGrid::getNumberWrittenIndexes() const
  {
    return numberWrittenIndexes_;
  }

  /*!
    Returns the total number of indexes written by the servers.
    \return the total number of indexes written by the servers
  */
  int CGrid::getTotalNumberWrittenIndexes() const
  {
    return totalNumberWrittenIndexes_;
  }

  /*!
    Returns the offset of indexes written by each server.
    \return the offset of indexes written by each server
  */
  int CGrid::getOffsetWrittenIndexes() const
  {
    return offsetWrittenIndexes_;
  }

  const CDistributionServer* CGrid::getDistributionServer() const
  {
    return serverDistribution_;
  }

  const CDistributionClient* CGrid::getDistributionClient() const
  {
    return clientDistribution_;
  }

  bool CGrid::doGridHaveDataDistributed()
  {
    if (isScalarGrid()) return false;
    else
      return isDataDistributed_;
  }

  void CGrid::recvIndex(vector<int> ranks, vector<CBufferIn*> buffers)
  {
    CContext* context = CContext::getCurrent();
    CContextServer* server = context->server;
    numberWrittenIndexes_ = totalNumberWrittenIndexes_ = offsetWrittenIndexes_ = 0;
    connectedServerRank_ = ranks;

    for (int n = 0; n < ranks.size(); n++)
    {
      int rank = ranks[n];
      CBufferIn& buffer = *buffers[n];

      buffer >> isDataDistributed_ >> isCompressible_;
      size_t dataSize = 0;

      if (isScalarGrid())
      {
        writtenDataSize_ = numberWrittenIndexes_ = totalNumberWrittenIndexes_ = 1;
        CArray<size_t,1> outIndex;
        buffer >> outIndex;
        outIndexFromClient.insert(std::make_pair(rank, outIndex));
        std::vector<int> nZoomBegin(1,0), nZoomSize(1,1), nGlob(1,1), nZoomBeginGlobal(1,0);
        serverDistribution_ = new CDistributionServer(server->intraCommRank, nZoomBegin, nZoomSize,
                                                      nZoomBeginGlobal, nGlob);
        return;
      }

      if (0 == serverDistribution_)
      {
        int idx = 0, numElement = axis_domain_order.numElements();
        int ssize = numElement;
        std::vector<int> indexMap(numElement);
        for (int i = 0; i < numElement; ++i)
        {
          indexMap[i] = idx;
          if (true == axis_domain_order(i))
          {
            ++ssize;
            idx += 2;
          }
          else
            ++idx;
        }

        int axisId = 0, domainId = 0;
        std::vector<CDomain*> domainList = getDomains();
        std::vector<CAxis*> axisList = getAxis();
        std::vector<int> nZoomBegin(ssize), nZoomSize(ssize), nGlob(ssize), nZoomBeginGlobal(ssize);
        for (int i = 0; i < numElement; ++i)
        {
          if (axis_domain_order(i))
          {
            nZoomBegin[indexMap[i]] = domainList[domainId]->zoom_ibegin_srv;
            nZoomSize[indexMap[i]]  = domainList[domainId]->zoom_ni_srv;
            nZoomBeginGlobal[indexMap[i]] = domainList[domainId]->global_zoom_ibegin;
            nGlob[indexMap[i]] = domainList[domainId]->ni_glo;

            nZoomBegin[indexMap[i] + 1] = domainList[domainId]->zoom_jbegin_srv;
            nZoomSize[indexMap[i] + 1] = domainList[domainId]->zoom_nj_srv;
            nZoomBeginGlobal[indexMap[i] + 1] = domainList[domainId]->global_zoom_jbegin;
            nGlob[indexMap[i] + 1] = domainList[domainId]->nj_glo;
            ++domainId;
          }
          else
          {
            nZoomBegin[indexMap[i]] = axisList[axisId]->zoom_begin_srv;
            nZoomSize[indexMap[i]]  = axisList[axisId]->zoom_size_srv;
            nZoomBeginGlobal[indexMap[i]] = axisList[axisId]->global_zoom_begin;
            nGlob[indexMap[i]] = axisList[axisId]->n_glo;
            ++axisId;
          }
        }
        dataSize = 1;
        for (int i = 0; i < nZoomSize.size(); ++i)
          dataSize *= nZoomSize[i];

        serverDistribution_ = new CDistributionServer(server->intraCommRank, nZoomBegin, nZoomSize,
                                                      nZoomBeginGlobal, nGlob);
      }

      CArray<size_t,1> outIndex;
      buffer >> outIndex;
      if (isDataDistributed_)
        serverDistribution_->computeLocalIndex(outIndex);
      else
      {
        dataSize = outIndex.numElements();
        for (int i = 0; i < outIndex.numElements(); ++i) outIndex(i) = i;
      }
      writtenDataSize_ += dataSize;

      outIndexFromClient.insert(std::make_pair(rank, outIndex));
      connectedDataSize_[rank] = outIndex.numElements();
      numberWrittenIndexes_ += outIndex.numElements();
    }

    if (isDataDistributed_)
    {
      MPI_Allreduce(&numberWrittenIndexes_, &totalNumberWrittenIndexes_, 1, MPI_INT, MPI_SUM, server->intraComm);
      MPI_Scan(&numberWrittenIndexes_, &offsetWrittenIndexes_, 1, MPI_INT, MPI_SUM, server->intraComm);
      offsetWrittenIndexes_ -= numberWrittenIndexes_;
    }
    else
      totalNumberWrittenIndexes_ = numberWrittenIndexes_;

    nbSenders = CClientServerMappingDistributed::computeConnectedClients(context->client->serverSize, context->client->clientSize, context->client->intraComm, ranks);
  }

   /*!
   \brief Dispatch event received from client
      Whenever a message is received in buffer of server, it will be processed depending on
   its event type. A new event type should be added in the switch list to make sure
   it processed on server side.
   \param [in] event: Received message
   */
  bool CGrid::dispatchEvent(CEventServer& event)
  {

    if (SuperClass::dispatchEvent(event)) return true;
    else
    {
      switch(event.type)
      {
        case EVENT_ID_INDEX :
          recvIndex(event);
          return true;
          break;

         case EVENT_ID_ADD_DOMAIN :
           recvAddDomain(event);
           return true;
           break;

         case EVENT_ID_ADD_AXIS :
           recvAddAxis(event);
           return true;
           break;
        default :
          ERROR("bool CDomain::dispatchEvent(CEventServer& event)",
                << "Unknown Event");
          return false;
      }
    }
  }

   ///---------------------------------------------------------------

   CDomain* CGrid::addDomain(const std::string& id)
   {
     return vDomainGroup_->createChild(id);
   }

   CAxis* CGrid::addAxis(const std::string& id)
   {
     return vAxisGroup_->createChild(id);
   }

   //! Change virtual field group to a new one
   void CGrid::setVirtualDomainGroup(CDomainGroup* newVDomainGroup)
   {
      this->vDomainGroup_ = newVDomainGroup;
   }

   //! Change virtual variable group to new one
   void CGrid::setVirtualAxisGroup(CAxisGroup* newVAxisGroup)
   {
      this->vAxisGroup_ = newVAxisGroup;
   }

   //----------------------------------------------------------------
   //! Create virtual field group, which is done normally on initializing file
   void CGrid::setVirtualDomainGroup(void)
   {
      this->setVirtualDomainGroup(CDomainGroup::create());
   }

   //! Create virtual variable group, which is done normally on initializing file
   void CGrid::setVirtualAxisGroup(void)
   {
      this->setVirtualAxisGroup(CAxisGroup::create());
   }

   /*!
   \brief Send a message to create a domain on server side
   \param[in] id String identity of domain that will be created on server
   */
   void CGrid::sendAddDomain(const string& id)
   {
    CContext* context=CContext::getCurrent();

    if (! context->hasServer )
    {
       CContextClient* client=context->client;

       CEventClient event(this->getType(),EVENT_ID_ADD_DOMAIN);
       if (client->isServerLeader())
       {
         CMessage msg;
         msg<<this->getId();
         msg<<id;
         const std::list<int>& ranks = client->getRanksServerLeader();
         for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
           event.push(*itRank,1,msg);
         client->sendEvent(event);
       }
       else client->sendEvent(event);
    }
   }

   /*!
   \brief Send a message to create an axis on server side
   \param[in] id String identity of axis that will be created on server
   */
   void CGrid::sendAddAxis(const string& id)
   {
    CContext* context=CContext::getCurrent();

    if (! context->hasServer )
    {
       CContextClient* client=context->client;

       CEventClient event(this->getType(),EVENT_ID_ADD_AXIS);
       if (client->isServerLeader())
       {
         CMessage msg;
         msg<<this->getId();
         msg<<id;
         const std::list<int>& ranks = client->getRanksServerLeader();
         for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
           event.push(*itRank,1,msg);
         client->sendEvent(event);
       }
       else client->sendEvent(event);
    }
   }

   /*!
   \brief Receive a message annoucing the creation of a domain on server side
   \param[in] event Received event
   */
   void CGrid::recvAddDomain(CEventServer& event)
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer >> id;
      get(id)->recvAddDomain(*buffer);
   }

   /*!
   \brief Receive a message annoucing the creation of a domain on server side
   \param[in] buffer Buffer containing message
   */
   void CGrid::recvAddDomain(CBufferIn& buffer)
   {
      string id;
      buffer >> id;
      addDomain(id);
   }

   /*!
   \brief Receive a message annoucing the creation of an axis on server side
   \param[in] event Received event
   */
   void CGrid::recvAddAxis(CEventServer& event)
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer >> id;
      get(id)->recvAddAxis(*buffer);
   }

   /*!
   \brief Receive a message annoucing the creation of an axis on server side
   \param[in] buffer Buffer containing message
   */
   void CGrid::recvAddAxis(CBufferIn& buffer)
   {
      string id;
      buffer >> id;
      addAxis(id);
   }

  /*!
  \brief Solve domain and axis references
  As field, domain and axis can refer to other domains or axis. In order to inherit correctly
  all attributes from their parents, they should be processed with this function
  \param[in] apply inherit all attributes of parents (true)
  */
  void CGrid::solveDomainAxisRefInheritance(bool apply)
  {
    CContext* context = CContext::getCurrent();
    unsigned int vecSize, i;
    std::vector<StdString>::iterator it, itE;
    setDomainList();
    it = domList_.begin(); itE = domList_.end();
    for (; it != itE; ++it)
    {
      CDomain* pDom = CDomain::get(*it);
      if (context->hasClient)
      {
        pDom->solveRefInheritance(apply);
        pDom->solveInheritanceTransformation();
      }
    }

    setAxisList();
    it = axisList_.begin(); itE = axisList_.end();
    for (; it != itE; ++it)
    {
      CAxis* pAxis = CAxis::get(*it);
      if (context->hasClient)
      {
        pAxis->solveRefInheritance(apply);
        pAxis->solveInheritanceTransformation();
      }
    }
  }

  bool CGrid::isTransformed()
  {
    return isTransformed_;
  }

  void CGrid::setTransformed()
  {
    isTransformed_ = true;
  }

  CGridTransformation* CGrid::getTransformations()
  {
    return transformations_;
  }

  /*!
     Complete all the necessary (and lacking) attributes of a grid
     This function is similar to gridTransformation but works only (till now) on generate_rectilinear_domain transformation
  */
  void CGrid::completeGrid(CGrid* transformGridSrc)
  {
    if (0 != transformGridSrc)
    {
      if (axis_domain_order.numElements() != transformGridSrc->axis_domain_order.numElements())
      {
        ERROR("CGrid::completeGrid(CGrid* transformGridSrc)",
             << "Two grids have different dimension size"
             << "Dimension of grid destination " << this->getId() << " is " << axis_domain_order.numElements() << std::endl
             << "Dimension of grid source " << transformGridSrc->getId() << " is " << transformGridSrc->axis_domain_order.numElements());
      }
      else
      {
        int ssize = axis_domain_order.numElements();
        for (int i = 0; i < ssize; ++i)
          if (axis_domain_order(i) != (transformGridSrc->axis_domain_order)(i))
            ERROR("CGrid::completeGrid(CGrid* transformGridSrc)",
                  << "Grids " << this->getId() << " and " << transformGridSrc->getId()
                  << " don't have elements in the same order");
      }
    }

    CGridGenerate gridGenerate(this, transformGridSrc);
    gridGenerate.completeGrid();
  }

  void CGrid::transformGrid(CGrid* transformGridSrc)
  {
    if (!transformGridSrc)
      ERROR("CGrid::transformGrid(CGrid* transformGridSrc)",
            << "Impossible to transform grid '" << getId() << "', the source grid is null.");

    if (isTransformed()) return;
    setTransformed();
    if (axis_domain_order.numElements() != transformGridSrc->axis_domain_order.numElements())
    {
      ERROR("CGrid::transformGrid(CGrid* transformGridSrc)",
           << "Two grids have different dimension size"
           << "Dimension of grid destination " << this->getId() << " is " << axis_domain_order.numElements() << std::endl
           << "Dimension of grid source " << transformGridSrc->getId() << " is " << transformGridSrc->axis_domain_order.numElements());
    }
    else
    {
      int ssize = axis_domain_order.numElements();
      for (int i = 0; i < ssize; ++i)
        if (axis_domain_order(i) != (transformGridSrc->axis_domain_order)(i))
          ERROR("CGrid::transformGrid(CGrid* transformGridSrc)",
                << "Grids " << this->getId() << " and " << transformGridSrc->getId()
                << " don't have elements in the same order");
    }

    transformations_ = new CGridTransformation(this, transformGridSrc);
    transformations_->computeAll();

    // Ok, now need to compute index of grid source
    transformGridSrc->checkMaskIndex(false);
  }

  /*!
  \brief Get the list of domain pointers
  \return list of domain pointers
  */
  std::vector<CDomain*> CGrid::getDomains()
  {
    std::vector<CDomain*> domList;
    if (!domList_.empty())
    {
      for (int i = 0; i < domList_.size(); ++i) domList.push_back(CDomain::get(domList_[i]));
    }
    return domList;
  }

  /*!
  \brief Get the list of  axis pointers
  \return list of axis pointers
  */
  std::vector<CAxis*> CGrid::getAxis()
  {
    std::vector<CAxis*> aList;
    if (!axisList_.empty())
      for (int i =0; i < axisList_.size(); ++i) aList.push_back(CAxis::get(axisList_[i]));

    return aList;
  }

  /*!
  \brief Set domain(s) of a grid from a list
  \param[in] domains list of domains
  */
  void CGrid::setDomainList(const std::vector<CDomain*> domains)
  {
    if (isDomListSet) return;
    std::vector<CDomain*> domList = this->getVirtualDomainGroup()->getAllChildren();
    if (!domains.empty() && domList.empty())
    {
      for (int i = 0; i < domains.size(); ++i)
        this->getVirtualDomainGroup()->addChild(domains[i]);
      domList = this->getVirtualDomainGroup()->getAllChildren();
    }

    if (!domList.empty())
    {
      int sizeDom = domList.size();
      domList_.resize(sizeDom);
      for (int i = 0; i < sizeDom; ++i)
      {
        domList_[i] = domList[i]->getId();
      }
      isDomListSet = true;
    }

  }

  /*!
  \brief Set axis(s) of a grid from a list
  \param[in] axis list of axis
  */
  void CGrid::setAxisList(const std::vector<CAxis*> axis)
  {
    if (isAxisListSet) return;
    std::vector<CAxis*> aList = this->getVirtualAxisGroup()->getAllChildren();
    if (!axis.empty() && aList.empty())
    {
      for (int i = 0; i < axis.size(); ++i)
        this->getVirtualAxisGroup()->addChild(axis[i]);
      aList = this->getVirtualAxisGroup()->getAllChildren();
    }

    if (!aList.empty())
    {
      int sizeAxis = aList.size();
      axisList_.resize(sizeAxis);
      for (int i = 0; i < sizeAxis; ++i)
      {
        axisList_[i] = aList[i]->getId();
      }
      isAxisListSet = true;
    }
  }

  /*!
  \brief Get list of id of domains
  \return id list of domains
  */
  std::vector<StdString> CGrid::getDomainList()
  {
    setDomainList();
    return domList_;
  }

  /*!
  \brief Get list of id of axis
  \return id list of axis
  */
  std::vector<StdString> CGrid::getAxisList()
  {
    setAxisList();
    return axisList_;
  }

  void CGrid::sendAllDomains()
  {
    std::vector<CDomain*> domList = this->getVirtualDomainGroup()->getAllChildren();
    int dSize = domList.size();
    for (int i = 0; i < dSize; ++i)
    {
      sendAddDomain(domList[i]->getId());
      domList[i]->sendAllAttributesToServer();
    }
  }

  void CGrid::sendAllAxis()
  {
    std::vector<CAxis*> aList = this->getVirtualAxisGroup()->getAllChildren();
    int aSize = aList.size();

    for (int i = 0; i < aSize; ++i)
    {
      sendAddAxis(aList[i]->getId());
      aList[i]->sendAllAttributesToServer();
    }
  }

  void CGrid::parse(xml::CXMLNode& node)
  {
    SuperClass::parse(node);

    // List order of axis and domain in a grid, if there is a domain, it will take value 1 (true), axis 0 (false)
    std::vector<bool> order;

    if (node.goToChildElement())
    {
      StdString domainName("domain");
      StdString axisName("axis");
      do
      {
        if (node.getElementName() == domainName) {
          order.push_back(true);
          this->getVirtualDomainGroup()->parseChild(node);
        }
        if (node.getElementName() == axisName) {
          order.push_back(false);
          this->getVirtualAxisGroup()->parseChild(node);
        }
      } while (node.goToNextElement());
      node.goToParentElement();
    }

    if (!order.empty())
    {
      int sizeOrd = order.size();
      axis_domain_order.resize(sizeOrd);
      for (int i = 0; i < sizeOrd; ++i)
      {
        axis_domain_order(i) = order[i];
      }
    }

    setDomainList();
    setAxisList();
   }
} // namespace xios
