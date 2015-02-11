#include "distribution_client.hpp"

namespace xios {

CDistributionClient::CDistributionClient(int rank, int dims, CArray<size_t,1>* globalIndex)
   : CDistribution(rank, dims, globalIndex),
   localDataIndex_(0), indexGlobalOnServer_(), localIndexSend2Server_(), axisDomainOrder_(),
   nLocal_(), nGlob_(), nBeginLocal_(), nBeginGlobal_(),nZoomBegin_(), nZoomEnd_(),
   dataNIndex_(), dataDims_(), dataBegin_(), dataIndex_(), domainMasks_(), axisMasks_(),
   gridMask_(), localDomainIndex_(), localAxisIndex_(), indexMap_(), connectedClients_(),
   isConnectedServerComputed_(false), indexDomainData_()
{
}

CDistributionClient::CDistributionClient(int rank, CGrid* grid)
   : CDistribution(rank, 0, 0),
   localDataIndex_(0), indexGlobalOnServer_(), localIndexSend2Server_(), axisDomainOrder_(),
   nLocal_(), nGlob_(), nBeginLocal_(), nBeginGlobal_(),nZoomBegin_(), nZoomEnd_(),
   dataNIndex_(), dataDims_(), dataBegin_(), dataIndex_(), domainMasks_(), axisMasks_(),
   gridMask_(), localDomainIndex_(), localAxisIndex_(), indexMap_(), connectedClients_(),
   isConnectedServerComputed_(false), indexDomainData_()
{
  readDistributionInfo(grid);
  createGlobalIndex();
}

CDistributionClient::~CDistributionClient()
{
  if (0 != localDataIndex_) delete localDataIndex_;
}

/*!
  Read information of a grid to generate distribution.
  Every grid is composed of several axis or/and domain(s). Their information are processed
stored and used to calculate index distribution between client and server
  \param [in] grid Grid to read
*/
void CDistributionClient::readDistributionInfo(CGrid* grid)
{
  std::vector<CDomain*> domList = grid->getDomains();
  std::vector<CAxis*> axisList = grid->getAxis();
  CArray<bool,1>& axisDomainOrder = grid->axisDomainOrder;

  std::vector<CDomain*>::iterator itbDom, iteDom, itDom;
  std::vector<CAxis*>::iterator itbAxis, iteAxis, itAxis;

  itbDom  = itDom  = domList.begin();  iteDom  = domList.end();
  itbAxis = itAxis = axisList.begin(); iteAxis = axisList.end();

  // First of all, every attribute of domain and axis should be checked
  for (;itDom != iteDom; ++itDom) (*itDom)->checkAttributesOnClient();
  for (;itAxis != iteAxis; ++itAxis) (*itAxis)->checkAttributes();

  // Then check mask of grid
  grid->checkMask();
  CArray<bool,3>& gridMask = grid->mask;

  ////////////////////////////////////////////////////////

  int gridDim = domList.size()*2 + axisList.size();

  // For now, just suppose that gridMask is all true, but we need to cope with this problem
  //  std::vector<std::vector<bool> > gridMask(gridDim);
//  int idxDomain = 0, idxAxis = 0;
//  for (int i = 0; i < axisDomainOrder.size(); ++i)
//  {
//    if (axisDomainOrder(i))
//    {
//      gridMask[idxDomain*2+i].resize(domList[idxDomain]->ni);
//      gridMask[idxDomain*2+i+1].resize(domList[idxDomain]->nj);
//      ++idxDomain;
//    }
//    else
//    {
//      gridMask[i].resize(axisList[idxAxis]->ni);
//      ++idxAxis;
//    }
//  }

  readDistributionInfo(domList, axisList, axisDomainOrder, gridMask);
}

/*!
  Read information from domain(s) and axis to generate distribution.
  All information related to domain, e.g ibegin, jbegin, ni, nj, ni_glo, nj_glo
as well as related to axis, e.g dataNIndex, dataIndex will be stored to compute
the distribution between clients and servers. Till now, every data structure of domain has been kept
like before, e.g: data_n_index to make sure a compability, however, it should be changed?
  \param [in] domList List of domains of grid
  \param [in] axisList List of axis of grid
  \param [in] axisDomainOrder order of axis and domain inside a grid. True if domain, false if axis
  \param [in] gridMask Mask of grid, for now, keep it 3 dimension, but it needs changing
*/
void CDistributionClient::readDistributionInfo(const std::vector<CDomain*>& domList,
                                               const std::vector<CAxis*>& axisList,
                                               const CArray<bool,1>& axisDomainOrder,
                                               const CArray<bool,3>& gridMask)
{
  numElement_ = axisDomainOrder.numElements(); // Number of element, e.x: Axis, Domain

  axisDomainOrder_.resize(numElement_);
  axisDomainOrder_ = axisDomainOrder;

  // Each domain or axis has its mask, of course
  domainMasks_.resize(domList.size());
  for (int i = 0; i < domainMasks_.size();++i)
  {
    domainMasks_[i].resize(domList[i]->mask.extent(0), domList[i]->mask.extent(1));
    domainMasks_[i] = domList[i]->mask;
  }

  axisMasks_.resize(axisList.size());
  for (int i = 0; i < axisMasks_.size(); ++i)
  {
    axisMasks_[i].resize(axisList[i]->mask.numElements());
    axisMasks_[i] = axisList[i]->mask;
  }

  gridMask_.resize(gridMask.extent(0), gridMask.extent(1), gridMask.extent(2));
  gridMask_ = gridMask;

  // Because domain and axis can be in any order (axis1, domain1, axis2, axis3, )
  // their position should be specified. In axisDomainOrder, domain == true, axis == false
  int idx = 0;
  indexMap_.resize(numElement_);
  this->dims_ = numElement_;
  for (int i = 0; i < numElement_; ++i)
  {
    indexMap_[i] = idx;
    if (true == axisDomainOrder(i))
    {
      ++(this->dims_);
      idx += 2;
    }
  }

  // Size of each dimension (local and global)
  nLocal_.resize(this->dims_);
  nGlob_.resize(this->dims_);
  nBeginLocal_.resize(this->dims_,0);
  nBeginGlobal_.resize(this->dims_,0);
  nZoomBegin_.resize(this->dims_);
  nZoomEnd_.resize(this->dims_);

  // Data_n_index of domain or axis (For now, axis uses its size as data_n_index
  dataNIndex_.resize(numElement_);
  dataDims_.resize(numElement_);
  dataBegin_.resize(this->dims_);

  // Data_*_index of each dimension
  dataIndex_.resize(this->dims_);

  // A trick to determine position of each domain in domainList
  int domIndex = 0, axisIndex = 0;
  idx = 0;

  // Update all the vectors above
  while (idx < numElement_)
  {
    bool isDomain = axisDomainOrder(idx);

    // If this is a domain
    if (isDomain)
    {
      // On the j axis
      nLocal_.at(indexMap_[idx]+1) = domList[domIndex]->nj.getValue();
      nGlob_.at(indexMap_[idx]+1)  = domList[domIndex]->nj_glo.getValue();
      nBeginLocal_.at(indexMap_[idx]+1) = 0;
      nBeginGlobal_.at(indexMap_[idx]+1) = domList[domIndex]->jbegin;
      nZoomBegin_.at((indexMap_[idx]+1)) = domList[domIndex]->zoom_jbegin;
      nZoomEnd_.at((indexMap_[idx]+1))   = domList[domIndex]->zoom_jbegin + domList[domIndex]->zoom_nj-1;

      dataBegin_.at(indexMap_[idx]+1) = (2 == domList[domIndex]->data_dim) ? domList[domIndex]->data_jbegin.getValue() : -1;
      dataIndex_.at(indexMap_[idx]+1).resize(domList[domIndex]->data_j_index.numElements());
      dataIndex_.at(indexMap_[idx]+1) = domList[domIndex]->data_j_index;

      // On the i axis
      nLocal_.at(indexMap_[idx]) = domList[domIndex]->ni.getValue();
      nGlob_.at(indexMap_[idx]) = domList[domIndex]->ni_glo.getValue();
      nBeginLocal_.at(indexMap_[idx]) = 0;
      nBeginGlobal_.at(indexMap_[idx]) = domList[domIndex]->ibegin;
      nZoomBegin_.at((indexMap_[idx])) = domList[domIndex]->zoom_ibegin;
      nZoomEnd_.at((indexMap_[idx]))   = domList[domIndex]->zoom_ibegin + domList[domIndex]->zoom_ni-1;

      dataBegin_.at(indexMap_[idx]) = domList[domIndex]->data_ibegin.getValue();
      dataIndex_.at(indexMap_[idx]).resize(domList[domIndex]->data_i_index.numElements());
      dataIndex_.at(indexMap_[idx]) = domList[domIndex]->data_i_index;

      dataNIndex_.at(idx) = domList[domIndex]->data_n_index.getValue();
      dataDims_.at(idx) = domList[domIndex]->data_dim.getValue();
      ++domIndex;
    }
    else // So it's an axis
    {
      nLocal_.at(indexMap_[idx]) = axisList[axisIndex]->zoom_size.getValue();
      nGlob_.at(indexMap_[idx]) = axisList[axisIndex]->size.getValue();
      nBeginLocal_.at(indexMap_[idx]) = axisList[axisIndex]->zoom_begin.getValue(); //ibegin.getValue();
      nBeginGlobal_.at(indexMap_[idx]) = axisList[axisIndex]->ibegin.getValue();
      nZoomBegin_.at((indexMap_[idx])) = axisList[axisIndex]->zoom_begin;
      nZoomEnd_.at((indexMap_[idx])) = axisList[axisIndex]->zoom_begin + axisList[axisIndex]->zoom_size-1;

      dataBegin_.at(indexMap_[idx]) = axisList[axisIndex]->data_begin.getValue();
      dataIndex_.at(indexMap_[idx]).resize(axisList[axisIndex]->data_index.numElements());
      dataIndex_.at(indexMap_[idx]) = axisList[axisIndex]->data_index;
      dataNIndex_.at(idx) = axisList[axisIndex]->data_index.numElements();
      dataDims_.at(idx) = 1;
      ++axisIndex;
    }
    ++idx;
  }
}

/*!
  Create local index of domain(s).
  A domain can have data index which even contains the "ghost" points. Very often, these
data surround the true data. In order to send correct data to server,
a client need to know index of the true data.
*/
void CDistributionClient::createLocalDomainDataIndex()
{
  int numDomain = 0;
  for (int i = 0; i < axisDomainOrder_.numElements(); ++i)
    if (axisDomainOrder_(i)) ++numDomain;

  localDomainIndex_.resize(numDomain*2);
  indexDomainData_.resize(numDomain);

  int idxDomain = 0;
  for (int i = 0; i < axisDomainOrder_.numElements(); ++i)
  {
    if (axisDomainOrder_(i))
    {
      int iIdx, jIdx = 0, count = 0;
      indexDomainData_[idxDomain].resize(dataNIndex_[i], false);
      for (int j = 0; j < dataNIndex_[i]; ++j)
      {
        iIdx = getDomainIndex(dataIndex_[indexMap_[i]](j), dataIndex_[indexMap_[i]+1](j),
                              dataBegin_[indexMap_[i]], dataBegin_[indexMap_[i]+1],
                              dataDims_[i], nLocal_[indexMap_[i]], jIdx);

        if ((iIdx >= nBeginLocal_[indexMap_[i]]) && (iIdx < nLocal_[indexMap_[i]]) &&
           (jIdx >= nBeginLocal_[indexMap_[i]+1]) && (jIdx < nLocal_[indexMap_[i]+1]) &&
           (domainMasks_[idxDomain](iIdx, jIdx)))
        {
          (localDomainIndex_[idxDomain]).push_back(iIdx);
          (localDomainIndex_[idxDomain*2+1]).push_back(jIdx);
          indexDomainData_[idxDomain][j] = true;
        }
      }
      ++idxDomain;
    }
  }
}

/*!
  Create local index of axis.
*/
void CDistributionClient::createLocalAxisDataIndex()
{
  int numAxis = 0;
  for (int i = 0; i < axisDomainOrder_.numElements(); ++i)
    if (!axisDomainOrder_(i)) ++numAxis;

  localAxisIndex_.resize(numAxis);

  int idxAxis = 0;
  for (int i = 0; i < axisDomainOrder_.numElements(); ++i)
  {
    if (!axisDomainOrder_(i))
    {
      int iIdx = 0;
      for (int j = 0; j < dataNIndex_[i]; ++j)
      {
        iIdx = getAxisIndex(dataIndex_[indexMap_[i]](j), dataBegin_[indexMap_[i]], nLocal_[indexMap_[i]]);
        if ((iIdx >= nBeginLocal_[indexMap_[i]]) &&
           (iIdx < nLocal_[indexMap_[i]]) && (axisMasks_[idxAxis](iIdx)))
        {
          localAxisIndex_[idxAxis].push_back(iIdx);
        }
      }
      ++idxAxis;
    }
  }
}

/*!
   Create global index on client
   In order to do the mapping between client-server, each client creates its own
global index of sending data. This global index is then used to calculate to which server
the client needs to send it data as well as which part of data belongs to the server.
So as to make clients and server coherent in order of index, global index is calculated by
take into account of C-convention, the rightmost dimension varies faster.
*/
void CDistributionClient::createGlobalIndex()
{
  createLocalDomainDataIndex();
  createLocalAxisDataIndex();

  int idxDomain = 0, idxAxis = 0;
  std::vector<int> eachElementSize(numElement_);

  // Precompute size of the loop
  for (int i = 0; i < numElement_; ++i)
  {
    if(axisDomainOrder_(i))
    {
      eachElementSize[i] = localDomainIndex_[idxDomain].size();
      idxDomain += 2;
    }
    else
    {
      eachElementSize[i] = localAxisIndex_[idxAxis].size();
      ++idxAxis;
    }
  }

  //   Compute size of the global index on client
  std::vector<int> idxLoop(numElement_,0);
  std::vector<int> currentIndex(this->dims_);
  int innerLoopSize = eachElementSize[0];
  size_t idx = 0, indexLocalDataOnClientCount = 0, indexSend2ServerCount = 0;
  size_t ssize = 1;
  for (int i = 0; i < numElement_; ++i) ssize *= eachElementSize[i];
  while (idx < ssize)
  {
    for (int i = 0; i < numElement_-1; ++i)
    {
      if (idxLoop[i] == eachElementSize[i])
      {
        idxLoop[i] = 0;
        ++idxLoop[i+1];
      }
    }

    // Outer index
    idxDomain = idxAxis = 0;
    for (int i = 1; i < numElement_; ++i)
    {
      if (axisDomainOrder_(i))
      {
        currentIndex[indexMap_[i]]   = localDomainIndex_[idxDomain][idxLoop[i]];
        currentIndex[indexMap_[i]+1] = localDomainIndex_[idxDomain+1][idxLoop[i]];
        idxDomain += 2;
      }
      else
      {
        currentIndex[indexMap_[i]]   = localAxisIndex_[idxAxis][idxLoop[i]];
        ++idxAxis;
      }
    }

    // Inner most index
    idxDomain = idxAxis = 0;
    for (int i = 0; i < innerLoopSize; ++i)
    {
      if (axisDomainOrder_(0))
      {
        currentIndex[0] = localDomainIndex_[idxDomain][i];
        currentIndex[1] = localDomainIndex_[idxDomain+1][i];
      }
      else currentIndex[0]   = localAxisIndex_[idxAxis][i];

      if (gridMask_(currentIndex[0], currentIndex[1], currentIndex[2]))
      {
        ++indexLocalDataOnClientCount;
        bool isIndexOnServer = true;
        for (int j = 0; j < this->dims_; ++j)
          isIndexOnServer = isIndexOnServer && ((currentIndex[j]+nBeginGlobal_[j]) <= nZoomEnd_[j])
                                            && (nZoomBegin_[j] <= (currentIndex[j]+nBeginGlobal_[j]));
        if (isIndexOnServer) ++indexSend2ServerCount;
      }

    }
    idxLoop[0] += innerLoopSize;
    idx += innerLoopSize;
  }

  // Fill in the global index
  this->globalIndex_ = new CArray<size_t,1>(indexSend2ServerCount);
  localDataIndex_ = new CArray<int,1>(indexLocalDataOnClientCount);

  eachElementSize = dataNIndex_;
  innerLoopSize = eachElementSize[0];
  ssize = 1; for (int i = 0; i < numElement_; ++i) ssize *= eachElementSize[i];
  idxLoop.assign(numElement_,0);
  idx = indexLocalDataOnClientCount = indexSend2ServerCount = 0;
  int count = 0;
  while (idx < ssize)
  {
    for (int i = 0; i < numElement_-1; ++i)
    {
      if (idxLoop[i] == eachElementSize[i])
      {
        idxLoop[i] = 0;
        ++idxLoop[i+1];
      }
    }

    // Outer index
    idxDomain = idxAxis = 0;
    bool isIndexDataCorrect = false;
    for (int i = 1; i < numElement_; ++i)
    {
      if (axisDomainOrder_(i))
      {
        if (indexDomainData_[idxDomain][idxLoop[i]])
        {
          currentIndex[indexMap_[i]]   = localDomainIndex_[idxDomain][idxLoop[i]];
          currentIndex[indexMap_[i]+1] = localDomainIndex_[idxDomain*2+1][idxLoop[i]];
          isIndexDataCorrect = true;
        }
        ++idxDomain;
      }
      else
      {
        currentIndex[indexMap_[i]]   = localAxisIndex_[idxAxis][idxLoop[i]];
        ++idxAxis;
      }
    }

    // Inner most index
    idxDomain = idxAxis = 0;
    int correctIndexDomain = 0;
    for (int i = 0; i < innerLoopSize; ++i)
    {
      if (axisDomainOrder_(0))
      {
        if (indexDomainData_[idxDomain][i])
        {
          currentIndex[0] = localDomainIndex_[idxDomain][correctIndexDomain];
          currentIndex[1] = localDomainIndex_[idxDomain+1][correctIndexDomain];
          isIndexDataCorrect = true;
          ++correctIndexDomain;
        } else isIndexDataCorrect = false;
      }
      else
      {
        currentIndex[0]   = localAxisIndex_[idxAxis][i];
      }

      if (isIndexDataCorrect && gridMask_(currentIndex[0], currentIndex[1], currentIndex[2]))
      {
        (*localDataIndex_)(indexLocalDataOnClientCount) = count;
        ++indexLocalDataOnClientCount;

        bool isIndexOnServer = true;
        for (int j = 0; j < this->dims_; ++j)
          isIndexOnServer = isIndexOnServer && ((currentIndex[j]+nBeginGlobal_[j]) <= nZoomEnd_[j])
                                            && (nZoomBegin_[j] <= (currentIndex[j]+nBeginGlobal_[j]));
        if (isIndexOnServer)
        {
          size_t mulDim = 1;
          size_t globalIndex = currentIndex[0] + nBeginGlobal_[0];
          for (int k = 1; k < this->dims_; ++k)
          {
            mulDim *= nGlob_[k-1];
            globalIndex += (currentIndex[k] + nBeginGlobal_[k])*mulDim;
          }
          (*this->globalIndex_)(indexSend2ServerCount) = globalIndex;
          ++indexSend2ServerCount;
        }
      }
      ++count;
    }
    idxLoop[0] += innerLoopSize;
    idx += innerLoopSize;
  }

//  std::cout << "global index " << *this->globalIndex_ << std::endl;
//  std::cout << "local index " << *localDataIndex_ << std::endl;
}

/*!
  Retrieve index i and index j of a domain from its data index
  Data contains not only true data, which are sent to servers, but also ghost data, which
very often play a role of border of each local data, so does data index. Because data of a domain
can be one dimension, or two dimensions, there is a need to convert data index to domain index
  \param [in] dataIIndex index of i data
  \param [in] dataJIndex index of j data
  \param [in] dataIBegin index begin of i data
  \param [in] dataJBegin index begin of j data
  \param [in] dataDim dimension of data (1 or 2)
  \param [in] ni local size ni of domain
  \param [out] j j index of domain
  \return i index of domain
*/
int CDistributionClient::getDomainIndex(const int& dataIIndex, const int& dataJIndex,
                                        const int& dataIBegin, const int& dataJBegin,
                                        const int& dataDim, const int& ni, int& j)
{
  int tempI = dataIIndex + dataIBegin,
      tempJ = (1 == dataDim) ? -1
                             : (dataJIndex + dataJBegin);
  int i = (dataDim == 1) ? (tempI - 1) % ni
                     : (tempI - 1) ;
  j = (dataDim == 1) ? (tempI - 1) / ni
                     : (tempJ - 1) ;

  return i;
}

/*!
  Retrieve index of an axis from its data index
  \param [in] dataIndex index of data
  \param [in] dataBegin index begin of data
  \param [in] ni local size of axis
  \return index of domain
*/
int CDistributionClient::getAxisIndex(const int& dataIndex, const int& dataBegin, const int& ni)
{
   int tempI = dataIndex + dataBegin;
   return ((tempI-1)%ni);
//   return ((tempI)%ni);
}

/*!
  Compute global index of each server distributed by band
  The classic distribution of servers: each server takes charges of writing data divided
into blocks on the second dimension of grid. If the grid contain a domain, this second dimension is nj.
  \param [in] nServer number of server
  \return vector of pointer to array of global index of servers
*/
std::vector<CArray<size_t,1>* > CDistributionClient::computeServerBandDistribution(int nServer)
{
  // It's not intelligent to allocate dynamic memory inside one function and dellocate in another
  // but it's a way to free a large amount of unnecessary memory
  // This function must NEVER made into public.
  size_t ssize = 1, idx = 0;
  for (int i = 0; i < nGlob_.size(); ++i) ssize *= nGlob_[i];
  std::vector<int> idxLoop(this->dims_,0);
  std::vector<int> indexServer(nServer,0);
  int njRangeSize;
  std::vector<int> njRangeBegin(nServer,0);
  std::vector<int> njRangeEnd(nServer,0);
  std::vector<CArray<size_t,1>* > globalIndexServer(nServer);

  int innerLoopSize = nGlob_[0], idxServer;
  if (1<nGlob_.size())
  {
    for (int i = 0; i < nServer; ++i)
    {
      if (0 < i) njRangeBegin[i] = njRangeEnd[i-1];
      njRangeSize = nGlob_[1] / nServer;
      if (i < nGlob_[1]%nServer) ++njRangeSize;
      njRangeEnd[i] = njRangeSize + njRangeBegin[i];
    }
    njRangeEnd[nServer-1] = nGlob_[1];

    // Compute size of each global index server array
    while (idx < ssize)
    {
      for (int i = 0; i < this->dims_-1; ++i)
      {
        if (idxLoop[i] == nGlob_[i])
        {
          idxLoop[i] = 0;
          ++idxLoop[i+1];
        }
      }

      for (int i = 0; i < nServer; ++i)
        if ((njRangeBegin[i]<=idxLoop[1]) && (idxLoop[1] < njRangeEnd[i]))
        {
          idxServer = i;
          break;
        }

      indexServer[idxServer] += innerLoopSize;
      idxLoop[0] += innerLoopSize;
      idx += innerLoopSize;
    }


    for (int i = 0; i < nServer; ++i) globalIndexServer[i] = new CArray<size_t,1>(indexServer[i]);

    // Fill in each global index server array
    idx = 0;
    idxLoop.assign(this->dims_,0);
    indexServer.assign(nServer,0);
    size_t globalIndex = 0;
    while (idx < ssize)
    {
      for (int i = 0; i < this->dims_-1; ++i)
      {
        if (idxLoop[i] == nGlob_[i])
        {
          idxLoop[i] = 0;
          ++idxLoop[i+1];
        }
      }

      for (int i = 0; i < nServer; ++i)
        if ((njRangeBegin[i]<=idxLoop[1]) && (idxLoop[1] < njRangeEnd[i]))
        {
          idxServer = i;
          break;
        }

      for (int i = 0; i < innerLoopSize; ++i)
      {
        (*globalIndexServer[idxServer])(indexServer[idxServer]) = globalIndex;
        ++indexServer[idxServer];
        ++globalIndex;
      }
      idxLoop[0] += innerLoopSize;
      idx += innerLoopSize;
    }
  }

  return globalIndexServer;
}

/*!
  Compute index mapping between cliens and servers
  On using global index of data on clients and servers, each client calculates which part
of data will be sent to the corresponding server. After the functions is called, client can use
all computed information to send correct data to server
  \param [in] nServer number of server
  \param [in] distributionType type of distribution, like band or plan
*/
void CDistributionClient::computeServerIndexMapping(int nServer, ServerDistributionType distributionType)
{
  std::vector<CArray<size_t,1>* > globalIndexServer;

  switch (distributionType)
  {
    case BAND_DISTRIBUTION:
      globalIndexServer = computeServerBandDistribution(nServer);
      break;
    default:
      break;
  }

  std::vector<CArray<size_t,1>::const_iterator> itBegin(nServer), itEnd(nServer), it(nServer);
  for (int i = 0; i < nServer; ++i)
  {
    itBegin[i] = it[i] = globalIndexServer[i]->begin();
    itEnd[i]   = globalIndexServer[i]->end();
  }

  size_t ssize = (this->globalIndex_)->numElements();
  for (int i = 0; i < ssize; ++i)
  {
    for (int j = 0; j < nServer; ++j)
    {
      // Just temporarily, it's bad.

      if (std::binary_search(itBegin[j], itEnd[j], (*this->globalIndex_)(i)))
      {
        // Just try to calculate local index server on client side
        (indexGlobalOnServer_[j]).push_back((*this->globalIndex_)(i));
        (localIndexSend2Server_[j]).push_back(i);
        continue;
      }
    }
  }

  for (int i = 0; i < nServer; ++i)
    if (0 != globalIndexServer[i]) delete globalIndexServer[i];
}

/*!
  Compute how many clients each server will receive data from
  On client can send data to several servers as well as one server can receive data originated from
some clients. In order to write data correctly, each server must know from how many clients it receives data
  \param [in] nbServer number of servers
  \param [in] nClient number of clients
  \param [in] clientIntraComm MPI communication of clients
  \return mapping of server rank and the number of connected clients
*/
std::map<int,int> CDistributionClient::computeConnectedClients(int nbServer, int nbClient, MPI_Comm& clientIntraComm)
{
  if (isConnectedServerComputed_) return connectedClients_;
  std::map<int, std::vector<size_t> >::const_iterator itbMap, iteMap, it;
  itbMap = it = indexGlobalOnServer_.begin();
  iteMap = indexGlobalOnServer_.end();

  std::vector<int> connectedServer;
  std::vector<bool> isConnected(nbServer,false);

  for (it = itbMap; it != iteMap; ++it)
  {
    for (int serverNum = 0; serverNum < nbServer; ++serverNum)
      if (it->first == serverNum) isConnected[serverNum] = true;
  }

  for(int serverNum = 0; serverNum<nbServer; ++serverNum)
    if (isConnected[serverNum])
      connectedServer.push_back(serverNum);


  int nbConnectedServer=connectedServer.size();
  int* recvCount=new int[nbClient];
  int* displ=new int[nbClient];
  int* sendBuff=new int[nbConnectedServer];
  valarray<int> clientRes(0,nbServer);

  for(int n=0;n<nbConnectedServer;n++) sendBuff[n]=connectedServer[n] ;

  // get connected server for everybody
  MPI_Allgather(&nbConnectedServer,1,MPI_INT,recvCount,1,MPI_INT,clientIntraComm) ;

  displ[0]=0 ;
  for(int n=1;n<nbClient;n++) displ[n]=displ[n-1]+recvCount[n-1] ;
  int recvSize=displ[nbClient-1]+recvCount[nbClient-1] ;
  int* recvBuff=new int[recvSize] ;


  MPI_Allgatherv(sendBuff,nbConnectedServer,MPI_INT,recvBuff,recvCount,displ,MPI_INT,clientIntraComm) ;
  for(int n=0;n<recvSize;n++) clientRes[recvBuff[n]]++ ;

//  std::map<int,int> nbSenders;
  for(int n=0;n<nbConnectedServer;n++)
  {
    connectedClients_[connectedServer[n]] = clientRes[connectedServer[n]];
  }

  isConnectedServerComputed_ = true;

  delete [] recvCount ;
  delete [] displ ;
  delete [] sendBuff ;
  delete [] recvBuff ;

  return connectedClients_;
}

/*!
  Return local index of data that is send to server
  \return mapping of server rank and local index of sending data on the client
*/
const std::map<int, std::vector<int> >& CDistributionClient::getLocalIndexSendToServer() const
{
  return localIndexSend2Server_;
}

/*!
  Return local data index of client
*/
const CArray<int,1>& CDistributionClient::getLocalDataIndexOnClient() const
{
  return (*localDataIndex_);
}

/*!
  Return global index of data on each connected server.
  On receiving data sent from client(s), each server with this global index, is able to
know where the data should be written.
  \return mapping of server rank and its global index.
*/
const std::map<int, std::vector<size_t> >& CDistributionClient::getGlobalIndexOnServer() const
{
  return indexGlobalOnServer_;
}

} // namespace xios
