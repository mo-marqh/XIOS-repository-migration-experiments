/*!
   \file distribution_client.cpp
   \author Ha NGUYEN
   \since 13 Jan 2015
   \date 09 Mars 2015

   \brief Index distribution on client side.
 */
#include "distribution_client.hpp"

namespace xios {

CDistributionClient::CDistributionClient(int rank, int dims, const CArray<size_t,1>& globalIndex)
   : CDistribution(rank, dims, globalIndex)
   , axisDomainOrder_()
   , nLocal_(), nGlob_(), nBeginLocal_(), nBeginGlobal_(),nZoomBegin_(), nZoomEnd_()
   , dataNIndex_(), dataDims_(), dataBegin_(), dataIndex_(), domainMasks_(), axisMasks_()
   , gridMask_(), indexMap_()
   , isDataDistributed_(true), axisNum_(0), domainNum_(0)
   , localDataIndex_(), localMaskIndex_()
   , globalLocalDataSendToServerMap_()
   , infoIndex_(), isComputed_(false)
   , elementLocalIndex_(), elementGlobalIndex_(), elementIndexData_()
   , elementZoomMask_(), elementNLocal_(), elementNGlobal_()
{
}

CDistributionClient::CDistributionClient(int rank, CGrid* grid)
   : CDistribution(rank, 0)
   , axisDomainOrder_()
   , nLocal_(), nGlob_(), nBeginLocal_(), nBeginGlobal_(),nZoomBegin_(), nZoomEnd_()
   , dataNIndex_(), dataDims_(), dataBegin_(), dataIndex_(), domainMasks_(), axisMasks_()
   , gridMask_(), indexMap_()
   , isDataDistributed_(true), axisNum_(0), domainNum_(0)
   , localDataIndex_(), localMaskIndex_()
   , globalLocalDataSendToServerMap_()
   , infoIndex_(), isComputed_(false)
   , elementLocalIndex_(), elementGlobalIndex_(), elementIndexData_()
   , elementZoomMask_(), elementNLocal_(), elementNGlobal_()
{
  readDistributionInfo(grid);
  createGlobalIndex();
}

CDistributionClient::~CDistributionClient()
{ /* Nothing to do */ }

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
  CArray<bool,1> axisDomainOrder = grid->axis_domain_order;

  std::vector<CDomain*>::iterator itbDom, iteDom, itDom;
  std::vector<CAxis*>::iterator itbAxis, iteAxis, itAxis;

  itbDom  = itDom  = domList.begin();  iteDom  = domList.end();
  itbAxis = itAxis = axisList.begin(); iteAxis = axisList.end();

  readDistributionInfo(domList, axisList, axisDomainOrder);

  // Then check mask of grid
  int gridDim = domList.size() * 2 + axisList.size();
  grid->checkMask();
  switch (gridDim) {
    case 1:
      readGridMaskInfo(grid->mask_1d);
      break;
    case 2:
      readGridMaskInfo(grid->mask_2d);
      break;
    case 3:
      readGridMaskInfo(grid->mask_3d);
      break;
    default:
      break;
  }
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
//  \param [in] gridMask Mask of grid, for now, keep it 3 dimension, but it needs changing
*/
void CDistributionClient::readDistributionInfo(const std::vector<CDomain*>& domList,
                                               const std::vector<CAxis*>& axisList,
                                               const CArray<bool,1>& axisDomainOrder)
{
  domainNum_ = domList.size();
  axisNum_   = axisList.size();
  numElement_ = axisDomainOrder.numElements(); // Number of element, e.x: Axis, Domain

  axisDomainOrder_.resize(numElement_);
  axisDomainOrder_ = axisDomainOrder;

  // Each domain or axis has its mask, of course
  domainMasks_.resize(domainNum_);
  for (int i = 0; i < domainNum_;++i)
  {
    domainMasks_[i].resize(domList[i]->mask_1d.numElements());
    domainMasks_[i] = domList[i]->mask_1d;
  }

  axisMasks_.resize(axisNum_);
  for (int i = 0; i < axisNum_; ++i)
  {
    axisMasks_[i].resize(axisList[i]->mask.numElements());
    axisMasks_[i] = axisList[i]->mask;
  }

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
    else ++idx;
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
  infoIndex_.resize(this->dims_);

  // A trick to determine position of each domain in domainList
  int domIndex = 0, axisIndex = 0;
  idx = 0;

  elementLocalIndex_.resize(numElement_);
  elementGlobalIndex_.resize(numElement_);
  elementIndexData_.resize(numElement_);
  elementZoomMask_.resize(numElement_);
  elementNLocal_.resize(numElement_);
  elementNGlobal_.resize(numElement_);
  elementNLocal_[0] = 1;
  elementNGlobal_[0] = 1;
  size_t localSize = 1, globalSize = 1;

  isDataDistributed_ = false;
  // Update all the vectors above
  for (idx = 0; idx < numElement_; ++idx)
  {
    bool isDomain = axisDomainOrder(idx);
    elementNLocal_[idx] = localSize;
    elementNGlobal_[idx] = globalSize;

    // If this is a domain
    if (isDomain)
    {
      // On the j axis
      nLocal_.at(indexMap_[idx]+1) = domList[domIndex]->nj.getValue();
      nGlob_.at(indexMap_[idx]+1)  = domList[domIndex]->nj_glo.getValue();
      nBeginLocal_.at(indexMap_[idx]+1) = 0;
      nBeginGlobal_.at(indexMap_[idx]+1) = domList[domIndex]->jbegin;
      nZoomBegin_.at((indexMap_[idx]+1)) = domList[domIndex]->global_zoom_jbegin;
      nZoomEnd_.at((indexMap_[idx]+1))   = domList[domIndex]->global_zoom_jbegin + domList[domIndex]->global_zoom_nj-1;

      dataBegin_.at(indexMap_[idx]+1) = domList[domIndex]->data_jbegin.getValue(); //(2 == domList[domIndex]->data_dim) ? domList[domIndex]->data_jbegin.getValue() : -1;
      dataIndex_.at(indexMap_[idx]+1) = &(domList[domIndex]->data_j_index);
      infoIndex_.at(indexMap_[idx]+1) = &(domList[domIndex]->j_index);

      // On the i axis
      nLocal_.at(indexMap_[idx]) = domList[domIndex]->ni.getValue();
      nGlob_.at(indexMap_[idx]) = domList[domIndex]->ni_glo.getValue();
      nBeginLocal_.at(indexMap_[idx]) = 0;
      nBeginGlobal_.at(indexMap_[idx]) = domList[domIndex]->ibegin;
      nZoomBegin_.at((indexMap_[idx])) = domList[domIndex]->global_zoom_ibegin;
      nZoomEnd_.at((indexMap_[idx]))   = domList[domIndex]->global_zoom_ibegin + domList[domIndex]->global_zoom_ni-1;

      dataBegin_.at(indexMap_[idx]) = domList[domIndex]->data_ibegin.getValue();
      dataIndex_.at(indexMap_[idx]) = &(domList[domIndex]->data_i_index);
      infoIndex_.at(indexMap_[idx]) = &(domList[domIndex]->i_index);

      dataNIndex_.at(idx) = domList[domIndex]->data_i_index.numElements();
      dataDims_.at(idx) = domList[domIndex]->data_dim.getValue();

      isDataDistributed_ |= domList[domIndex]->isDistributed();

      localSize *= nLocal_.at(indexMap_[idx]+1)* nLocal_.at(indexMap_[idx]);
      globalSize *= nGlob_.at(indexMap_[idx]+1)* nGlob_.at(indexMap_[idx]);
      ++domIndex;
    }
    else // So it's an axis
    {
      nLocal_.at(indexMap_[idx]) = axisList[axisIndex]->n.getValue();
      nGlob_.at(indexMap_[idx]) = axisList[axisIndex]->n_glo.getValue();
      nBeginLocal_.at(indexMap_[idx]) = 0;
      nBeginGlobal_.at(indexMap_[idx]) = axisList[axisIndex]->begin.getValue();
      nZoomBegin_.at((indexMap_[idx])) = axisList[axisIndex]->global_zoom_begin;
      nZoomEnd_.at((indexMap_[idx])) = axisList[axisIndex]->global_zoom_begin + axisList[axisIndex]->global_zoom_n-1;

      dataBegin_.at(indexMap_[idx]) = axisList[axisIndex]->data_begin.getValue();
      dataIndex_.at(indexMap_[idx]) = &(axisList[axisIndex]->data_index);
      infoIndex_.at(indexMap_[idx]) = &(axisList[axisIndex]->index);
      dataNIndex_.at(idx) = axisList[axisIndex]->data_index.numElements();
      dataDims_.at(idx) = 1;

      isDataDistributed_ |= axisList[axisIndex]->isDistributed();

      localSize *= nLocal_.at(indexMap_[idx]);
      globalSize *= nGlob_.at(indexMap_[idx]);

      ++axisIndex;
    }
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
  int idxDomain = 0;
  for (int i = 0; i < axisDomainOrder_.numElements(); ++i)
  {
    if (axisDomainOrder_(i))
    {
      elementIndexData_[i].resize(dataNIndex_[i]);
      elementIndexData_[i] = false;
      int iIdx, jIdx = 0, count = 0, localIndex;
      for (int j = 0; j < dataNIndex_[i]; ++j)
      {
        iIdx = getDomainIndex((*dataIndex_[indexMap_[i]])(j), (*dataIndex_[indexMap_[i]+1])(j),
                              dataBegin_[indexMap_[i]], dataBegin_[indexMap_[i]+1],
                              dataDims_[i], nLocal_[indexMap_[i]], jIdx);

        if ((iIdx >= nBeginLocal_[indexMap_[i]]) && (iIdx < nLocal_[indexMap_[i]]) &&
           (jIdx >= nBeginLocal_[indexMap_[i]+1]) && (jIdx < nLocal_[indexMap_[i]+1]) &&
           (domainMasks_[idxDomain](iIdx + jIdx*nLocal_[indexMap_[i]])))
        {
          ++count;
          elementIndexData_[i](j) = true;
        }
      }

      elementLocalIndex_[i].resize(count);
      elementGlobalIndex_[i].resize(count);
      elementZoomMask_[i].resize(count);
      elementZoomMask_[i] = false;
      count = 0;
      CArray<bool,1>& tmpIndexElementData = elementIndexData_[i];
      CArray<bool,1>& tmpZoomMaskElement = elementZoomMask_[i];
      CArray<int,1>& tmpLocalElementIndex = elementLocalIndex_[i];
      CArray<size_t,1>& tmpGlobalElementIndex = elementGlobalIndex_[i];
      for (int j = 0; j < dataNIndex_[i]; ++j)
      {
        if (tmpIndexElementData(j))
        {
          iIdx = getDomainIndex((*dataIndex_[indexMap_[i]])(j), (*dataIndex_[indexMap_[i]+1])(j),
                                dataBegin_[indexMap_[i]], dataBegin_[indexMap_[i]+1],
                                dataDims_[i], nLocal_[indexMap_[i]], jIdx);
          localIndex = tmpLocalElementIndex(count) = iIdx + jIdx * nLocal_[indexMap_[i]];
          tmpGlobalElementIndex(count) = (*infoIndex_[indexMap_[i]])(localIndex) + ((*infoIndex_[indexMap_[i]+1])(localIndex))*nGlob_[indexMap_[i]];
          if ((((*infoIndex_[indexMap_[i]])(localIndex)) <= nZoomEnd_[indexMap_[i]])
             && (nZoomBegin_[indexMap_[i]] <= ((*infoIndex_[indexMap_[i]])(localIndex)))
             && (((*infoIndex_[indexMap_[i]+1])(localIndex)) <= nZoomEnd_[indexMap_[i]+1])
             && (nZoomBegin_[indexMap_[i]+1] <= ((*infoIndex_[indexMap_[i]+1])(localIndex))))
          {
            tmpZoomMaskElement(count) = true;
          }
          ++count;
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
  int idxAxis = 0;
  for (int i = 0; i < axisDomainOrder_.numElements(); ++i)
  {
    if (!axisDomainOrder_(i))
    {
      elementIndexData_[i].resize(dataNIndex_[i]);
      elementIndexData_[i] = false;
      int iIdx = 0, count = 0, localIndex = 0;
      for (int j = 0; j < dataNIndex_[i]; ++j)
      {
        iIdx = getAxisIndex((*dataIndex_[indexMap_[i]])(j), dataBegin_[indexMap_[i]], nLocal_[indexMap_[i]]);
        if ((iIdx >= nBeginLocal_[indexMap_[i]]) &&
           (iIdx < nLocal_[indexMap_[i]]) && (axisMasks_[idxAxis](iIdx)))
        {
          ++count;
          elementIndexData_[i](j) = true;
        }
      }

      elementLocalIndex_[i].resize(count);
      elementGlobalIndex_[i].resize(count);
      elementZoomMask_[i].resize(count);
      elementZoomMask_[i] = false;
      count = 0;
      CArray<bool,1>& tmpIndexElementData = elementIndexData_[i];
      CArray<bool,1>& tmpZoomMaskElement = elementZoomMask_[i];
      CArray<int,1>& tmpLocalElementIndex = elementLocalIndex_[i];
      CArray<size_t,1>& tmpGlobalElementIndex = elementGlobalIndex_[i];
      for (int j = 0; j < dataNIndex_[i]; ++j)
      {
        if (tmpIndexElementData(j))
        {
          iIdx = tmpLocalElementIndex(count) = getAxisIndex((*dataIndex_[indexMap_[i]])(j), dataBegin_[indexMap_[i]], nLocal_[indexMap_[i]]);
          tmpGlobalElementIndex(count) = (*infoIndex_[indexMap_[i]])(iIdx);
          if ((((*infoIndex_[indexMap_[i]])(iIdx)) <= nZoomEnd_[indexMap_[i]])
             && (nZoomBegin_[indexMap_[i]] <= ((*infoIndex_[indexMap_[i]])(iIdx))))
          {
            tmpZoomMaskElement(count) = true;
          }
          ++count;
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
void CDistributionClient::createGlobalIndexSendToServer()
{
  if (isComputed_) return;
  isComputed_ = true;
  createLocalDomainDataIndex();
  createLocalAxisDataIndex();

  int idxDomain = 0, idxAxis = 0;
  std::vector<int> eachElementSize(numElement_);

  // Precompute size of the loop
  for (int i = 0; i < numElement_; ++i)
  {
    eachElementSize[i] = elementLocalIndex_[i].numElements();
  }

  //   Compute size of the global index on client
  std::vector<StdSize> idxLoop(numElement_,0);
  std::vector<StdSize> currentIndex(numElement_,0);
  std::vector<StdSize> currentGlobalIndex(numElement_,0);
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

    // Find out outer index
    // Depending the inner-most element is axis or domain,
    // The outer loop index begins correspondingly at one (1) or zero (0)
    for (int i = 1; i < numElement_; ++i)
    {
      currentIndex[i] = elementLocalIndex_[i](idxLoop[i]);
    }

    // Inner most index
    for (int i = 0; i < innerLoopSize; ++i)
    {
      int gridMaskIndex = 0;
      for (int k = 0; k < this->numElement_; ++k)
      {
        gridMaskIndex += (currentIndex[k])*elementNLocal_[k];
      }

      if (gridMask_(gridMaskIndex))
      {
        ++indexLocalDataOnClientCount;
        bool isIndexOnServer = true;

        for (int idxElement = 0; idxElement < this->numElement_; ++idxElement)
        {
          isIndexOnServer = isIndexOnServer && elementZoomMask_[idxElement](idxLoop[idxElement]);
        }
        if (isIndexOnServer) ++indexSend2ServerCount;
      }
    }
    idxLoop[0] += innerLoopSize;
    idx += innerLoopSize;
  }

  // Now allocate these arrays
  localDataIndex_.resize(indexLocalDataOnClientCount);
  localMaskIndex_.resize(indexSend2ServerCount);
  globalLocalDataSendToServerMap_.rehash(std::ceil(indexSend2ServerCount/globalLocalDataSendToServerMap_.max_load_factor())); //globalLocalDataSendToServerMap_.reserve(indexSend2ServerCount);

  // We need to loop with data index
  idxLoop.assign(numElement_,0);
  idx = indexLocalDataOnClientCount = indexSend2ServerCount = 0;
  ssize = 1; for (int i = 0; i < numElement_; ++i) ssize *= dataNIndex_[i];
  innerLoopSize = dataNIndex_[0];
  int countLocalData = 0;
  std::vector<int> correctOuterIndex(numElement_,0);
  bool isOuterIndexCorrect = true;
  while (idx < ssize)
  {
    for (int i = 0; i < numElement_-1; ++i)
    {
      if (idxLoop[i] == dataNIndex_[i])
      {
        idxLoop[i] = 0;
        correctOuterIndex[i] = 0;
        ++idxLoop[i+1];
        if (isOuterIndexCorrect) ++correctOuterIndex[i+1];
      }
    }

    // Depending the inner-most element axis or domain,
    // The outer loop index begins correspondingly at one (1) or zero (0)
    bool isIndexElementDataCorrect = true;
    for (int i = 1; i < numElement_; ++i)
    {
      if (elementIndexData_[i](idxLoop[i]))
      {
        currentIndex[i] = elementLocalIndex_[i](correctOuterIndex[i]);
        currentGlobalIndex[i] = elementGlobalIndex_[i](correctOuterIndex[i]);
        isIndexElementDataCorrect &= true;
      }
      else isIndexElementDataCorrect = false;
    }

    isOuterIndexCorrect = isIndexElementDataCorrect;

    // Inner most index
    int correctIndexElement = 0;
    for (int i = 0; i < innerLoopSize; ++i)
    {
      bool isCurrentIndexDataCorrect = isOuterIndexCorrect;
      if (elementIndexData_[0](i))
      {
        currentIndex[0] = elementLocalIndex_[0](correctIndexElement);
        currentGlobalIndex[0] = elementGlobalIndex_[0](correctIndexElement);
        isCurrentIndexDataCorrect &= true;
        ++correctIndexElement;
      }
      else isCurrentIndexDataCorrect = false;

      if (isCurrentIndexDataCorrect)
      {
        int gridMaskIndex = 0; //currentIndex[0];
        for (int k = 0; k < this->numElement_; ++k)
        {
          gridMaskIndex += (currentIndex[k])*elementNLocal_[k];
        }

        if (gridMask_(gridMaskIndex))
        {
          localDataIndex_[indexLocalDataOnClientCount] = countLocalData;
          bool isIndexOnServer = true;
          for (int idxElement = 0; idxElement < this->numElement_; ++idxElement)
          {
            isIndexOnServer = isIndexOnServer && elementZoomMask_[idxElement](idxLoop[idxElement]);
          }

          if (isIndexOnServer)
          {
            size_t globalIndex = 0; //currentGlobalIndex[0];
            for (int k = 0; k < numElement_; ++k)
            {
              globalIndex += (currentGlobalIndex[k])*elementNGlobal_[k];
            }
            globalLocalDataSendToServerMap_[globalIndex] = indexLocalDataOnClientCount;
            localMaskIndex_[indexSend2ServerCount] = gridMaskIndex;
            ++indexSend2ServerCount;
          }
          ++indexLocalDataOnClientCount;
        }
      }
      ++countLocalData;
    }
    idxLoop[0] += innerLoopSize;
    idx += innerLoopSize;
  }
}

///*!
//  Create local index of domain(s).
//  A domain can have data index which even contains the "ghost" points. Very often, these
//data surround the true data. In order to send correct data to server,
//a client need to know index of the true data.
//*/
//void CDistributionClient::createLocalDomainDataIndex()
//{
//  int numDomain = 0;
//  for (int i = 0; i < axisDomainOrder_.numElements(); ++i)
//    if (axisDomainOrder_(i)) ++numDomain;
//
//  localDomainIndex_.resize(numDomain*2);
//  indexDomainData_.resize(numDomain);
//
//  int idxDomain = 0;
//  for (int i = 0; i < axisDomainOrder_.numElements(); ++i)
//  {
//    if (axisDomainOrder_(i))
//    {
//      int iIdx, jIdx = 0, count = 0;
//      indexDomainData_[idxDomain].resize(dataNIndex_[i], false);
//      for (int j = 0; j < dataNIndex_[i]; ++j)
//      {
//        iIdx = getDomainIndex((*dataIndex_[indexMap_[i]])(j), (*dataIndex_[indexMap_[i]+1])(j),
//                              dataBegin_[indexMap_[i]], dataBegin_[indexMap_[i]+1],
//                              dataDims_[i], nLocal_[indexMap_[i]], jIdx);
//
//        if ((iIdx >= nBeginLocal_[indexMap_[i]]) && (iIdx < nLocal_[indexMap_[i]]) &&
//           (jIdx >= nBeginLocal_[indexMap_[i]+1]) && (jIdx < nLocal_[indexMap_[i]+1]) &&
//           (domainMasks_[idxDomain](iIdx + jIdx*nLocal_[indexMap_[i]])))
//        {
//          (localDomainIndex_[idxDomain]).push_back(iIdx);
//          (localDomainIndex_[idxDomain*2+1]).push_back(jIdx);
//          indexDomainData_[idxDomain][j] = true;
//        }
//      }
//      ++idxDomain;
//    }
//  }
//}

///*!
//  Create local index of axis.
//*/
//void CDistributionClient::createLocalAxisDataIndex()
//{
//  int numAxis = 0;
//  for (int i = 0; i < axisDomainOrder_.numElements(); ++i)
//    if (!axisDomainOrder_(i)) ++numAxis;
//
//  localAxisIndex_.resize(numAxis);
//  indexAxisData_.resize(numAxis);
//
//  int idxAxis = 0;
//  for (int i = 0; i < axisDomainOrder_.numElements(); ++i)
//  {
//    if (!axisDomainOrder_(i))
//    {
//      int iIdx = 0;
//      indexAxisData_[idxAxis].resize(dataNIndex_[i], false);
//      for (int j = 0; j < dataNIndex_[i]; ++j)
//      {
//        iIdx = getAxisIndex((*dataIndex_[indexMap_[i]])(j), dataBegin_[indexMap_[i]], nLocal_[indexMap_[i]]);
//        if ((iIdx >= nBeginLocal_[indexMap_[i]]) &&
//           (iIdx < nLocal_[indexMap_[i]]) && (axisMasks_[idxAxis](iIdx)))
//        {
//          localAxisIndex_[idxAxis].push_back(iIdx);
//          indexAxisData_[idxAxis][j] = true;
//        }
//      }
//      ++idxAxis;
//    }
//  }
//}

void CDistributionClient::createGlobalIndex()
{
  size_t ssize = 1, idx = 0;
  for (int i = 0; i < this->dims_; ++i)
  ssize *= nLocal_[i];

  this->globalIndex_.resize(ssize);
  std::vector<int> idxLoop(this->numElement_,0);
  int innnerLoopSize = (*infoIndex_[0]).numElements();
  while (idx < ssize)
  {
    for (int i = 0; i < this->numElement_; ++i)
    {
      if (idxLoop[i] == (*infoIndex_[indexMap_[i]]).numElements())
      {
        idxLoop[i] = 0;
        ++idxLoop[i+1];
      }
    }

    for (int i = 0; i < innnerLoopSize; ++i)
    {
      size_t globalIndex = (*infoIndex_[0])(idxLoop[0]);
      size_t mulDim = 1;
      for (int idxElement = 0; idxElement < this->numElement_; ++idxElement)
      {
        if (axisDomainOrder_(idxElement))
        {
          int jb = (0 == idxElement) ? 1 : 0;
          for (int j = jb; j <= 1; ++j)
          {
            mulDim *= nGlob_[indexMap_[idxElement]+j-1];
            globalIndex += ((*infoIndex_[indexMap_[idxElement]+j])(idxLoop[idxElement]))*mulDim;
          }
        }
        else
        {
          if (0 != idxElement)
          {
            mulDim *= nGlob_[indexMap_[idxElement]-1];
            globalIndex += ((*infoIndex_[indexMap_[idxElement]])(idxLoop[idxElement]))*mulDim;
          }
        }
      }

      this->globalIndex_(idx) = globalIndex;
      ++idxLoop[0];
      ++idx;
    }
  }

}


///*!
//   Create global index on client
//   In order to do the mapping between client-server, each client creates its own
//global index of sending data. This global index is then used to calculate to which server
//the client needs to send it data as well as which part of data belongs to the server.
//So as to make clients and server coherent in order of index, global index is calculated by
//take into account of C-convention, the rightmost dimension varies faster.
//*/
//void CDistributionClient::createGlobalIndexSendToServer()
//{
//  if (isComputed_) return;
//  isComputed_ = true;
//  createLocalDomainDataIndex();
//  createLocalAxisDataIndex();
//
//  int idxDomain = 0, idxAxis = 0;
//  std::vector<int> eachElementSize(numElement_);
//
//  // Precompute size of the loop
//  for (int i = 0; i < numElement_; ++i)
//  {
//    if(axisDomainOrder_(i))
//    {
//      eachElementSize[i] = localDomainIndex_[idxDomain].size();
//      idxDomain += 2;
//    }
//    else
//    {
//      eachElementSize[i] = localAxisIndex_[idxAxis].size();
//      ++idxAxis;
//    }
//  }
//
//  //   Compute size of the global index on client
//  std::vector<StdSize> idxLoop(numElement_,0);
//  std::vector<StdSize> currentIndex(this->dims_,0);
//  int innerLoopSize = eachElementSize[0];
//  size_t idx = 0, indexLocalDataOnClientCount = 0, indexSend2ServerCount = 0;
//  size_t ssize = 1;
//  for (int i = 0; i < numElement_; ++i) ssize *= eachElementSize[i];
//  while (idx < ssize)
//  {
//    for (int i = 0; i < numElement_-1; ++i)
//    {
//      if (idxLoop[i] == eachElementSize[i])
//      {
//        idxLoop[i] = 0;
//        ++idxLoop[i+1];
//      }
//    }
//
//    // Find out outer index
//    // Depending the inner-most element is axis or domain,
//    // The outer loop index begins correspondingly at one (1) or zero (0)
//    idxDomain = idxAxis = 0;
//    if (axisDomainOrder_(0)) ++idxDomain;
//    else ++idxAxis;
//    for (int i = 1; i < numElement_; ++i)
//    {
//      if (axisDomainOrder_(i))
//      {
//        currentIndex[indexMap_[i]]   = localDomainIndex_[idxDomain][idxLoop[i]];
//        currentIndex[indexMap_[i]+1] = localDomainIndex_[idxDomain+1][idxLoop[i]];
//        idxDomain += 2;
//      }
//      else
//      {
//        currentIndex[indexMap_[i]]   = localAxisIndex_[idxAxis][idxLoop[i]];
//        ++idxAxis;
//      }
//    }
//
//    // Inner most index
//    idxDomain = idxAxis = 0;
//    for (int i = 0; i < innerLoopSize; ++i)
//    {
//      if (axisDomainOrder_(0))
//      {
//        currentIndex[0] = localDomainIndex_[idxDomain][i];
//        currentIndex[1] = localDomainIndex_[idxDomain+1][i];
//      }
//      else currentIndex[0]   = localAxisIndex_[idxAxis][i];
//
//      StdSize gridMaskIndex = currentIndex[0];
//      int mulDimMask = 1;
//      for (int k = 1; k < this->dims_; ++k)
//      {
//        mulDimMask *= nLocal_[k-1];
//        gridMaskIndex += (currentIndex[k])*mulDimMask;
//      }
//
//      if (gridMask_(gridMaskIndex))
//      {
//        ++indexLocalDataOnClientCount;
//        bool isIndexOnServer = true;
//
//        for (int idxElement = 0; idxElement < this->numElement_; ++idxElement)
//        {
//          int actualIdx = 0;
//          if (axisDomainOrder_(idxElement))
//          {
//            actualIdx = currentIndex[indexMap_[idxElement]]+currentIndex[indexMap_[idxElement]+1]*nLocal_[indexMap_[idxElement]];
//            isIndexOnServer = isIndexOnServer && (((*infoIndex_[indexMap_[idxElement]])(actualIdx)) <= nZoomEnd_[indexMap_[idxElement]])
//                                              && (nZoomBegin_[indexMap_[idxElement]] <= ((*infoIndex_[indexMap_[idxElement]])(actualIdx)))
//                                              && (((*infoIndex_[indexMap_[idxElement]+1])(actualIdx)) <= nZoomEnd_[indexMap_[idxElement]+1])
//                                              && (nZoomBegin_[indexMap_[idxElement]+1] <= ((*infoIndex_[indexMap_[idxElement]+1])(actualIdx)));
//          }
//          else
//          {
//            isIndexOnServer = isIndexOnServer && (((*infoIndex_[indexMap_[idxElement]])(currentIndex[indexMap_[idxElement]])) <= nZoomEnd_[indexMap_[idxElement]])
//                                              && (nZoomBegin_[indexMap_[idxElement]] <= ((*infoIndex_[indexMap_[idxElement]])(currentIndex[indexMap_[idxElement]])));
//          }
//        }
//        if (isIndexOnServer) ++indexSend2ServerCount;
//      }
//
//    }
//    idxLoop[0] += innerLoopSize;
//    idx += innerLoopSize;
//  }
//
//  // Now allocate these arrays
//  localDataIndex_.resize(indexLocalDataOnClientCount);
//  localMaskIndex_.resize(indexSend2ServerCount);
//  globalLocalDataSendToServerMap_.rehash(std::ceil(indexSend2ServerCount/globalLocalDataSendToServerMap_.max_load_factor())); //globalLocalDataSendToServerMap_.reserve(indexSend2ServerCount);
//
//  // We need to loop with data index
//  idxLoop.assign(numElement_,0);
//  idx = indexLocalDataOnClientCount = indexSend2ServerCount = 0;
//  ssize = 1; for (int i = 0; i < numElement_; ++i) ssize *= dataNIndex_[i];
//  innerLoopSize = dataNIndex_[0];
//  int countLocalData = 0;
//  std::vector<int> correctOuterIndex(numElement_,0);
//  bool isOuterIndexCorrect = true;
//  while (idx < ssize)
//  {
//    for (int i = 0; i < numElement_-1; ++i)
//    {
//      if (idxLoop[i] == dataNIndex_[i])
//      {
//        idxLoop[i] = 0;
//        correctOuterIndex[i] = 0;
//        ++idxLoop[i+1];
//        if (isOuterIndexCorrect) ++correctOuterIndex[i+1];
//      }
//    }
//
//    // Depending the inner-most element axis or domain,
//    // The outer loop index begins correspondingly at one (1) or zero (0)
//    idxDomain = idxAxis = 0;
//    if (axisDomainOrder_(0)) ++idxDomain;
//    else ++idxAxis;
//    bool isIndexDomainDataCorrect = true;
//    bool isIndexAxisDataCorrect = true;
//
//    for (int i = 1; i < numElement_; ++i)
//    {
//      if (axisDomainOrder_(i))
//      {
//        if (indexDomainData_[idxDomain][idxLoop[i]])
//        {
//          currentIndex[indexMap_[i]]   = localDomainIndex_[idxDomain][correctOuterIndex[i]];
//          currentIndex[indexMap_[i]+1] = localDomainIndex_[idxDomain*2+1][correctOuterIndex[i]];
//          isIndexDomainDataCorrect &= true;
//        }
//        else isIndexDomainDataCorrect = false;
//        ++idxDomain;
//      }
//      else
//      {
//        if (indexAxisData_[idxAxis][idxLoop[i]])
//        {
//          currentIndex[indexMap_[i]]   = localAxisIndex_[idxAxis][correctOuterIndex[i]];
//          isIndexAxisDataCorrect &= true;
//        }
//        else isIndexAxisDataCorrect = false;
//        ++idxAxis;
//      }
//    }
//
//    isOuterIndexCorrect = (isIndexAxisDataCorrect) && (isIndexDomainDataCorrect);
//
//    // Inner most index
//    idxDomain = idxAxis = 0;
//    int correctIndexDomain = 0, correctIndexAxis = 0;
//    for (int i = 0; i < innerLoopSize; ++i)
//    {
//      bool isCurrentIndexDomainDataCorrect = isIndexDomainDataCorrect;
//      bool isCurrentIndexAxisDataCorrect = isIndexAxisDataCorrect;
//
//      if (axisDomainOrder_(0))
//      {
//        if (indexDomainData_[idxDomain][i])
//        {
//          currentIndex[0] = localDomainIndex_[idxDomain][correctIndexDomain];
//          currentIndex[1] = localDomainIndex_[idxDomain+1][correctIndexDomain];
//          isCurrentIndexDomainDataCorrect &= true;
//          ++correctIndexDomain;
//        }
//        else isCurrentIndexDomainDataCorrect = false;
//      }
//      else
//      {
//        if (indexAxisData_[idxAxis][i])
//        {
//          currentIndex[0] = localAxisIndex_[idxAxis][correctIndexAxis];
//          isCurrentIndexAxisDataCorrect &= true;
//          ++correctIndexAxis;
//        }
//        else isCurrentIndexAxisDataCorrect = false;
//      }
//
//      int gridMaskIndex = currentIndex[0];
//      int mulDimMask = 1;
//      for (int k = 1; k < this->dims_; ++k)
//      {
//        mulDimMask *= nLocal_[k-1];
//        gridMaskIndex += (currentIndex[k])*mulDimMask;
//      }
//
//      if (isCurrentIndexDomainDataCorrect &&
//          isCurrentIndexAxisDataCorrect &&
//          gridMask_(gridMaskIndex))
//      {
//        localDataIndex_[indexLocalDataOnClientCount] = countLocalData;
//        bool isIndexOnServer = true;
//        for (int idxElement = 0; idxElement < this->numElement_; ++idxElement)
//        {
//          int actualIdx = 0;
//          if (axisDomainOrder_(idxElement))
//          {
//            actualIdx = currentIndex[indexMap_[idxElement]]+currentIndex[indexMap_[idxElement]+1]*nLocal_[indexMap_[idxElement]];
//            isIndexOnServer = isIndexOnServer && (((*infoIndex_[indexMap_[idxElement]])(actualIdx)) <= nZoomEnd_[indexMap_[idxElement]])
//                                              && (nZoomBegin_[indexMap_[idxElement]] <= ((*infoIndex_[indexMap_[idxElement]])(actualIdx)))
//                                              && (((*infoIndex_[indexMap_[idxElement]+1])(actualIdx)) <= nZoomEnd_[indexMap_[idxElement]+1])
//                                              && (nZoomBegin_[indexMap_[idxElement]+1] <= ((*infoIndex_[indexMap_[idxElement]+1])(actualIdx)));
//          }
//          else
//          {
//            isIndexOnServer = isIndexOnServer && (((*infoIndex_[indexMap_[idxElement]])(currentIndex[indexMap_[idxElement]])) <= nZoomEnd_[indexMap_[idxElement]])
//                                              && (nZoomBegin_[indexMap_[idxElement]] <= ((*infoIndex_[indexMap_[idxElement]])(currentIndex[indexMap_[idxElement]])));
//          }
//        }
//
//        if (isIndexOnServer)
//        {
//          int actualIdx = (axisDomainOrder_(0)) ? currentIndex[0]+currentIndex[1]*nLocal_[0]
//                                                : currentIndex[0];
//          size_t globalIndex = (*infoIndex_[0])(actualIdx); //idxLoop[0] + nBeginGlobal_[0];
//          size_t mulDim = 1;
//          for (int idxElement = 0; idxElement < this->numElement_; ++idxElement)
//          {
//            if (axisDomainOrder_(idxElement))
//            {
//              actualIdx = currentIndex[indexMap_[idxElement]]+currentIndex[indexMap_[idxElement]+1]*nLocal_[indexMap_[idxElement]];
//              int jb = (0 == idxElement) ? 1 : 0;
//              for (int j = jb; j <= 1; ++j)
//              {
//                mulDim *= nGlob_[indexMap_[idxElement]+j-1];
//                globalIndex += ((*infoIndex_[indexMap_[idxElement]+j])(actualIdx))*mulDim;
//              }
//            }
//            else
//            {
//              if (0 != idxElement)
//              {
//                mulDim *= nGlob_[indexMap_[idxElement]-1];
//                globalIndex += ((*infoIndex_[indexMap_[idxElement]])(currentIndex[indexMap_[idxElement]]))*mulDim;
//              }
//            }
//          }
//          globalLocalDataSendToServerMap_[globalIndex] = indexLocalDataOnClientCount;
//          localMaskIndex_[indexSend2ServerCount] = gridMaskIndex;
//          ++indexSend2ServerCount;
//        }
//        ++indexLocalDataOnClientCount;
//      }
//      ++countLocalData;
//    }
//    idxLoop[0] += innerLoopSize;
//    idx += innerLoopSize;
//  }
//}

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
      tempJ = (dataJIndex + dataJBegin);
  int i = (dataDim == 1) ? (tempI) % ni
                         : (tempI) ;
  j = (dataDim == 1) ? (tempI) / ni
                     : (tempJ) ;

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
   return ((tempI)%ni);
}

/*!
  Return global local data mapping of client
*/
CDistributionClient::GlobalLocalDataMap& CDistributionClient::getGlobalLocalDataSendToServer()
{
  if (!isComputed_) createGlobalIndexSendToServer();
  return globalLocalDataSendToServerMap_;
}

/*!
  Return local data index of client
*/
const std::vector<int>& CDistributionClient::getLocalDataIndexOnClient()
{
  if (!isComputed_) createGlobalIndexSendToServer();
  return localDataIndex_;
}

/*!
  Return local mask index of client
*/
const std::vector<int>& CDistributionClient::getLocalMaskIndexOnClient()
{
  if (!isComputed_) createGlobalIndexSendToServer();
  return localMaskIndex_;
}

} // namespace xios
