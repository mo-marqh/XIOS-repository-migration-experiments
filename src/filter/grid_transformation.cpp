/*!
   \file grid_transformation.cpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 09 June 2015

   \brief Interface for all transformations.
 */
#include "grid_transformation.hpp"
#include "axis_algorithm_inverse.hpp"
#include "axis_algorithm_zoom.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "transformation_mapping.hpp"

#include "axis_algorithm_transformation.hpp"

namespace xios {
CGridTransformation::CGridTransformation(CGrid* destination, CGrid* source)
: gridSource_(source), gridDestination_(destination), originalGridSource_(source),
  globalIndexOfCurrentGridSource_(0), globalIndexOfOriginalGridSource_(0)
{
  //Verify the compatibity between two grids
  int numElement = gridDestination_->axis_domain_order.numElements();
  if (numElement != gridSource_->axis_domain_order.numElements())
    ERROR("CGridTransformation::CGridTransformation(CGrid* destination, CGrid* source)",
       << "Two grids have different number of elements"
       << "Number of elements of grid source " <<gridSource_->getId() << " is " << gridSource_->axis_domain_order.numElements()  << std::endl
       << "Number of elements of grid destination " <<gridDestination_->getId() << " is " << numElement);

  for (int i = 0; i < numElement; ++i)
  {
    if (gridDestination_->axis_domain_order(i) != gridSource_->axis_domain_order(i))
      ERROR("CGridTransformation::CGridTransformation(CGrid* destination, CGrid* source)",
         << "Transformed grid and its grid source have incompatible elements"
         << "Grid source " <<gridSource_->getId() << std::endl
         << "Grid destination " <<gridDestination_->getId());
  }

  std::vector<CAxis*> axisSrcTmp = gridSource_->getAxis(), axisSrc;
  std::vector<CDomain*> domainSrcTmp = gridSource_->getDomains(), domainSrc;
  for (int idx = 0; idx < axisSrcTmp.size(); ++idx)
  {
    CAxis* axis = CAxis::createAxis();
    axis->setAttributes(axisSrcTmp[idx]);
    axisSrc.push_back(axis);
  }

  for (int idx = 0; idx < domainSrcTmp.size(); ++idx)
  {
    CDomain* domain = CDomain::createDomain();
    domain->setAttributes(domainSrcTmp[idx]);
    domainSrc.push_back(domain);
  }

  gridSource_ = CGrid::createGrid(domainSrc, axisSrc, gridDestination_->axis_domain_order);
  gridSourceDimensionSize_ = gridSource_->getGlobalDimension();
  gridDestinationDimensionSize_ = gridDestination_->getGlobalDimension();

  initializeMappingOfOriginalGridSource();
  initializeAlgorithms();
}

void CGridTransformation::initializeMappingOfOriginalGridSource()
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;

  CDistributionClient distribution(client->clientRank, originalGridSource_);
  const CArray<size_t,1>& globalIndexGridDestSendToServer = distribution.getGlobalDataIndexSendToServer();

  globalIndexOfCurrentGridSource_   = new CArray<size_t,1>(globalIndexGridDestSendToServer.numElements());
  globalIndexOfOriginalGridSource_  = new CArray<size_t,1>(globalIndexGridDestSendToServer.numElements());
  *globalIndexOfCurrentGridSource_  = globalIndexGridDestSendToServer;
  *globalIndexOfOriginalGridSource_ = globalIndexGridDestSendToServer;
}

CGridTransformation::~CGridTransformation()
{
  std::list<CGenericAlgorithmTransformation*>::const_iterator itb = algoTransformation_.begin(), it,
                                                              ite = algoTransformation_.end();
  for (it = itb; it != ite; ++it) delete (*it);

  std::map<int, std::vector<CArray<int,1>* > >::const_iterator itMapRecv, iteMapRecv;
  itMapRecv = localIndexToReceiveOnGridDest_.begin();
  iteMapRecv = localIndexToReceiveOnGridDest_.end();
  for (; itMapRecv != iteMapRecv; ++itMapRecv)
  {
    int numVec = (itMapRecv->second).size();
    for (int idx = 0; idx < numVec; ++idx) delete (itMapRecv->second)[idx];
  }

  std::map<int, CArray<int,1>* >::const_iterator itMap, iteMap;
  itMap = localIndexToSendFromGridSource_.begin();
  iteMap = localIndexToSendFromGridSource_.end();
  for (; itMap != iteMap; ++itMap) delete (itMap->second);

  if (0 != globalIndexOfCurrentGridSource_) delete globalIndexOfCurrentGridSource_;
  if (0 != globalIndexOfOriginalGridSource_) delete globalIndexOfOriginalGridSource_;
}

void CGridTransformation::initializeAlgorithms()
{
  initializeAxisAlgorithms();
  initializeDomainAlgorithms();
}

/*!
  Initialize the algorithms corresponding to transformation info contained in each axis.
If an axis has transformations, these transformations will be represented in form of vector of CTransformation pointers
In general, each axis can have several transformations performed on itself. However, should they be done seperately or combinely (of course in order)?
For now, one approach is to do these combinely but maybe this needs changing.
*/
void CGridTransformation::initializeAxisAlgorithms()
{
  std::vector<int> axisPositionInGrid;
  std::vector<CAxis*> axisListDestP = gridDestination_->getAxis();
  std::vector<CAxis*> axisListSrcP = gridSource_->getAxis();
  if (!axisListDestP.empty())
  {
    int idx = 0;
    for (int i = 0; i < gridDestination_->axis_domain_order.numElements(); ++i)
    {
      if (false == (gridDestination_->axis_domain_order)(i))
      {
        axisPositionInGrid.push_back(idx);
        ++idx;
      }
      else idx += 2;
    }

    for (int i = 0; i < axisListDestP.size(); ++i)
    {
      elementPosition2AxisPositionInGrid_[axisPositionInGrid[i]] = i;
      if (axisListDestP[i]->hasTransformation())
      {
        CAxis::TransMapTypes trans = axisListDestP[i]->getAllTransformations();
        CAxis::TransMapTypes::const_iterator itb = trans.begin(), it,
                                             ite = trans.end();
        int transformationOrder = 0;
        for (it = itb; it != ite; ++it)
        {
          listAlgos_.push_back(std::make_pair(axisPositionInGrid[i], std::make_pair(it->first, transformationOrder)));
          ++transformationOrder;
        }
      }
    }
  }
}

void CGridTransformation::initializeDomainAlgorithms()
{

}

void CGridTransformation::selectAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder)
{
   selectAxisAlgo(elementPositionInGrid, transType, transformationOrder);
}

void CGridTransformation::selectAxisAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder)
{
  std::vector<CAxis*> axisListDestP = gridDestination_->getAxis();
  std::vector<CAxis*> axisListSrcP = gridSource_->getAxis();

  int axisIndex =  elementPosition2AxisPositionInGrid_[elementPositionInGrid];
  CAxis::TransMapTypes trans = axisListDestP[axisIndex]->getAllTransformations();
  CAxis::TransMapTypes::const_iterator it = trans.begin();

  for (int i = 0; i < transformationOrder; ++i, ++it) {}  // Find the correct transformation

  CZoomAxis* zoomAxis = 0;
  CGenericAlgorithmTransformation* algo = 0;
  switch (transType)
  {
    case TRANS_ZOOM_AXIS:
      zoomAxis = dynamic_cast<CZoomAxis*> (it->second);
      algo = new CAxisAlgorithmZoom(axisListDestP[axisIndex], axisListSrcP[axisIndex], zoomAxis);
      break;
    case TRANS_INVERSE_AXIS:
      algo = new CAxisAlgorithmInverse(axisListDestP[axisIndex], axisListSrcP[axisIndex]);
      break;
    default:
      break;
  }
  algoTransformation_.push_back(algo);

}

void CGridTransformation::selectDomainAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder)
{

}

void CGridTransformation::setUpGrid(int elementPositionInGrid, ETranformationType transType)
{
  std::vector<CAxis*> axisListDestP = gridDestination_->getAxis();
  std::vector<CAxis*> axisListSrcP = gridSource_->getAxis();

  int axisIndex;
  switch (transType)
  {
    case TRANS_ZOOM_AXIS:
    case TRANS_INVERSE_AXIS:
      axisIndex =  elementPosition2AxisPositionInGrid_[elementPositionInGrid];
      axisListSrcP[axisIndex]->duplicateAttributes(axisListDestP[axisIndex]);
      break;
    default:
      break;
  }
}

void CGridTransformation::computeAll()
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;

  ListAlgoType::const_iterator itb = listAlgos_.begin(),
                               ite = listAlgos_.end(), it;
  CGenericAlgorithmTransformation* algo = 0;
  for (it = itb; it != ite; ++it)
  {
    std::map<size_t, std::set<size_t> > globaIndexMapFromDestToSource;
    int elementPositionInGrid = it->first;
    ETranformationType transType = (it->second).first;
    int transformationOrder = (it->second).second;

    // First of all, select an algorithm
    selectAlgo(elementPositionInGrid, transType, transformationOrder);
    algo = algoTransformation_.back();

    // Recalculate the distribution of grid destination
    CDistributionClient distributionClientDest(client->clientRank, gridDestination_);
    const CArray<size_t,1>& globalIndexGridDestSendToServer = distributionClientDest.getGlobalDataIndexSendToServer();

   std::cout << "global index grid  dest send to server " << globalIndexGridDestSendToServer << std::endl;
    // ComputeTransformation of global index of each element
    std::vector<int> gridDestinationDimensionSize = gridDestination_->getGlobalDimension();
    int elementPosition = it->first;
    algo->computeGlobalSourceIndex(elementPosition,
                                   gridDestinationDimensionSize,
                                   globalIndexGridDestSendToServer,
                                   globaIndexMapFromDestToSource);

    // Compute transformation of global indexes among grids
    computeTransformationFromOriginalGridSource(globaIndexMapFromDestToSource);

    // Now grid destination becomes grid source in a new transformation
    setUpGrid(elementPositionInGrid, transType);
  }

 std::cout << "global index destination 0 final " << *globalIndexOfCurrentGridSource_ << std::endl;
 std::cout << "global index destination 1 final " << *globalIndexOfOriginalGridSource_ << std::endl;
  updateFinalGridDestination();
  computeFinalTransformationMapping();
}


/*!
  After applying the algorithms, there are some informations on grid destination needing change, for now, there are:
   +) mask
*/
void CGridTransformation::updateFinalGridDestination()
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;

  //First of all, retrieve info of local mask of grid destination
  CDistributionClient distributionClientDest(client->clientRank, gridDestination_);
  const CArray<int, 1>& localMaskIndexOnClientDest = distributionClientDest.getLocalMaskIndexOnClient();
  std::cout << "local mask " << localMaskIndexOnClientDest << std::endl;

  const CArray<size_t,1>& globalIndexOnClientDest = distributionClientDest.getGlobalDataIndexSendToServer();
  std::cout << "global index " << globalIndexOnClientDest <<  std::endl;
  CArray<size_t, 1>::const_iterator itbArr, itArr, iteArr;
  itbArr = globalIndexOnClientDest.begin();
  iteArr = globalIndexOnClientDest.end();

  // Then find out which index became invalid (become masked after being applied the algorithms, or demande some masked points from grid source)
  int num = globalIndexOfOriginalGridSource_->numElements();
  const size_t sfmax = NumTraits<unsigned long>::sfmax();
  int maskIndexNum = 0;
  for (int idx = 0; idx < num; ++idx)
  {
    if (sfmax == (*globalIndexOfOriginalGridSource_)(idx))
    {
      size_t maskedGlobalIndex = (*globalIndexOfCurrentGridSource_)(idx);
      itArr = std::find(itbArr, iteArr, maskedGlobalIndex);
      if (iteArr != itArr) ++maskIndexNum;
    }
  }

  CArray<int,1>* maskIndexToModify = new CArray<int,1>(maskIndexNum);
  maskIndexNum = 0;
  for (int idx = 0; idx < num; ++idx)
  {
    if (sfmax == (*globalIndexOfOriginalGridSource_)(idx))
    {
      size_t maskedGlobalIndex = (*globalIndexOfCurrentGridSource_)(idx);
      itArr = std::find(itbArr, iteArr, maskedGlobalIndex);
      if (iteArr != itArr)
      {
        int localIdx = std::distance(itbArr, itArr);
        (*maskIndexToModify)(maskIndexNum) = (localMaskIndexOnClientDest)(localIdx);
        ++maskIndexNum;
      }
    }
  }

  std::cout << "index to modify " << *maskIndexToModify << std::endl;
  gridDestination_->modifyMask(*maskIndexToModify);

  delete maskIndexToModify;
}

/*!
  A transformation from a grid source to grid destination often passes several intermediate grids, which play a role of
temporary grid source and/or grid destination. This function makes sure that global index of original grid source are mapped correctly to
the final grid destination
*/
void CGridTransformation::computeTransformationFromOriginalGridSource(const std::map<size_t, std::set<size_t> >& globaIndexMapFromDestToSource)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;

  CTransformationMapping transformationMap(gridDestination_, gridSource_);

    // Then compute transformation mapping among clients
  transformationMap.computeTransformationMapping(globaIndexMapFromDestToSource);

  const std::map<int,std::vector<std::vector<size_t> > >& globalIndexToReceive = transformationMap.getGlobalIndexReceivedOnGridDestMapping();
  const std::map<int,std::vector<size_t> >& globalIndexToSend = transformationMap.getGlobalIndexSendToGridDestMapping();

 // Sending global index of original grid source
  std::map<int,std::vector<size_t> >::const_iterator itbSend = globalIndexToSend.begin(), itSend,
                                                     iteSend = globalIndexToSend.end();
  CArray<size_t,1>::const_iterator itbArr = globalIndexOfCurrentGridSource_->begin(), itArr,
                                   iteArr = globalIndexOfCurrentGridSource_->end();
 int sendBuffSize = 0;
 for (itSend = itbSend; itSend != iteSend; ++itSend) sendBuffSize += (itSend->second).size();

 std::cout << "global index destination 0 before" << *globalIndexOfCurrentGridSource_ << std::endl;
 std::cout << "global index destination 1 before" << *globalIndexOfOriginalGridSource_ << std::endl;

 typedef unsigned long Scalar;
 unsigned long* sendBuff, *currentSendBuff;
 if (0 != sendBuffSize) sendBuff = new unsigned long [sendBuffSize];
 for (StdSize idx = 0; idx < sendBuffSize; ++idx) sendBuff[idx] = NumTraits<Scalar>::sfmax();

 int currentBuffPosition = 0;
 for (itSend = itbSend; itSend != iteSend; ++itSend)
 {
   int destRank = itSend->first;
   const std::vector<size_t>& globalIndexOfCurrentGridSourceToSend = itSend->second;
   int countSize = globalIndexOfCurrentGridSourceToSend.size();
   for (int idx = 0; idx < (countSize); ++idx)
   {
     itArr = std::find(itbArr, iteArr, globalIndexOfCurrentGridSourceToSend[idx]);
     if (iteArr != itArr)
     {
       int index = std::distance(itbArr, itArr);
       sendBuff[idx+currentBuffPosition] = (*globalIndexOfOriginalGridSource_)(index);
     }
   }
   currentSendBuff = sendBuff + currentBuffPosition;
   MPI_Send(currentSendBuff, countSize, MPI_UNSIGNED_LONG, destRank, 14, client->intraComm);
   currentBuffPosition += countSize;
 }

 // Receiving global index of grid source sending from current grid source
 std::map<int,std::vector<std::vector<size_t> > >::const_iterator itbRecv = globalIndexToReceive.begin(), itRecv,
                                                                  iteRecv = globalIndexToReceive.end();
 int recvBuffSize = 0;
 for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv) recvBuffSize += (itRecv->second).size();

 unsigned long* recvBuff, *currentRecvBuff;
 if (0 != recvBuffSize) recvBuff = new unsigned long [recvBuffSize];
 for (StdSize idx = 0; idx < recvBuffSize; ++idx) recvBuff[idx] = NumTraits<Scalar>::sfmax();

 int currentRecvBuffPosition = 0;
 for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv)
 {
   MPI_Status status;
   int srcRank = itRecv->first;
   int countSize = (itRecv->second).size();
   currentRecvBuff = recvBuff + currentRecvBuffPosition;
   MPI_Recv(currentRecvBuff, countSize, MPI_UNSIGNED_LONG, srcRank, 14, client->intraComm, &status);
   currentRecvBuffPosition += countSize;
 }

 int nbCurrentGridSource = 0;
 for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv)
 {
   int ssize = (itRecv->second).size();
   for (int idx = 0; idx < ssize; ++idx)
   {
     nbCurrentGridSource += (itRecv->second)[idx].size();
   }
 }

 if (globalIndexOfCurrentGridSource_->numElements()  != nbCurrentGridSource)
 {
   if ((0 != nbCurrentGridSource) && (0 != globalIndexOfCurrentGridSource_))
   {
     delete globalIndexOfCurrentGridSource_;
     globalIndexOfCurrentGridSource_ = new CArray<size_t,1>(nbCurrentGridSource);
   }
 }

 if (globalIndexOfOriginalGridSource_->numElements() != nbCurrentGridSource)
 {
   if ((0 != nbCurrentGridSource) && (0 != globalIndexOfOriginalGridSource_))
   {
     delete globalIndexOfOriginalGridSource_;
     globalIndexOfOriginalGridSource_ = new CArray<size_t,1>(nbCurrentGridSource);
   }
 }

 int k = 0;
 currentRecvBuff = recvBuff;
 for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv)
 {
   int countSize = (itRecv->second).size();
   for (int idx = 0; idx < countSize; ++idx, ++currentRecvBuff)
   {
     int ssize = (itRecv->second)[idx].size();
     for (int i = 0; i < ssize; ++i)
     {
       (*globalIndexOfCurrentGridSource_)(k) = (itRecv->second)[idx][i];
       (*globalIndexOfOriginalGridSource_)(k) = *currentRecvBuff;
       ++k;
     }
   }
 }

 std::cout << "global index destination 0 after " << *globalIndexOfCurrentGridSource_ << std::endl;
 std::cout << "global index destination 1 after " << *globalIndexOfOriginalGridSource_ << std::endl;
 if (0 != sendBuffSize) delete [] sendBuff;
 if (0 != recvBuffSize) delete [] recvBuff;
}

/*!
  Compute transformation mapping between grid source and grid destination
  The transformation between grid source and grid destination is represented in form of mapping between global index
of two grids. Then local index mapping between data on each grid will be found out thanks to these global indexes
*/
void CGridTransformation::computeFinalTransformationMapping()
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;

  CTransformationMapping transformationMap(gridDestination_, originalGridSource_);

  std::map<size_t, std::set<size_t> > globaIndexMapFromDestToSource;

  int nb = globalIndexOfCurrentGridSource_->numElements();
  const size_t sfmax = NumTraits<unsigned long>::sfmax();
  for (int idx = 0; idx < nb; ++idx)
  {
    if (sfmax != (*globalIndexOfOriginalGridSource_)(idx))
      globaIndexMapFromDestToSource[(*globalIndexOfCurrentGridSource_)(idx)].insert((*globalIndexOfOriginalGridSource_)(idx));
  }

  // Then compute transformation mapping among clients
  transformationMap.computeTransformationMapping(globaIndexMapFromDestToSource);

  const std::map<int,std::vector<std::vector<size_t> > >& globalIndexToReceive = transformationMap.getGlobalIndexReceivedOnGridDestMapping();
  const std::map<int,std::vector<size_t> >& globalIndexToSend = transformationMap.getGlobalIndexSendToGridDestMapping();

  CDistributionClient distributionClientDest(client->clientRank, gridDestination_);
  CDistributionClient distributionClientSrc(client->clientRank, originalGridSource_);

//  const CArray<int, 1>& localIndexOnClientDest = distributionClientDest.getLocalDataIndexOnClient(); //gridDestination_->getDistributionClient()->getLocalDataIndexOnClient();
  const CArray<int, 1>& localIndexOnClientDest = distributionClientDest.getLocalDataIndexSendToServer();
  const CArray<size_t,1>& globalIndexOnClientDest = distributionClientDest.getGlobalDataIndexSendToServer(); //gridDestination_->getDistributionClient()->getGlobalDataIndexSendToServer();

 std::cout << "dest: local index " << localIndexOnClientDest << std::endl;
 std::cout << "dest: global index " << globalIndexOnClientDest << std::endl;
  const CArray<int, 1>& localIndexOnClientSrc = distributionClientSrc.getLocalDataIndexOnClient(); //gridSource_->getDistributionClient()->getLocalDataIndexOnClient();
  const CArray<size_t,1>& globalIndexOnClientSrc = distributionClientSrc.getGlobalDataIndexSendToServer(); //gridSource_->getDistributionClient()->getGlobalDataIndexSendToServer();
 std::cout << "src: local index " << localIndexOnClientSrc << std::endl;
 std::cout << "src: global index " << globalIndexOnClientSrc << std::endl;
  std::vector<size_t>::const_iterator itbVec, itVec, iteVec;
  CArray<size_t, 1>::const_iterator itbArr, itArr, iteArr;

  std::map<int,std::vector<std::vector<size_t> > >::const_iterator itbMapRecv, itMapRecv, iteMapRecv;

  // Find out local index on grid destination (received)
  itbMapRecv = globalIndexToReceive.begin();
  iteMapRecv = globalIndexToReceive.end();
  itbArr = globalIndexOnClientDest.begin();
  iteArr = globalIndexOnClientDest.end();
  for (itMapRecv = itbMapRecv; itMapRecv != iteMapRecv; ++itMapRecv)
  {
    int sourceRank = itMapRecv->first;
    int numGlobalIndex = (itMapRecv->second).size();
    for (int i = 0; i < numGlobalIndex; ++i)
    {
      int vecSize = ((itMapRecv->second)[i]).size();
      CArray<int,1>* ptr = new CArray<int,1>(vecSize);
      localIndexToReceiveOnGridDest_[sourceRank].push_back(ptr);
      for (int idx = 0; idx < vecSize; ++idx)
      {
        itArr = std::find(itbArr, iteArr, (itMapRecv->second)[i][idx]);
        if (iteArr != itArr)
        {
          int localIdx = std::distance(itbArr, itArr);
          (*localIndexToReceiveOnGridDest_[sourceRank][i])(idx) = localIndexOnClientDest(localIdx); // Local index of un-extracted data (only domain)
//          (*localIndexToReceiveOnGridDest_[sourceRank][i])(idx) = (localIdx); // Local index of extracted data
        }
      }
    }
//    std::cout << "local index to receive from source Rank = " << sourceRank << (*localIndexToReceiveOnGridDest_[sourceRank][i]) << std::endl;
  }

  std::map<int,std::vector<size_t> >::const_iterator itbMap, itMap, iteMap;
  // Find out local index on grid source (to send)
  itbMap = globalIndexToSend.begin();
  iteMap = globalIndexToSend.end();
  itbArr = globalIndexOnClientSrc.begin();
  iteArr = globalIndexOnClientSrc.end();
  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    CArray<int,1>* ptr = new CArray<int,1>((itMap->second).size());
    localIndexToSendFromGridSource_[itMap->first] = ptr;
    int destRank = itMap->first;
    int vecSize = (itMap->second).size();
    for (int idx = 0; idx < vecSize; ++idx)
    {
      itArr = std::find(itbArr, iteArr, (itMap->second)[idx]);
      if (iteArr != itArr)
      {
        int localIdx = std::distance(itbArr, itArr);
//        (*localIndexToSendFromGridSource_[destRank])(idx) = localIndexOnClientSrc(localIdx);
        (*localIndexToSendFromGridSource_[destRank])(idx) = (localIdx);
      }
    }
    std::cout << "local index to send to dest Rank = " << destRank << (*localIndexToSendFromGridSource_[destRank]) << std::endl;
  }
}

/*!
  Local index of data which need sending from the grid source
  \return local index of data
*/
std::map<int, CArray<int,1>* > CGridTransformation::getLocalIndexToSendFromGridSource()
{
  return localIndexToSendFromGridSource_;
}

/*!
  Local index of data which will be received on the grid destination
  \return local index of data
*/
std::map<int, std::vector<CArray<int,1>* > > CGridTransformation::getLocalIndexToReceiveOnGridDest()
{
  return localIndexToReceiveOnGridDest_;
}

}
