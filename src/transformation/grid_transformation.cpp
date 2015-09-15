/*!
   \file grid_transformation.cpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 02 Jul 2015

   \brief Interface for all transformations.
 */
#include "grid_transformation.hpp"
#include "axis_algorithm_inverse.hpp"
#include "axis_algorithm_zoom.hpp"
#include "axis_algorithm_interpolate.hpp"
#include "domain_algorithm_zoom.hpp"
#include "domain_algorithm_interpolate.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "transformation_mapping.hpp"
#include "axis_algorithm_transformation.hpp"
#include "distribution_client.hpp"

namespace xios {
CGridTransformation::CGridTransformation(CGrid* destination, CGrid* source)
: gridSource_(source), gridDestination_(destination), originalGridSource_(source),
  globalIndexOfCurrentGridSource_(), globalIndexOfOriginalGridSource_(), weightOfGlobalIndexOfOriginalGridSource_(0), algoTypes_()
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
    domain->checkAttributesOnClient();
    domainSrc.push_back(domain);
  }

  gridSource_ = CGrid::createGrid(domainSrc, axisSrc, gridDestination_->axis_domain_order);

  initializeMappingOfOriginalGridSource();
  initializeAlgorithms();
}

/*!
  Initialize the mapping between the first grid source and the original one
  In a series of transformation, for each step, there is a need to "create" a new grid that plays a role of "temporary" source.
Because at the end of the series, we need to know about the index mapping between the final grid destination and original grid source,
for each transformation, we need to make sure that the current "temporary source" maps its global index correctly to the original one.
*/
void CGridTransformation::initializeMappingOfOriginalGridSource()
{
  CContext* context = CContext::getCurrent();
  CContextClient* client = context->client;

  CDistributionClient distribution(client->clientRank, originalGridSource_);
  const std::vector<size_t>& globalIndexGridSrcSendToServer = distribution.getGlobalDataIndexSendToServer();

  weightOfGlobalIndexOfOriginalGridSource_.resize(globalIndexGridSrcSendToServer.size());
  globalIndexOfCurrentGridSource_  = globalIndexGridSrcSendToServer;
  globalIndexOfOriginalGridSource_ = globalIndexGridSrcSendToServer;
  weightOfGlobalIndexOfOriginalGridSource_ = 1.0;
}

CGridTransformation::~CGridTransformation()
{
  std::list<CGenericAlgorithmTransformation*>::const_iterator itb = algoTransformation_.begin(), it,
                                                              ite = algoTransformation_.end();
  for (it = itb; it != ite; ++it) delete (*it);
}

/*!
  Initialize the algorithms (transformations)
*/
void CGridTransformation::initializeAlgorithms()
{
  std::vector<int> axisPositionInGrid;
  std::vector<int> domPositionInGrid;
  std::vector<CAxis*> axisListDestP = gridDestination_->getAxis();
  std::vector<CDomain*> domListDestP = gridDestination_->getDomains();

  int idx = 0;
  for (int i = 0; i < gridDestination_->axis_domain_order.numElements(); ++i)
  {
    if (false == (gridDestination_->axis_domain_order)(i))
    {
      axisPositionInGrid.push_back(idx);
      ++idx;
    }
    else
    {
      ++idx;
      domPositionInGrid.push_back(idx);
      ++idx;
    }
  }

  for (int i = 0; i < axisListDestP.size(); ++i)
  {
    elementPosition2AxisPositionInGrid_[axisPositionInGrid[i]] = i;
  }

  for (int i = 0; i < domListDestP.size(); ++i)
  {
    elementPosition2DomainPositionInGrid_[domPositionInGrid[i]] = i;
  }

  idx = 0;
  for (int i = 0; i < gridDestination_->axis_domain_order.numElements(); ++i)
  {
    if (false == (gridDestination_->axis_domain_order)(i))
    {
      initializeAxisAlgorithms(idx);
      ++idx;
    }
    else
    {
      ++idx;
      initializeDomainAlgorithms(idx);
      ++idx;
    }
  }
}



/*!
  Initialize the algorithms corresponding to transformation info contained in each axis.
If an axis has transformations, these transformations will be represented in form of vector of CTransformation pointers
In general, each axis can have several transformations performed on itself. However, should they be done seperately or combinely (of course in order)?
For now, one approach is to do these combinely but maybe this needs changing.
\param [in] axisPositionInGrid position of an axis in grid. (for example: a grid with one domain and one axis, position of domain is 1, position of axis is 2)
*/
void CGridTransformation::initializeAxisAlgorithms(int axisPositionInGrid)
{
  std::vector<CAxis*> axisListDestP = gridDestination_->getAxis();
  if (!axisListDestP.empty())
  {
    if (axisListDestP[elementPosition2AxisPositionInGrid_[axisPositionInGrid]]->hasTransformation())
    {
      CAxis::TransMapTypes trans = axisListDestP[elementPosition2AxisPositionInGrid_[axisPositionInGrid]]->getAllTransformations();
      CAxis::TransMapTypes::const_iterator itb = trans.begin(), it,
                                           ite = trans.end();
      int transformationOrder = 0;
      for (it = itb; it != ite; ++it)
      {
        listAlgos_.push_back(std::make_pair(axisPositionInGrid, std::make_pair(it->first, transformationOrder)));
        algoTypes_.push_back(false);
        ++transformationOrder;
      }
    }
  }
}

/*!
  Initialize the algorithms corresponding to transformation info contained in each domain.
If a domain has transformations, they will be represented in form of vector of CTransformation pointers
In general, each domain can have several transformations performed on itself.
\param [in] domPositionInGrid position of a domain in grid. (for example: a grid with one domain and one axis, position of domain is 1, position of axis is 2)
*/
void CGridTransformation::initializeDomainAlgorithms(int domPositionInGrid)
{
  std::vector<CDomain*> domListDestP = gridDestination_->getDomains();
  if (!domListDestP.empty())
  {
    if (domListDestP[elementPosition2DomainPositionInGrid_[domPositionInGrid]]->hasTransformation())
    {
      CDomain::TransMapTypes trans = domListDestP[elementPosition2DomainPositionInGrid_[domPositionInGrid]]->getAllTransformations();
      CDomain::TransMapTypes::const_iterator itb = trans.begin(), it,
                                             ite = trans.end();
      int transformationOrder = 0;
      for (it = itb; it != ite; ++it)
      {
        listAlgos_.push_back(std::make_pair(domPositionInGrid, std::make_pair(it->first, transformationOrder)));
        algoTypes_.push_back(true);
        ++transformationOrder;
      }
    }
  }

}

/*!
  Select algorithm correspoding to its transformation type and its position in each element
  \param [in] elementPositionInGrid position of element in grid. e.g: a grid has 1 domain and 1 axis, then position of domain is 1 (because it contains 2 basic elements)
                                             and position of axis is 2
  \param [in] transType transformation type, for now we have Zoom_axis, inverse_axis
  \param [in] transformationOrder position of the transformation in an element (an element can have several transformation)
  \param [in] isDomainAlgo flag to specify type of algorithm (for domain or axis)
*/
void CGridTransformation::selectAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder, bool isDomainAlgo)
{
   if (isDomainAlgo) selectDomainAlgo(elementPositionInGrid, transType, transformationOrder);
   else selectAxisAlgo(elementPositionInGrid, transType, transformationOrder);
}

/*!
  Select algorithm of an axis correspoding to its transformation type and its position in each element
  \param [in] elementPositionInGrid position of element in grid. e.g: a grid has 1 domain and 1 axis, then position of domain is 1 (because it contains 2 basic elements)
                                             and position of axis is 2
  \param [in] transType transformation type, for now we have Zoom_axis, inverse_axis
  \param [in] transformationOrder position of the transformation in an element (an element can have several transformation)
*/
void CGridTransformation::selectAxisAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder)
{
  std::vector<CAxis*> axisListDestP = gridDestination_->getAxis();
  std::vector<CAxis*> axisListSrcP = gridSource_->getAxis();

  int axisIndex =  elementPosition2AxisPositionInGrid_[elementPositionInGrid];
  CAxis::TransMapTypes trans = axisListDestP[axisIndex]->getAllTransformations();
  CAxis::TransMapTypes::const_iterator it = trans.begin();

  for (int i = 0; i < transformationOrder; ++i, ++it) {}  // Find the correct transformation

  CZoomAxis* zoomAxis = 0;
  CInterpolateAxis* interpAxis = 0;
  CGenericAlgorithmTransformation* algo = 0;
  switch (transType)
  {
    case TRANS_INTERPOLATE_AXIS:
      interpAxis = dynamic_cast<CInterpolateAxis*> (it->second);
      algo = new CAxisAlgorithmInterpolate(axisListDestP[axisIndex], axisListSrcP[axisIndex], interpAxis);
      break;
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

/*!
  Select algorithm of a domain correspoding to its transformation type and its position in each element
  \param [in] elementPositionInGrid position of element in grid. e.g: a grid has 1 domain and 1 axis, then position of domain is 1 (because it contains 2 basic elements)
                                             and position of axis is 2
  \param [in] transType transformation type, for now we have Zoom_axis, inverse_axis
  \param [in] transformationOrder position of the transformation in an element (an element can have several transformation)
*/
void CGridTransformation::selectDomainAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder)
{
  std::vector<CDomain*> domainListDestP = gridDestination_->getDomains();
  std::vector<CDomain*> domainListSrcP = gridSource_->getDomains();

  int domainIndex =  elementPosition2DomainPositionInGrid_[elementPositionInGrid];
  CDomain::TransMapTypes trans = domainListDestP[domainIndex]->getAllTransformations();
  CDomain::TransMapTypes::const_iterator it = trans.begin();

  for (int i = 0; i < transformationOrder; ++i, ++it) {}  // Find the correct transformation

  CZoomDomain* zoomDomain = 0;
  CInterpolateDomain* interpFileDomain = 0;
  CGenericAlgorithmTransformation* algo = 0;
  switch (transType)
  {
    case TRANS_INTERPOLATE_DOMAIN:
      interpFileDomain = dynamic_cast<CInterpolateDomain*> (it->second);
      algo = new CDomainAlgorithmInterpolate(domainListDestP[domainIndex], domainListSrcP[domainIndex],interpFileDomain);
      break;
    case TRANS_ZOOM_DOMAIN:
      zoomDomain = dynamic_cast<CZoomDomain*> (it->second);
      algo = new CDomainAlgorithmZoom(domainListDestP[domainIndex], domainListSrcP[domainIndex], zoomDomain);
      break;
    default:
      break;
  }
  algoTransformation_.push_back(algo);
}

/*!
  Assign the current grid destination to the grid source in the new transformation.
The current grid destination plays the role of grid source in next transformation (if any).
Only element on which the transformation is performed is modified
  \param [in] elementPositionInGrid position of element in grid
  \param [in] transType transformation type
*/
void CGridTransformation::setUpGrid(int elementPositionInGrid, ETranformationType transType)
{
  std::vector<CAxis*> axisListDestP = gridDestination_->getAxis();
  std::vector<CAxis*> axisListSrcP = gridSource_->getAxis();

  std::vector<CDomain*> domListDestP = gridDestination_->getDomains();
  std::vector<CDomain*> domListSrcP = gridSource_->getDomains();

  int axisIndex, domainIndex;
  switch (transType)
  {
    case TRANS_INTERPOLATE_DOMAIN:
    case TRANS_ZOOM_DOMAIN:
      domainIndex = elementPosition2DomainPositionInGrid_[elementPositionInGrid];
      domListSrcP[domainIndex]->duplicateAttributes(domListDestP[domainIndex]);
      break;

    case TRANS_INTERPOLATE_AXIS:
    case TRANS_ZOOM_AXIS:
    case TRANS_INVERSE_AXIS:
      axisIndex =  elementPosition2AxisPositionInGrid_[elementPositionInGrid];
      axisListSrcP[axisIndex]->duplicateAttributes(axisListDestP[axisIndex]);
      break;
    default:
      break;
  }
}

/*!
  Perform all transformations
  For each transformation, there are some things to do:
  -) Chose the correct algorithm by transformation type and position of element
  -) Calculate the mapping of global index between the current grid source and grid destination
  -) Calculate the mapping of global index between current grid DESTINATION and ORIGINAL grid SOURCE
  -) Make current grid destination become grid source in the next transformation
*/
void CGridTransformation::computeAll()
{
  CContext* context = CContext::getCurrent();
  CContextClient* client = context->client;

  ListAlgoType::const_iterator itb = listAlgos_.begin(),
                               ite = listAlgos_.end(), it;
  CGenericAlgorithmTransformation* algo = 0;
  int nbAgloTransformation = 0; // Only count for executed transformation. Generate domain is a special one, not executed in the list
  for (it = itb; it != ite; ++it)
  {
    int elementPositionInGrid = it->first;
    ETranformationType transType = (it->second).first;
    int transformationOrder = (it->second).second;
    std::map<size_t, std::vector<std::pair<size_t,double> > > globaIndexWeightFromDestToSource;

    // First of all, select an algorithm
    selectAlgo(elementPositionInGrid, transType, transformationOrder, algoTypes_[std::distance(itb, it)]);
    algo = algoTransformation_.back();

    if (0 != algo) // Only registered transformation can be executed
    {
      // Recalculate the distribution of grid destination
      CDistributionClient distributionClientDest(client->clientRank, gridDestination_);
      const std::vector<size_t>& globalIndexGridDestSendToServer = distributionClientDest.getGlobalDataIndexSendToServer();

      // ComputeTransformation of global index of each element
      std::vector<int> gridDestinationDimensionSize = gridDestination_->getGlobalDimension();
      std::vector<int> gridSrcDimensionSize = gridSource_->getGlobalDimension();
      int elementPosition = it->first;
      algo->computeGlobalSourceIndex(elementPosition,
                                     gridDestinationDimensionSize,
                                     gridSrcDimensionSize,
                                     globalIndexGridDestSendToServer,
                                     globaIndexWeightFromDestToSource);

      // Compute transformation of global indexes among grids
      computeTransformationFromOriginalGridSource(globaIndexWeightFromDestToSource);

      // Now grid destination becomes grid source in a new transformation
      setUpGrid(elementPositionInGrid, transType);
      ++nbAgloTransformation;
    }
  }

  if (0 != nbAgloTransformation)
  {
    updateFinalGridDestination();
    computeFinalTransformationMapping();
  }
}


/*!
  After applying the algorithms, there are some informations on grid destination needing change, for now, there are:
   +) mask
*/
void CGridTransformation::updateFinalGridDestination()
{
  CContext* context = CContext::getCurrent();
  CContextClient* client = context->client;

  //First of all, retrieve info of local mask of grid destination
  CDistributionClient distributionClientDest(client->clientRank, gridDestination_);
  const std::vector<int>& localMaskIndexOnClientDest = distributionClientDest.getLocalMaskIndexOnClient();
  const std::vector<size_t>& globalIndexOnClientDest = distributionClientDest.getGlobalDataIndexSendToServer();

  std::vector<size_t>::const_iterator itbArr, itArr, iteArr;
  itbArr = globalIndexOnClientDest.begin();
  iteArr = globalIndexOnClientDest.end();

  // Then find out which index became invalid (become masked after being applied the algorithms, or demande some masked points from grid source)
  int num = globalIndexOfOriginalGridSource_.size();
  const size_t sfmax = NumTraits<unsigned long>::sfmax();
  int maskIndexNum = 0;
  for (int idx = 0; idx < num; ++idx)
  {
    if (sfmax == globalIndexOfOriginalGridSource_[idx])
    {
      size_t maskedGlobalIndex = globalIndexOfCurrentGridSource_[idx];
      itArr = std::find(itbArr, iteArr, maskedGlobalIndex);
      if (iteArr != itArr) ++maskIndexNum;
    }
  }

  CArray<int,1> maskIndexToModify(maskIndexNum);
  maskIndexNum = 0;
  for (int idx = 0; idx < num; ++idx)
  {
    if (sfmax == globalIndexOfOriginalGridSource_[idx])
    {
      size_t maskedGlobalIndex = globalIndexOfCurrentGridSource_[idx];
      itArr = std::find(itbArr, iteArr, maskedGlobalIndex);
      if (iteArr != itArr)
      {
        int localIdx = std::distance(itbArr, itArr);
        maskIndexToModify(maskIndexNum) = localMaskIndexOnClientDest[localIdx];
        ++maskIndexNum;
      }
    }
  }

  gridDestination_->modifyMask(maskIndexToModify);
}

/*!
  A transformation from a grid source to grid destination often passes several intermediate grids, which play a role of
temporary grid source and/or grid destination. This function makes sure that global index of original grid source are mapped correctly to
the final grid destination
*/
void CGridTransformation::computeTransformationFromOriginalGridSource(const std::map<size_t, std::vector<std::pair<size_t,double> > >& globaIndexMapFromDestToSource)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client = context->client;

  CTransformationMapping transformationMap(gridDestination_, gridSource_);

    // Then compute transformation mapping among clients
  transformationMap.computeTransformationMapping(globaIndexMapFromDestToSource);

  const std::map<int,std::vector<std::vector<std::pair<size_t,double> > > >& globalIndexToReceive = transformationMap.getGlobalIndexReceivedOnGridDestMapping();
  const std::map<int,std::vector<size_t> >& globalIndexToSend = transformationMap.getGlobalIndexSendToGridDestMapping();

 // Sending global index of original grid source
  std::map<int,std::vector<size_t> >::const_iterator itbSend = globalIndexToSend.begin(), itSend,
                                                     iteSend = globalIndexToSend.end();
  std::vector<size_t>::const_iterator itbArr = globalIndexOfCurrentGridSource_.begin(), itArr,
                                      iteArr = globalIndexOfCurrentGridSource_.end();
 int sendBuffSize = 0;
 for (itSend = itbSend; itSend != iteSend; ++itSend) sendBuffSize += (itSend->second).size();

 typedef unsigned long Scalar;
 unsigned long* sendBuff, *currentSendBuff;
 if (0 != sendBuffSize) sendBuff = new unsigned long [sendBuffSize];
 for (StdSize idx = 0; idx < sendBuffSize; ++idx) sendBuff[idx] = NumTraits<Scalar>::sfmax();

 std::map<int, MPI_Request> requests;

 std::vector<int> permutIndex(globalIndexOfCurrentGridSource_.size());
 typedef XIOSBinarySearchWithIndex<size_t> BinarySearch;
 XIOSAlgorithms::fillInIndex(globalIndexOfCurrentGridSource_.size(), permutIndex);
 XIOSAlgorithms::sortWithIndex<size_t, CVectorStorage>(globalIndexOfCurrentGridSource_, permutIndex);
 BinarySearch searchCurrentSrc(globalIndexOfCurrentGridSource_);
 std::vector<int>::iterator itbIndex = permutIndex.begin(), itIndex,
                            iteIndex = permutIndex.end();

  // Find out local index on grid destination (received)
 int currentBuffPosition = 0;
 for (itSend = itbSend; itSend != iteSend; ++itSend)
 {
   int destRank = itSend->first;
   const std::vector<size_t>& globalIndexOfCurrentGridSourceToSend = itSend->second;
   int countSize = globalIndexOfCurrentGridSourceToSend.size();
   for (int idx = 0; idx < (countSize); ++idx)
   {
     if (searchCurrentSrc.search(itbIndex, iteIndex, globalIndexOfCurrentGridSourceToSend[idx], itIndex))
     {
       sendBuff[idx+currentBuffPosition] = globalIndexOfOriginalGridSource_[*itIndex];
     }
   }
   currentSendBuff = sendBuff + currentBuffPosition;
   MPI_Isend(currentSendBuff, countSize, MPI_UNSIGNED_LONG, destRank, 14, client->intraComm, &requests[destRank]);
   currentBuffPosition += countSize;
 }

 // Receiving global index of grid source sending from current grid source
 std::map<int,std::vector<std::vector<std::pair<size_t,double> > > >::const_iterator itbRecv = globalIndexToReceive.begin(), itRecv,
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

 if (globalIndexOfCurrentGridSource_.size() != nbCurrentGridSource)
 {
   globalIndexOfCurrentGridSource_.resize(nbCurrentGridSource);
   globalIndexOfOriginalGridSource_.resize(nbCurrentGridSource);
   weightOfGlobalIndexOfOriginalGridSource_.resize(nbCurrentGridSource);
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
       globalIndexOfCurrentGridSource_[k] = ((itRecv->second)[idx][i]).first;
       weightOfGlobalIndexOfOriginalGridSource_(k) = ((itRecv->second)[idx][i]).second;
       globalIndexOfOriginalGridSource_[k] = *currentRecvBuff;
       ++k;
     }
   }
 }

 std::map<int, MPI_Request>::iterator itRequest;
 for (itRequest = requests.begin(); itRequest != requests.end(); ++itRequest)
   MPI_Wait(&itRequest->second, MPI_STATUS_IGNORE);

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
  CContextClient* client = context->client;

  CTransformationMapping transformationMap(gridDestination_, originalGridSource_);

  std::map<size_t, std::vector<std::pair<size_t,double> > > globaIndexWeightFromDestToSource;
  int nb = globalIndexOfCurrentGridSource_.size();
  const size_t sfmax = NumTraits<unsigned long>::sfmax();
  for (int idx = 0; idx < nb; ++idx)
  {
    if (sfmax != globalIndexOfOriginalGridSource_[idx])
      globaIndexWeightFromDestToSource[globalIndexOfCurrentGridSource_[idx]].push_back(make_pair(globalIndexOfOriginalGridSource_[idx], weightOfGlobalIndexOfOriginalGridSource_(idx))) ;
  }

  // Then compute transformation mapping among clients
  transformationMap.computeTransformationMapping(globaIndexWeightFromDestToSource);

  const std::map<int,std::vector<std::vector<std::pair<size_t,double> > > >& globalIndexToReceive = transformationMap.getGlobalIndexReceivedOnGridDestMapping();
  const std::map<int,std::vector<size_t> >& globalIndexToSend = transformationMap.getGlobalIndexSendToGridDestMapping();

  CDistributionClient distributionClientDest(client->clientRank, gridDestination_);
  CDistributionClient distributionClientSrc(client->clientRank, originalGridSource_);

  const std::vector<size_t>& globalIndexOnClientDest = distributionClientDest.getGlobalDataIndexSendToServer();
  const std::vector<size_t>& globalIndexOnClientSrc = distributionClientSrc.getGlobalDataIndexSendToServer();

  std::vector<size_t>::const_iterator itbArr, itArr, iteArr;
  std::vector<int>::const_iterator itIndex, itbIndex, iteIndex;
  std::map<int,std::vector<std::vector<std::pair<size_t,double> > > >::const_iterator itbMapRecv, itMapRecv, iteMapRecv;

  std::vector<int> permutIndex;
  typedef XIOSBinarySearchWithIndex<size_t> BinarySearch;

  // Find out local index on grid destination (received)
  XIOSAlgorithms::fillInIndex(globalIndexOnClientDest.size(), permutIndex);
  XIOSAlgorithms::sortWithIndex<size_t, CVectorStorage>(globalIndexOnClientDest, permutIndex);
  itbIndex = permutIndex.begin();
  iteIndex = permutIndex.end();
  BinarySearch searchClientDest(globalIndexOnClientDest);
  itbMapRecv = globalIndexToReceive.begin();
  iteMapRecv = globalIndexToReceive.end();
  for (itMapRecv = itbMapRecv; itMapRecv != iteMapRecv; ++itMapRecv)
  {
    int sourceRank = itMapRecv->first;
    int numGlobalIndex = (itMapRecv->second).size();
    for (int i = 0; i < numGlobalIndex; ++i)
    {
      int vecSize = ((itMapRecv->second)[i]).size();
      std::vector<std::pair<int,double> > tmpVec;
      for (int idx = 0; idx < vecSize; ++idx)
      {
        size_t globalIndex = (itMapRecv->second)[i][idx].first;
        double weight = (itMapRecv->second)[i][idx].second;
        if (searchClientDest.search(itbIndex, iteIndex, globalIndex, itIndex))
        {
          tmpVec.push_back(make_pair(*itIndex, weight));
        }
      }
      localIndexToReceiveOnGridDest_[sourceRank].push_back(tmpVec);
    }
  }

  // Find out local index on grid source (to send)
  std::map<int,std::vector<size_t> >::const_iterator itbMap, itMap, iteMap;
  XIOSAlgorithms::fillInIndex(globalIndexOnClientSrc.size(), permutIndex);
  XIOSAlgorithms::sortWithIndex<size_t, CVectorStorage>(globalIndexOnClientSrc, permutIndex);
  itbIndex = permutIndex.begin();
  iteIndex = permutIndex.end();
  BinarySearch searchClientSrc(globalIndexOnClientSrc);
  itbMap = globalIndexToSend.begin();
  iteMap = globalIndexToSend.end();
  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    int destRank = itMap->first;
    int vecSize = itMap->second.size();
    localIndexToSendFromGridSource_[destRank].resize(vecSize);
    for (int idx = 0; idx < vecSize; ++idx)
    {
      if (searchClientSrc.search(itbIndex, iteIndex, itMap->second[idx], itIndex))
      {
        localIndexToSendFromGridSource_[destRank](idx) = *itIndex;
      }
    }
  }
}

/*!
  Local index of data which need sending from the grid source
  \return local index of data
*/
const std::map<int, CArray<int,1> >& CGridTransformation::getLocalIndexToSendFromGridSource() const
{
  return localIndexToSendFromGridSource_;
}

/*!
  Local index of data which will be received on the grid destination
  \return local index of data
*/
const std::map<int,std::vector<std::vector<std::pair<int,double> > > >& CGridTransformation::getLocalIndexToReceiveOnGridDest() const
{
  return localIndexToReceiveOnGridDest_;
}

}
