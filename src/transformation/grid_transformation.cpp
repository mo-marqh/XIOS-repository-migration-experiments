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
#include "mpi_tag.hpp"
#include <boost/unordered_map.hpp>

namespace xios {
CGridTransformation::CGridTransformation(CGrid* destination, CGrid* source)
: gridSource_(source), gridDestination_(destination), originalGridSource_(source),
  algoTypes_(), nbAlgos_(0), currentGridIndexToOriginalGridIndex_(), tempGrids_(),
  auxInputs_(), dynamicalTransformation_(false)

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

  initializeMappingOfOriginalGridSource();
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

  // Initialize algorithms
  initializeAlgorithms();

  ListAlgoType::const_iterator itb = listAlgos_.begin(),
                               ite = listAlgos_.end(), it;

  for (it = itb; it != ite; ++it)
  {
    ETranformationType transType = (it->second).first;
    if (!isSpecialTransformation(transType)) ++nbAlgos_;
  }
}

CGridTransformation::~CGridTransformation()
{
  std::vector<CGenericAlgorithmTransformation*>::const_iterator itb = algoTransformation_.begin(), it,
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
        std::vector<StdString> auxInput = (it->second)->checkAuxInputs();
        for (int idx = 0; idx < auxInput.size(); ++idx) auxInputs_.push_back(auxInput[idx]);
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
        std::vector<StdString> auxInput = (it->second)->checkAuxInputs();
        for (int idx = 0; idx < auxInput.size(); ++idx) auxInputs_.push_back(auxInput[idx]);
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
void CGridTransformation::setUpGrid(int elementPositionInGrid, ETranformationType transType, int nbTransformation)
{
  if (!tempGrids_.empty() && (getNbAlgo()-1) == tempGrids_.size())
  {
    gridSource_ = tempGrids_[nbTransformation];
    return;
  }

  std::vector<CAxis*> axisListDestP = gridDestination_->getAxis();
  std::vector<CAxis*> axisListSrcP = gridSource_->getAxis(), axisSrc;

  std::vector<CDomain*> domListDestP = gridDestination_->getDomains();
  std::vector<CDomain*> domListSrcP = gridSource_->getDomains(), domainSrc;

  int axisIndex = -1, domainIndex = -1;
  switch (transType)
  {
    case TRANS_INTERPOLATE_DOMAIN:
    case TRANS_ZOOM_DOMAIN:
      domainIndex = elementPosition2DomainPositionInGrid_[elementPositionInGrid];
      break;

    case TRANS_INTERPOLATE_AXIS:
    case TRANS_ZOOM_AXIS:
    case TRANS_INVERSE_AXIS:
      axisIndex =  elementPosition2AxisPositionInGrid_[elementPositionInGrid];
      break;
    default:
      break;
  }

  for (int idx = 0; idx < axisListSrcP.size(); ++idx)
  {
    CAxis* axis = CAxis::createAxis();
    if (axisIndex != idx) axis->axis_ref.setValue(axisListSrcP[idx]->getId());
    else axis->axis_ref.setValue(axisListDestP[idx]->getId());
    axis->solveRefInheritance(true);
    axis->checkAttributesOnClient();
    axisSrc.push_back(axis);
  }

  for (int idx = 0; idx < domListSrcP.size(); ++idx)
  {
    CDomain* domain = CDomain::createDomain();
    if (domainIndex != idx) domain->domain_ref.setValue(domListSrcP[idx]->getId());
    else domain->domain_ref.setValue(domListDestP[idx]->getId());
    domain->solveRefInheritance(true);
    domain->checkAttributesOnClient();
    domainSrc.push_back(domain);
  }

  gridSource_ = CGrid::createGrid(domainSrc, axisSrc, gridDestination_->axis_domain_order);
  gridSource_->computeGridGlobalDimension(domainSrc, axisSrc, gridDestination_->axis_domain_order);

  tempGrids_.push_back(gridSource_);
}

/*!
  Perform all transformations
  For each transformation, there are some things to do:
  -) Chose the correct algorithm by transformation type and position of element
  -) Calculate the mapping of global index between the current grid source and grid destination
  -) Calculate the mapping of global index between current grid DESTINATION and ORIGINAL grid SOURCE
  -) Make current grid destination become grid source in the next transformation
*/
void CGridTransformation::computeAll(const std::vector<CArray<double,1>* >& dataAuxInputs)
{
  if (nbAlgos_ < 1) return;
  if (!auxInputs_.empty() && !dynamicalTransformation_) { dynamicalTransformation_ = true; return; }
  if (dynamicalTransformation_) DestinationIndexMap().swap(currentGridIndexToOriginalGridIndex_);  // Reset map

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
    DestinationIndexMap globaIndexWeightFromDestToSource;

    // First of all, select an algorithm
    if (!dynamicalTransformation_ || (algoTransformation_.size() < listAlgos_.size()))
    {
      selectAlgo(elementPositionInGrid, transType, transformationOrder, algoTypes_[std::distance(itb, it)]);
      algo = algoTransformation_.back();
    }
    else
      algo = algoTransformation_[std::distance(itb, it)];

    if (0 != algo) // Only registered transformation can be executed
    {
      algo->computeIndexSourceMapping(dataAuxInputs);

      // Recalculate the distribution of grid destination
      CDistributionClient distributionClientDest(client->clientRank, gridDestination_);
      const CDistributionClient::GlobalLocalDataMap& globalLocalIndexGridDestSendToServer = distributionClientDest.getGlobalLocalDataSendToServer();

      // ComputeTransformation of global index of each element
      std::vector<int> gridDestinationDimensionSize = gridDestination_->getGlobalDimension();
      std::vector<int> gridSrcDimensionSize = gridSource_->getGlobalDimension();
      int elementPosition = it->first;
      algo->computeGlobalSourceIndex(elementPosition,
                                     gridDestinationDimensionSize,
                                     gridSrcDimensionSize,
                                     globalLocalIndexGridDestSendToServer,
                                     globaIndexWeightFromDestToSource);

      if (1 < nbAlgos_)
      {
        // Compute transformation of global indexes among grids
        computeTransformationFromOriginalGridSource(globaIndexWeightFromDestToSource);

        // Now grid destination becomes grid source in a new transformation
        if (nbAgloTransformation != (nbAlgos_-1)) setUpGrid(elementPositionInGrid, transType, nbAgloTransformation);
      }
      else
      {
        currentGridIndexToOriginalGridIndex_.swap(globaIndexWeightFromDestToSource);
      }

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
  const CDistributionClient::GlobalLocalDataMap& globalIndexOnClientDest = distributionClientDest.getGlobalLocalDataSendToServer();

  CDistributionClient::GlobalLocalDataMap::const_iterator itbArr, itArr, iteArr;
  itbArr = globalIndexOnClientDest.begin();
  iteArr = globalIndexOnClientDest.end();

  DestinationIndexMap::const_iterator iteGlobalMap = currentGridIndexToOriginalGridIndex_.end();
  const size_t sfmax = NumTraits<unsigned long>::sfmax();
  int maskIndexNum = 0;
  for (itArr = itbArr; itArr != iteArr; ++itArr)
  {
    if (iteGlobalMap != currentGridIndexToOriginalGridIndex_.find(itArr->first))
    {
      const std::vector<std::pair<int, std::pair<size_t,double> > >& vecIndex = currentGridIndexToOriginalGridIndex_[itArr->first];
      for (int idx = 0; idx < vecIndex.size(); ++idx)
      {
        if (sfmax == (vecIndex[idx].second).first)
        {
          ++maskIndexNum;
          break;
        }
      }
    }
  }

  CArray<int,1> maskIndexToModify(maskIndexNum);
  maskIndexNum = 0;
  for (itArr = itbArr; itArr != iteArr; ++itArr)
  {
    if (iteGlobalMap != currentGridIndexToOriginalGridIndex_.find(itArr->first))
    {
      const std::vector<std::pair<int, std::pair<size_t,double> > >& vecIndex = currentGridIndexToOriginalGridIndex_[itArr->first];
      for (int idx = 0; idx < vecIndex.size(); ++idx)
      {
        if (sfmax == (vecIndex[idx].second).first)
        {
          int localIdx = std::distance(itbArr, itArr);
          maskIndexToModify(maskIndexNum) = localMaskIndexOnClientDest[localIdx];
          ++maskIndexNum;
          break;
        }
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
void CGridTransformation::computeTransformationFromOriginalGridSource(const DestinationIndexMap& globaIndexMapFromDestToSource)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client = context->client;

  if (currentGridIndexToOriginalGridIndex_.empty())
  {
    currentGridIndexToOriginalGridIndex_ = globaIndexMapFromDestToSource;
    return;
  }

  CTransformationMapping transformationMap(gridDestination_, gridSource_);

    // Then compute transformation mapping among clients
  transformationMap.computeTransformationMapping(globaIndexMapFromDestToSource);

  const CTransformationMapping::ReceivedIndexMap& globalIndexToReceive = transformationMap.getGlobalIndexReceivedOnGridDestMapping();
  const CTransformationMapping::SentIndexMap& globalIndexToSend = transformationMap.getGlobalIndexSendToGridDestMapping();

 // Sending global index of original grid source
  CTransformationMapping::SentIndexMap::const_iterator itbSend = globalIndexToSend.begin(), itSend,
                                                       iteSend = globalIndexToSend.end();
 int sendBuffSize = 0;
 for (itSend = itbSend; itSend != iteSend; ++itSend) sendBuffSize += (itSend->second).size();
 // We use the first element of each block to send number of element in this block
 sendBuffSize += globalIndexToSend.size();


 typedef unsigned long Scalar;
 Scalar* sendBuff, *currentSendBuff;
 if (0 != sendBuffSize) sendBuff = new Scalar [sendBuffSize];
 for (StdSize idx = 0; idx < sendBuffSize; ++idx) sendBuff[idx] = NumTraits<Scalar>::sfmax();

 std::map<int, MPI_Request> requestsCurrentGrid, requestsOriginalGridGlobalIndex, requestsOriginalGridLocalIndex, requestsWeightGrid;
 DestinationIndexMap::const_iterator iteGlobalIndex = currentGridIndexToOriginalGridIndex_.end();

  // Only send global index of original source corresponding to non-masked index
  // Use first position of each block to specify the number of elemnt in this block
 int globalIndexOriginalSrcSendBuffSize = 0;
 int currentBuffPosition = 0;
 for (itSend = itbSend; itSend != iteSend; ++itSend)
 {
   int destRank = itSend->first;
   const std::vector<std::pair<int, size_t> >& globalIndexOfCurrentGridSourceToSend = itSend->second;
   int countSize  = globalIndexOfCurrentGridSourceToSend.size();
   size_t countBlock = 0;
   for (int idx = 0; idx < countSize; ++idx)
   {
     size_t index = globalIndexOfCurrentGridSourceToSend[idx].second;
     if (iteGlobalIndex != currentGridIndexToOriginalGridIndex_.find(index))
     {
       globalIndexOriginalSrcSendBuffSize += currentGridIndexToOriginalGridIndex_[index].size() + 1; // 1 for number of elements in this block
       sendBuff[idx+currentBuffPosition+1] = index;
       countBlock += currentGridIndexToOriginalGridIndex_[index].size() + 1;
     }
   }
   sendBuff[currentBuffPosition] = countBlock;
   currentSendBuff = sendBuff + currentBuffPosition;
   MPI_Isend(currentSendBuff, countSize +1, MPI_UNSIGNED_LONG, destRank, MPI_GRID_TRANSFORMATION_CURRENT_GRID_INDEX, client->intraComm, &requestsCurrentGrid[destRank]);
   currentBuffPosition += countSize + 1;
 }

 Scalar* sendOriginalGlobalIndexBuff, *currentOriginalGlobalIndexSendBuff;
 if (0 != globalIndexOriginalSrcSendBuffSize) sendOriginalGlobalIndexBuff = new Scalar [globalIndexOriginalSrcSendBuffSize];
 double* sendOriginalWeightBuff, *currentOriginalWeightSendBuff;
 if (0 != globalIndexOriginalSrcSendBuffSize) sendOriginalWeightBuff = new double [globalIndexOriginalSrcSendBuffSize];

 currentBuffPosition = 0;
 for (itSend = itbSend; itSend != iteSend; ++itSend)
 {
   int destRank = itSend->first;
   const std::vector<std::pair<int, size_t> >& globalIndexOfCurrentGridSourceToSend = itSend->second;
   int countSize = globalIndexOfCurrentGridSourceToSend.size();
   int increaseStep = 0;
   for (int idx = 0; idx < countSize; ++idx)
   {
     size_t index = globalIndexOfCurrentGridSourceToSend[idx].second;
     if (iteGlobalIndex != currentGridIndexToOriginalGridIndex_.find(index))
     {
       size_t vectorSize = currentGridIndexToOriginalGridIndex_[index].size();
       sendOriginalGlobalIndexBuff[currentBuffPosition+increaseStep]  = vectorSize;
       sendOriginalWeightBuff[currentBuffPosition+increaseStep] = (double)vectorSize;
       const std::vector<std::pair<int, std::pair<size_t,double> > >& indexWeightPair = currentGridIndexToOriginalGridIndex_[index];
       for (size_t i = 0; i < vectorSize; ++i)
       {
         ++increaseStep;
         sendOriginalGlobalIndexBuff[currentBuffPosition+increaseStep]  = (indexWeightPair[i].second).first;
         sendOriginalWeightBuff[currentBuffPosition+increaseStep] = (indexWeightPair[i].second).second;
       }
       ++increaseStep;
     }
   }

   currentOriginalGlobalIndexSendBuff = sendOriginalGlobalIndexBuff + currentBuffPosition;
   currentOriginalWeightSendBuff = sendOriginalWeightBuff + currentBuffPosition;
   if (0 != increaseStep)
   {
     MPI_Isend(currentOriginalGlobalIndexSendBuff, increaseStep, MPI_UNSIGNED_LONG, destRank,
               MPI_GRID_TRANSFORMATION_ORIGINAL_GRID_GLOBAL_INDEX, client->intraComm, &requestsOriginalGridGlobalIndex[destRank]);
     MPI_Isend(currentOriginalWeightSendBuff, increaseStep, MPI_DOUBLE, destRank,
               MPI_GRID_TRANSFORMATION_ORIGINAL_GRID_WEIGHT, client->intraComm, &requestsWeightGrid[destRank]);
   }
   currentBuffPosition += increaseStep;
 }


 // Receiving global index of grid source sending from current grid source
 CTransformationMapping::ReceivedIndexMap::const_iterator itbRecv = globalIndexToReceive.begin(), itRecv,
                                                          iteRecv = globalIndexToReceive.end();
 int recvBuffSize = 0;
 for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv) recvBuffSize += (itRecv->second).size();
 recvBuffSize += globalIndexToReceive.size();

 Scalar* recvBuff, *currentRecvBuff;
 if (0 != recvBuffSize) recvBuff = new Scalar [recvBuffSize];
 for (StdSize idx = 0; idx < recvBuffSize; ++idx) recvBuff[idx] = NumTraits<Scalar>::sfmax();

 std::map<int,int> countBlockMap;
 int globalIndexOriginalSrcRecvBuffSize = 0;
 int currentRecvBuffPosition = 0;
 for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv)
 {
   MPI_Status status;
   int srcRank = itRecv->first;
   int countSize = (itRecv->second).size();
   currentRecvBuff = recvBuff + currentRecvBuffPosition;
   MPI_Recv(currentRecvBuff, countSize +1, MPI_UNSIGNED_LONG, srcRank, MPI_GRID_TRANSFORMATION_CURRENT_GRID_INDEX, client->intraComm, &status);
   globalIndexOriginalSrcRecvBuffSize += *currentRecvBuff;
   countBlockMap[srcRank] = *currentRecvBuff;
   currentRecvBuffPosition += countSize +1;
 }

 Scalar* recvOriginalGlobalIndexBuff, *currentOriginalGlobalIndexRecvBuff;
 if (0 != globalIndexOriginalSrcRecvBuffSize) recvOriginalGlobalIndexBuff = new Scalar [globalIndexOriginalSrcRecvBuffSize];
 double* recvOriginalWeightBuff, *currentOriginalWeightRecvBuff;
 if (0 != globalIndexOriginalSrcRecvBuffSize) recvOriginalWeightBuff = new double [globalIndexOriginalSrcRecvBuffSize];

 int countBlock = 0;
 currentRecvBuffPosition = 0;
 currentBuffPosition = 0;
 for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv)
 {
   MPI_Status statusGlobalIndex, statusLocalIndex, statusWeight;
   int srcRank = itRecv->first;
   countBlock = countBlockMap[srcRank];
   currentOriginalGlobalIndexRecvBuff = recvOriginalGlobalIndexBuff + currentBuffPosition;
   currentOriginalWeightRecvBuff = recvOriginalWeightBuff + currentBuffPosition;
   if (0 != countBlock)
   {
     MPI_Recv(currentOriginalGlobalIndexRecvBuff, countBlock, MPI_UNSIGNED_LONG, srcRank, MPI_GRID_TRANSFORMATION_ORIGINAL_GRID_GLOBAL_INDEX, client->intraComm, &statusGlobalIndex);
     MPI_Recv(currentOriginalWeightRecvBuff, countBlock, MPI_DOUBLE, srcRank, MPI_GRID_TRANSFORMATION_ORIGINAL_GRID_WEIGHT, client->intraComm, &statusWeight);
   }
   currentBuffPosition += countBlock;
 }

 // We process everything in here, even case of masked index
 // The way to process masked index needs discussing
 const size_t sfmax = NumTraits<unsigned long>::sfmax();
 DestinationIndexMap currentToOriginalTmp;

 currentRecvBuffPosition = 0;
 currentRecvBuff = recvBuff;
 currentOriginalGlobalIndexRecvBuff  = recvOriginalGlobalIndexBuff;
 currentOriginalWeightRecvBuff = recvOriginalWeightBuff;
 for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv)
 {
   int countBlockRank = countBlockMap[itRecv->first];

   ++currentRecvBuff;  // it's very subtle here, pay attention
   int countSize = (itRecv->second).size();
   for (int idx = 0; idx < countSize; ++idx)
   {
      ++currentRecvBuff;
     int ssize = (itRecv->second)[idx].size();
     if (sfmax != *currentRecvBuff)
     {
       if (0 != countBlockRank)
       {
         countBlock = *(currentOriginalGlobalIndexRecvBuff+currentRecvBuffPosition);
         for (int i = 0; i < ssize; ++i)
         {
           for (int j = 0; j < countBlock; ++j)
           {
             size_t globalOriginalIndex = *(currentOriginalGlobalIndexRecvBuff+currentRecvBuffPosition+j+1);
             int currentGridLocalIndex = (itRecv->second)[idx][i].first;
             double weightGlobal = *(currentOriginalWeightRecvBuff+currentRecvBuffPosition+j+1) * (itRecv->second)[idx][i].second.second;
             currentToOriginalTmp[(itRecv->second)[idx][i].second.first].push_back(make_pair(currentGridLocalIndex,make_pair(globalOriginalIndex,weightGlobal)));
           }
         }
         currentRecvBuffPosition += countBlock+1;
       }
     }
//     else
//     {
//       for (int i = 0; i < ssize; ++i)
//       {
//         currentToOriginalTmp[(itRecv->second)[idx][i].first].push_back(make_pair(sfmax,1.0));
//       }
//     }
   }
 }

 currentGridIndexToOriginalGridIndex_.swap(currentToOriginalTmp);

 std::map<int, MPI_Request>::iterator itRequest;
 for (itRequest = requestsCurrentGrid.begin(); itRequest != requestsCurrentGrid.end(); ++itRequest)
   MPI_Wait(&itRequest->second, MPI_STATUS_IGNORE);
 for (itRequest = requestsOriginalGridGlobalIndex.begin(); itRequest != requestsOriginalGridGlobalIndex.end(); ++itRequest)
   MPI_Wait(&itRequest->second, MPI_STATUS_IGNORE);
 for (itRequest = requestsWeightGrid.begin(); itRequest != requestsWeightGrid.end(); ++itRequest)
   MPI_Wait(&itRequest->second, MPI_STATUS_IGNORE);

 if (0 != sendBuffSize) delete [] sendBuff;
 if (0 != recvBuffSize) delete [] recvBuff;
 if (0 != globalIndexOriginalSrcSendBuffSize) delete [] sendOriginalGlobalIndexBuff;
 if (0 != globalIndexOriginalSrcSendBuffSize) delete [] sendOriginalWeightBuff;
 if (0 != globalIndexOriginalSrcRecvBuffSize) delete [] recvOriginalGlobalIndexBuff;
 if (0 != globalIndexOriginalSrcRecvBuffSize) delete [] recvOriginalWeightBuff;
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

  transformationMap.computeTransformationMapping(currentGridIndexToOriginalGridIndex_);

  const CTransformationMapping::ReceivedIndexMap& globalIndexToReceive = transformationMap.getGlobalIndexReceivedOnGridDestMapping();
  CTransformationMapping::ReceivedIndexMap::const_iterator itbMapRecv, itMapRecv, iteMapRecv;
  itbMapRecv = globalIndexToReceive.begin();
  iteMapRecv = globalIndexToReceive.end();
  for (itMapRecv = itbMapRecv; itMapRecv != iteMapRecv; ++itMapRecv)
  {
    int sourceRank = itMapRecv->first;
    int numGlobalIndex = (itMapRecv->second).size();
    localIndexToReceiveOnGridDest_[sourceRank].resize(numGlobalIndex);
    for (int i = 0; i < numGlobalIndex; ++i)
    {
      int vecSize = ((itMapRecv->second)[i]).size();
      for (int idx = 0; idx < vecSize; ++idx)
      {
        const std::pair<int, std::pair<size_t,double> >& tmpPair = (itMapRecv->second)[i][idx];
        localIndexToReceiveOnGridDest_[sourceRank][i].push_back(make_pair(tmpPair.first, tmpPair.second.second));
      }
    }
  }

  // Find out local index on grid source (to send)
  const CTransformationMapping::SentIndexMap& globalIndexToSend = transformationMap.getGlobalIndexSendToGridDestMapping();
  CTransformationMapping::SentIndexMap::const_iterator itbMap, itMap, iteMap;
  itbMap = globalIndexToSend.begin();
  iteMap = globalIndexToSend.end();
  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    int destRank = itMap->first;
    int vecSize = itMap->second.size();
    localIndexToSendFromGridSource_[destRank].resize(vecSize);
    for (int idx = 0; idx < vecSize; ++idx)
    {
      localIndexToSendFromGridSource_[destRank](idx) = itMap->second[idx].first;
    }
  }
}

bool CGridTransformation::isSpecialTransformation(ETranformationType transType)
{
  bool res;
  switch (transType)
  {
    case TRANS_GENERATE_RECTILINEAR_DOMAIN:
     res = true;
     break;
    default:
     res = false;
     break;
  }

  return res;
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
