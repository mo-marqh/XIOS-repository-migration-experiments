#include "grid_transformation.hpp"
#include "axis_inverse.hpp"
#include "transformation_mapping.hpp"
#include "transformation_enum.hpp"
#include "axis_algorithm_transformation.hpp"

namespace xios {
CGridTransformation::CGridTransformation(CGrid* destination, CGrid* source)
: gridSource_(source), gridDestination_(destination)
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

  gridSourceDimensionSize_ = gridSource_->getGlobalDimension();
  gridDestinationDimensionSize_ = gridDestination_->getGlobalDimension();
  initializeAlgorithms();
}

CGridTransformation::~CGridTransformation()
{
  std::map<int, std::vector<CGenericAlgorithmTransformation*> >::const_iterator itb = algoTransformation_.begin(), it,
                                                                                ite = algoTransformation_.end();
  for (it = itb; it != ite; ++it)
    for (int idx = 0; idx < (it->second).size(); ++idx)
      delete (it->second)[idx];

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
      if (axisListDestP[i]->hasTransformation())
      {
        CGenericAlgorithmTransformation* algo = 0;
        CAxis::TransMapTypes trans = axisListDestP[i]->getAllTransformations();
        CAxis::TransMapTypes::const_iterator itb = trans.begin(), it,
                                             ite = trans.end();
        std::vector<ETranformationType> algoAxis;
        for (it = itb; it != ite; ++it)
        {
          algoAxis.push_back(it->first);
        }
        algo = new CAxisAlgorithmTransformation(axisListDestP[i], axisListSrcP[i], algoAxis);
        algoTransformation_[axisPositionInGrid[i]].push_back(algo);
      }
    }
  }
}

void CGridTransformation::initializeDomainAlgorithms()
{

}

/*!
  Compute index mapping representing transformation between two grids
  Each domain and each axis can contain some information of transformation, these information are then converted into
form of global index mapping reprensenting transformation between two grids.
*/
void CGridTransformation::computeTransformation()
{
  const CArray<size_t,1>& globalIndexGridDestSendToServer = gridDestination_->getDistributionClient()->getGlobalDataIndexSendToServer();
  std::map<int, std::vector<CGenericAlgorithmTransformation*> >::const_iterator itbMap, itMap, iteMap;
  itbMap = algoTransformation_.begin();
  iteMap = algoTransformation_.end();

  std::vector<CGenericAlgorithmTransformation*>::const_iterator itbVec, itVec, iteVec;

  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    int elementPosition = itMap->first;
    itbVec = (itMap->second).begin();
    iteVec = (itMap->second).end();
    for (itVec = itbVec; itVec != iteVec; ++itVec)
    {
      (*itVec)->computeGlobalSourceIndex(elementPosition,
                                         gridDestinationDimensionSize_,
                                         globalIndexGridDestSendToServer,
                                         globaIndexMapFromDestToSource_);
    }
  }
}

/*!
  Compute transformation mapping between grid source and grid destination
  The transformation between grid source and grid destination is represented in form of mapping between global index
of two grids. Then local index mapping between data on each grid will be found out thanks to these global indexes
*/
void CGridTransformation::computeTransformationMapping()
{
  CTransformationMapping transformationMap(gridDestination_, gridSource_);

  // First of all, need to compute global index mapping representing transformation algorithms
  computeTransformation();

  // Then compute transformation mapping among clients
  transformationMap.computeTransformationMapping(globaIndexMapFromDestToSource_);

  const std::map<int,std::vector<std::vector<size_t> > >& globalIndexToReceive = transformationMap.getGlobalIndexReceivedOnGridDestMapping();
  const std::map<int,std::vector<size_t> >& globalIndexToSend = transformationMap.getGlobalIndexSendToGridDestMapping();

  CArray<int, 1> localIndexOnClientDest = gridDestination_->getDistributionClient()->getLocalDataIndexOnClient();
  CArray<size_t,1> globalIndexOnClientDest = gridDestination_->getDistributionClient()->getGlobalDataIndexSendToServer();

  CArray<int, 1> localIndexOnClientSrc = gridSource_->getDistributionClient()->getLocalDataIndexOnClient();
  CArray<size_t,1> globalIndexOnClientSrc = gridSource_->getDistributionClient()->getGlobalDataIndexSendToServer();

  std::vector<size_t>::const_iterator itbVec, itVec, iteVec;
  CArray<size_t, 1>::iterator itbArr, itArr, iteArr;

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
//          (*localIndexToReceiveOnGridDest_[sourceRank][i])(idx) = localIndexOnClientDest(localIdx); // Local index of un-extracted data (only domain)
          (*localIndexToReceiveOnGridDest_[sourceRank][i])(idx) = (localIdx); // Local index of extracted data
        }
      }
    }
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
