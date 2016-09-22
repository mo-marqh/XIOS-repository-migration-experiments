/*!
   \file domain_algorithm_expand.cpp
   \author Ha NGUYEN
   \since 08 Aug 2016
   \date 19 Sep 2016

   \brief Algorithm for expanding an domain.
 */
#include "domain_algorithm_expand.hpp"
#include "expand_domain.hpp"
#include "mesh.hpp"
#include "domain.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "context.hpp"
#include "context_client.hpp"

namespace xios {
CGenericAlgorithmTransformation* CDomainAlgorithmExpand::create(CGrid* gridDst, CGrid* gridSrc,
                                                               CTransformation<CDomain>* transformation,
                                                               int elementPositionInGrid,
                                                               std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                               std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                               std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                               std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                               std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                               std::map<int, int>& elementPositionInGridDst2DomainPosition)
{
  std::vector<CDomain*> domainListDestP = gridDst->getDomains();
  std::vector<CDomain*> domainListSrcP  = gridSrc->getDomains();

  CExpandDomain* expandDomain = dynamic_cast<CExpandDomain*> (transformation);
  int domainDstIndex = elementPositionInGridDst2AxisPosition[elementPositionInGrid];
  int domainSrcIndex = elementPositionInGridSrc2AxisPosition[elementPositionInGrid];

  return (new CDomainAlgorithmExpand(domainListDestP[domainDstIndex], domainListSrcP[domainSrcIndex], expandDomain));
}

bool CDomainAlgorithmExpand::registerTrans()
{
  CGridTransformationFactory<CDomain>::registerTransformation(TRANS_EXPAND_DOMAIN, create);
}

CDomainAlgorithmExpand::CDomainAlgorithmExpand(CDomain* domainDestination,
                                               CDomain* domainSource,
                                               CExpandDomain* expandDomain)
: CDomainAlgorithmTransformation(domainDestination, domainSource)
{
  if (domainDestination == domainSource)
  {
    ERROR("CDomainAlgorithmExpand::CDomainAlgorithmExpand(CDomain* domainDestination,CDomain* domainSource, CExpandDomain* expandDomain)",
           << "Domain source and domain destination are the same. Please make sure domain destination refers to domain source" << std::endl
           << "Domain source " <<domainSource->getId() << std::endl
           << "Domain destination " <<domainDestination->getId() << std::endl);
  }

//  if (!domainDestination->hasRefTo(domainSource))
//  {
//    ERROR("CDomainAlgorithmExpand::CDomainAlgorithmExpand(CDomain* domainDestination,CDomain* domainSource, CExpandDomain* expandDomain)",
//           << "Domain domain destination must refer to domain source (directly or indirectly) by domain_ref " << std::endl
//           << "Domain source " <<domainSource->getId() << std::endl
//           << "Domain destination " <<domainDestination->getId() << std::endl);
//  }

  this->type_ = (ELEMENT_MODIFICATION_WITH_DATA);
  expandDomain->checkValid(domainDestination);

  switch (expandDomain->type)
  {
    case CExpandDomain::type_attr::node :
      expandDomainNodeConnectivity(domainDestination,
                                   domainSource);
      break;
    case CExpandDomain::type_attr::edge :
      expandDomainEdgeConnectivity(domainDestination,
                                   domainSource);
      break;
    default:
      break;
  }
}

/*!
 *  Expand domain with edge-type neighbor
 *  \param[in/out] domainDestination domain destination and will be modified
 *  \param[in] domainSource domain source
 */
void CDomainAlgorithmExpand::expandDomainEdgeConnectivity(CDomain* domainDestination,
                                                          CDomain* domainSource)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;

  CArray<double,2>& bounds_lon_src = domainSource->bounds_lon_1d;
  CArray<double,2>& bounds_lat_src = domainSource->bounds_lat_1d;
  CArray<int,2> neighborsSrc;

  int type = 1; // For edge
  CMesh mesh;
  mesh.getGlobalNghbFaces(type, client->intraComm, domainSource->i_index, bounds_lon_src, bounds_lat_src, neighborsSrc);
  updateDomainAttributes(domainDestination, domainSource, neighborsSrc);
}

/*!
 *  Expand domain with node-type neighbor
 *  \param[in/out] domainDestination domain destination and will be modified
 *  \param[in] domainSource domain source
 */
void CDomainAlgorithmExpand::expandDomainNodeConnectivity(CDomain* domainDestination,
                                                          CDomain* domainSource)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;

  CArray<double,2>& bounds_lon_src = domainSource->bounds_lon_1d;
  CArray<double,2>& bounds_lat_src = domainSource->bounds_lat_1d;
  CArray<int,2> neighborsSrc;

  int type = 0; // For node
  CMesh mesh;
  mesh.getGlobalNghbFaces(type, client->intraComm, domainSource->i_index, bounds_lon_src, bounds_lat_src, neighborsSrc);
  updateDomainAttributes(domainDestination, domainSource, neighborsSrc);
}

/*!
 *  Extend domain destination and update its attributes
 *  Suppose that domain destination and domain source have the same values for all attributes (by inheritance)
 *  \param [in/out] domainDestination domain destination
 *  \param [in] domainSource domain source
 *  \param [in] neighborsDomainSrc domain extended part
 */
void CDomainAlgorithmExpand::updateDomainAttributes(CDomain* domainDestination,
                                                    CDomain* domainSource,
                                                    CArray<int,2>& neighborsDomainSrc)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;

  // First of all, "copy" all attributes of domain source to domain destination
  StdString domainDstRef = (!domainDestination->domain_ref.isEmpty()) ? domainDestination->domain_ref.getValue()
                                                                      : "";
  if (domainDstRef != domainSource->getId())
  {
    domainDestination->domain_ref.setValue(domainSource->getId());
    domainDestination->solveRefInheritance(true);
  }

  if (domainDstRef.empty()) domainDestination->domain_ref.reset();
  else domainDestination->domain_ref.setValue(domainDstRef);

  // Now extend domain destination
  int niGlob = domainSource->ni_glo;
  CArray<bool,1>& mask_1d_src = domainSource->mask_1d;
  CArray<int,1>& i_index_src = domainSource->i_index;
  CArray<double,1>& lon_src = domainSource->lonvalue_1d;
  CArray<double,1>& lat_src = domainSource->latvalue_1d;
  CArray<double,2>& bounds_lon_src = domainSource->bounds_lon_1d;
  CArray<double,2>& bounds_lat_src = domainSource->bounds_lat_1d;
  CArray<int,1>& data_i_index_src = domainSource->data_i_index;

  int oldNbLocal = i_index_src.numElements(), index, globalIndex;
  // Uncompress data_i_index
  CArray<int,1> data_i_index_src_full(oldNbLocal);
  int nbUnMaskedPointOnLocalDomain = 0;
  data_i_index_src_full = -1; // Suppose all values are masked
  for (int idx = 0; idx < data_i_index_src.numElements(); ++idx)
  {
    int dataIdx = data_i_index_src(idx);
    if ((0 <= dataIdx) && (dataIdx < oldNbLocal))
    {
      data_i_index_src_full(nbUnMaskedPointOnLocalDomain) = dataIdx;
      ++nbUnMaskedPointOnLocalDomain;
    }
  }

  CArray<bool,1>& mask_1d_dst = domainDestination->mask_1d;
  CArray<int,1>& i_index_dst = domainDestination->i_index;
  CArray<int,1>& j_index_dst = domainDestination->j_index;
  CArray<double,1>& lon_dst = domainDestination->lonvalue_1d;
  CArray<double,1>& lat_dst = domainDestination->latvalue_1d;
  CArray<double,2>& bounds_lon_dst = domainDestination->bounds_lon_1d;
  CArray<double,2>& bounds_lat_dst = domainDestination->bounds_lat_1d;
  CArray<int,1>& data_i_index_dst = domainDestination->data_i_index;
  CArray<int,1>& data_j_index_dst = domainDestination->data_j_index;

  // Resize all array-like attributes of domain destination
  int nbNeighbor    = neighborsDomainSrc.shape()[1];
  int newNbLocalDst = nbNeighbor + oldNbLocal;
  int nVertex       = bounds_lon_dst.shape()[0];

  mask_1d_dst.resizeAndPreserve(newNbLocalDst);
  i_index_dst.resizeAndPreserve(newNbLocalDst);
  j_index_dst.resizeAndPreserve(newNbLocalDst);
  lon_dst.resizeAndPreserve(newNbLocalDst);
  lat_dst.resizeAndPreserve(newNbLocalDst);
  bounds_lon_dst.resizeAndPreserve(nVertex, newNbLocalDst);
  bounds_lat_dst.resizeAndPreserve(nVertex, newNbLocalDst);
  CArray<int,1> data_i_index_dst_full(newNbLocalDst);
  data_i_index_dst_full(Range(0,oldNbLocal-1)) = data_i_index_src_full;
  data_i_index_dst_full(Range(oldNbLocal,newNbLocalDst-1)) = -1;

  // 1. Fill in array relating to global index (i_index, j_index, transmap, etc, ...)
  // Global index mapping between destination and source
  this->transformationMapping_.resize(1);
  this->transformationWeight_.resize(1);
  TransformationIndexMap& transMap = this->transformationMapping_[0];
  TransformationWeightMap& transWeight = this->transformationWeight_[0];

  transMap.rehash(std::ceil(newNbLocalDst/transMap.max_load_factor()));
  transWeight.rehash(std::ceil(newNbLocalDst/transWeight.max_load_factor()));
  // First, index mapping for local domain
  for (int idx = 0; idx < oldNbLocal; ++idx)
  {
    index = i_index_dst(idx);
    transMap[index].push_back(index);
    transWeight[index].push_back(1.0);
  }
  // Then, index mapping for extended part
  for (int idx = 0; idx < nbNeighbor; ++idx)
  {
    index = idx + oldNbLocal;
    globalIndex = neighborsDomainSrc(0,idx);
    i_index_dst(index) = globalIndex;
    j_index_dst(index) = 0;
    transMap[globalIndex].push_back(globalIndex);
    transWeight[globalIndex].push_back(1.0);
  }

  // 2. Exchange local info among domains (lon,lat,bounds,mask,etc,...)
  CClientClientDHTDouble::Index2VectorInfoTypeMap localData;
  localData.rehash(std::ceil(oldNbLocal/localData.max_load_factor()));
  // Information exchanged among domains (attention to their order), number in parentheses presents size of data
  // lon(1) + lat(1) + bounds_lon(nVertex) + bounds_lat(nVertex) + mask(1) + data_i_index(1)
  int dataPackageSize =  1 + 1 + // lon + lat
                         nVertex + nVertex + //bounds_lon + bounds_lat
                         1 + // mask_1d_dst;
                         1; // data_i_index
  // Initialize database
  for (int idx = 0; idx < oldNbLocal; ++idx)
  {
    index = i_index_src(idx);
    localData[index].resize(dataPackageSize);
    std::vector<double>& data = localData[index];

    //Pack data
    int dataIdx = 0;
    data[dataIdx] = lon_src(idx);++dataIdx;
    data[dataIdx] = lat_src(idx);++dataIdx;
    for (int i = 0; i < nVertex; ++i)
    {
      data[dataIdx] = bounds_lon_src(i,idx); ++dataIdx;
    }
    for (int i = 0; i < nVertex; ++i)
    {
      data[dataIdx] = bounds_lat_src(i,idx); ++dataIdx;
    }
    data[dataIdx] = mask_1d_src(idx) ? 1.0 : -1; ++dataIdx;
    data[dataIdx] = data_i_index_src_full(idx);
  }

  CClientClientDHTDouble dhtData(localData,client->intraComm);
  CArray<size_t,1> neighborInd(nbNeighbor);
  for (int idx = 0; idx < nbNeighbor; ++idx)
    neighborInd(idx) = neighborsDomainSrc(0,idx);

  // Compute local data on other domains
  dhtData.computeIndexInfoMapping(neighborInd);
  CClientClientDHTDouble::Index2VectorInfoTypeMap& neighborData = dhtData.getInfoIndexMap();
  CClientClientDHTDouble::Index2VectorInfoTypeMap::iterator ite = neighborData.end(), it;
  // Ok get neighbor data
  size_t nIdx;
  int nbUnMaskedPointOnExtendedPart = 0;
  for (int idx = 0; idx < nbNeighbor; ++idx)
  {
    nIdx  = neighborInd(idx);
    it = neighborData.find(nIdx);
    if (ite != it)
    {
      index = idx + oldNbLocal;
      std::vector<double>& data = it->second;
      // Unpack data
      int dataIdx = 0;
      lon_dst(index) = data[dataIdx]; ++dataIdx;
      lat_dst(index) = data[dataIdx]; ++dataIdx;
      for (int i = 0; i < nVertex; ++i)
      {
        bounds_lon_dst(i,index) = data[dataIdx]; ++dataIdx;
      }
      for (int i = 0; i < nVertex; ++i)
      {
        bounds_lat_dst(i,index) = data[dataIdx]; ++dataIdx;
      }
      mask_1d_dst(index) = (1.0 == data[dataIdx]) ? true : false; ++dataIdx;
      data_i_index_dst_full(index) = (int)(data[dataIdx]);
      if (0 <= data_i_index_dst_full(index))
      {
        data_i_index_dst_full(index) = index;
        ++nbUnMaskedPointOnExtendedPart;
      }
    }
  }


  // Finally, update data_i_index
  int nbUnMaskedPointOnNewDstDomain = (nbUnMaskedPointOnExtendedPart + nbUnMaskedPointOnLocalDomain);
  int count = 0, dataIdx;
  for (int idx = 0; idx < newNbLocalDst; ++idx)
  {
    dataIdx = data_i_index_dst_full(idx);
    if ((0 <= dataIdx) && (dataIdx < newNbLocalDst))
    {
      ++count;
    }
  }

  data_i_index_dst.resize(count);
  data_j_index_dst.resize(count);
  data_j_index_dst = 0;

  count = 0;
  for (int idx = 0; idx < newNbLocalDst; ++idx)
  {
    dataIdx = data_i_index_dst_full(idx);
    if ((0 <= dataIdx) && (dataIdx < newNbLocalDst))
    {
      data_i_index_dst(count) = dataIdx;
      ++count;
    }
  }

  // Update ni
  domainDestination->ni.setValue(newNbLocalDst);

}


/*!
  Compute the index mapping between domain on grid source and one on grid destination
*/
void CDomainAlgorithmExpand::computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs)
{

}

}
