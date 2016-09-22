/*!
   \file domain_algorithm_compute_connectivity.cpp
   \author Ha NGUYEN
   \since 15 Jul 2016
   \date 15 Jul 2016

   \brief Algorithm for compute_connectivity on an domain.
 */
#include "domain_algorithm_compute_connectivity.hpp"
#include "compute_connectivity_domain.hpp"
#include "mesh.hpp"
#include "domain.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"

namespace xios {
CGenericAlgorithmTransformation* CDomainAlgorithmComputeConnectivity::create(CGrid* gridDst, CGrid* gridSrc,
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

  CComputeConnectivityDomain* compute_connectivityDomain = dynamic_cast<CComputeConnectivityDomain*> (transformation);
  int domainDstIndex = elementPositionInGridDst2AxisPosition[elementPositionInGrid];
  int domainSrcIndex = elementPositionInGridSrc2AxisPosition[elementPositionInGrid];

  return (new CDomainAlgorithmComputeConnectivity(domainListDestP[domainDstIndex], domainListSrcP[domainSrcIndex], compute_connectivityDomain));
}

bool CDomainAlgorithmComputeConnectivity::registerTrans()
{
  CGridTransformationFactory<CDomain>::registerTransformation(TRANS_COMPUTE_CONNECTIVITY_DOMAIN, create);
}

CDomainAlgorithmComputeConnectivity::CDomainAlgorithmComputeConnectivity(CDomain* domainDestination, CDomain* domainSource,
                                                                         CComputeConnectivityDomain* compute_connectivityDomain)
: CDomainAlgorithmTransformation(domainDestination, domainSource)
{
  this->type_ = (ELEMENT_NO_MODIFICATION_WITHOUT_DATA);
  compute_connectivityDomain->checkValid(domainDestination);
  int& nbNeighborMax = compute_connectivityDomain->n_neighbor_max;
  CArray<int,1>& nbNeighbor = compute_connectivityDomain->n_neighbor;
  CArray<int,2>& localNeighbors = compute_connectivityDomain->local_neighbor;
  switch (compute_connectivityDomain->type)
  {
    case CComputeConnectivityDomain::type_attr::node :
      computeNodeConnectivity(domainDestination,
                              nbNeighborMax,
                              nbNeighbor,
                              localNeighbors);
      break;
    case CComputeConnectivityDomain::type_attr::edge :
      computeEdgeConnectivity(domainDestination,
                              nbNeighborMax,
                              nbNeighbor,
                              localNeighbors);
      break;
    default:
      break;
  }
}

void CDomainAlgorithmComputeConnectivity::computeNodeConnectivity(CDomain* domain,
                                                                  int& nbConnectivityMax,
                                                                  CArray<int,1>& nbConnectivity,
                                                                  CArray<int,2>& localConnectivity)
{
  CMesh mesh;
  CArray<double,1>& lon = domain->lonvalue_1d;
  CArray<double,1>& lat = domain->latvalue_1d;
  CArray<double,2>& bounds_lon = domain->bounds_lon_1d;
  CArray<double,2>& bounds_lat = domain->bounds_lat_1d;
  mesh.createMesh(lon, lat, bounds_lon, bounds_lat);
  CArray<int, 2>& face_nodes = mesh.face_nodes;
  int nvertex = mesh.nvertex;
  int nFaces  = mesh.nbFaces;
  int nNodes  = mesh.nbNodes;
  std::vector<std::vector<size_t> > nodeFaceConnectivity(nNodes, std::vector<size_t>());
  for (int nf = 0; nf < nFaces; ++nf)
  {
    for (int nv = 0; nv < nvertex; ++nv)
    {
      nodeFaceConnectivity[face_nodes(nv,nf)].push_back(nf);
    }
  }

  if (nFaces != nbConnectivity.numElements()) nbConnectivity.resize(nFaces);
  nbConnectivityMax = 1;
  for (int nf = 0; nf < nFaces; ++nf)
  {
    std::set<size_t> neighFaces;
    for (int nv = 0; nv < nvertex; ++nv)
    {
      std::vector<size_t>& tmpFaces = nodeFaceConnectivity[face_nodes(nv,nf)];
      for (int nFn = 0; nFn < tmpFaces.size(); ++nFn)
      {
        neighFaces.insert(tmpFaces[nFn]);
      }
    }
    if (nbConnectivityMax < (neighFaces.size()-1)) nbConnectivityMax = neighFaces.size()-1;
  }
  if ((nbConnectivityMax != localConnectivity.shape()[0]) || (nFaces != localConnectivity.shape()[1]))
    localConnectivity.resize(nbConnectivityMax, nFaces);

  for (int nf = 0; nf < nFaces; ++nf)
  {
    std::set<size_t> neighFaces;
    for (int nv = 0; nv < nvertex; ++nv)
    {
      std::vector<size_t>& tmpFaces = nodeFaceConnectivity[face_nodes(nv,nf)];
      for (int nFn = 0; nFn < tmpFaces.size(); ++nFn)
      {
        neighFaces.insert(tmpFaces[nFn]);
      }
    }

    neighFaces.erase(nf);
    nbConnectivity(nf) = neighFaces.size();
    std::set<size_t>::iterator it = neighFaces.begin(), ite = neighFaces.end();
    for (int idx = 0; it != ite; ++it, ++idx)
    {
      localConnectivity(idx, nf) = *it;
    }
  }
}

void CDomainAlgorithmComputeConnectivity::computeEdgeConnectivity(CDomain* domain,
                                                                  int& nbConnectivityMax,
                                                                  CArray<int,1>& nbConnectivity,
                                                                  CArray<int,2>& localConnectivity)
{
  CMesh mesh;
  CArray<double,1>& lon = domain->lonvalue_1d;
  CArray<double,1>& lat = domain->latvalue_1d;
  CArray<double,2>& bounds_lon = domain->bounds_lon_1d;
  CArray<double,2>& bounds_lat = domain->bounds_lat_1d;
  mesh.createMesh(lon, lat, bounds_lon, bounds_lat);

  CArray<int, 2>& face_edges = mesh.face_edges;
  int nvertex = mesh.nvertex;
  int nFaces  = mesh.nbFaces;
  int nEdges  = mesh.nbEdges;
  std::vector<std::vector<size_t> > edgeFaceConnectivity(nEdges, std::vector<size_t>());
  for (int nf = 0; nf < nFaces; ++nf)
  {
    for (int nv = 0; nv < nvertex; ++nv)
    {
      edgeFaceConnectivity[face_edges(nv,nf)].push_back(nf);
    }
  }

  if (nFaces != nbConnectivity.numElements()) nbConnectivity.resize(nFaces);
  nbConnectivityMax = 1;
  for (int nf = 0; nf < nFaces; ++nf)
  {
    std::set<size_t> neighFaces;
    for (int nv = 0; nv < nvertex; ++nv)
    {
      std::vector<size_t>& tmpFaces = edgeFaceConnectivity[face_edges(nv,nf)];
      for (int nFn = 0; nFn < tmpFaces.size(); ++nFn)
      {
        neighFaces.insert(tmpFaces[nFn]);
      }
    }
    if (nbConnectivityMax < (neighFaces.size()-1)) nbConnectivityMax = neighFaces.size() - 1;
  }
  if ((nbConnectivityMax != localConnectivity.shape()[0]) || (nFaces != localConnectivity.shape()[1]))
    localConnectivity.resize(nbConnectivityMax, nFaces);

  for (int nf = 0; nf < nFaces; ++nf)
  {
    std::set<size_t> neighFaces;
    for (int nv = 0; nv < nvertex; ++nv)
    {
      std::vector<size_t>& tmpFaces = edgeFaceConnectivity[face_edges(nv,nf)];
      for (int nFn = 0; nFn < tmpFaces.size(); ++nFn)
      {
        neighFaces.insert(tmpFaces[nFn]);
      }
    }

    neighFaces.erase(nf);
    nbConnectivity(nf) = neighFaces.size();
    std::set<size_t>::iterator it = neighFaces.begin(), ite = neighFaces.end();
    for (int idx = 0; it != ite; ++it, ++idx)
    {
      localConnectivity(idx, nf) = *it;
    }
  }

}

/*!
  Compute the index mapping between domain on grid source and one on grid destination
*/
void CDomainAlgorithmComputeConnectivity::computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs)
{

}

}
