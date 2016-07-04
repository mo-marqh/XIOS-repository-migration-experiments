/*!
  \file mesh.cpp
  \author Olga Abramkina
  \brief Definition of class CMesh.
*/

#include "mesh.hpp"

namespace xios {

/// ////////////////////// Définitions ////////////////////// ///

  CMesh::CMesh(void) :  nbFaces{0}, nbNodes{0}, nbEdges{0}
            ,   nvertex{0}
            ,  nodesAreWritten{false}, edgesAreWritten{false}, facesAreWritten{false}
            ,  node_lon(), node_lat()
            ,  edge_lon(), edge_lat(), edge_nodes()
            ,  face_lon(), face_lat()
            ,  face_nodes()
  {
  }


  CMesh::~CMesh(void)
  {
  }

  std::map <StdString, CMesh> CMesh::meshList = std::map <StdString, CMesh>();
  //CMesh* CMesh::getMesh;

///---------------------------------------------------------------
/*!
 * \fn bool CMesh::getMesh (StdString meshName)
 * Returns a pointer to a mesh. If a mesh has not been created, creates it and adds its name to the list of meshes meshList.
 * \param [in] meshName  The name of a mesh ("name" attribute of a domain).
 */
  CMesh::CMesh* CMesh::getMesh (StdString meshName)
  {
    if ( CMesh::meshList.find(meshName) == CMesh::meshList.end() )
    {
      CMesh::CMesh newMesh;
      CMesh::meshList.insert( make_pair(meshName, newMesh) );

    }
    return &meshList[meshName];
  }

///----------------------------------------------------------------
  int hashPair(int first, int second)
  {
    int seed = first + 0x9e3779b9 ;
    seed ^= second + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    return seed ;
  }

///----------------------------------------------------------------
/*!
 * \fn size_t CMesh::nodeIndex (double lon, double lat)
 * Returns its index if a node exists; otherwise adds the node and returns -1.
 * Precision check is implemented with two hash values for each dimension, longitude and latitude.
 * \param [in] lon Node longitude in degrees.
 * \param [in] lat Node latitude in degrees ranged from 0 to 360.
 * \return node index if a node exists; -1 otherwise
 */
  size_t CMesh::nodeIndex (double lon, double lat)
  {
    double minBoundLon = 0. ;
    double maxBoundLon = 360. ;
    double minBoundLat = -90 ;
    double maxBoundLat = 90 ;
    double prec=1e-11 ;
    double precLon=prec ;
    double precLat=prec ;

    size_t maxsize_t=numeric_limits<size_t>::max() ;
    if ( (maxBoundLon-minBoundLon)/maxsize_t > precLon) precLon=(maxBoundLon-minBoundLon)/maxsize_t ;
    if ( (maxBoundLat-minBoundLat)/maxsize_t > precLat) precLat=(maxBoundLat-minBoundLat)/maxsize_t ;

    size_t iMinLon=0 ;
    size_t iMaxLon=(maxBoundLon-minBoundLon)/precLon ;
    size_t iMinLat=0 ;
    size_t iMaxLat=(maxBoundLat-minBoundLat)/precLat ;

    size_t hash0,hash1,hash2,hash3 ;
    size_t lon0,lon1,lat0,lat1 ;

    lon0=(lon-minBoundLon)/precLon ;
    if ( ((lon0+1)*precLon + lon0*precLon)/2 > lon-minBoundLon)
    {
      if (lon0==iMinLon) lon1=iMaxLon ;
      else lon1=lon0-1 ;
    }
    else
    {
      if (lon0==iMaxLon) lon1=iMinLon ;
      else lon1=lon0+1 ;
    }

    lat0=(lat-minBoundLat)/precLat ;
    if ( ((lat0+1)*precLat + lat0*precLat)/2 > lat-minBoundLat)
    {
      if (lat0==iMinLat) lat1=lat0 ;
      else lat1=lat0-1 ;
    }
    else
    {
      if (lat0==iMaxLat) lat1=lat0 ;
      else lat1=lat0+1 ;
    }

    hash0=hashPair(lon0,lat0) ;
    hash1=hashPair(lon0,lat1) ;
    hash2=hashPair(lon1,lat0) ;
    hash3=hashPair(lon1,lat1) ;

    boost::unordered_map<size_t, size_t>::iterator end = hashed_map_nodes.end() ;
    size_t mapSize = hashed_map_nodes.size();
    if (hashed_map_nodes.find(hash0)==end && hashed_map_nodes.find(hash1)==end && hashed_map_nodes.find(hash2)==end && hashed_map_nodes.find(hash3)==end)
    {
      hashed_map_nodes[hash0] = mapSize ;
      hashed_map_nodes[hash1] = mapSize + 1;
      hashed_map_nodes[hash2] = mapSize + 2;
      hashed_map_nodes[hash3] = mapSize + 3;
      return -1;
    }
    else
      return ( (hashed_map_nodes[hash0]+1) / 4 );

  } // nodeIndex()

///----------------------------------------------------------------
/*!
 * \fn void CMesh::hashDblDbl (double lon, double lat)
 * Creates two hash values for each dimension, longitude and latitude.
 * \param [in] lon Node longitude in degrees.
 * \param [in] lat Node latitude in degrees ranged from 0 to 360.
 */

  void hashDblDbl (double lon, double lat)
  {
    double minBoundLon = 0. ;
    double maxBoundLon = 360. ;
    double minBoundLat = -90 ;
    double maxBoundLat = 90 ;
    double prec=1e-11 ;
    double precLon=prec ;
    double precLat=prec ;

    size_t maxsize_t=numeric_limits<size_t>::max() ;
    if ( (maxBoundLon-minBoundLon)/maxsize_t > precLon) precLon=(maxBoundLon-minBoundLon)/maxsize_t ;
    if ( (maxBoundLat-minBoundLat)/maxsize_t > precLat) precLat=(maxBoundLat-minBoundLat)/maxsize_t ;

    size_t iMinLon=0 ;
    size_t iMaxLon=(maxBoundLon-minBoundLon)/precLon ;
    size_t iMinLat=0 ;
    size_t iMaxLat=(maxBoundLat-minBoundLat)/precLat ;

    size_t hash0,hash1,hash2,hash3 ;
    size_t lon0,lon1,lat0,lat1 ;

    lon0=(lon-minBoundLon)/precLon ;
    if ( ((lon0+1)*precLon + lon0*precLon)/2 > lon-minBoundLon)
    {
      if (lon0==iMinLon) lon1=iMaxLon ;
      else lon1=lon0-1 ;
    }
    else
    {
      if (lon0==iMaxLon) lon1=iMinLon ;
      else lon1=lon0+1 ;
    }

    lat0=(lat-minBoundLat)/precLat ;
    if ( ((lat0+1)*precLat + lat0*precLat)/2 > lat-minBoundLat)
    {
      if (lat0==iMinLat) lat1=lat0 ;
      else lat1=lat0-1 ;
    }
    else
    {
      if (lat0==iMaxLat) lat1=lat0 ;
      else lat1=lat0+1 ;
    }

    hash0=hashPair(lon0,lat0) ;
    hash1=hashPair(lon0,lat1) ;
    hash2=hashPair(lon1,lat0) ;
    hash3=hashPair(lon1,lat1) ;

  } // hashDblDbl

///----------------------------------------------------------------
  std::pair<int,int> make_ordered_pair(int a, int b)
  {
    if ( a < b )
      return std::pair<int,int>(a,b);
    else
      return std::pair<int,int>(b,a);
  }

///----------------------------------------------------------------
/*!
 * \fn void CMesh::createMesh(const CArray<double, 1>& lonvalue, const CArray<double, 1>& latvalue,
            const CArray<double, 2>& bounds_lon, const CArray<double, 2>& bounds_lat)
 * Creates or updates a mesh for the three types of mesh elements: nodes, edges, and faces.
 * \param [in] lonvalue  Array of longitudes.
 * \param [in] latvalue  Array of latitudes.
 * \param [in] bounds_lon Array of boundary longitudes. Its size depends on the element type.
 * \param [in] bounds_lat Array of boundary latitudes. Its size depends on the element type.
 */
  void CMesh::createMesh(const CArray<double, 1>& lonvalue, const CArray<double, 1>& latvalue,
            const CArray<double, 2>& bounds_lon, const CArray<double, 2>& bounds_lat)
  {
    nvertex = (bounds_lon.numElements() == 0) ? 1 : bounds_lon.rows();

    if (nvertex == 1)
    {
      nbNodes = lonvalue.numElements();
      node_lon.resizeAndPreserve(nbNodes);
      node_lat.resizeAndPreserve(nbNodes);
      for (int nn = 0; nn < nbNodes; ++nn)
      {
        if (map_nodes.find(make_pair (lonvalue(nn), latvalue(nn))) == map_nodes.end())
        {
          map_nodes[make_pair (lonvalue(nn), latvalue(nn))] = nn ;
          node_lon(nn) = lonvalue(nn);
          node_lat(nn) = latvalue(nn);
        }
      }
    }
    else if (nvertex == 2)
    {
      nbEdges = bounds_lon.shape()[1];

      // Create nodes and edge_node connectivity
      node_lon.resizeAndPreserve(nbEdges*nvertex); // Max possible number of nodes
      node_lat.resizeAndPreserve(nbEdges*nvertex);
      edge_nodes.resizeAndPreserve(nvertex, nbEdges);

      for (int ne = 0; ne < nbEdges; ++ne)
      {
        for (int nv = 0; nv < nvertex; ++nv)
        {
          if (map_nodes.find(make_pair (bounds_lon(nv, ne), bounds_lat(nv ,ne))) == map_nodes.end())
          {
            map_nodes[make_pair (bounds_lon(nv, ne), bounds_lat(nv, ne))] = nbNodes ;
            edge_nodes(nv,ne) = nbNodes ;
            node_lon(nbNodes) = bounds_lon(nv, ne);
            node_lat(nbNodes) = bounds_lat(nv, ne);
            ++nbNodes ;
          }
          else
            edge_nodes(nv,ne) = map_nodes[make_pair (bounds_lon(nv, ne), bounds_lat(nv ,ne))];
        }
      }
      node_lon.resizeAndPreserve(nbNodes);
      node_lat.resizeAndPreserve(nbNodes);

      // Create edges
      edge_lon.resizeAndPreserve(nbEdges);
      edge_lat.resizeAndPreserve(nbEdges);

      for (int ne = 0; ne < nbEdges; ++ne)
      {
        if (map_edges.find(make_ordered_pair (edge_nodes(0,ne), edge_nodes(1,ne))) == map_edges.end())
        {
          map_edges[make_ordered_pair ( edge_nodes(0,ne), edge_nodes(1,ne) )] = ne ;
          edge_lon(ne) = lonvalue(ne);
          edge_lat(ne) = latvalue(ne);
        }

      }
      edgesAreWritten = true;
    }
    else
    {
      nbFaces = bounds_lon.shape()[1];
  
      // Create nodes and face_node connectivity
      node_lon.resizeAndPreserve(nbFaces*nvertex);  // Max possible number of nodes
      node_lat.resizeAndPreserve(nbFaces*nvertex);
      face_nodes.resize(nvertex, nbFaces);
  
      for (int nf = 0; nf < nbFaces; ++nf)
      {
        for (int nv = 0; nv < nvertex; ++nv)
        {
          if (map_nodes.find(make_pair (bounds_lon(nv, nf), bounds_lat(nv ,nf))) == map_nodes.end())
          {
            map_nodes[make_pair (bounds_lon(nv, nf), bounds_lat(nv, nf))] = nbNodes ;
            face_nodes(nv,nf) = nbNodes ;
            node_lon(nbNodes) = bounds_lon(nv, nf);
            node_lat(nbNodes) = bounds_lat(nv ,nf);
            ++nbNodes ;
          }
          else
          {
            face_nodes(nv,nf) = map_nodes[make_pair (bounds_lon(nv, nf), bounds_lat(nv ,nf))];
          }
        }
      }
      node_lon.resizeAndPreserve(nbNodes);
      node_lat.resizeAndPreserve(nbNodes);
  
      // Create edges and edge_nodes connectivity
      edge_lon.resizeAndPreserve(nbFaces*nvertex); // Max possible number of edges
      edge_lat.resizeAndPreserve(nbFaces*nvertex);
      edge_nodes.resizeAndPreserve(2, nbFaces*nvertex);
      for (int nf = 0; nf < nbFaces; ++nf)
      {
        for (int nv1 = 0; nv1 < nvertex; ++nv1)
        {
          int nv2 = (nv1 < nvertex -1 ) ? (nv1 + 1) : (nv1 + 1 - nvertex); // cyclic rotation
          if (map_edges.find(make_ordered_pair (face_nodes(nv1,nf), face_nodes(nv2,nf))) == map_edges.end())
          {
            map_edges[make_ordered_pair (face_nodes(nv1,nf), face_nodes(nv2,nf))] = nbEdges ;
            edge_nodes(Range::all(),nbEdges) = face_nodes(nv1,nf), face_nodes(nv2,nf);
            edge_lon(nbEdges) = ( abs( node_lon(face_nodes(nv1,nf)) - node_lon(face_nodes(nv2,nf))) < 180.) ?
                        (( node_lon(face_nodes(nv1,nf)) + node_lon(face_nodes(nv2,nf))) * 0.5) :
                        (( node_lon(face_nodes(nv1,nf)) + node_lon(face_nodes(nv2,nf))) * 0.5 -180.);;
            edge_lat(nbEdges) = ( node_lat(face_nodes(nv1,nf)) + node_lat(face_nodes(nv2,nf)) ) * 0.5;
            ++nbEdges;
          }
        }
      }
      edge_nodes.resizeAndPreserve(2, nbEdges);
      edge_lon.resizeAndPreserve(nbEdges);
      edge_lat.resizeAndPreserve(nbEdges);
  
      // Create faces
      face_lon.resize(nbFaces);
      face_lat.resize(nbFaces);
      face_lon = lonvalue;
      face_lat = latvalue;
      facesAreWritten = true;
    } // nvertex > 2
    
  } // createMesh()

///----------------------------------------------------------------
/*!
 * \fn void CMesh::createMeshEpsilon(const CArray<double, 1>& lonvalue, const CArray<double, 1>& latvalue,
            const CArray<double, 2>& bounds_lon, const CArray<double, 2>& bounds_lat)
 * Creates or updates a mesh for the three types of mesh elements: nodes, edges, and faces.
 * Precision check is implemented with two hash values for each dimension, longitude and latitude.
 * \param [in] lonvalue  Array of longitudes.
 * \param [in] latvalue  Array of latitudes.
 * \param [in] bounds_lon Array of boundary longitudes. Its size depends on the element type.
 * \param [in] bounds_lat Array of boundary latitudes. Its size depends on the element type.
 */
  void CMesh::createMeshEpsilon(const CArray<double, 1>& lonvalue, const CArray<double, 1>& latvalue,
            const CArray<double, 2>& bounds_lon, const CArray<double, 2>& bounds_lat)
  {
    nvertex = (bounds_lon.numElements() == 0) ? 1 : bounds_lon.rows();

    if (nvertex == 1)
    {
      nbNodes = lonvalue.numElements();
      node_lon.resizeAndPreserve(nbNodes);
      node_lat.resizeAndPreserve(nbNodes);
      for (int nn = 0; nn < nbNodes; ++nn)
      {
        if ( nodeIndex(lonvalue(nn), latvalue(nn)) == -1 )
        {
          node_lon(nn) = lonvalue(nn);
          node_lat(nn) = latvalue(nn);
        }
      }
    nodesAreWritten = true;
    }
    else if (nvertex == 2)
    {
      nbEdges = bounds_lon.shape()[1];

      // Create nodes and edge_node connectivity
      node_lon.resizeAndPreserve(nbEdges*nvertex); // Max possible number of nodes
      node_lat.resizeAndPreserve(nbEdges*nvertex);
      edge_nodes.resizeAndPreserve(nvertex, nbEdges);

      for (int ne = 0; ne < nbEdges; ++ne)
      {
        for (int nv = 0; nv < nvertex; ++nv)
        {
          if ( nodeIndex(bounds_lon(nv, ne), bounds_lat(nv ,ne)) == -1)
          {
            edge_nodes(nv,ne) = nbNodes ;
            node_lon(nbNodes) = bounds_lon(nv, ne);
            node_lat(nbNodes) = bounds_lat(nv, ne);
            ++nbNodes ;
          }
          else
            edge_nodes(nv,ne) = nodeIndex(bounds_lon(nv, ne), bounds_lat(nv ,ne));
        }
      }
      node_lon.resizeAndPreserve(nbNodes);
      node_lat.resizeAndPreserve(nbNodes);

      // Create edges
      edge_lon.resizeAndPreserve(nbEdges);
      edge_lat.resizeAndPreserve(nbEdges);

      for (int ne = 0; ne < nbEdges; ++ne)
      {
        if (map_edges.find(make_ordered_pair (edge_nodes(0,ne), edge_nodes(1,ne))) == map_edges.end())
        {
          map_edges[make_ordered_pair ( edge_nodes(0,ne), edge_nodes(1,ne) )] = ne ;
          edge_lon(ne) = lonvalue(ne);
          edge_lat(ne) = latvalue(ne);
        }
      }
      edgesAreWritten = true;
    } // nvertex = 2
    else
    {
      nbFaces = bounds_lon.shape()[1];

      // Create nodes and face_node connectivity
      node_lon.resizeAndPreserve(nbFaces*nvertex);  // Max possible number of nodes
      node_lat.resizeAndPreserve(nbFaces*nvertex);
      face_nodes.resize(nvertex, nbFaces);

      for (int nf = 0; nf < nbFaces; ++nf)
      {
        for (int nv = 0; nv < nvertex; ++nv)
        {
          if ( nodeIndex(bounds_lon(nv, nf), bounds_lat(nv ,nf)) == -1)
          {
            face_nodes(nv,nf) = nbNodes ;
            node_lon(nbNodes) = bounds_lon(nv, nf);
            node_lat(nbNodes) = bounds_lat(nv ,nf);
            ++nbNodes ;
          }
          else
          {
            face_nodes(nv,nf) = nodeIndex(bounds_lon(nv, nf), bounds_lat(nv ,nf));
          }
        }
      }
      node_lon.resizeAndPreserve(nbNodes);
      node_lat.resizeAndPreserve(nbNodes);

      // Create edges and edge_nodes connectivity
      edge_lon.resizeAndPreserve(nbFaces*nvertex); // Max possible number of edges
      edge_lat.resizeAndPreserve(nbFaces*nvertex);
      edge_nodes.resizeAndPreserve(2, nbFaces*nvertex);
      for (int nf = 0; nf < nbFaces; ++nf)
      {
        for (int nv1 = 0; nv1 < nvertex; ++nv1)
        {
          int nv2 = (nv1 < nvertex -1 ) ? (nv1 + 1) : (nv1 + 1 - nvertex); // cyclic rotation
          if (map_edges.find(make_ordered_pair (face_nodes(nv1,nf), face_nodes(nv2,nf))) == map_edges.end())
          {
            map_edges[make_ordered_pair (face_nodes(nv1,nf), face_nodes(nv2,nf))] = nbEdges ;
            edge_nodes(Range::all(),nbEdges) = face_nodes(nv1,nf), face_nodes(nv2,nf);
            edge_lon(nbEdges) = ( abs( node_lon(face_nodes(nv1,nf)) - node_lon(face_nodes(nv2,nf))) < 180.) ?
                        (( node_lon(face_nodes(nv1,nf)) + node_lon(face_nodes(nv2,nf))) * 0.5) :
                        (( node_lon(face_nodes(nv1,nf)) + node_lon(face_nodes(nv2,nf))) * 0.5 -180.);;
            edge_lat(nbEdges) = ( node_lat(face_nodes(nv1,nf)) + node_lat(face_nodes(nv2,nf)) ) * 0.5;
            ++nbEdges;
          }
        }
      }
      edge_nodes.resizeAndPreserve(2, nbEdges);
      edge_lon.resizeAndPreserve(nbEdges);
      edge_lat.resizeAndPreserve(nbEdges);

      // Create faces
      face_lon.resize(nbFaces);
      face_lat.resize(nbFaces);
      face_lon = lonvalue;
      face_lat = latvalue;
      facesAreWritten = true;
    } // nvertex >= 3

  } // createMeshEpsilon

} // namespace xios
