/*!
  \file mesh.hpp
  \author Olga Abramkina
  \brief Declaration of class CMesh.
*/

#ifndef __XIOS_CMesh__
#define __XIOS_CMesh__
 
#include "array_new.hpp"
#include "client_client_dht_template_impl.hpp"
#include "dht_auto_indexing.hpp"

namespace xios {
  
   /// ////////////////////// Déclarations ////////////////////// ///

   class CMesh;

   ///--------------------------------------------------------------

/*!
  \class CMesh
  \brief CMesh class.
*/ 
  class CMesh
  {
  
    public:

      CMesh(void);
      ~CMesh(void);
    
      int nbNodes;
      int nbEdges;
      int nbFaces;
      int nvertex;  

      bool nodesAreWritten;
      bool edgesAreWritten;
      bool facesAreWritten;
      
      CArray<double, 1> node_lon;
      CArray<double, 1> node_lat;
      
      CArray<double, 1> edge_lon;
      CArray<double, 1> edge_lat;
      CArray<int, 2> edge_nodes;

      CArray<double, 1> face_lon;
      CArray<double, 1> face_lat;
      CArray<int, 2> face_nodes;
      CArray<int, 2> face_edges;
      CArray<int, 2> edge_faces;
      CArray<int, 2> face_faces;

      void createMesh(const CArray<double, 1>&, const CArray<double, 1>&, 
            const CArray<double, 2>&, const CArray<double, 2>& );
                        
      void createMeshEpsilon(const MPI_Comm&, const CArray<double, 1>&, const CArray<double, 1>&,
            const CArray<double, 2>&, const CArray<double, 2>& );
            
      static CMesh* getMesh(StdString);

    private:

      static std::map <StdString, CMesh> meshList;
      vector<size_t> createHashes (double, double);

      size_t nodeIndex (double, double);                           // redundant in parallel version with epsilon precision
      boost::unordered_map <size_t, size_t> hashed_map_nodes;      // redundant in parallel version with epsilon precision
      boost::unordered_map <pair<double,double>, int> map_nodes;   // redundant in parallel version with epsilon precision
      boost::unordered_map <pair<int,int>, int> map_edges;         // redundant in parallel version with epsilon precision

  }; 

} // namespace xios

#endif //__XIOS_CMesh__
