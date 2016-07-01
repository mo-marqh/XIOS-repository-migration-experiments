/*!
  \file mesh.hpp
  \author Olga Abramkina
  \brief Declaration of class CMesh.
*/

#ifndef __XIOS_CMesh__
#define __XIOS_CMesh__
 
#include "array_new.hpp"

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

      void createMesh(const CArray<double, 1>&, const CArray<double, 1>&, 
            const CArray<double, 2>&, const CArray<double, 2>& );
                        
      void createMeshEpsilon(const CArray<double, 1>&, const CArray<double, 1>&, 
            const CArray<double, 2>&, const CArray<double, 2>& );
            
      bool isWritten (StdString);    
      static CMesh* getMesh;
          
       private:

      static std::map <StdString, CMesh*> meshList;

      size_t nodeIndex (double, double);      
      boost::unordered_map <size_t, size_t> hashed_map_nodes;
      boost::unordered_map <pair<double,double>, int> map_nodes;
      boost::unordered_map <pair<int,int>, int> map_edges;

  }; 

} // namespace xios

#endif //__XIOS_CMesh__
