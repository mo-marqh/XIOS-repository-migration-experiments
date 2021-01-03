#ifndef __GRID_TRANSFORM_CONNECTOR_HPP__
#define __GRID_TRANSFORM_CONNECTOR_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "distributed_view.hpp"
#include "local_view.hpp"
#include "grid_scatterer_connector.hpp"
#include "grid_gatherer_connector.hpp"
#include "mpi.hpp"

namespace xios
{
 
  
  class CGridTransformConnector
  {
     
    public:
      CGridTransformConnector(vector<CLocalView*> srcViews, vector<CLocalView*> remoteViews, MPI_Comm localComm) 
                          : srcViews_(srcViews), remoteViews_(remoteViews), localComm_(localComm) 
                          { computeConnector();}
    
      void computeConnector(void) ; 
    protected:
     MPI_Comm localComm_ ;
     vector<CLocalView*> srcViews_ ;
     vector<CLocalView*> remoteViews_ ;
     map<int,int> recvRankSize_ ;

     vector<CScattererConnector*> scattererConnector_ ;
     vector<CGathererConnector*>  gathererConnector_ ;
     CGridScattererConnector* gridScattererConnector_ ;
     CGridGathererConnector* gridGathererConnector_ ;
  
    public:
      template<typename T> 
      void transfer(const CArray<T,1>& dataIn, CArray<T,1>& dataOut)
      {
        map<int,CArray<T,1>> tmpArrayIn ;
        gridScattererConnector_->transfer(dataIn, tmpArrayIn) ;
        vector<MPI_Request> requests ;
        MPI_Request request ;
        for(auto it : tmpArrayIn)
        {
          auto& array = it.second ; 
          MPI_Issend(array.dataFirst(), array.numElements(), MPI_GetType<T>(), it.first, 0, localComm_, &request) ;
          requests.push_back(request) ;
        }
        
        map<int,CArray<T,1>> tmpArrayOut ;
        for(auto it : recvRankSize_)
        {
          auto& array = tmpArrayOut[it.first] ;
          array.resize(it.second) ;
          MPI_Irecv(array.dataFirst(), array.numElements(), MPI_GetType<T>(), it.first, 0, localComm_, &request) ;
          requests.push_back(request) ;
        }
        
        vector<MPI_Status> status(requests.size()) ;
        MPI_Waitall(requests.size(), requests.data(),status.data()) ;
        
        const double nanValue = std::numeric_limits<double>::quiet_NaN();
        gridGathererConnector_->transfer(tmpArrayOut, dataOut, nanValue) ;
      }
  
  };

}

#endif