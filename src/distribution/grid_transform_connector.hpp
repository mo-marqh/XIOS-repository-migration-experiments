#ifndef __GRID_TRANSFORM_CONNECTOR_HPP__
#define __GRID_TRANSFORM_CONNECTOR_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "distributed_view.hpp"
#include "local_view.hpp"
#include "grid_scatterer_connector.hpp"
#include "grid_gatherer_connector.hpp"
#include "reduction_types.hpp"
#include "mpi.hpp"

namespace xios
{
 
  
  class CGridTransformConnector
  {
     
    public:
      CGridTransformConnector(vector<shared_ptr<CLocalView>> srcViews, vector<shared_ptr<CLocalView>> remoteViews, MPI_Comm localComm) 
                          : srcViews_(srcViews), remoteViews_(remoteViews), localComm_(localComm) 
                          { }
    
      void computeConnector(bool eliminateRedundant=true) ; 
    protected:
     MPI_Comm localComm_ ;
     vector<shared_ptr<CLocalView>> srcViews_ ;
     vector<shared_ptr<CLocalView>> remoteViews_ ;
     map<int,int> recvRankSize_ ;

     vector<shared_ptr<CScattererConnector>> scattererConnector_ ;
     vector<shared_ptr<CGathererConnector>>  gathererConnector_ ;
     shared_ptr<CGridScattererConnector> gridScattererConnector_ ;
     shared_ptr<CGridGathererConnector> gridGathererConnector_ ;
  
    public:
      template<typename T> 
      void transfer(const CArray<T,1>& dataIn, CArray<T,1>& dataOut, EReduction op = EReduction::none)
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

        if (op == EReduction::none) gridGathererConnector_->transfer(tmpArrayOut, dataOut, nanValue) ;
        else gridGathererConnector_->transfer(tmpArrayOut, dataOut, op) ;
      }
  
  };

}

#endif