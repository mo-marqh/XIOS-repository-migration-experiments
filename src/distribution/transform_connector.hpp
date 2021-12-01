#ifndef __TRANSFORM_CONNECTOR_HPP__
#define __TRANSFORM_CONNECTOR_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "distributed_view.hpp"
#include "local_view.hpp"
#include "scatterer_connector.hpp"
#include "gatherer_connector.hpp"
#include "mpi.hpp"

namespace xios
{
 
  
  class CTransformConnector
  {
     
    public:
      CTransformConnector(shared_ptr<CLocalView> srcView, shared_ptr<CLocalView> dstView, MPI_Comm localComm) 
                          : srcView_(srcView), dstView_(dstView), localComm_(localComm) {}
    
      void computeConnector(void) ; 
    private:

      MPI_Comm localComm_ ;
      shared_ptr<CLocalView> srcView_ ;
      shared_ptr<CLocalView> dstView_ ;
      map<int,int> recvRankSize_ ;
      shared_ptr<CScattererConnector> scattererConnector_ ;
      shared_ptr<CGathererConnector>  gathererConnector_ ;
   
    public:
      template<typename T> 
      void transfer(int repeat, int sizeT, const CArray<T,1>& dataIn, CArray<T,1>& dataOut)
      {
        map<int,CArray<T,1>> tmpArrayIn ;
        scattererConnector_->transfer(repeat, sizeT, dataIn, tmpArrayIn) ;
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
          array.resize(it.second*repeat*sizeT) ;
          MPI_Irecv(array.dataFirst(), array.numElements(), MPI_GetType<T>(), it.first, 0, localComm_, &request) ;
          requests.push_back(request) ;
        }
        
        vector<MPI_Status> status(requests.size()) ;
        MPI_Waitall(requests.size(), requests.data(),status.data()) ;
        
        const double nanValue = std::numeric_limits<double>::quiet_NaN();
        gathererConnector_->transfer(repeat, sizeT , tmpArrayOut, dataOut, nanValue) ;
      }

      template<typename T> 
      void transfer(int sizeT, const CArray<T,1>& dataIn, CArray<T,1>& dataOut)
      {
        transfer(1, sizeT, dataIn, dataOut) ;
      }
   
      template<typename T> 
      void transfer(const CArray<T,1>& dataIn, CArray<T,1>& dataOut)
      {
        transfer(1, 1, dataIn, dataOut) ;
      }

  };

}

#endif