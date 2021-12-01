#ifndef __TRANSFER_TRANSFORM_CONNECTOR_HPP__
#define __TRANSFER_TRANSFORM_CONNECTOR_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "local_view.hpp"



namespace xios
{
  
  class CTransferTransformConnector
  {
    
    private:
      shared_ptr<CLocalView> srcView_;
      shared_ptr<CLocalView> dstView_;

      vector<int> connector_;    
      vector<bool> mask_ ;  //  sizeof dstSize_ 
      int srcSize_ ;
      int dstSize_ ;

     void computeConnector(unordered_map<int, int>& indexMap) ;
      
    public:

    CTransferTransformConnector(shared_ptr<CLocalView> srcView, shared_ptr<CLocalView> dstView, unordered_map<int, int>& indexMap) ;
 
    void transfer(int repeat, int sizeT, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
    {
      int dstSlice = dstSize_*sizeT ;
      int srcSlice = srcSize_*sizeT ;
      dataOut.resize(repeat* dstSlice) ;
      dataOut=0 ; // what to do about missing value => next step ?

      const double* input = dataIn.dataFirst()  ;
      double* output = dataOut.dataFirst()  ;

      for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice)
      {
        const double* in = input;
        double* out = output ;
        int k=0 ;
        for(int i=0; i<dstSize_; i++, out+=sizeT)
          if (mask_[i])
          {
            for(int l=0; l<sizeT; l++) out[l] = in[connector_[k]*sizeT+l] ;
            k++ ;
          }
      }
    }

    void transfer(int sizeT, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
    { 
      transfer(1,sizeT, dataIn, dataOut) ;
    }
    
    void transfer(const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
    {
      transfer(1,1,dataIn, dataOut) ;
    }

  
  };

}

#endif