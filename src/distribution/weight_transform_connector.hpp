#ifndef __WEIGHT_TRANSFORM_CONNECTOR_HPP__
#define __WEIGHT_TRANSFORM_CONNECTOR_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "local_view.hpp"



namespace xios
{
  
  class CWeightTransformConnector
  {
    
    private:
      CLocalView* srcView_;
      CLocalView* dstView_;

      vector<double> weights_; //  sizeof sum(nWeights_)  
      vector<int> connector_;  //  sizeof sum(nWeights_)  
      vector<int> nWeights_ ;  //  sizeof dstSize_ 
      int srcSize_ ;
      int dstSize_ ;

     void computeConnector(unordered_map<int, std::vector<int>>& indexMap, unordered_map<int, std::vector<double>>& weightMap) ;
      
    public:

    CWeightTransformConnector(CLocalView* srcView, CLocalView* dstView, unordered_map<int, std::vector<int>>& indexMap, 
                              unordered_map<int, std::vector<double>>& weightMap) ;
 
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
          for(int j=0 ; j<nWeights_[i] ; j++,k++)
            for(int l=0; l<sizeT; l++) out[l] += in[connector_[k]*sizeT+l]*weights_[k] ;
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