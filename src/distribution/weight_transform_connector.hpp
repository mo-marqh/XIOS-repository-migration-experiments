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
      shared_ptr<CLocalView> srcView_;
      shared_ptr<CLocalView> dstView_;

      vector<double> weights_; //  sizeof sum(nWeights_)  
      vector<int> connector_;  //  sizeof sum(nWeights_)  
      vector<int> nWeights_ ;  //  sizeof dstSize_ 
      int srcSize_ ;
      int dstSize_ ;
      bool detectMissingValue_ ;
      bool renormalize_ ;

     void computeConnector(unordered_map<int, std::vector<int>>& indexMap, unordered_map<int, std::vector<double>>& weightMap) ;
      
    public:

    CWeightTransformConnector(shared_ptr<CLocalView> srcView, shared_ptr<CLocalView> dstView, unordered_map<int, std::vector<int>>& indexMap, 
                              unordered_map<int, std::vector<double>>& weightMap, bool detectMissingValue, bool renormalize) ;
 
    void transfer(int repeat, int sizeT, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
    {
      int dstSlice = dstSize_*sizeT ;
      int srcSlice = srcSize_*sizeT ;
      dataOut.resize(repeat* dstSlice) ;
      double defaultValue = std::numeric_limits<double>::quiet_NaN();
      dataOut = defaultValue ; // what to do about missing value => next step ?
      const double* input = dataIn.dataFirst()  ;
      double* output = dataOut.dataFirst()  ;
      vector<bool> isFirst(dstSlice) ;
      size_t pos ;
      
      if (renormalize_)
      {
        double inVal ;
        vector<double> renormalizeFactor(repeat*dstSlice,1) ;
        double* renorm = renormalizeFactor.data() ;
        for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice, renorm+=dstSlice)
        {
          const double* in = input;
          double* out = output ;
          double* ren = renorm ;
          isFirst.assign(dstSlice,true) ;
          pos=0 ;
          int k=0 ;
          for(int i=0; i<dstSize_; i++, out+=sizeT, pos+=sizeT, ren+=sizeT)
            for(int j=0 ; j<nWeights_[i] ; j++,k++)
              for(int l=0; l<sizeT; l++)
              {
                inVal=in[connector_[k]*sizeT+l] ;
                if (!std::isnan(inVal))
                {
                  if (isFirst[pos+l])  { out[l] = inVal*weights_[k] ; isFirst[pos+l]=false ;}
                  else out[l] += inVal*weights_[k] ;
                }
                else  ren[l] -= weights_[k] ;
              }
          for(int i=0; i<dstSlice; i++) output[i] /= renorm[i] ; 
        }
      }
      else if (detectMissingValue_)
      {
        double inVal ;
        for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice)
        {
          const double* in = input;
          double* out = output ;
          isFirst.assign(dstSlice,true) ;
          pos=0 ;
          int k=0 ;
          for(int i=0; i<dstSize_; i++, out+=sizeT, pos+=sizeT)
            for(int j=0 ; j<nWeights_[i] ; j++,k++)
              for(int l=0; l<sizeT; l++)
              {
                inVal=in[connector_[k]*sizeT+l] ;
                if (!std::isnan(inVal))
                {
                  if (isFirst[pos+l])  { out[l] = inVal*weights_[k] ; isFirst[pos+l]=false ;}
                  else out[l] += inVal*weights_[k] ;
                }
              }
        }

      }
      else
      {
        for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice)
        {
          const double* in = input;
          double* out = output ;
          isFirst.assign(dstSlice,true) ;
          pos=0 ;
          int k=0 ;
          for(int i=0; i<dstSize_; i++, out+=sizeT, pos+=sizeT)
            for(int j=0 ; j<nWeights_[i] ; j++,k++)
              for(int l=0; l<sizeT; l++) 
                if (isFirst[pos+l])  { out[l] = in[connector_[k]*sizeT+l]*weights_[k] ; isFirst[pos+l]=false ;}
                else out[l] += in[connector_[k]*sizeT+l]*weights_[k] ;
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