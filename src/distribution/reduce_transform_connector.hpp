#ifndef __REDUCE_TRANSFORM_CONNECTOR_HPP__
#define __REDUCE_TRANSFORM_CONNECTOR_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "local_view.hpp"
#include "reduction_types.hpp"
#include <algorithm>



namespace xios
{
  
  class CReduceTransformConnector
  {
    
    private:
      CLocalView* srcView_;
      CLocalView* dstView_;

      vector<int> connector_;  //  sizeof sum(nWeights_)  
      vector<int> nSrc_ ;  //  sizeof dstSize_ 
      int srcSize_ ;
      int dstSize_ ;
      bool detectMissingValue_ ;
      EReduction type_ ;
      
      typedef void (CReduceTransformConnector::* transferPtr)(int, int, const CArray<double,1>& , CArray<double,1>&) ;
      transferPtr transfer_ ;
      
     void computeConnector(unordered_map<int, std::vector<int>>& indexMap) ;
      
    public:

    CReduceTransformConnector(CLocalView* srcView, CLocalView* dstView, EReduction op, unordered_map<int, std::vector<int>>& indexMap, 
                              bool detectMissingValue=true) ;
 
    void transfer(const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
    {
      transfer(1,1,dataIn, dataOut) ;
    }
        void transfer(int sizeT, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
    { 
      transfer(1,sizeT, dataIn, dataOut) ;
    }

    void transfer(int repeat, int sizeT, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
    {
      (this->*transfer_)(repeat, sizeT, dataIn, dataOut) ;
    }
    
    private :
    
    void transferSum(int repeat, int sizeT, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
    {
      double defaultValue = std::numeric_limits<double>::quiet_NaN();

      int dstSlice = dstSize_*sizeT ;
      int srcSlice = srcSize_*sizeT ;
      dataOut.resize(repeat* dstSlice) ;
      dataOut=0 ;
           
      const double* input = dataIn.dataFirst()  ;
      double* output = dataOut.dataFirst()  ;

      if (detectMissingValue_)
      {
        vector<bool> touched(repeat* dstSlice, false) ;
        int pos=0 ;
        for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice, pos+=dstSlice)
        {
          const double* in = input;
          double* out = output ;
          int k=0 ;
          for(int i=0; i<dstSize_; i++, out+=sizeT, pos+=sizeT)
          {
            if (nSrc_[i]==0) for(int l=0; l<sizeT; l++) out[l] = defaultValue ;
            else 
            {
              for(int j=0 ; j<nSrc_[i] ; j++,k++)
                for(int l=0; l<sizeT; l++) 
                {
                  if (! std::isnan(in[connector_[k]*sizeT+l]) ) 
                  {
                    if (touched[pos+l]) out[l] += in[connector_[k]*sizeT+l] ;
                    else 
                    {
                      touched[pos+l] = true ;
                      out[l] = in[connector_[k]*sizeT+l] ;
                    }
                  }
                }
            }
          }
        }

        output = dataOut.dataFirst()  ;
        pos=0 ;
        for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice, pos+=dstSlice)
        {
          const double* in = input;
          double* out = output ;
          int k=0 ;
          int pos=0 ;
          for(int i=0; i<dstSize_; i++, out+=sizeT, pos+=sizeT)
              for(int j=0 ; j<nSrc_[i] ; j++,k++)
                for(int l=0; l<sizeT; l++) if (!touched[pos+l]) out[l] = defaultValue ;
        }

      }
      else
      {
        for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice)
        {
          const double* in = input;
          double* out = output ;
          int k=0 ;
          for(int i=0; i<dstSize_; i++, out+=sizeT)
            if (nSrc_[i]==0)
              for(int l=0; l<sizeT; l++) out[l] = defaultValue ;
            else 
            {
              for(int j=0 ; j<nSrc_[i] ; j++,k++)
                for(int l=0; l<sizeT; l++) out[l] += in[connector_[k]*sizeT+l] ;
            }
        }
      }
    }

    void transferMin(int repeat, int sizeT, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
    {
      double defaultValue = std::numeric_limits<double>::quiet_NaN();

      int dstSlice = dstSize_*sizeT ;
      int srcSlice = srcSize_*sizeT ;
      dataOut.resize(repeat* dstSlice) ;
      dataOut=0 ;
           
      const double* input = dataIn.dataFirst()  ;
      double* output = dataOut.dataFirst()  ;

      if (detectMissingValue_)
      {
        vector<bool> touched(repeat* dstSlice, false) ;
        int pos=0 ;
        for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice, pos+=dstSlice)
        {
          const double* in = input;
          double* out = output ;
          int k=0 ;
          for(int i=0; i<dstSize_; i++, out+=sizeT, pos+=sizeT)
          {
            if (nSrc_[i]==0) for(int l=0; l<sizeT; l++) out[l] = defaultValue ;
            else 
            {
              for(int j=0 ; j<nSrc_[i] ; j++,k++)
                for(int l=0; l<sizeT; l++) 
                {
                  if (! std::isnan(in[connector_[k]*sizeT+l]) ) 
                  {
                    if (touched[pos+l]) out[l]=min(out[l],in[connector_[k]*sizeT+l]) ;
                    else 
                    {
                      touched[pos+l] = true ;
                      out[l] = in[connector_[k]*sizeT+l] ;
                    }
                  }
                }
            }
          }
        }

        output = dataOut.dataFirst()  ;
        pos=0 ;
        for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice, pos+=dstSlice)
        {
          const double* in = input;
          double* out = output ;
          int k=0 ;
          int pos=0 ;
          for(int i=0; i<dstSize_; i++, out+=sizeT, pos+=sizeT)
              for(int j=0 ; j<nSrc_[i] ; j++,k++)
                for(int l=0; l<sizeT; l++) if (!touched[pos+l]) out[l] = defaultValue ;
        }

      }
      else
      {
        for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice)
        {
          const double* in = input;
          double* out = output ;
          int k=0 ;
          for(int i=0; i<dstSize_; i++, out+=sizeT)
            if (nSrc_[i]==0)
              for(int l=0; l<sizeT; l++) out[l] = defaultValue ;
            else 
            {
              for(int j=0 ; j<nSrc_[i] ; j++,k++)
                for(int l=0; l<sizeT; l++) out[l]=min(out[l],in[connector_[k]*sizeT+l]) ;
            }
        }
      }
    }

    void transferMax(int repeat, int sizeT, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
    {
      double defaultValue = std::numeric_limits<double>::quiet_NaN();

      int dstSlice = dstSize_*sizeT ;
      int srcSlice = srcSize_*sizeT ;
      dataOut.resize(repeat* dstSlice) ;
      dataOut=0 ;
           
      const double* input = dataIn.dataFirst()  ;
      double* output = dataOut.dataFirst()  ;

      if (detectMissingValue_)
      {
        vector<bool> touched(repeat* dstSlice, false) ;
        int pos=0 ;
        for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice, pos+=dstSlice)
        {
          const double* in = input;
          double* out = output ;
          int k=0 ;
          for(int i=0; i<dstSize_; i++, out+=sizeT, pos+=sizeT)
          {
            if (nSrc_[i]==0) for(int l=0; l<sizeT; l++) out[l] = defaultValue ;
            else 
            {
              for(int j=0 ; j<nSrc_[i] ; j++,k++)
                for(int l=0; l<sizeT; l++) 
                {
                  if (! std::isnan(in[connector_[k]*sizeT+l]) ) 
                  {
                    if (touched[pos+l]) out[l]=max(out[l],in[connector_[k]*sizeT+l]) ;
                    else 
                    {
                      touched[pos+l] = true ;
                      out[l] = in[connector_[k]*sizeT+l] ;
                    }
                  }
                }
            }
          }
        }

        output = dataOut.dataFirst()  ;
        pos=0 ;
        for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice, pos+=dstSlice)
        {
          const double* in = input;
          double* out = output ;
          int k=0 ;
          int pos=0 ;
          for(int i=0; i<dstSize_; i++, out+=sizeT, pos+=sizeT)
              for(int j=0 ; j<nSrc_[i] ; j++,k++)
                for(int l=0; l<sizeT; l++) if (!touched[pos+l]) out[l] = defaultValue ;
        }

      }
      else
      {
        for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice)
        {
          const double* in = input;
          double* out = output ;
          int k=0 ;
          for(int i=0; i<dstSize_; i++, out+=sizeT)
            if (nSrc_[i]==0)
              for(int l=0; l<sizeT; l++) out[l] = defaultValue ;
            else 
            {
              for(int j=0 ; j<nSrc_[i] ; j++,k++)
                for(int l=0; l<sizeT; l++) out[l]=max(out[l],in[connector_[k]*sizeT+l]) ;
            }
        }
      }
    }

    void transferAverage(int repeat, int sizeT, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
    {
      double defaultValue = std::numeric_limits<double>::quiet_NaN();

      int dstSlice = dstSize_*sizeT ;
      int srcSlice = srcSize_*sizeT ;
      dataOut.resize(repeat* dstSlice) ;
      dataOut=0 ;
           
      const double* input = dataIn.dataFirst()  ;
      double* output = dataOut.dataFirst()  ;

      if (detectMissingValue_)
      {
        vector<int> touched(repeat* dstSlice, 0) ;
        int pos=0 ;
        for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice, pos+=dstSlice)
        {
          const double* in = input;
          double* out = output ;
          int k=0 ;
          for(int i=0; i<dstSize_; i++, out+=sizeT, pos+=sizeT)
          {
            if (nSrc_[i]==0) for(int l=0; l<sizeT; l++) out[l] = defaultValue ;
            else 
            {
              for(int j=0 ; j<nSrc_[i] ; j++,k++)
                for(int l=0; l<sizeT; l++) 
                {
                  if (! std::isnan(in[connector_[k]*sizeT+l]) ) 
                  {
                    if (touched[pos+l]) out[l] += in[connector_[k]*sizeT+l] ;
                    else 
                    {
                      touched[pos+l]++ ;
                      out[l] = in[connector_[k]*sizeT+l] ;
                    }
                  }
                }
            }
          }
        }

        output = dataOut.dataFirst()  ;
        pos=0 ;
        for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice, pos+=dstSlice)
        {
          const double* in = input;
          double* out = output ;
          int k=0 ;
          int pos=0 ;
          for(int i=0; i<dstSize_; i++, out+=sizeT, pos+=sizeT)
              for(int j=0 ; j<nSrc_[i] ; j++,k++)
                for(int l=0; l<sizeT; l++) 
                  if (touched[pos+l]==0) out[l] = defaultValue ;
                  else out[l]/=touched[pos+l] ;
        }

      }
      else
      {
        for(int r=0;r<repeat;r++, input+=srcSlice, output+=dstSlice)
        {
          const double* in = input;
          double* out = output ;
          int k=0 ;
          for(int i=0; i<dstSize_; i++, out+=sizeT)
            if (nSrc_[i]==0)
              for(int l=0; l<sizeT; l++) out[l] = defaultValue ;
            else 
            {
              for(int j=0 ; j<nSrc_[i] ; j++,k++)
                for(int l=0; l<sizeT; l++) out[l] += in[connector_[k]*sizeT+l] ;
            }
          
          out = output ;
          for(int i=0; i<dstSize_; i++, out+=sizeT)
            if (nSrc_[i]!=0)
              for(int l=0; l<sizeT; l++) out[l]/=nSrc_[i];
        }
      }
    }
  
  };

}

#endif