#ifndef __XIOS_ONETCDF4_PLUGIN__
#define __XIOS_ONETCDF4_PLUGIN__

#include "xios_spl.hpp"

#if !defined(USING_NETCDF_PAR)
#include "exception.hpp"
#endif

#include "mpi.hpp"
namespace xios
{
  /*!
  \class CONetCDF4Plugin
   This class computes parameters for HDF5 plugin compression filters : SZ, ZFP ...
  */
  class CONetCDF4Plugin
  {
  public:
    //! Compute HDF5 plugin parameters from SZ parameters
    static void interpretParametersSZ(const CArray<double,1>& inParams, size_t* nOutParams, unsigned int **outParams)
    {
      if (inParams.numElements()!=5)
      {
          ERROR("CONetCDF4Plugin::interpretParametersSZ(...)", "The SZ compressor requires 5 parameters : "
                << "compression_params=\"(0,4)[mode abs_err rel_err pw_rel_err psnr]\" must be specified,"
                << " see details in https://www.mcs.anl.gov/~shdi/download/sz-2.0-user-guide.pdf" );
      }
      *nOutParams = 9;
      *outParams = new unsigned int[*nOutParams];
      
      for (size_t iParam=0 ; iParam<(*nOutParams) ; iParam++) (*outParams)[iParam] = 0;

      // 1st parameter is the SZ error bound mode
      //   https://github.com/szcompressor/SZ/blob/master/hdf5-filter/H5Z-SZ/docs/H5Z-SZ-Guide.pdf
      (*outParams)[0] = (unsigned int)(inParams(0));
      
      // 1 double -> 2 unsigned int
      for (size_t iParam=0 ; iParam<4 ; iParam++)
      {
        memcpy( &((*outParams)[1+iParam*2]), &(inParams(1+iParam)), sizeof(double) );
        unsigned int tmp_swap      = (*outParams)[1+iParam*2  ];
        (*outParams)[1+iParam*2  ] = (*outParams)[1+iParam*2+1];
        (*outParams)[1+iParam*2+1] = tmp_swap;
      }
    }
      
    //! Compute HDF5 plugin parameters from ZFP parameters
    static void interpretParametersZFP(const CArray<double,1>& inParams, size_t* nOutParams, unsigned int **outParams)
    {
      if (inParams.numElements()==0) // N = f( mode )
      {
          ERROR("CONetCDF4Plugin::interpretParametersSZ(...)", "The 1st parameter of ZFP compressor is the mode, must be lower or equal to 5" );
      }
      
      // https://github.com/LLNL/H5Z-ZFP/blob/master/test/test_write.c#L237
      // https://github.com/LLNL/H5Z-ZFP/blob/master/src/H5Zzfp_version.h
      if      (inParams(0) == 1) *nOutParams = 4; // RATE       (mode=1) : 1 double
      else if (inParams(0) == 2) *nOutParams = 3; // PRECISION  (mode=2) : 2 unsigned int
      else if (inParams(0) == 3) *nOutParams = 4; // ACCURACY   (mode=3) : 1 double
      else if (inParams(0) == 4) *nOutParams = 6; // EXPERT     (mode=4) : 3 unsigned int + 1 int
      else if (inParams(0) == 5) *nOutParams = 1; // REVERSIBLE (mode=5) : -

      *outParams = new unsigned int[*nOutParams];

      // https://github.com/LLNL/H5Z-ZFP/blob/master/src/H5Zzfp_plugin.h
      (*outParams)[0] = (unsigned int)(inParams(0));
      (*outParams)[1] = 0;
      if ((inParams(0) == 1)||(inParams(0) == 3))
      {
        memcpy( &((*outParams)[2]), &(inParams(1)), sizeof(double) );
      }
      else if (inParams(0) == 2)
      {
        (*outParams)[2] = (unsigned int)(inParams(1));
      }
      else if (inParams(0) == 4)
      {
        (*outParams)[2] = (unsigned int)(inParams(1));
        (*outParams)[3] = (unsigned int)(inParams(2));
        (*outParams)[4] = (unsigned int)(inParams(3));
        (*outParams)[5] = (unsigned int)(inParams(4));
      }

    }
  };
}


#endif // __XIOS_ONETCDF4_PLUGIN__
