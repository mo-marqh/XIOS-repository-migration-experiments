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
      
  };
}


#endif // __XIOS_ONETCDF4_PLUGIN__
