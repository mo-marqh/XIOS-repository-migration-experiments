#ifndef __XIOS_MPI_HPP__
#define __XIOS_MPI_HPP__

/* skip C++ Binding for mpich , intel MPI */
#define MPICH_SKIP_MPICXX

/* skip C++ Binding for SGI MPI library */
#define MPI_NO_CPPBIND

/* skip C++ Binding for OpenMPI */
#define OMPI_SKIP_MPICXX

#include <mpi.h>
#include <climits> 
#include <cstdint>

#if SIZE_MAX == UCHAR_MAX
   #define MPI_SIZE_T MPI_UNSIGNED_CHAR
#elif SIZE_MAX == USHRT_MAX
   #define MPI_SIZE_T MPI_UNSIGNED_SHORT
#elif SIZE_MAX == UINT_MAX
   #define MPI_SIZE_T MPI_UNSIGNED
#elif SIZE_MAX == ULONG_MAX
   #define MPI_SIZE_T MPI_UNSIGNED_LONG
#elif SIZE_MAX == ULLONG_MAX
   #define MPI_SIZE_T MPI_UNSIGNED_LONG_LONG
#else
   #error "Unable to find MPI_SIZE_T equivalent type"
#endif

#include "mpi_tools.hpp"

#endif
