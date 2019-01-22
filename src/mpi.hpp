#ifndef __XIOS_MPI_HPP__
#define __XIOS_MPI_HPP__

/* skip C++ Binding for mpich , intel MPI */
#define MPICH_SKIP_MPICXX

/* skip C++ Binding for SGI MPI library */
#define MPI_NO_CPPBIND

/* skip C++ Binding for OpenMPI */
#define OMPI_SKIP_MPICXX

#ifdef _usingEP
#include "ep_lib.hpp"
#include "ep_declaration.hpp"
#endif

#include <mpi.h>

#ifdef _usingMPI

#define ep_lib  

#define EP_INT MPI_INT
#define EP_FLOAT MPI_FLOAT
#define EP_DOUBLE MPI_DOUBLE
#define EP_CHAR MPI_CHAR
#define EP_LONG MPI_LONG
#define EP_LONG_LONG_INT MPI_LONG_LONG_INT
#define EP_UNSIGNED_LONG  MPI_UNSIGNED_LONG
#define EP_UNSIGNED_CHAR  MPI_UNSIGNED_CHAR


#define EP_COMM_WORLD MPI_COMM_WORLD
#define EP_COMM_NULL MPI_COMM_NULL
#define EP_INFO_NULL MPI_INFO_NULL

#define EP_MAX MPI_MAX
#define EP_MIN MPI_MIN
#define EP_SUM MPI_SUM
#define EP_LOR MPI_LOR

#endif

#endif
