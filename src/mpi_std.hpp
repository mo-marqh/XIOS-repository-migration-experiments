#ifndef __XIOS_MPI_HPP__
#define __XIOS_MPI_HPP__

/* skip C++ Binding for mpich , intel MPI */
#define MPICH_SKIP_MPICXX

/* skip C++ Binding for SGI MPI library */
#define MPI_NO_CPPBIND

/* skip C++ Binding for OpenMPI */
#define OMPI_SKIP_MPICXX


//#include "mpi_wrapper.hpp"
#ifdef _usingEP
#include "ep_lib.hpp"
#include "ep_declaration.hpp"
#endif

#include <mpi.h>

#endif
