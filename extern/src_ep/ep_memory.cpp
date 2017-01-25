#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

namespace ep_lib
{



  int MPI_Alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr)
  {
    #ifdef _serialized
    #pragma omp critical (_mpi_call)
    #endif // _serialized
    ::MPI_Alloc_mem(size.mpi_aint, info.mpi_info, baseptr);
    return 0;
   }
   
  int MPI_Alloc_mem(unsigned long size, MPI_Info info, void *baseptr)
  {
    #ifdef _serialized
    #pragma omp critical (_mpi_call)
    #endif // _serialized
    ::MPI_Alloc_mem(size, info.mpi_info, baseptr);
    return 0;
   }



}


