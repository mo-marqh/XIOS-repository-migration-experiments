#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"


namespace ep_lib
{

  int MPI_Finalize()
  {
    printf("calling EP Finalize\n");

    int id = omp_get_thread_num();

    if(id == 0)
    {
      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Finalize();
    }
    return 0;
  }

  int MPI_Abort(MPI_Comm comm, int errorcode)
  {
    int id = omp_get_thread_num();

    if(id == 0)
    {
      ::MPI_Comm mpi_comm = static_cast< ::MPI_Comm >(comm.mpi_comm);

      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Abort(mpi_comm, errorcode);
    }
    return 0;
  }

}


