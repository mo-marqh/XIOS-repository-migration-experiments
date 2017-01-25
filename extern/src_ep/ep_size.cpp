#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

namespace ep_lib
{




  int MPI_Comm_size(MPI_Comm comm, int* size)
  {
    
    if(comm.is_ep)
    {
      if(!comm.is_intercomm)
      {
        *size = comm.ep_comm_ptr->size_rank_info[0].second;

        return 0;
      }
      else
      {
        *size = comm.ep_comm_ptr->intercomm->size_rank_info[0].second;

        return 0;
      }

    }

    Debug("Calling EP_Comm_size\n");

    if(comm.mpi_comm)
    {
      ::MPI_Comm mpi_comm = static_cast< ::MPI_Comm > (comm.mpi_comm); 
      int mpi_size;

      ::MPI_Comm_size(mpi_comm, &mpi_size);
      printf("============ ep comm size called for non ep comm, %d\n", mpi_size);
      *size = mpi_size;
      return 0;
    }

    else
    {
      *size = MPI_UNDEFINED;
      printf("============ ep comm size called for non ep comm not defined\n");
      return 0;
    }
  }

  int MPI_Comm_remote_size(MPI_Comm comm, int *size)
  {
    if(!comm.is_ep)
    {
      if(comm.mpi_comm)
      {
        ::MPI_Comm mpi_comm = static_cast< ::MPI_Comm > (comm.mpi_comm); 
        #ifdef _serialized
        #pragma omp critical (_mpi_call)
        #endif // _serialized
        ::MPI_Comm_remote_size(mpi_comm, size);
        return 0;
      }

      else
      {
        *size = MPI_UNDEFINED;
        return 0;
      }
    }
    
    if(comm.is_intercomm)
    {
      *size = comm.ep_comm_ptr->intercomm->remote_rank_map->size();
      return 0;
    }
    *size = MPI_UNDEFINED;
    return 0; 
  }
}


