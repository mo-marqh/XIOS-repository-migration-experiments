#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

namespace ep_lib
{



  int MPI_Comm_rank(MPI_Comm comm, int* rank)
  {

    if(comm.is_ep)
    {
      Debug("Calling EP_Comm_rank\n");

      if(comm.is_intercomm)
      {
        *rank = comm.ep_comm_ptr->intercomm->size_rank_info[0].first;
        return 0;
      }
      else
      {
        *rank = comm.ep_comm_ptr->size_rank_info[0].first;
        return 0;
      }
    }


    if(comm.mpi_comm)
    {
      ::MPI_Comm mpi_comm = static_cast< ::MPI_Comm > (comm.mpi_comm);
      ::MPI_Comm_rank(mpi_comm, rank);
      return 0;
    }
    else
    {
      printf("rank is undefined\n");

      *rank = MPI_UNDEFINED;
      return 0;
    }

  }



}


