#include "ep_lib.hpp"
#include <mpi.h>
//#include "ep_declaration.hpp"

namespace ep_lib
{



  int MPI_Barrier(MPI_Comm comm)
  {

    if(comm.is_ep)
    {
      int ep_rank_loc = comm.ep_comm_ptr->size_rank_info[1].first;

      MPI_Barrier_local(comm);

      if(ep_rank_loc == 0)
      {
        ::MPI_Comm mpi_comm = static_cast< ::MPI_Comm > (comm.mpi_comm);
        ::MPI_Barrier(mpi_comm);
      }

      MPI_Barrier_local(comm);

      return 0;
    }
    else if(comm.mpi_comm !=0 )
    {
      ::MPI_Comm mpi_comm = static_cast< ::MPI_Comm> (comm.mpi_comm);
      ::MPI_Barrier(mpi_comm);
      return 0;
    }

    else return 0;
  }

  int MPI_Barrier_local(MPI_Comm comm)
  {
    //Message_Check(comm);
    comm.ep_barrier->wait();
  }


}


