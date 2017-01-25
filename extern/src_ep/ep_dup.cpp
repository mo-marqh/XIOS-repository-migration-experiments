#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

namespace ep_lib
{



  int MPI_Comm_dup_dev(MPI_Comm comm, MPI_Comm *newcomm)
  {
    if(!comm.is_ep)
    {
      Debug("Comm_dup MPI\n");
      newcomm = new MPI_Comm;
      newcomm->is_ep = comm.is_ep;

      ::MPI_Comm input = static_cast< ::MPI_Comm>(comm.mpi_comm);
      ::MPI_Comm output;

      
      ::MPI_Comm_dup(input, &output);

      newcomm->mpi_comm = output;

      return 0;
    }

    if(!comm.mpi_comm) return 0;


    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;


    if(0 == my_rank)
    {
      MPI_Info info;
      ::MPI_Comm mpi_dup;

      ::MPI_Comm in_comm = static_cast< ::MPI_Comm>(comm.mpi_comm);

      ::MPI_Comm_dup(in_comm, &mpi_dup);

      MPI_Comm_create_endpoints(mpi_dup, num_ep, info, newcomm);
      comm.ep_comm_ptr->comm_list->mem_bridge = newcomm;
    }

    MPI_Barrier_local(comm);

    newcomm = &(comm.ep_comm_ptr->comm_list->mem_bridge[my_rank]);

    return MPI_SUCCESS;
  }



  int MPI_Comm_dup(MPI_Comm comm, MPI_Comm *newcomm)
  {
    if(!comm.is_ep)
    {
      Debug("Comm_dup MPI\n");
      newcomm = new MPI_Comm;
      newcomm->is_ep = comm.is_ep;

      ::MPI_Comm input = static_cast< ::MPI_Comm>(comm.mpi_comm);
      ::MPI_Comm output;

      
      ::MPI_Comm_dup(input, &output);

      newcomm->mpi_comm = output;

      return 0;
    }

    if(!comm.mpi_comm) return 0;


    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;


    if(0 == my_rank)
    {
      MPI_Info info;
      MPI_Comm *out_comm;
      ::MPI_Comm mpi_dup;

      ::MPI_Comm in_comm = static_cast< ::MPI_Comm>(comm.mpi_comm);

      ::MPI_Comm_dup(in_comm, &mpi_dup);

      MPI_Comm_create_endpoints(mpi_dup, num_ep, info, out_comm);
      comm.ep_comm_ptr->comm_list->mem_bridge = out_comm;
    }

    MPI_Barrier(comm);

    *newcomm = (comm.ep_comm_ptr->comm_list->mem_bridge[my_rank]);

    return MPI_SUCCESS;
  }




}


