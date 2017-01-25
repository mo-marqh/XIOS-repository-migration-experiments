#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

namespace ep_lib
{



  int MPI_Comm_free(MPI_Comm *comm)
  {
    //return 0;
    
    if(comm == NULL) return 0;

    MPI_Barrier(*comm);


    if(! comm->is_ep)
    {
      if(comm->mpi_comm)
      {
        ::MPI_Comm mpi_comm = static_cast< ::MPI_Comm>(comm->mpi_comm);

        ::MPI_Comm_free(&mpi_comm);
        Debug("comm is MPI, freed\n");
      }
      return 0;
    }

    else
    {
      if(! comm->mpi_comm) return 0;

      int ep_rank, ep_rank_loc, mpi_rank;
      int ep_size, num_ep, mpi_size;

      ep_rank = comm->ep_comm_ptr->size_rank_info[0].first;
      ep_rank_loc = comm->ep_comm_ptr->size_rank_info[1].first;
      mpi_rank = comm->ep_comm_ptr->size_rank_info[2].first;
      ep_size = comm->ep_comm_ptr->size_rank_info[0].second;
      num_ep = comm->ep_comm_ptr->size_rank_info[1].second;
      mpi_size = comm->ep_comm_ptr->size_rank_info[2].second;

      MPI_Barrier(*comm);


      #pragma omp critical (memory_free)
      if(comm->is_intercomm && comm->ep_comm_ptr->intercomm)
      {
        if(comm->ep_comm_ptr->intercomm->local_rank_map) comm->ep_comm_ptr->intercomm->local_rank_map->clear();
        if(comm->ep_comm_ptr->intercomm->remote_rank_map) comm->ep_comm_ptr->intercomm->remote_rank_map->clear();
        if(comm->ep_comm_ptr->intercomm->intercomm_rank_map) comm->ep_comm_ptr->intercomm->intercomm_rank_map->clear();
        Debug("intercomm local/remote/intercomm_rank_map emptied\n");
      }


      if(ep_rank_loc == 0)
      {
        Debug("comm is EP, mpi_comm_ptr != NULL\n");


/*
        if(comm->my_buffer)
        {
          if(comm->my_buffer->buf_int) delete[] comm->my_buffer->buf_int; Debug("buf_int freed\n");
          if(comm->my_buffer->buf_float) delete[] comm->my_buffer->buf_float; Debug("buf_float freed\n");
          if(comm->my_buffer->buf_double) delete[] comm->my_buffer->buf_double; Debug("buf_double freed\n");
          if(comm->my_buffer->buf_long) delete[] comm->my_buffer->buf_long; Debug("buf_long freed\n");
          if(comm->my_buffer->buf_ulong) delete[] comm->my_buffer->buf_ulong; Debug("buf_ulong freed\n");
          if(comm->my_buffer->buf_char) delete[] comm->my_buffer->buf_char; Debug("buf_char freed\n");
        }
*/
        if(comm->ep_barrier)
        {
          comm->ep_barrier->~OMPbarrier();
          Debug("ep_barrier freed\n");
        }


        if(comm->rank_map)
        {
          comm->rank_map->clear();
          Debug("rank_map emptied\n");
        }


        if(comm->is_intercomm && comm->ep_comm_ptr->intercomm->mpi_inter_comm)
        {
          ::MPI_Comm mpi_comm = static_cast< ::MPI_Comm>(comm->ep_comm_ptr->intercomm->mpi_inter_comm);

          ::MPI_Comm_free(&mpi_comm);
          comm->ep_comm_ptr->intercomm->mpi_inter_comm = NULL;
          Debug("mpi_intercomm freed\n");
        }



        for(int i=0; i<num_ep; i++)
        {
          if(comm->ep_comm_ptr->comm_list[i].ep_comm_ptr->message_queue)
          {
            comm->ep_comm_ptr->comm_list[i].ep_comm_ptr->message_queue->clear();
            Debug("message queue freed\n");
          }

          if(comm->ep_comm_ptr->comm_list[i].ep_comm_ptr)
          {
            delete comm->ep_comm_ptr->comm_list[i].ep_comm_ptr;
            Debug("ep_comm_ptr freed\n");
          }
        }

        if(comm->mpi_comm)
        {
          ::MPI_Comm mpi_comm = static_cast< ::MPI_Comm>(comm->mpi_comm);
          #ifdef _serialized
          #pragma omp critical (_mpi_call)
          #endif // _serialized
          ::MPI_Comm_free(&mpi_comm);
          comm->mpi_comm = NULL;
//          printf("label = %d, mpi_comm freed\n", comm->ep_comm_ptr->comm_label);
        }

       if(comm) {delete[] comm->ep_comm_ptr->comm_list; Debug("comm freed\n");}

      }
      return 0;
    }



  }




}


