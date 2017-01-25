/*!
   \file ep_bcast.cpp
   \since 2 may 2016

   \brief Definitions of MPI collective function: MPI_Bcast
 */

#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

using namespace std;


namespace ep_lib
{
  int MPI_Bcast_local(void *buffer, int count, MPI_Datatype datatype, MPI_Comm comm)
  {
    if(datatype == MPI_INT)
    {
      return MPI_Bcast_local_int(buffer, count, comm);
    }
    else if(datatype == MPI_FLOAT)
    {
      return MPI_Bcast_local_float(buffer, count, comm);
    }
    else if(datatype == MPI_DOUBLE)
    {
      return MPI_Bcast_local_double(buffer, count, comm);
    }
    else if(datatype == MPI_CHAR)
    {
      return MPI_Bcast_local_char(buffer, count, comm);
    }
    else if(datatype == MPI_LONG)
    {
      return MPI_Bcast_local_long(buffer, count, comm);
    }
    else if(datatype == MPI_UNSIGNED_LONG)
    {
      return MPI_Bcast_local_char(buffer, count, comm);
    }
    else
    {
      printf("MPI_Bcast Datatype not supported!\n");
      exit(0);
    }
  }

  int MPI_Bcast_local_int(void *buf, int count, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    int *buffer = comm.my_buffer->buf_int;
    int *tmp = static_cast<int*>(buf);

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if(my_rank == 0)
      {
        #pragma omp critical (write_to_buffer)
        {
          copy(tmp+j, tmp+j+min(BUFFER_SIZE, count-j), buffer);
        }
        #pragma omp flush
      }

      MPI_Barrier_local(comm);



      if(my_rank != 0)
      {
        #pragma omp flush
        #pragma omp critical (read_from_buffer)
        {
          copy(buffer, buffer+min(BUFFER_SIZE, count-j), tmp+j);
        }
      }

      MPI_Barrier_local(comm);
    }
  }

  int MPI_Bcast_local_float(void *buf, int count, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    float *buffer = comm.my_buffer->buf_float;
    float *tmp = static_cast<float*>(buf);

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if(my_rank == 0)
      {
        #pragma omp critical (write_to_buffer)
        {
          copy(tmp+j, tmp+j+min(BUFFER_SIZE, count-j), buffer);
        }
        #pragma omp flush
      }

      MPI_Barrier_local(comm);


      if(my_rank != 0)
      {
        #pragma omp flush
        #pragma omp critical (read_from_buffer)
        {
          copy(buffer, buffer+min(BUFFER_SIZE, count-j), tmp+j);
        }
      }

      MPI_Barrier_local(comm);
    }
  }

  int MPI_Bcast_local_double(void *buf, int count, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    double *buffer = comm.my_buffer->buf_double;
    double *tmp = static_cast<double*>(buf);

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if(my_rank == 0)
      {
        #pragma omp critical (write_to_buffer)
        {
          copy(tmp+j, tmp+j+min(BUFFER_SIZE, count-j), buffer);
        }
        #pragma omp flush
      }

      MPI_Barrier_local(comm);


      if(my_rank != 0)
      {
        #pragma omp flush
        #pragma omp critical (read_from_buffer)
        {
          copy(buffer, buffer+min(BUFFER_SIZE, count-j), tmp+j);
        }
      }

      MPI_Barrier_local(comm);
    }
  }


  int MPI_Bcast_local_char(void *buf, int count, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    char *buffer = comm.my_buffer->buf_char;
    char *tmp = static_cast<char*>(buf);

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if(my_rank == 0)
      {
        #pragma omp critical (write_to_buffer)
        {
          copy(tmp+j, tmp+j+min(BUFFER_SIZE, count-j), buffer);
        }
        #pragma omp flush
      }

      MPI_Barrier_local(comm);


      if(my_rank != 0)
      {
        #pragma omp flush
        #pragma omp critical (read_from_buffer)
        {
          copy(buffer, buffer+min(BUFFER_SIZE, count-j), tmp+j);
        }
      }

      MPI_Barrier_local(comm);
    }
  }

  int MPI_Bcast_local_long(void *buf, int count, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    long *buffer = comm.my_buffer->buf_long;
    long *tmp = static_cast<long*>(buf);

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if(my_rank == 0)
      {
        #pragma omp critical (write_to_buffer)
        {
          copy(tmp+j, tmp+j+min(BUFFER_SIZE, count-j), buffer);
        }
        #pragma omp flush
      }

      MPI_Barrier_local(comm);


      if(my_rank != 0)
      {
        #pragma omp flush
        #pragma omp critical (read_from_buffer)
        {
          copy(buffer, buffer+min(BUFFER_SIZE, count-j), tmp+j);
        }
      }

      MPI_Barrier_local(comm);
    }
  }

  int MPI_Bcast_local_ulong(void *buf, int count, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    unsigned long *buffer = comm.my_buffer->buf_ulong;
    unsigned long *tmp = static_cast<unsigned long*>(buf);

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if(my_rank == 0)
      {
        #pragma omp critical (write_to_buffer)
        {
          copy(tmp+j, tmp+j+min(BUFFER_SIZE, count-j), buffer);
        }
        #pragma omp flush
      }

      MPI_Barrier_local(comm);


      if(my_rank != 0)
      {
        #pragma omp flush
        #pragma omp critical (read_from_buffer)
        {
          copy(buffer, buffer+min(BUFFER_SIZE, count-j), tmp+j);
        }
      }

      MPI_Barrier_local(comm);
    }
  }


  int MPI_Bcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm)
  {

    if(!comm.is_ep)
    {
      ::MPI_Bcast(buffer, count, static_cast< ::MPI_Datatype>(datatype), root, static_cast< ::MPI_Comm>(comm.mpi_comm));
      return 0;
    }


    int ep_rank, ep_rank_loc, mpi_rank;
    int ep_size, num_ep, mpi_size;

    ep_rank = comm.ep_comm_ptr->size_rank_info[0].first;
    ep_rank_loc = comm.ep_comm_ptr->size_rank_info[1].first;
    mpi_rank = comm.ep_comm_ptr->size_rank_info[2].first;
    ep_size = comm.ep_comm_ptr->size_rank_info[0].second;
    num_ep = comm.ep_comm_ptr->size_rank_info[1].second;
    mpi_size = comm.ep_comm_ptr->size_rank_info[2].second;



    int root_mpi_rank = comm.rank_map->at(root).second;
    int root_ep_rank_loc = comm.rank_map->at(root).first;


    // if root is not master thread, send first to master
    if(root_ep_rank_loc != 0 && mpi_rank == root_mpi_rank)
    {
      innode_memcpy(root_ep_rank_loc, buffer, 0, buffer, count, datatype, comm);
    }


    if(ep_rank_loc==0)
    {
      ::MPI_Bcast(buffer, count, static_cast< ::MPI_Datatype>(datatype), root_mpi_rank, static_cast< ::MPI_Comm>(comm.mpi_comm));
    }

    MPI_Bcast_local(buffer, count, datatype, comm);

    return 0;
  }


}
