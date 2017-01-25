/*!
   \file ep_gather.cpp
   \since 2 may 2016

   \brief Definitions of MPI collective function: MPI_Gather, MPI_Allgather
 */

#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"


using namespace std;

namespace ep_lib
{

  int MPI_Gather_local(const void *sendbuf, int count, MPI_Datatype datatype, void *recvbuf, MPI_Comm comm)
  {
    if(datatype == MPI_INT)
    {
      Debug("datatype is INT\n");
      return MPI_Gather_local_int(sendbuf, count, recvbuf, comm);
    }
    else if(datatype == MPI_FLOAT)
    {
      Debug("datatype is FLOAT\n");
      return MPI_Gather_local_float(sendbuf, count, recvbuf, comm);
    }
    else if(datatype == MPI_DOUBLE)
    {
      Debug("datatype is DOUBLE\n");
      return MPI_Gather_local_double(sendbuf, count, recvbuf, comm);
    }
    else if(datatype == MPI_LONG)
    {
      Debug("datatype is LONG\n");
      return MPI_Gather_local_long(sendbuf, count, recvbuf, comm);
    }
    else if(datatype == MPI_UNSIGNED_LONG)
    {
      Debug("datatype is uLONG\n");
      return MPI_Gather_local_ulong(sendbuf, count, recvbuf, comm);
    }
    else if(datatype == MPI_CHAR)
    {
      Debug("datatype is CHAR\n");
      return MPI_Gather_local_char(sendbuf, count, recvbuf, comm);
    }
    else
    {
      printf("MPI_Gather Datatype not supported!\n");
      exit(0);
    }
  }

  int MPI_Gather_local_int(const void *sendbuf, int count, void *recvbuf, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    int *buffer = comm.my_buffer->buf_int;
    int *send_buf = static_cast<int*>(const_cast<void*>(sendbuf));
    int *recv_buf = static_cast<int*>(recvbuf);

    if(my_rank == 0)
    {
      copy(send_buf, send_buf+count, recv_buf);
    }

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      for(int k=1; k<num_ep; k++)
      {
        if(my_rank == k)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);

        if(my_rank == 0)
        {
          #pragma omp flush
          #pragma omp critical (read_from_buffer)
          {
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j+k*count);
          }
        }

        MPI_Barrier_local(comm);
      }
    }
  }

  int MPI_Gather_local_float(const void *sendbuf, int count, void *recvbuf, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    float *buffer = comm.my_buffer->buf_float;
    float *send_buf = static_cast<float*>(const_cast<void*>(sendbuf));
    float *recv_buf = static_cast<float*>(recvbuf);

    if(my_rank == 0)
    {
      copy(send_buf, send_buf+count, recv_buf);
    }

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      for(int k=1; k<num_ep; k++)
      {
        if(my_rank == k)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);

        if(my_rank == 0)
        {
          #pragma omp flush
          #pragma omp critical (read_from_buffer)
          {
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j+k*count);
          }
        }

        MPI_Barrier_local(comm);
      }
    }
  }

  int MPI_Gather_local_double(const void *sendbuf, int count, void *recvbuf, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    double *buffer = comm.my_buffer->buf_double;
    double *send_buf = static_cast<double*>(const_cast<void*>(sendbuf));
    double *recv_buf = static_cast<double*>(recvbuf);

    if(my_rank == 0)
    {
      copy(send_buf, send_buf+count, recv_buf);
    }

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      for(int k=1; k<num_ep; k++)
      {
        if(my_rank == k)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);

        if(my_rank == 0)
        {
          #pragma omp flush
          #pragma omp critical (read_from_buffer)
          {
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j+k*count);
          }
        }

        MPI_Barrier_local(comm);
      }
    }
  }

  int MPI_Gather_local_long(const void *sendbuf, int count, void *recvbuf, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    long *buffer = comm.my_buffer->buf_long;
    long *send_buf = static_cast<long*>(const_cast<void*>(sendbuf));
    long *recv_buf = static_cast<long*>(recvbuf);

    if(my_rank == 0)
    {
      copy(send_buf, send_buf+count, recv_buf);
    }

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      for(int k=1; k<num_ep; k++)
      {
        if(my_rank == k)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);

        if(my_rank == 0)
        {
          #pragma omp flush
          #pragma omp critical (read_from_buffer)
          {
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j+k*count);
          }
        }

        MPI_Barrier_local(comm);
      }
    }
  }

  int MPI_Gather_local_ulong(const void *sendbuf, int count, void *recvbuf, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    unsigned long *buffer = comm.my_buffer->buf_ulong;
    unsigned long *send_buf = static_cast<unsigned long*>(const_cast<void*>(sendbuf));
    unsigned long *recv_buf = static_cast<unsigned long*>(recvbuf);

    if(my_rank == 0)
    {
      copy(send_buf, send_buf+count, recv_buf);
    }

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      for(int k=1; k<num_ep; k++)
      {
        if(my_rank == k)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);

        if(my_rank == 0)
        {
          #pragma omp flush
          #pragma omp critical (read_from_buffer)
          {
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j+k*count);
          }
        }

        MPI_Barrier_local(comm);
      }
    }
  }


  int MPI_Gather_local_char(const void *sendbuf, int count, void *recvbuf, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    char *buffer = comm.my_buffer->buf_char;
    char *send_buf = static_cast<char*>(const_cast<void*>(sendbuf));
    char *recv_buf = static_cast<char*>(recvbuf);

    if(my_rank == 0)
    {
      copy(send_buf, send_buf+count, recv_buf);
    }

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      for(int k=1; k<num_ep; k++)
      {
        if(my_rank == k)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);

        if(my_rank == 0)
        {
          #pragma omp flush
          #pragma omp critical (read_from_buffer)
          {
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j+k*count);
          }
        }

        MPI_Barrier_local(comm);
      }
    }
  }



  int MPI_Gather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
  {
    if(!comm.is_ep && comm.mpi_comm)
    {
      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Gather(sendbuf, sendcount, static_cast< ::MPI_Datatype>(sendtype), recvbuf, recvcount, static_cast< ::MPI_Datatype>(recvtype),
                   root, static_cast< ::MPI_Comm>(comm.mpi_comm));
      return 0;
    }

    if(!comm.mpi_comm) return 0;

    assert(static_cast< ::MPI_Datatype>(sendtype) == static_cast< ::MPI_Datatype>(recvtype) && sendcount == recvcount);

    MPI_Datatype datatype = sendtype;
    int count = sendcount;

    int ep_rank, ep_rank_loc, mpi_rank;
    int ep_size, num_ep, mpi_size;

    ep_rank = comm.ep_comm_ptr->size_rank_info[0].first;
    ep_rank_loc = comm.ep_comm_ptr->size_rank_info[1].first;
    mpi_rank = comm.ep_comm_ptr->size_rank_info[2].first;
    ep_size = comm.ep_comm_ptr->size_rank_info[0].second;
    num_ep = comm.ep_comm_ptr->size_rank_info[1].second;
    mpi_size = comm.ep_comm_ptr->size_rank_info[2].second;


    int root_mpi_rank = comm.rank_map->at(root).second;
    int root_ep_loc = comm.rank_map->at(root).first;


    ::MPI_Aint datasize, lb;
    #ifdef _serialized
    #pragma omp critical (_mpi_call)
    #endif // _serialized
    ::MPI_Type_get_extent(static_cast< ::MPI_Datatype>(datatype), &lb, &datasize);

    void *local_gather_recvbuf;

    if(ep_rank_loc==0)
    {
      local_gather_recvbuf = new void*[datasize*num_ep*count];
    }

    // local gather to master
    MPI_Gather_local(sendbuf, count, datatype, local_gather_recvbuf, comm);

    //MPI_Gather

    if(ep_rank_loc == 0)
    {
      int *gatherv_recvcnt;
      int *gatherv_displs;
      int gatherv_cnt = count*num_ep;

      gatherv_recvcnt = new int[mpi_size];
      gatherv_displs = new int[mpi_size];

      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Allgather(&gatherv_cnt, 1, MPI_INT_STD, gatherv_recvcnt, 1, MPI_INT_STD, static_cast< ::MPI_Comm>(comm.mpi_comm));

      gatherv_displs[0] = 0;
      for(int i=1; i<mpi_size; i++)
      {
        gatherv_displs[i] = gatherv_recvcnt[i-1] + gatherv_displs[i-1];
      }

      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Gatherv(local_gather_recvbuf, count*num_ep, static_cast< ::MPI_Datatype>(datatype), recvbuf, gatherv_recvcnt,
                    gatherv_displs, static_cast< ::MPI_Datatype>(datatype), root_mpi_rank, static_cast< ::MPI_Comm>(comm.mpi_comm));

      delete[] gatherv_recvcnt;
      delete[] gatherv_displs;
    }


    if(root_ep_loc != 0 && mpi_rank == root_mpi_rank) // root is not master, master send to root and root receive from master
    {
      innode_memcpy(0, recvbuf, root_ep_loc, recvbuf, count*ep_size, datatype, comm);
    }



    if(ep_rank_loc==0)
    {
      if(datatype == MPI_INT)
      {
        delete[] static_cast<int*>(local_gather_recvbuf);
      }
      else if(datatype == MPI_FLOAT)
      {
        delete[] static_cast<float*>(local_gather_recvbuf);
      }
      else if(datatype == MPI_DOUBLE)
      {
        delete[] static_cast<double*>(local_gather_recvbuf);
      }
      else if(datatype == MPI_CHAR)
      {
        delete[] static_cast<char*>(local_gather_recvbuf);
      }
      else if(datatype == MPI_LONG)
      {
        delete[] static_cast<long*>(local_gather_recvbuf);
      }
      else// if(datatype == MPI_UNSIGNED_LONG)
      {
        delete[] static_cast<unsigned long*>(local_gather_recvbuf);
      }
    }


  }


  int MPI_Allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
  {
    if(!comm.is_ep && comm.mpi_comm)
    {
      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Allgather(sendbuf, sendcount, static_cast< ::MPI_Datatype>(sendtype), recvbuf, recvcount, static_cast< ::MPI_Datatype>(recvtype),
                      static_cast< ::MPI_Comm>(comm.mpi_comm));
      return 0;
    }

    if(!comm.mpi_comm) return 0;

    assert(static_cast< ::MPI_Datatype>(sendtype) == static_cast< ::MPI_Datatype>(recvtype) && sendcount == recvcount);

    MPI_Datatype datatype = sendtype;
    int count = sendcount;

    int ep_rank, ep_rank_loc, mpi_rank;
    int ep_size, num_ep, mpi_size;

    ep_rank = comm.ep_comm_ptr->size_rank_info[0].first;
    ep_rank_loc = comm.ep_comm_ptr->size_rank_info[1].first;
    mpi_rank = comm.ep_comm_ptr->size_rank_info[2].first;
    ep_size = comm.ep_comm_ptr->size_rank_info[0].second;
    num_ep = comm.ep_comm_ptr->size_rank_info[1].second;
    mpi_size = comm.ep_comm_ptr->size_rank_info[2].second;


    ::MPI_Aint datasize, lb;
    #ifdef _serialized
    #pragma omp critical (_mpi_call)
    #endif // _serialized
    ::MPI_Type_get_extent(static_cast< ::MPI_Datatype>(datatype), &lb, &datasize);

    void *local_gather_recvbuf;

    if(ep_rank_loc==0)
    {
      local_gather_recvbuf = new void*[datasize*num_ep*count];
    }

    // local gather to master
    MPI_Gather_local(sendbuf, count, datatype, local_gather_recvbuf, comm);

    //MPI_Gather

    if(ep_rank_loc == 0)
    {
      int *gatherv_recvcnt;
      int *gatherv_displs;
      int gatherv_cnt = count*num_ep;

      gatherv_recvcnt = new int[mpi_size];
      gatherv_displs = new int[mpi_size];

      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Allgather(&gatherv_cnt, 1, MPI_INT_STD, gatherv_recvcnt, 1, MPI_INT_STD, static_cast< ::MPI_Comm>(comm.mpi_comm));

      gatherv_displs[0] = 0;
      for(int i=1; i<mpi_size; i++)
      {
        gatherv_displs[i] = gatherv_recvcnt[i-1] + gatherv_displs[i-1];
      }

      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Allgatherv(local_gather_recvbuf, count*num_ep, static_cast< ::MPI_Datatype>(datatype), recvbuf, gatherv_recvcnt,
                    gatherv_displs, static_cast< ::MPI_Datatype>(datatype), static_cast< ::MPI_Comm>(comm.mpi_comm));

      delete[] gatherv_recvcnt;
      delete[] gatherv_displs;
    }

    MPI_Bcast_local(recvbuf, count*ep_size, datatype, comm);


    if(ep_rank_loc==0)
    {
      if(datatype == MPI_INT)
      {
        delete[] static_cast<int*>(local_gather_recvbuf);
      }
      else if(datatype == MPI_FLOAT)
      {
        delete[] static_cast<float*>(local_gather_recvbuf);
      }
      else if(datatype == MPI_DOUBLE)
      {
        delete[] static_cast<double*>(local_gather_recvbuf);
      }
      else if(datatype == MPI_CHAR)
      {
        delete[] static_cast<char*>(local_gather_recvbuf);
      }
      else if(datatype == MPI_LONG)
      {
        delete[] static_cast<long*>(local_gather_recvbuf);
      }
      else// if(datatype == MPI_UNSIGNED_LONG)
      {
        delete[] static_cast<unsigned long*>(local_gather_recvbuf);
      }
    }
  }


}
