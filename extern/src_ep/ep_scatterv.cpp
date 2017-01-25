/*!
   \file ep_gather.cpp
   \since 2 may 2016

   \brief Definitions of MPI collective function: MPI_Scatterv
 */

#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

using namespace std;

namespace ep_lib
{

  int MPI_Scatterv_local(const void *sendbuf, const int sendcounts[], const int displs[], MPI_Datatype datatype, void *recvbuf, MPI_Comm comm)
  {
    if(datatype == MPI_INT)
    {
      Debug("datatype is INT\n");
      return MPI_Scatterv_local_int(sendbuf, sendcounts, displs, recvbuf, comm);
    }
    else if(datatype == MPI_FLOAT)
    {
      Debug("datatype is FLOAT\n");
      return MPI_Scatterv_local_float(sendbuf, sendcounts, displs, recvbuf, comm);
    }
    else if(datatype == MPI_DOUBLE)
    {
      Debug("datatype is DOUBLE\n");
      return MPI_Scatterv_local_double(sendbuf, sendcounts, displs, recvbuf, comm);
    }
    else if(datatype == MPI_LONG)
    {
      Debug("datatype is LONG\n");
      return MPI_Scatterv_local_long(sendbuf, sendcounts, displs, recvbuf, comm);
    }
    else if(datatype == MPI_UNSIGNED_LONG)
    {
      Debug("datatype is uLONG\n");
      return MPI_Scatterv_local_ulong(sendbuf, sendcounts, displs, recvbuf, comm);
    }
    else if(datatype == MPI_CHAR)
    {
      Debug("datatype is CHAR\n");
      return MPI_Scatterv_local_char(sendbuf, sendcounts, displs, recvbuf, comm);
    }
    else
    {
      printf("MPI_scatterv Datatype not supported!\n");
      exit(0);
    }
  }

  int MPI_Scatterv_local_int(const void *sendbuf, const int sendcounts[], const int displs[], void *recvbuf, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;


    int *buffer = comm.my_buffer->buf_int;
    int *send_buf = static_cast<int*>(const_cast<void*>(sendbuf));
    int *recv_buf = static_cast<int*>(recvbuf);

    for(int k=0; k<num_ep; k++)
    {
      int count = sendcounts[k];
      for(int j=0; j<count; j+=BUFFER_SIZE)
      {
        if(my_rank == 0)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+displs[k]+j, send_buf+displs[k]+j+min(BUFFER_SIZE, count-j), buffer);
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);

        if(my_rank == k)
        {
          #pragma omp critical (read_from_buffer)
          {
            #pragma omp flush
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
          }
        }
        MPI_Barrier_local(comm);
      }
    }
  }

  int MPI_Scatterv_local_float(const void *sendbuf, const int sendcounts[], const int displs[], void *recvbuf, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;


    float *buffer = comm.my_buffer->buf_float;
    float *send_buf = static_cast<float*>(const_cast<void*>(sendbuf));
    float *recv_buf = static_cast<float*>(recvbuf);

    for(int k=0; k<num_ep; k++)
    {
      int count = sendcounts[k];
      for(int j=0; j<count; j+=BUFFER_SIZE)
      {
        if(my_rank == 0)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+displs[k]+j, send_buf+displs[k]+j+min(BUFFER_SIZE, count-j), buffer);
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);

        if(my_rank == k)
        {
          #pragma omp critical (read_from_buffer)
          {
            #pragma omp flush
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
          }
        }
        MPI_Barrier_local(comm);
      }
    }
  }

  int MPI_Scatterv_local_double(const void *sendbuf, const int sendcounts[], const int displs[], void *recvbuf, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;


    double *buffer = comm.my_buffer->buf_double;
    double *send_buf = static_cast<double*>(const_cast<void*>(sendbuf));
    double *recv_buf = static_cast<double*>(recvbuf);

    for(int k=0; k<num_ep; k++)
    {
      int count = sendcounts[k];
      for(int j=0; j<count; j+=BUFFER_SIZE)
      {
        if(my_rank == 0)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+displs[k]+j, send_buf+displs[k]+j+min(BUFFER_SIZE, count-j), buffer);
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);

        if(my_rank == k)
        {
          #pragma omp critical (read_from_buffer)
          {
            #pragma omp flush
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
          }
        }
        MPI_Barrier_local(comm);
      }
    }
  }

  int MPI_Scatterv_local_long(const void *sendbuf, const int sendcounts[], const int displs[], void *recvbuf, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;


    long *buffer = comm.my_buffer->buf_long;
    long *send_buf = static_cast<long*>(const_cast<void*>(sendbuf));
    long *recv_buf = static_cast<long*>(recvbuf);

    for(int k=0; k<num_ep; k++)
    {
      int count = sendcounts[k];
      for(int j=0; j<count; j+=BUFFER_SIZE)
      {
        if(my_rank == 0)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+displs[k]+j, send_buf+displs[k]+j+min(BUFFER_SIZE, count-j), buffer);
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);

        if(my_rank == k)
        {
          #pragma omp critical (read_from_buffer)
          {
            #pragma omp flush
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
          }
        }
        MPI_Barrier_local(comm);
      }
    }
  }


  int MPI_Scatterv_local_ulong(const void *sendbuf, const int sendcounts[], const int displs[], void *recvbuf, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;


    unsigned long *buffer = comm.my_buffer->buf_ulong;
    unsigned long *send_buf = static_cast<unsigned long*>(const_cast<void*>(sendbuf));
    unsigned long *recv_buf = static_cast<unsigned long*>(recvbuf);

    for(int k=0; k<num_ep; k++)
    {
      int count = sendcounts[k];
      for(int j=0; j<count; j+=BUFFER_SIZE)
      {
        if(my_rank == 0)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+displs[k]+j, send_buf+displs[k]+j+min(BUFFER_SIZE, count-j), buffer);
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);

        if(my_rank == k)
        {
          #pragma omp critical (read_from_buffer)
          {
            #pragma omp flush
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
          }
        }
        MPI_Barrier_local(comm);
      }
    }
  }


  int MPI_Scatterv_local_char(const void *sendbuf, const int sendcounts[], const int displs[], void *recvbuf, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;


    char *buffer = comm.my_buffer->buf_char;
    char *send_buf = static_cast<char*>(const_cast<void*>(sendbuf));
    char *recv_buf = static_cast<char*>(recvbuf);

    for(int k=0; k<num_ep; k++)
    {
      int count = sendcounts[k];
      for(int j=0; j<count; j+=BUFFER_SIZE)
      {
        if(my_rank == 0)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+displs[k]+j, send_buf+displs[k]+j+min(BUFFER_SIZE, count-j), buffer);
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);

        if(my_rank == k)
        {
          #pragma omp critical (read_from_buffer)
          {
            #pragma omp flush
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
          }
        }
        MPI_Barrier_local(comm);
      }
    }
  }


  int MPI_Scatterv(const void *sendbuf, const int sendcounts[], const int displs[], MPI_Datatype sendtype, void *recvbuf, int recvcount,
                   MPI_Datatype recvtype, int root, MPI_Comm comm)
  {
    if(!comm.is_ep)
    {
      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Scatterv(sendbuf, sendcounts, displs, static_cast< ::MPI_Datatype>(sendtype), recvbuf, recvcount,
                     static_cast< ::MPI_Datatype>(recvtype), root, static_cast< ::MPI_Comm>(comm.mpi_comm));
      return 0;
    }
    if(!comm.mpi_comm) return 0;

    assert(static_cast< ::MPI_Datatype>(sendtype) == static_cast< ::MPI_Datatype>(recvtype));

    MPI_Datatype datatype = sendtype;

    int root_mpi_rank = comm.rank_map->at(root).second;
    int root_ep_loc = comm.rank_map->at(root).first;

    int ep_rank, ep_rank_loc, mpi_rank;
    int ep_size, num_ep, mpi_size;

    ep_rank = comm.ep_comm_ptr->size_rank_info[0].first;
    ep_rank_loc = comm.ep_comm_ptr->size_rank_info[1].first;
    mpi_rank = comm.ep_comm_ptr->size_rank_info[2].first;
    ep_size = comm.ep_comm_ptr->size_rank_info[0].second;
    num_ep = comm.ep_comm_ptr->size_rank_info[1].second;
    mpi_size = comm.ep_comm_ptr->size_rank_info[2].second;
    
    MPI_Bcast(const_cast<int*>(sendcounts), ep_size, MPI_INT, root, comm);
    MPI_Bcast(const_cast<int*>(displs), ep_size, MPI_INT, root, comm);


    int count = recvcount;

    ::MPI_Aint datasize, lb;
    #ifdef _serialized
    #pragma omp critical (_mpi_call)
    #endif // _serialized
    ::MPI_Type_get_extent(static_cast< ::MPI_Datatype>(datatype), &lb, &datasize);

    assert(accumulate(sendcounts, sendcounts+ep_size-1, 0) == displs[ep_size-1]); // Only for contunuous gather.


    void *master_sendbuf;
    void *local_recvbuf;

    if(root_ep_loc!=0 && mpi_rank == root_mpi_rank)
    {
      int count_sum = accumulate(sendcounts, sendcounts+ep_size, 0);
      if(ep_rank_loc == 0) master_sendbuf = new void*[datasize*count_sum];

      innode_memcpy(root_ep_loc, sendbuf, 0, master_sendbuf, count_sum, datatype, comm);
    }



    if(ep_rank_loc == 0)
    {
      int mpi_sendcnt = accumulate(sendcounts+ep_rank, sendcounts+ep_rank+num_ep, 0);
      int mpi_scatterv_sendcnt[mpi_size];
      int mpi_displs[mpi_size];

      local_recvbuf = new void*[datasize*mpi_sendcnt];

      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Gather(&mpi_sendcnt, 1, MPI_INT_STD, mpi_scatterv_sendcnt, 1, MPI_INT_STD, root_mpi_rank, static_cast< ::MPI_Comm>(comm.mpi_comm));

      mpi_displs[0] = displs[0];
      for(int i=1; i<mpi_size; i++)
        mpi_displs[i] = mpi_displs[i-1] + mpi_scatterv_sendcnt[i-1];


      if(root_ep_loc!=0)
      {
        #ifdef _serialized
        #pragma omp critical (_mpi_call)
        #endif // _serialized
        ::MPI_Scatterv(master_sendbuf, mpi_scatterv_sendcnt, mpi_displs, static_cast< ::MPI_Datatype>(datatype),
                     local_recvbuf, mpi_sendcnt, static_cast< ::MPI_Datatype>(datatype), root_mpi_rank, static_cast< ::MPI_Comm>(comm.mpi_comm));
      }
      else
      {
        #ifdef _serialized
        #pragma omp critical (_mpi_call)
        #endif // _serialized
        ::MPI_Scatterv(sendbuf, mpi_scatterv_sendcnt, mpi_displs, static_cast< ::MPI_Datatype>(datatype),
                     local_recvbuf, mpi_sendcnt, static_cast< ::MPI_Datatype>(datatype), root_mpi_rank, static_cast< ::MPI_Comm>(comm.mpi_comm));
      }
    }

    int local_displs[num_ep];
    local_displs[0] = 0;
    for(int i=1; i<num_ep; i++)
    {
      local_displs[i] = displs[ep_rank-ep_rank_loc+i]-displs[ep_rank-ep_rank_loc];
    }

    MPI_Scatterv_local(local_recvbuf, sendcounts+ep_rank-ep_rank_loc, local_displs, datatype, recvbuf, comm);

    if(ep_rank_loc == 0)
    {
      if(datatype == MPI_INT)
      {
        if(root_ep_loc!=0 && mpi_rank == root_mpi_rank) delete[] static_cast<int*>(master_sendbuf);
        delete[] static_cast<int*>(local_recvbuf);
      }
      else if(datatype == MPI_FLOAT)
      {
        if(root_ep_loc!=0 && mpi_rank == root_mpi_rank) delete[] static_cast<float*>(master_sendbuf);
        delete[] static_cast<float*>(local_recvbuf);
      }
      else  if(datatype == MPI_DOUBLE)
      {
        if(root_ep_loc!=0 && mpi_rank == root_mpi_rank) delete[] static_cast<double*>(master_sendbuf);
        delete[] static_cast<double*>(local_recvbuf);
      }
      else  if(datatype == MPI_LONG)
      {
        if(root_ep_loc!=0 && mpi_rank == root_mpi_rank) delete[] static_cast<long*>(master_sendbuf);
        delete[] static_cast<long*>(local_recvbuf);
      }
      else  if(datatype == MPI_UNSIGNED_LONG)
      {
        if(root_ep_loc!=0 && mpi_rank == root_mpi_rank) delete[] static_cast<unsigned long*>(master_sendbuf);
        delete[] static_cast<unsigned long*>(local_recvbuf);
      }
      else // if(datatype == MPI_DOUBLE)
      {
        if(root_ep_loc!=0 && mpi_rank == root_mpi_rank) delete[] static_cast<char*>(master_sendbuf);
        delete[] static_cast<char*>(local_recvbuf);
      }
    }

  }
}
