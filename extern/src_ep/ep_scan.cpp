/*!
   \file ep_scan.cpp
   \since 2 may 2016

   \brief Definitions of MPI collective function: MPI_Scan
 */

#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

using namespace std;

namespace ep_lib
{
  template<typename T>
  T max_op(T a, T b)
  {
    return max(a,b);
  }

  template<typename T>
  T min_op(T a, T b)
  {
    return min(a,b);
  }


  int MPI_Scan_local(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
  {
    if(datatype == MPI_INT)
    {
      return MPI_Scan_local_int(sendbuf, recvbuf, count, op, comm);
    }
    else if(datatype == MPI_FLOAT)
    {
      return MPI_Scan_local_float(sendbuf, recvbuf, count, op, comm);
    }
    else if(datatype == MPI_DOUBLE)
    {
      return MPI_Scan_local_double(sendbuf, recvbuf, count, op, comm);
    }
    else if(datatype == MPI_LONG)
    {
      return MPI_Scan_local_long(sendbuf, recvbuf, count, op, comm);
    }
    else if(datatype == MPI_UNSIGNED_LONG)
    {
      return MPI_Scan_local_ulong(sendbuf, recvbuf, count, op, comm);
    }
    else if(datatype == MPI_CHAR)
    {
      return MPI_Scan_local_char(sendbuf, recvbuf, count, op, comm);
    }
    else
    {
      printf("MPI_Scan Datatype not supported!\n");
      exit(0);
    }

  }




  int MPI_Scan_local_int(const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    int *buffer = comm.my_buffer->buf_int;
    int *send_buf = static_cast<int*>(const_cast<void*>(sendbuf));
    int *recv_buf = static_cast<int*>(recvbuf);

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if(my_rank == 0)
      {

        #pragma omp critical (write_to_buffer)
        {
          copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
          copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), recv_buf+j);
          #pragma omp flush
        }
      }

      MPI_Barrier_local(comm);

      for(int k=1; k<num_ep; k++)
      {
        #pragma omp critical (write_to_buffer)
        {
          if(my_rank == k)
          {
            #pragma omp flush
            if(op == MPI_SUM)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, std::plus<int>());
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else if(op == MPI_MAX)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, max_op<int>);
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else if(op == MPI_MIN)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, min_op<int>);
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else
            {
              printf("Supported operation: MPI_SUM, MPI_MAX, MPI_MIN\n");
              exit(1);
            }
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);
      }
    }

  }

  int MPI_Scan_local_float(const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    float *buffer = comm.my_buffer->buf_float;
    float *send_buf = static_cast<float*>(const_cast<void*>(sendbuf));
    float *recv_buf = static_cast<float*>(recvbuf);

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if(my_rank == 0)
      {

        #pragma omp critical (write_to_buffer)
        {
          copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
          copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), recv_buf+j);
          #pragma omp flush
        }
      }

      MPI_Barrier_local(comm);

      for(int k=1; k<num_ep; k++)
      {
        #pragma omp critical (write_to_buffer)
        {
          if(my_rank == k)
          {
            #pragma omp flush
            if(op == MPI_SUM)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, std::plus<float>());
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else if(op == MPI_MAX)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, max_op<float>);
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);

            }
            else if(op == MPI_MIN)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, min_op<float>);
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);

            }
            else
            {
              printf("Supported operation: MPI_SUM, MPI_MAX, MPI_MIN\n");
              exit(1);
            }
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);
      }
    }
  }

  int MPI_Scan_local_double(const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    double *buffer = comm.my_buffer->buf_double;
    double *send_buf = static_cast<double*>(const_cast<void*>(sendbuf));
    double *recv_buf = static_cast<double*>(recvbuf);

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if(my_rank == 0)
      {

        #pragma omp critical (write_to_buffer)
        {
          copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
          copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), recv_buf+j);
          #pragma omp flush
        }
      }

      MPI_Barrier_local(comm);

      for(int k=1; k<num_ep; k++)
      {
        #pragma omp critical (write_to_buffer)
        {
          if(my_rank == k)
          {
            #pragma omp flush
            if(op == MPI_SUM)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, std::plus<double>());
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else if(op == MPI_MAX)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, max_op<double>);
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else if(op == MPI_MIN)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, min_op<double>);
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else
            {
              printf("Supported operation: MPI_SUM, MPI_MAX, MPI_MIN\n");
              exit(1);
            }
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);
      }
    }
  }

  int MPI_Scan_local_long(const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    long *buffer = comm.my_buffer->buf_long;
    long *send_buf = static_cast<long*>(const_cast<void*>(sendbuf));
    long *recv_buf = static_cast<long*>(recvbuf);

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if(my_rank == 0)
      {

        #pragma omp critical (write_to_buffer)
        {
          copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
          copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), recv_buf+j);
          #pragma omp flush
        }
      }

      MPI_Barrier_local(comm);

      for(int k=1; k<num_ep; k++)
      {
        #pragma omp critical (write_to_buffer)
        {
          if(my_rank == k)
          {
            #pragma omp flush
            if(op == MPI_SUM)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, std::plus<long>());
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else if(op == MPI_MAX)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, max_op<long>);
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else if(op == MPI_MIN)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, min_op<long>);
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else
            {
              printf("Supported operation: MPI_SUM, MPI_MAX, MPI_MIN\n");
              exit(1);
            }
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);
      }
    }
  }

  int MPI_Scan_local_ulong(const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    unsigned long *buffer = comm.my_buffer->buf_ulong;
    unsigned long *send_buf = static_cast<unsigned long*>(const_cast<void*>(sendbuf));
    unsigned long *recv_buf = static_cast<unsigned long*>(recvbuf);

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if(my_rank == 0)
      {

        #pragma omp critical (write_to_buffer)
        {
          copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
          copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), recv_buf+j);
          #pragma omp flush
        }
      }

      MPI_Barrier_local(comm);

      for(int k=1; k<num_ep; k++)
      {
        #pragma omp critical (write_to_buffer)
        {
          if(my_rank == k)
          {
            #pragma omp flush
            if(op == MPI_SUM)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, std::plus<unsigned long>());
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else if(op == MPI_MAX)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, max_op<unsigned long>);
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else if(op == MPI_MIN)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, min_op<unsigned long>);
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else
            {
              printf("Supported operation: MPI_SUM, MPI_MAX, MPI_MIN\n");
              exit(1);
            }
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);
      }
    }
  }

  int MPI_Scan_local_char(const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    char *buffer = comm.my_buffer->buf_char;
    char *send_buf = static_cast<char*>(const_cast<void*>(sendbuf));
    char *recv_buf = static_cast<char*>(recvbuf);

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if(my_rank == 0)
      {

        #pragma omp critical (write_to_buffer)
        {
          copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
          copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), recv_buf+j);
          #pragma omp flush
        }
      }

      MPI_Barrier_local(comm);

      for(int k=1; k<num_ep; k++)
      {
        #pragma omp critical (write_to_buffer)
        {
          if(my_rank == k)
          {
            #pragma omp flush
            if(op == MPI_SUM)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, std::plus<char>());
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else if(op == MPI_MAX)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, max_op<char>);
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else if(op == MPI_MIN)
            {
              transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, min_op<char>);
              copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
            }
            else
            {
              printf("Supported operation: MPI_SUM, MPI_MAX, MPI_MIN\n");
              exit(1);
            }
            #pragma omp flush
          }
        }

        MPI_Barrier_local(comm);
      }
    }
  }


  int MPI_Scan(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
  {
    if(!comm.is_ep)
    {
      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Scan(sendbuf, recvbuf, count, static_cast< ::MPI_Datatype>(datatype),
                 static_cast< ::MPI_Op>(op), static_cast< ::MPI_Comm>(comm.mpi_comm));
      return 0;
    }

    if(!comm.mpi_comm) return 0;

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

    void* local_scan_recvbuf;
    local_scan_recvbuf = new void*[datasize * count];


    // local scan
    MPI_Scan_local(sendbuf, recvbuf, count, datatype, op, comm);

//     MPI_scan
    void* local_sum;
    void* mpi_scan_recvbuf;


    mpi_scan_recvbuf = new void*[datasize*count];

    if(ep_rank_loc == 0)
    {
      local_sum = new void*[datasize*count];
    }


    MPI_Reduce_local(sendbuf, local_sum, count, datatype, op, comm);

    if(ep_rank_loc == 0)
    {
      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Exscan(local_sum, mpi_scan_recvbuf, count, static_cast< ::MPI_Datatype>(datatype), static_cast< ::MPI_Op>(op), static_cast< ::MPI_Comm>(comm.mpi_comm));
    }


    if(mpi_rank > 0)
    {
      MPI_Bcast_local(mpi_scan_recvbuf, count, datatype, comm);
    }


    if(datatype == MPI_DOUBLE)
    {
      double* sum_buf = static_cast<double*>(mpi_scan_recvbuf);
      double* recv_buf = static_cast<double*>(recvbuf);

      if(mpi_rank != 0)
      {
        if(op == MPI_SUM)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] += sum_buf[i];
          }
        }
        else if (op == MPI_MAX)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] = max(recv_buf[i], sum_buf[i]);
          }
        }
        else if(op == MPI_MIN)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] = min(recv_buf[i], sum_buf[i]);
          }
        }
        else
        {
          printf("Support operator for MPI_Scan is MPI_SUM, MPI_MAX, and MPI_MIN\n");
          exit(1);
        }
      }

      delete[] static_cast<double*>(mpi_scan_recvbuf);
      if(ep_rank_loc == 0)
      {
        delete[] static_cast<double*>(local_sum);
      }
    }

    else if(datatype == MPI_FLOAT)
    {
      float* sum_buf = static_cast<float*>(mpi_scan_recvbuf);
      float* recv_buf = static_cast<float*>(recvbuf);

      if(mpi_rank != 0)
      {
        if(op == MPI_SUM)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] += sum_buf[i];
          }
        }
        else if (op == MPI_MAX)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] = max(recv_buf[i], sum_buf[i]);
          }
        }
        else if(op == MPI_MIN)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] = min(recv_buf[i], sum_buf[i]);
          }
        }
        else
        {
          printf("Support operator for MPI_Scan is MPI_SUM, MPI_MAX, and MPI_MIN\n");
          exit(1);
        }
      }

      delete[] static_cast<float*>(mpi_scan_recvbuf);
      if(ep_rank_loc == 0)
      {
        delete[] static_cast<float*>(local_sum);
      }
    }

    else if(datatype == MPI_INT)
    {
      int* sum_buf = static_cast<int*>(mpi_scan_recvbuf);
      int* recv_buf = static_cast<int*>(recvbuf);

      if(mpi_rank != 0)
      {
        if(op == MPI_SUM)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] += sum_buf[i];
          }
        }
        else if (op == MPI_MAX)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] = max(recv_buf[i], sum_buf[i]);
          }
        }
        else if(op == MPI_MIN)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] = min(recv_buf[i], sum_buf[i]);
          }
        }
        else
        {
          printf("Support operator for MPI_Scan is MPI_SUM, MPI_MAX, and MPI_MIN\n");
          exit(1);
        }
      }

      delete[] static_cast<int*>(mpi_scan_recvbuf);
      if(ep_rank_loc == 0)
      {
        delete[] static_cast<int*>(local_sum);
      }
    }

    else if(datatype == MPI_LONG)
    {
      long* sum_buf = static_cast<long*>(mpi_scan_recvbuf);
      long* recv_buf = static_cast<long*>(recvbuf);

      if(mpi_rank != 0)
      {
        if(op == MPI_SUM)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] += sum_buf[i];
          }
        }
        else if (op == MPI_MAX)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] = max(recv_buf[i], sum_buf[i]);
          }
        }
        else if(op == MPI_MIN)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] = min(recv_buf[i], sum_buf[i]);
          }
        }
        else
        {
          printf("Support operator for MPI_Scan is MPI_SUM, MPI_MAX, and MPI_MIN\n");
          exit(1);
        }
      }

      delete[] static_cast<long*>(mpi_scan_recvbuf);
      if(ep_rank_loc == 0)
      {
        delete[] static_cast<long*>(local_sum);
      }
    }

    else if(datatype == MPI_UNSIGNED_LONG)
    {
      unsigned long* sum_buf = static_cast<unsigned long*>(mpi_scan_recvbuf);
      unsigned long* recv_buf = static_cast<unsigned long*>(recvbuf);

      if(mpi_rank != 0)
      {
        if(op == MPI_SUM)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] += sum_buf[i];
          }
        }
        else if (op == MPI_MAX)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] = max(recv_buf[i], sum_buf[i]);
          }
        }
        else if(op == MPI_MIN)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] = min(recv_buf[i], sum_buf[i]);
          }
        }
        else
        {
          printf("Support operator for MPI_Scan is MPI_SUM, MPI_MAX, and MPI_MIN\n");
          exit(1);
        }
      }

      delete[] static_cast<unsigned long*>(mpi_scan_recvbuf);
      if(ep_rank_loc == 0)
      {
        delete[] static_cast<unsigned long*>(local_sum);
      }
    }

    else if(datatype == MPI_CHAR)
    {
      char* sum_buf = static_cast<char*>(mpi_scan_recvbuf);
      char* recv_buf = static_cast<char*>(recvbuf);

      if(mpi_rank != 0)
      {
        if(op == MPI_SUM)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] += sum_buf[i];
          }
        }
        else if (op == MPI_MAX)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] = max(recv_buf[i], sum_buf[i]);
          }
        }
        else if(op == MPI_MIN)
        {
          for(int i=0; i<count; i++)
          {
            recv_buf[i] = min(recv_buf[i], sum_buf[i]);
          }
        }
        else
        {
          printf("Support operator for MPI_Scan is MPI_SUM, MPI_MAX, and MPI_MIN\n");
          exit(1);
        }
      }

      delete[] static_cast<char*>(mpi_scan_recvbuf);
      if(ep_rank_loc == 0)
      {
        delete[] static_cast<char*>(local_sum);
      }
    }


  }

}
