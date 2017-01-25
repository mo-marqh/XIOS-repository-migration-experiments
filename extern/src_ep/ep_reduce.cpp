/*!
   \file ep_reduce.cpp
   \since 2 may 2016

   \brief Definitions of MPI collective function: MPI_Reduce, MPI_Allreduce
 */

#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

using namespace std;


namespace ep_lib {

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


  int MPI_Reduce_local(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
  {
    if(datatype == MPI_INT)
    {
      Debug("datatype is INT\n");
      return MPI_Reduce_local_int(sendbuf, recvbuf, count, op, comm);
    }
    else if(datatype == MPI_FLOAT)
    {
      Debug("datatype is FLOAT\n");
      return MPI_Reduce_local_float(sendbuf, recvbuf, count, op, comm);
    }
    else if(datatype == MPI_DOUBLE)
    {
      Debug("datatype is DOUBLE\n");
      return MPI_Reduce_local_double(sendbuf, recvbuf, count, op, comm);
    }
    else if(datatype == MPI_LONG)
    {
      Debug("datatype is DOUBLE\n");
      return MPI_Reduce_local_long(sendbuf, recvbuf, count, op, comm);
    }
    else if(datatype == MPI_UNSIGNED_LONG)
    {
      Debug("datatype is DOUBLE\n");
      return MPI_Reduce_local_ulong(sendbuf, recvbuf, count, op, comm);
    }
    else if(datatype == MPI_CHAR)
    {
      Debug("datatype is DOUBLE\n");
      return MPI_Reduce_local_char(sendbuf, recvbuf, count, op, comm);
    }
    else
    {
      printf("MPI_Reduce Datatype not supported!\n");
      exit(0);
    }
  }


  int MPI_Reduce_local_int(const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    int *buffer = comm.my_buffer->buf_int;
    int *send_buf = static_cast<int*>(const_cast<void*>(sendbuf));
    int *recv_buf = static_cast<int*>(const_cast<void*>(recvbuf));

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if( 0 == my_rank )
      {
        #pragma omp critical (write_to_buffer)
        copy(send_buf+j, send_buf+j + min(BUFFER_SIZE, count-j), buffer);
        #pragma omp flush
      }

      MPI_Barrier_local(comm);

      if(my_rank !=0 )
      {
        #pragma omp critical (write_to_buffer)
        {
          #pragma omp flush
          if(op == MPI_SUM)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, std::plus<int>());
          }

          else if (op == MPI_MAX)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, max_op<int>);
          }

          else if (op == MPI_MIN)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, min_op<int>);
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

      if(my_rank == 0)
      {
        #pragma omp flush
        copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
      }
      MPI_Barrier_local(comm);
    }
  }


  int MPI_Reduce_local_float(const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    float *buffer = comm.my_buffer->buf_float;
    float *send_buf = static_cast<float*>(const_cast<void*>(sendbuf));
    float *recv_buf = static_cast<float*>(const_cast<void*>(recvbuf));

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if( 0 == my_rank )
      {
        #pragma omp critical (write_to_buffer)
        copy(send_buf+j, send_buf+j + min(BUFFER_SIZE, count-j), buffer);
        #pragma omp flush
      }

      MPI_Barrier_local(comm);

      if(my_rank !=0 )
      {
        #pragma omp critical (write_to_buffer)
        {
          #pragma omp flush

          if(op == MPI_SUM)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, std::plus<float>());
          }

          else if (op == MPI_MAX)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, max_op<float>);
          }

          else if (op == MPI_MIN)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, min_op<float>);
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

      if(my_rank == 0)
      {
        #pragma omp flush
        copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
      }
      MPI_Barrier_local(comm);
    }
  }

  int MPI_Reduce_local_double(const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    double *buffer = comm.my_buffer->buf_double;
    double *send_buf = static_cast<double*>(const_cast<void*>(sendbuf));
    double *recv_buf = static_cast<double*>(const_cast<void*>(recvbuf));

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if( 0 == my_rank )
      {
        #pragma omp critical (write_to_buffer)
        copy(send_buf+j, send_buf+j + min(BUFFER_SIZE, count-j), buffer);
        #pragma omp flush
      }

      MPI_Barrier_local(comm);

      if(my_rank !=0 )
      {
        #pragma omp critical (write_to_buffer)
        {
          #pragma omp flush


          if(op == MPI_SUM)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, std::plus<double>());
          }

          else if (op == MPI_MAX)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, max_op<double>);
          }


          else if (op == MPI_MIN)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, min_op<double>);
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

      if(my_rank == 0)
      {
        #pragma omp flush
        copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
      }
      MPI_Barrier_local(comm);
    }
  }

  int MPI_Reduce_local_long(const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    long *buffer = comm.my_buffer->buf_long;
    long *send_buf = static_cast<long*>(const_cast<void*>(sendbuf));
    long *recv_buf = static_cast<long*>(const_cast<void*>(recvbuf));

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if( 0 == my_rank )
      {
        #pragma omp critical (write_to_buffer)
        copy(send_buf+j, send_buf+j + min(BUFFER_SIZE, count-j), buffer);
        #pragma omp flush
      }

      MPI_Barrier_local(comm);

      if(my_rank !=0 )
      {
        #pragma omp critical (write_to_buffer)
        {
          #pragma omp flush


          if(op == MPI_SUM)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, std::plus<long>());
          }

          else if (op == MPI_MAX)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, max_op<long>);
          }


          else if (op == MPI_MIN)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, min_op<long>);
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

      if(my_rank == 0)
      {
        #pragma omp flush
        copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
      }
      MPI_Barrier_local(comm);
    }
  }

  int MPI_Reduce_local_ulong(const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    unsigned long *buffer = comm.my_buffer->buf_ulong;
    unsigned long *send_buf = static_cast<unsigned long*>(const_cast<void*>(sendbuf));
    unsigned long *recv_buf = static_cast<unsigned long*>(const_cast<void*>(recvbuf));

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if( 0 == my_rank )
      {
        #pragma omp critical (write_to_buffer)
        copy(send_buf+j, send_buf+j + min(BUFFER_SIZE, count-j), buffer);
        #pragma omp flush
      }

      MPI_Barrier_local(comm);

      if(my_rank !=0 )
      {
        #pragma omp critical (write_to_buffer)
        {
          #pragma omp flush


          if(op == MPI_SUM)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, std::plus<unsigned long>());
          }

          else if (op == MPI_MAX)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, max_op<unsigned long>);
          }


          else if (op == MPI_MIN)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, min_op<unsigned long>);
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

      if(my_rank == 0)
      {
        #pragma omp flush
        copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
      }
      MPI_Barrier_local(comm);
    }
  }

  int MPI_Reduce_local_char(const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm)
  {
    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;

    char *buffer = comm.my_buffer->buf_char;
    char *send_buf = static_cast<char*>(const_cast<void*>(sendbuf));
    char *recv_buf = static_cast<char*>(const_cast<void*>(recvbuf));

    for(int j=0; j<count; j+=BUFFER_SIZE)
    {
      if( 0 == my_rank )
      {
        #pragma omp critical (write_to_buffer)
        copy(send_buf+j, send_buf+j + min(BUFFER_SIZE, count-j), buffer);
        #pragma omp flush
      }

      MPI_Barrier_local(comm);

      if(my_rank !=0 )
      {
        #pragma omp critical (write_to_buffer)
        {
          #pragma omp flush


          if(op == MPI_SUM)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, std::plus<char>());
          }

          else if (op == MPI_MAX)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, max_op<char>);
          }


          else if (op == MPI_MIN)
          {
            transform(buffer, buffer+min(BUFFER_SIZE, count-j), send_buf+j, buffer, min_op<char>);
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

      if(my_rank == 0)
      {
        #pragma omp flush
        copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
      }
      MPI_Barrier_local(comm);
    }
  }


  int MPI_Reduce(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm)
  {
    if(!comm.is_ep && comm.mpi_comm)
    {
      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Reduce(sendbuf, recvbuf, count, static_cast< ::MPI_Datatype>(datatype), static_cast< ::MPI_Op>(op), root,
                   static_cast< ::MPI_Comm>(comm.mpi_comm));
      return 0;
    }


    if(!comm.mpi_comm) return 0;

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


    ::MPI_Aint recvsize, lb;
    #ifdef _serialized
    #pragma omp critical (_mpi_call)
    #endif // _serialized
    ::MPI_Type_get_extent(static_cast< ::MPI_Datatype>(datatype), &lb, &recvsize);

    void *local_recvbuf;
    if(ep_rank_loc==0)
    {
      local_recvbuf = new void*[recvsize*count];
    }

    MPI_Reduce_local(sendbuf, local_recvbuf, count, datatype, op, comm);


    if(ep_rank_loc==0)
    {
      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Reduce(local_recvbuf, recvbuf, count, static_cast< ::MPI_Datatype>(datatype), static_cast< ::MPI_Op>(op), root_mpi_rank, static_cast< ::MPI_Comm>(comm.mpi_comm));
    }

    if(root_ep_loc != 0 && mpi_rank == root_mpi_rank) // root is not master, master send to root and root receive from master
    {
      innode_memcpy(0, recvbuf, root_ep_loc, recvbuf, count, datatype, comm);
    }

    if(ep_rank_loc==0)
    {
      if(datatype == MPI_INT) delete[] static_cast<int*>(local_recvbuf);
      else if(datatype == MPI_FLOAT) delete[] static_cast<float*>(local_recvbuf);
      else if(datatype == MPI_DOUBLE) delete[] static_cast<double*>(local_recvbuf);
      else if(datatype == MPI_LONG) delete[] static_cast<long*>(local_recvbuf);
      else if(datatype == MPI_UNSIGNED_LONG) delete[] static_cast<unsigned long*>(local_recvbuf);
      else delete[] static_cast<char*>(local_recvbuf);
    }

    Message_Check(comm);

    return 0;
  }




  int MPI_Allreduce(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
  {
    if(!comm.is_ep && comm.mpi_comm)
    {
      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Allreduce(sendbuf, recvbuf, count, static_cast< ::MPI_Datatype>(datatype), static_cast< ::MPI_Op>(op),
                      static_cast< ::MPI_Comm>(comm.mpi_comm));
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


    ::MPI_Aint recvsize, lb;
    #ifdef _serialized
    #pragma omp critical (_mpi_call)
    #endif // _serialized
    ::MPI_Type_get_extent(static_cast< ::MPI_Datatype>(datatype), &lb, &recvsize);

    void *local_recvbuf;
    if(ep_rank_loc==0)
    {
      local_recvbuf = new void*[recvsize*count];
    }

    MPI_Reduce_local(sendbuf, local_recvbuf, count, datatype, op, comm);


    if(ep_rank_loc==0)
    {
      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Allreduce(local_recvbuf, recvbuf, count, static_cast< ::MPI_Datatype>(datatype), static_cast< ::MPI_Op>(op), static_cast< ::MPI_Comm>(comm.mpi_comm));
    }

    MPI_Bcast_local(recvbuf, count, datatype, comm);

    if(ep_rank_loc==0)
    {
      if(datatype == MPI_INT) delete[] static_cast<int*>(local_recvbuf);
      else if(datatype == MPI_FLOAT) delete[] static_cast<float*>(local_recvbuf);
      else if(datatype == MPI_DOUBLE) delete[] static_cast<double*>(local_recvbuf);
      else if(datatype == MPI_LONG) delete[] static_cast<long*>(local_recvbuf);
      else if(datatype == MPI_UNSIGNED_LONG) delete[] static_cast<unsigned long*>(local_recvbuf);
      else delete[] static_cast<char*>(local_recvbuf);
    }

    Message_Check(comm);

    return 0;
  }


  int MPI_Reduce_scatter(const void *sendbuf, void *recvbuf, const int recvcounts[], MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
  {

    if(!comm.is_ep && comm.mpi_comm)
    {
      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Reduce_scatter(sendbuf, recvbuf, recvcounts, static_cast< ::MPI_Datatype>(datatype), static_cast< ::MPI_Op>(op),
                           static_cast< ::MPI_Comm>(comm.mpi_comm));
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

    void* local_buf;
    void* local_buf2;
    int local_buf_size = accumulate(recvcounts, recvcounts+ep_size, 0);
    int local_buf2_size = accumulate(recvcounts+ep_rank-ep_rank_loc, recvcounts+ep_rank-ep_rank_loc+num_ep, 0);

    ::MPI_Aint datasize, lb;
    #ifdef _serialized
    #pragma omp critical (_mpi_call)
    #endif // _serialized
    ::MPI_Type_get_extent(static_cast< ::MPI_Datatype>(datatype), &lb, &datasize);

    if(ep_rank_loc == 0)
    {
      local_buf = new void*[local_buf_size*datasize];
      local_buf2 = new void*[local_buf2_size*datasize];
    }
    MPI_Reduce_local(sendbuf, local_buf, local_buf_size, MPI_INT, op, comm);


    if(ep_rank_loc == 0)
    {
      int local_recvcnt[mpi_size];
      for(int i=0; i<mpi_size; i++)
      {
        local_recvcnt[i] = accumulate(recvcounts+ep_rank, recvcounts+ep_rank+num_ep, 0);
      }
      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Reduce_scatter(local_buf, local_buf2, local_recvcnt, static_cast< ::MPI_Datatype>(datatype),
                         static_cast< ::MPI_Op>(op), static_cast< ::MPI_Comm>(comm.mpi_comm));
    }


    int displs[num_ep];
    displs[0] = 0;
    for(int i=1; i<num_ep; i++)
    {
      displs[i] = displs[i-1] + recvcounts[ep_rank-ep_rank_loc+i-1];
    }

    MPI_Scatterv_local(local_buf2, recvcounts+ep_rank-ep_rank_loc, displs, datatype, recvbuf, comm);

    if(ep_rank_loc == 0)
    {
      if(datatype == MPI_INT)
      {
        delete[] static_cast<int*>(local_buf);
        delete[] static_cast<int*>(local_buf2);
      }
      else if(datatype == MPI_FLOAT)
      {
        delete[] static_cast<float*>(local_buf);
        delete[] static_cast<float*>(local_buf2);
      }
      else if(datatype == MPI_DOUBLE)
      {
        delete[] static_cast<double*>(local_buf);
        delete[] static_cast<double*>(local_buf2);
      }
      else if(datatype == MPI_LONG)
      {
        delete[] static_cast<long*>(local_buf);
        delete[] static_cast<long*>(local_buf2);
      }
      else if(datatype == MPI_UNSIGNED_LONG)
      {
        delete[] static_cast<unsigned long*>(local_buf);
        delete[] static_cast<unsigned long*>(local_buf2);
      }
      else // if(datatype == MPI_DOUBLE)
      {
        delete[] static_cast<char*>(local_buf);
        delete[] static_cast<char*>(local_buf2);
      }
    }

    Message_Check(comm);
    return 0;
  }
}

