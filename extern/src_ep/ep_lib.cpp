#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"
#include <iostream>
#include <fstream>

using namespace std;


namespace ep_lib
{


  int tag_combine(int real_tag, int src, int dest)
  {
    int a = real_tag << 16;
    int b = src << 8;
    int c = dest;

    return a+b+c;
  }

  int get_ep_rank(MPI_Comm comm, int ep_rank_loc, int mpi_rank)
  {
    for(int i=0; i<comm.rank_map->size(); i++)
    {
      if(   ( comm.rank_map->at(i).first  == ep_rank_loc )
         && ( comm.rank_map->at(i).second == mpi_rank ) )
      {
        return i;
      }
    }
    printf("rank not find\n");
  }
  
  int get_ep_rank_intercomm(MPI_Comm comm, int ep_rank_loc, int mpi_rank)
  {
    // intercomm
    int inter_rank;
    for(int i=0; i<comm.ep_comm_ptr->intercomm->intercomm_rank_map->size(); i++)
    {
      if(   ( comm.ep_comm_ptr->intercomm->intercomm_rank_map->at(i).first  == ep_rank_loc )
         && ( comm.ep_comm_ptr->intercomm->intercomm_rank_map->at(i).second == mpi_rank ) )
      {
        inter_rank =  i;
        break;
      }
    }

    for(int i=0; i<comm.ep_comm_ptr->intercomm->remote_rank_map->size(); i++)
    {
      if(  comm.ep_comm_ptr->intercomm->remote_rank_map->at(i).first  == inter_rank  )
      {
        printf("get_ep_rank for intercomm, ep_rank_loc = %d, mpi_rank = %d => ep_src = %d\n", ep_rank_loc, mpi_rank, i);
        return i;
      }
    }

    printf("rank not find\n");
    
  }


  int innode_memcpy(int sender, const void* sendbuf, int receiver, void* recvbuf, int count, MPI_Datatype datatype, MPI_Comm comm)
  {
    int ep_rank, ep_rank_loc, mpi_rank;
    int ep_size, num_ep, mpi_size;

    ep_rank = comm.ep_comm_ptr->size_rank_info[0].first;
    ep_rank_loc = comm.ep_comm_ptr->size_rank_info[1].first;
    mpi_rank = comm.ep_comm_ptr->size_rank_info[2].first;
    ep_size = comm.ep_comm_ptr->size_rank_info[0].second;
    num_ep = comm.ep_comm_ptr->size_rank_info[1].second;
    mpi_size = comm.ep_comm_ptr->size_rank_info[2].second;



    if(datatype == MPI_INT)
    {

      int* send_buf = static_cast<int*>(const_cast<void*>(sendbuf));
      int* recv_buf = static_cast<int*>(recvbuf);
      int* buffer = comm.my_buffer->buf_int;

      for(int j=0; j<count; j+=BUFFER_SIZE)
      {
        if(ep_rank_loc == sender)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
          }
          #pragma omp flush
        }

        MPI_Barrier_local(comm);


        if(ep_rank_loc == receiver)
        {
          #pragma omp flush
          #pragma omp critical (read_from_buffer)
          {
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
          }
        }

        MPI_Barrier_local(comm);
      }
    }
    else if(datatype == MPI_FLOAT)
    {

      float* send_buf = static_cast<float*>(const_cast<void*>(sendbuf));
      float* recv_buf = static_cast<float*>(recvbuf);
      float* buffer = comm.my_buffer->buf_float;

      for(int j=0; j<count; j+=BUFFER_SIZE)
      {
        if(ep_rank_loc == sender)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
          }
          #pragma omp flush
        }

        MPI_Barrier_local(comm);


        if(ep_rank_loc == receiver)
        {
          #pragma omp flush
          #pragma omp critical (read_from_buffer)
          {
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
          }
        }

        MPI_Barrier_local(comm);
      }
    }
    else if(datatype == MPI_DOUBLE)
    {


      double* send_buf = static_cast<double*>(const_cast<void*>(sendbuf));
      double* recv_buf = static_cast<double*>(recvbuf);
      double* buffer = comm.my_buffer->buf_double;

      for(int j=0; j<count; j+=BUFFER_SIZE)
      {
        if(ep_rank_loc == sender)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
          }
          #pragma omp flush
        }

        MPI_Barrier_local(comm);


        if(ep_rank_loc == receiver)
        {
          #pragma omp flush
          #pragma omp critical (read_from_buffer)
          {
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
          }
        }

        MPI_Barrier_local(comm);
      }
    }
    else if(datatype == MPI_LONG)
    {
      long* send_buf = static_cast<long*>(const_cast<void*>(sendbuf));
      long* recv_buf = static_cast<long*>(recvbuf);
      long* buffer = comm.my_buffer->buf_long;

      for(int j=0; j<count; j+=BUFFER_SIZE)
      {
        if(ep_rank_loc == sender)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
          }
          #pragma omp flush
        }

        MPI_Barrier_local(comm);


        if(ep_rank_loc == receiver)
        {
          #pragma omp flush
          #pragma omp critical (read_from_buffer)
          {
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
          }
        }

        MPI_Barrier_local(comm);
      }
    }
    else if(datatype == MPI_UNSIGNED_LONG)
    {
      unsigned long* send_buf = static_cast<unsigned long*>(const_cast<void*>(sendbuf));
      unsigned long* recv_buf = static_cast<unsigned long*>(recvbuf);
      unsigned long* buffer = comm.my_buffer->buf_ulong;

      for(int j=0; j<count; j+=BUFFER_SIZE)
      {
        if(ep_rank_loc == sender)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
          }
          #pragma omp flush
        }

        MPI_Barrier_local(comm);


        if(ep_rank_loc == receiver)
        {
          #pragma omp flush
          #pragma omp critical (read_from_buffer)
          {
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
          }
        }

        MPI_Barrier_local(comm);
      }
    }
    else if(datatype == MPI_CHAR)
    {
      char* send_buf = static_cast<char*>(const_cast<void*>(sendbuf));
      char* recv_buf = static_cast<char*>(recvbuf);
      char* buffer = comm.my_buffer->buf_char;

      for(int j=0; j<count; j+=BUFFER_SIZE)
      {
        if(ep_rank_loc == sender)
        {
          #pragma omp critical (write_to_buffer)
          {
            copy(send_buf+j, send_buf+j+min(BUFFER_SIZE, count-j), buffer);
          }
          #pragma omp flush
        }

        MPI_Barrier_local(comm);


        if(ep_rank_loc == receiver)
        {
          #pragma omp flush
          #pragma omp critical (read_from_buffer)
          {
            copy(buffer, buffer+min(BUFFER_SIZE, count-j), recv_buf+j);
          }
        }

        MPI_Barrier_local(comm);
      }
    }
    else
    {
      printf("datatype not supported!!\n");
      exit(1);
    }
    return 0;
  }


  int MPI_Get_count(const MPI_Status *status, MPI_Datatype datatype, int *count)
  {
/*
    ::MPI_Aint datasize, char_size, lb;

    ::MPI_Type_get_extent(static_cast< ::MPI_Datatype>(datatype), &lb, &datasize);
    ::MPI_Type_get_extent(MPI_CHAR, &lb, &char_size);

    *count = status->char_count / ( datasize/ char_size);

    //printf("MPI_Get_count, status_count  = %d\n", *count);
    return 0;
*/
    ::MPI_Status *mpi_status = static_cast< ::MPI_Status* >(status->mpi_status);
    ::MPI_Datatype mpi_datatype = static_cast< ::MPI_Datatype >(datatype);

    ::MPI_Get_count(mpi_status, mpi_datatype, count);
  }

  double MPI_Wtime()
  {
    return ::MPI_Wtime();

  }

  void check_sum_send(const void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, int type)
  {
    int src_rank;
    int int_count;
    ::MPI_Aint datasize, intsize, charsize, lb;
    
    ::MPI_Type_get_extent(static_cast< ::MPI_Datatype>(datatype), &lb, &datasize);
    ::MPI_Type_get_extent(MPI_CHAR_STD, &lb, &intsize);

    int_count = count * datasize / intsize ;

    char *buffer = static_cast< char* >(const_cast< void*> (buf));
    
    unsigned long sum = 0;
    for(int i = 0; i<int_count; i++)
      sum += *(buffer+i); 


    MPI_Comm_rank(comm, &src_rank);
    
    ofstream myfile;
    myfile.open ("send_log.txt", ios::app);
    if (myfile.is_open())
    {
      myfile << "type = " << type << " src = "<< src_rank<< " dest = "<< dest <<" tag = "<< tag << "  count = "<< count << " sum = "<< sum << "\n";
      myfile.close();  
    }
    else printf("Unable to open file\n");

  }


  void check_sum_recv(void *buf, int count, MPI_Datatype datatype, int src, int tag, MPI_Comm comm, int type)
  {
    int dest_rank;
    int int_count;
    ::MPI_Aint datasize, intsize, charsize, lb;
    
    ::MPI_Type_get_extent(static_cast< ::MPI_Datatype>(datatype), &lb, &datasize);
    ::MPI_Type_get_extent(MPI_CHAR_STD, &lb, &intsize);

    int_count = count * datasize / intsize ;

    char *buffer = static_cast< char* >(buf);
    
    unsigned long sum = 0;
    for(int i = 0; i<int_count; i++)
      sum += *(buffer+i); 


    MPI_Comm_rank(comm, &dest_rank);
    
    ofstream myfile;
    myfile.open ("recv_log.txt", ios::app);
    if (myfile.is_open())
    {
      myfile << "type = " << type << " src = "<< src << " dest = "<< dest_rank <<" tag = "<< tag << "  count = "<< count << " sum = "<< sum << "\n";
      myfile.close();  
    }
    else printf("Unable to open file\n");

  }
}






