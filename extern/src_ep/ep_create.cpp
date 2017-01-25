/*!
   \file ep_create.cpp
   \since 2 may 2016

   \brief Definitions of MPI endpoint function: MPI_Comm_create_endpoints
 */

#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

using namespace std;



namespace ep_lib {

  /*!
    Dynamic creation of endpoints for each MPI process.
    The output of this function is an array of communicator handles of length num_ep, where each handle
    corresponds to a nez local randk in the output communicator.
    Once created, endpoints behave as MPI processes.
    \param [in] mpi_comm Parent MPI communicator.
    \param [in] num_ep Number of endpoints per process.
    \param [out] info Information of the EP creation.
    \param [out] out_comm_hdls Handles of EP communicators.
  */
  #ifdef _intelmpi
  int MPI_Comm_create_endpoints(int base_comm_ptr, int num_ep, MPI_Info info, MPI_Comm *& out_comm_hdls)
  {
    int base_rank;
    int base_size;

    ::MPI_Comm mpi_base_comm = static_cast< ::MPI_Comm> (base_comm_ptr);

    ::MPI_Comm_size(mpi_base_comm, &base_size);  // ep_lib::mpi_comm_size
    ::MPI_Comm_rank(mpi_base_comm, &base_rank);  // ep_lib::mpi_comm_rank
									  // parent_comm can also be endpoints communicators

    std::vector<int> recv_num_ep(base_size);

    out_comm_hdls = new MPI_Comm[num_ep];

    for (int idx = 0; idx < num_ep; ++idx)
    {
      out_comm_hdls[idx].is_ep = true;
      out_comm_hdls[idx].is_intercomm = false;
      out_comm_hdls[idx].ep_comm_ptr = new ep_communicator;
      out_comm_hdls[idx].mpi_comm = base_comm_ptr;
      out_comm_hdls[idx].ep_comm_ptr->comm_list = out_comm_hdls;
      out_comm_hdls[idx].ep_comm_ptr->comm_label = 0;
    }

    ::MPI_Allgather(&num_ep, 1, MPI_INT_STD, &recv_num_ep[0], 1, MPI_INT_STD, mpi_base_comm);


    int sum = 0;  // representing total ep number of process with smaller rank
    for (int i = 0; i < base_rank; ++i) {sum += recv_num_ep[i]; }

    int ep_size = std::accumulate(recv_num_ep.begin(), recv_num_ep.end(), 0);

    out_comm_hdls[0].ep_barrier = new OMPbarrier(num_ep);

    out_comm_hdls[0].my_buffer = new BUFFER;
    out_comm_hdls[0].my_buffer->buf_double = new double[BUFFER_SIZE];
    out_comm_hdls[0].my_buffer->buf_float  = new float[BUFFER_SIZE];
    out_comm_hdls[0].my_buffer->buf_int    = new int[BUFFER_SIZE];
    out_comm_hdls[0].my_buffer->buf_long   = new long[BUFFER_SIZE];
    out_comm_hdls[0].my_buffer->buf_ulong  = new unsigned long[BUFFER_SIZE];
    out_comm_hdls[0].my_buffer->buf_char   = new char[BUFFER_SIZE];

    out_comm_hdls[0].rank_map = new RANK_MAP;
    out_comm_hdls[0].rank_map->resize(ep_size);


    for (int i = 1; i < num_ep; i++)
    {
      out_comm_hdls[i].ep_barrier = out_comm_hdls[0].ep_barrier;
      out_comm_hdls[i].my_buffer  = out_comm_hdls[0].my_buffer;
      out_comm_hdls[i].rank_map   = out_comm_hdls[0].rank_map;
    }


    for (int i = 0; i < num_ep; i++)
    {
      out_comm_hdls[i].ep_comm_ptr->size_rank_info[0] = std::make_pair(sum+i, ep_size);
      out_comm_hdls[i].ep_comm_ptr->size_rank_info[1] = std::make_pair(i, num_ep);
      out_comm_hdls[i].ep_comm_ptr->size_rank_info[2] = std::make_pair(base_rank, base_size);

      out_comm_hdls[i].ep_comm_ptr->message_queue = new Message_list;
    }


    int ind = 0;

    for(int i=0; i<base_size; i++)
    {
      for(int j=0; j<recv_num_ep[i]; j++)
      {
        out_comm_hdls[0].rank_map->at(ind) = make_pair(j, i);
        ind++;
      }
    }

    printf("ep_lib::MPI_Comm_create_endpoints()       OK from int\n");

    return 0;

  } //MPI_Comm_create_endpoints

  #elif _openmpi
  int MPI_Comm_create_endpoints(void* base_comm_ptr, int num_ep, MPI_Info info, MPI_Comm *& out_comm_hdls)
  {

    int base_rank;
    int base_size;

    ::MPI_Comm mpi_base_comm = static_cast< ::MPI_Comm> (base_comm_ptr);

    ::MPI_Comm_size(mpi_base_comm, &base_size);  // ep_lib::mpi_comm_size
    ::MPI_Comm_rank(mpi_base_comm, &base_rank);  // ep_lib::mpi_comm_rank
                                                 // parent_comm can also be endpoints communicators ?
    std::vector<int> recv_num_ep(base_size);

    out_comm_hdls = new MPI_Comm[num_ep];

    for (int idx = 0; idx < num_ep; ++idx)
    {
      out_comm_hdls[idx].is_ep = true;
      out_comm_hdls[idx].is_intercomm = false;
      out_comm_hdls[idx].ep_comm_ptr = new ep_communicator;
      out_comm_hdls[idx].mpi_comm = base_comm_ptr;
      out_comm_hdls[idx].ep_comm_ptr->comm_list = out_comm_hdls;
      out_comm_hdls[idx].ep_comm_ptr->comm_label = 0;
    }

    ::MPI_Allgather(&num_ep, 1, MPI_INT_STD, &recv_num_ep[0], 1, MPI_INT_STD, mpi_base_comm);

    int sum = 0;  // representing total ep number of process with smaller rank
    for (int i = 0; i < base_rank; ++i) {sum += recv_num_ep[i]; }

    int ep_size = std::accumulate(recv_num_ep.begin(), recv_num_ep.end(), 0);

    out_comm_hdls[0].ep_barrier = new OMPbarrier(num_ep);

    out_comm_hdls[0].my_buffer = new BUFFER;
    out_comm_hdls[0].my_buffer->buf_double = new double[BUFFER_SIZE];
    out_comm_hdls[0].my_buffer->buf_float  = new float[BUFFER_SIZE];
    out_comm_hdls[0].my_buffer->buf_int    = new int[BUFFER_SIZE];
    out_comm_hdls[0].my_buffer->buf_long   = new long[BUFFER_SIZE];
    out_comm_hdls[0].my_buffer->buf_ulong  = new unsigned long[BUFFER_SIZE];
    out_comm_hdls[0].my_buffer->buf_char   = new char[BUFFER_SIZE];

    out_comm_hdls[0].rank_map = new RANK_MAP;
    out_comm_hdls[0].rank_map->resize(ep_size);


    for (int i = 1; i < num_ep; i++)
    {
      out_comm_hdls[i].ep_barrier = out_comm_hdls[0].ep_barrier;
      out_comm_hdls[i].my_buffer  = out_comm_hdls[0].my_buffer;
      out_comm_hdls[i].rank_map   = out_comm_hdls[0].rank_map;
    }


    for (int i = 0; i < num_ep; i++)
    {
      out_comm_hdls[i].ep_comm_ptr->size_rank_info[0] = std::make_pair(sum+i, ep_size);
      out_comm_hdls[i].ep_comm_ptr->size_rank_info[1] = std::make_pair(i, num_ep);
      out_comm_hdls[i].ep_comm_ptr->size_rank_info[2] = std::make_pair(base_rank, base_size);

      out_comm_hdls[i].ep_comm_ptr->message_queue = new Message_list;
    }


    int ind = 0;

    for(int i=0; i<base_size; i++)
    {
      for(int j=0; j<recv_num_ep[i]; j++)
      {
        out_comm_hdls[0].rank_map->at(ind) = make_pair(j, i);
        ind++;
      }
    }

    printf("ep_lib::MPI_Comm_create_endpoints()       OK from void*\n");

    return 0;

  } //MPI_Comm_create_endpoints

  #endif
  
  
  int MPI_Comm_create_endpoints(MPI_Comm base_comm, int num_ep, MPI_Info info, MPI_Comm *& out_comm_hdls)
  {

    int base_rank;
    int base_size;
    
    assert(!base_comm.is_ep);

    ::MPI_Comm mpi_base_comm = static_cast< ::MPI_Comm> (base_comm.mpi_comm);

    ::MPI_Comm_size(mpi_base_comm, &base_size);  // ep_lib::mpi_comm_size
    ::MPI_Comm_rank(mpi_base_comm, &base_rank);  // ep_lib::mpi_comm_rank
                    // parent_comm can also be endpoints communicators

    std::vector<int> recv_num_ep(base_size);

    out_comm_hdls = new MPI_Comm[num_ep];

    for (int idx = 0; idx < num_ep; ++idx)
    {
      out_comm_hdls[idx].is_ep = true;
      out_comm_hdls[idx].is_intercomm = false;
      out_comm_hdls[idx].ep_comm_ptr = new ep_communicator;
      out_comm_hdls[idx].mpi_comm = base_comm.mpi_comm;
      out_comm_hdls[idx].ep_comm_ptr->comm_list = out_comm_hdls;
      out_comm_hdls[idx].ep_comm_ptr->comm_label = 0;
    }

    ::MPI_Allgather(&num_ep, 1, MPI_INT_STD, &recv_num_ep[0], 1, MPI_INT_STD, mpi_base_comm);


    int sum = 0;  // representing total ep number of process with smaller rank
    for (int i = 0; i < base_rank; ++i) {sum += recv_num_ep[i]; }

    int ep_size = std::accumulate(recv_num_ep.begin(), recv_num_ep.end(), 0);

    out_comm_hdls[0].ep_barrier = new OMPbarrier(num_ep);

    out_comm_hdls[0].my_buffer = new BUFFER;
    out_comm_hdls[0].my_buffer->buf_double = new double[BUFFER_SIZE];
    out_comm_hdls[0].my_buffer->buf_float  = new float[BUFFER_SIZE];
    out_comm_hdls[0].my_buffer->buf_int    = new int[BUFFER_SIZE];
    out_comm_hdls[0].my_buffer->buf_long   = new long[BUFFER_SIZE];
    out_comm_hdls[0].my_buffer->buf_ulong  = new unsigned long[BUFFER_SIZE];
    out_comm_hdls[0].my_buffer->buf_char   = new char[BUFFER_SIZE];

    out_comm_hdls[0].rank_map = new RANK_MAP;
    out_comm_hdls[0].rank_map->resize(ep_size);


    for (int i = 1; i < num_ep; i++)
    {
      out_comm_hdls[i].ep_barrier = out_comm_hdls[0].ep_barrier;
      out_comm_hdls[i].my_buffer  = out_comm_hdls[0].my_buffer;
      out_comm_hdls[i].rank_map   = out_comm_hdls[0].rank_map;
    }


    for (int i = 0; i < num_ep; i++)
    {
      out_comm_hdls[i].ep_comm_ptr->size_rank_info[0] = std::make_pair(sum+i, ep_size);
      out_comm_hdls[i].ep_comm_ptr->size_rank_info[1] = std::make_pair(i, num_ep);
      out_comm_hdls[i].ep_comm_ptr->size_rank_info[2] = std::make_pair(base_rank, base_size);

      out_comm_hdls[i].ep_comm_ptr->message_queue = new Message_list;
    }


    int ind = 0;

    for(int i=0; i<base_size; i++)
    {
      for(int j=0; j<recv_num_ep[i]; j++)
      {
        out_comm_hdls[0].rank_map->at(ind) = make_pair(j, i);
        ind++;
      }
    }

    printf("ep_lib::MPI_Comm_create_endpoints()       OK from MPI_Comm\n");

    return 0;

  } //MPI_Comm_create_endpoints


} //namespace ep_lib
