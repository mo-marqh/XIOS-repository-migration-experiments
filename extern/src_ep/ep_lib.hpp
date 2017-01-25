#ifndef EP_LIB_HPP_INCLUDED
#define EP_LIB_HPP_INCLUDED

#include "ep_type.hpp"
#include "ep_lib_intercomm.hpp"
#include "ep_lib_local.hpp"
#include "ep_lib_collective.hpp"
#include "ep_tag.hpp"
#include "ep_lib_fortran.hpp"


namespace ep_lib
{
#ifdef _intelmpi
  typedef int MPI_Datatype;
  typedef int MPI_Op;
#elif _openmpi
  typedef void* MPI_Datatype;
  typedef void* MPI_Op;
#endif

  #define MPI_ANY_SOURCE -2 
  #define MPI_ANY_TAG -1 

  int MPI_Init_thread(int* argc, char*** argv, int required, int*provided);

  int MPI_Init(int *argc, char ***argv);

  int MPI_Initialized(int *flag);

  int MPI_Comm_rank(MPI_Comm comm, int* rank);

  int MPI_Comm_size(MPI_Comm comm, int* rank);

  int MPI_Comm_free(MPI_Comm* comm);

  int MPI_Finalize();
  
  double MPI_Wtime();

  int MPI_Abort(MPI_Comm comm, int errorcode);

  int MPI_Get_count(const MPI_Status *status, MPI_Datatype datatype, int *count);

  #ifdef _openmpi
  int MPI_Comm_create_endpoints(void* mpi_comm, int num_ep, MPI_Info info, MPI_Comm *& out_comm_hdls); // from MPI to create endpoints
  #elif _intelmpi
  int MPI_Comm_create_endpoints(int mpi_comm, int num_ep, MPI_Info info, MPI_Comm *& out_comm_hdls); // from MPI to create endpoints
  #endif
  
  int MPI_Comm_create_endpoints(MPI_Comm base_comm, int num_ep, MPI_Info info, MPI_Comm *& out_comm_hdls); // from EP to create endpoints

  int MPI_Send(void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm);
  int MPI_Ssend(void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm);
  int MPI_Isend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);
  int MPI_Issend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);

  int tag_combine(int real_tag, int src, int dest);
  int get_ep_rank(MPI_Comm comm, int ep_rank_loc, int mpi_rank);
  int get_ep_rank_intercomm(MPI_Comm comm, int ep_rank_loc, int mpi_rank);

  int Message_Check(MPI_Comm comm);

  int MPI_Recv  (void *buf, int count, MPI_Datatype datatype, int src, int tag, MPI_Comm comm, MPI_Status *status);
  int MPI_Irecv (void *buf, int count, MPI_Datatype datatype, int src, int tag, MPI_Comm comm, MPI_Request *request);
  int MPI_Mrecv (void *buf, int count, MPI_Datatype datatype, MPI_Message *message, MPI_Status *status);
  int MPI_Imrecv(void *buf, int count, MPI_Datatype datatype, MPI_Message *message, MPI_Request *request);


  int MPI_Test(MPI_Request *request, int *flag, MPI_Status *status);
  int MPI_Testall(int count, MPI_Request *array_of_requests, int *flag, MPI_Status *array_of_statuses);

  int MPI_Wait(MPI_Request *request, MPI_Status *status);
  int MPI_Waitall(int count, MPI_Request *array_of_requests, MPI_Status *array_of_statuses);


  int MPI_Iprobe(int source, int tag, MPI_Comm comm, int *flag, MPI_Status *status);
  int MPI_Improbe(int source, int tag, MPI_Comm comm, int *flag, MPI_Message *message, MPI_Status *status);
  
  int MPI_Alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr);
  
  int MPI_Alloc_mem(unsigned long size, MPI_Info info, void *baseptr);


  void check_sum_send(const void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, int type);
  void check_sum_recv(void *buf, int count, MPI_Datatype datatype, int src, int tag, MPI_Comm comm, int type);
}



#endif // EP_LIB_HPP_INCLUDED
