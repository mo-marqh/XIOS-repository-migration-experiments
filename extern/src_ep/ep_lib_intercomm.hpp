#ifndef EP_LIB_INTERCOMM_HPP_INCLUDED
#define EP_LIB_INTERCOMM_HPP_INCLUDED

namespace ep_lib
{
  #ifdef _intelmpi
  typedef int MPI_Datatype;
  typedef int MPI_Op;
  #elif _openmpi
  typedef void* MPI_Datatype;
  typedef void* MPI_Op;
  #endif

  int Message_Check_intercomm(MPI_Comm comm);

  int MPI_Isend_intercomm(const void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);
  int MPI_Issend_intercomm(const void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);


  int MPI_Iprobe_intercomm(int source, int tag, MPI_Comm comm, int *flag, MPI_Status *status);
  int MPI_Iprobe_any_source(int tag, MPI_Comm comm, int *flag, MPI_Status *status);
  int MPI_Improbe_intercomm(int source, int tag, MPI_Comm comm, int *flag, MPI_Message *message, MPI_Status *status);
  int MPI_Improbe_any_source(int tag, MPI_Comm comm, int *flag, MPI_Message *message, MPI_Status *status);

  int MPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm);

  int MPI_Intercomm_create(MPI_Comm local_comm, int local_leader, MPI_Comm peer_comm, int remote_leader, int tag, MPI_Comm *newintercomm);

  int MPI_Intercomm_create_kernel(MPI_Comm local_comm, int local_leader, MPI_Comm peer_comm, int remote_leader, int tag, MPI_Comm *newintercomm);
  
  #ifdef _intelmpi
  int MPI_Intercomm_create_from_world(MPI_Comm local_comm, int local_leader, int peer_comm_ptr, int mpi_remote_leader, int tag, MPI_Comm *newintercomm);
  #elif _openmpi
  int MPI_Intercomm_create_from_world(MPI_Comm local_comm, int local_leader, void* peer_comm_ptr, int mpi_remote_leader, int tag, MPI_Comm *newintercomm);
  #endif
  
  int MPI_Intercomm_create_unique_leader(MPI_Comm local_comm, int local_leader, MPI_Comm peer_comm, int remote_leader, int tag, MPI_Comm *newintercomm);

  int MPI_Intercomm_merge(MPI_Comm intercomm, bool high, MPI_Comm *newintracomm);

  int MPI_Intercomm_merge_unique_leader(MPI_Comm intercomm, bool high, MPI_Comm *newintracomm);

  int MPI_Comm_remote_size(MPI_Comm comm, int *size);

  int MPI_Comm_test_inter(MPI_Comm comm, int *flag);
}


#endif // EP_LIB_INTERCOMM_HPP_INCLUDED
