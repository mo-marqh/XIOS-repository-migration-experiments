#ifndef EP_LIB_LOCAL_HPP_INCLUDED
#define EP_LIB_LOCAL_HPP_INCLUDED

namespace ep_lib
{
  #ifdef _intelmpi
  typedef int MPI_Datatype;
  typedef int MPI_Op;
  #elif _openmpi
  typedef void* MPI_Datatype;
  typedef void* MPI_Op;
  #endif


  int MPI_Reduce_local       (const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);
  int MPI_Reduce_local_int   (const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);
  int MPI_Reduce_local_float (const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);
  int MPI_Reduce_local_double(const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);
  int MPI_Reduce_local_long  (const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);
  int MPI_Reduce_local_ulong (const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);
  int MPI_Reduce_local_char  (const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);


  int MPI_Scan_local       (const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);
  int MPI_Scan_local_int   (const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);
  int MPI_Scan_local_float (const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);
  int MPI_Scan_local_double(const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);
  int MPI_Scan_local_long  (const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);
  int MPI_Scan_local_ulong (const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);
  int MPI_Scan_local_char  (const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);

  int MPI_Exscan_local       (const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);
  int MPI_Exscan_local_int   (const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);
  int MPI_Exscan_local_float (const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);
  int MPI_Exscan_local_double(const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);
  int MPI_Exscan_local_long  (const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);
  int MPI_Exscan_local_ulong (const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);
  int MPI_Exscan_local_char  (const void *sendbuf, void *recvbuf, int count, MPI_Op op, MPI_Comm comm);

  int MPI_Bcast_local       (void *buffer, int count, MPI_Datatype datatype, MPI_Comm comm);
  int MPI_Bcast_local_int   (void *buffer, int count, MPI_Comm comm);
  int MPI_Bcast_local_float (void *buffer, int count, MPI_Comm comm);
  int MPI_Bcast_local_double(void *buffer, int count, MPI_Comm comm);
  int MPI_Bcast_local_long  (void *buffer, int count, MPI_Comm comm);
  int MPI_Bcast_local_ulong (void *buffer, int count, MPI_Comm comm);
  int MPI_Bcast_local_char  (void *buffer, int count, MPI_Comm comm);

  int MPI_Gather_local       (const void *sendbuf, int count, MPI_Datatype datatype, void *recvbuf, MPI_Comm comm);
  int MPI_Gather_local_int   (const void *sendbuf, int count, void *recvbuf, MPI_Comm comm);
  int MPI_Gather_local_float (const void *sendbuf, int count, void *recvbuf, MPI_Comm comm);
  int MPI_Gather_local_double(const void *sendbuf, int count, void *recvbuf, MPI_Comm comm);
  int MPI_Gather_local_long  (const void *sendbuf, int count, void *recvbuf, MPI_Comm comm);
  int MPI_Gather_local_ulong (const void *sendbuf, int count, void *recvbuf, MPI_Comm comm);
  int MPI_Gather_local_char  (const void *sendbuf, int count, void *recvbuf, MPI_Comm comm);


  int MPI_Gatherv_local       (const void *sendbuf, int count, MPI_Datatype datatype, void *recvbuf,
                               const int recvcounts[], const int displs[], MPI_Comm comm);
  int MPI_Gatherv_local_int   (const void *sendbuf, int count, void *recvbuf, const int recvcounts[], const int displs[], MPI_Comm comm);
  int MPI_Gatherv_local_float (const void *sendbuf, int count, void *recvbuf, const int recvcounts[], const int displs[], MPI_Comm comm);
  int MPI_Gatherv_local_double(const void *sendbuf, int count, void *recvbuf, const int recvcounts[], const int displs[], MPI_Comm comm);
  int MPI_Gatherv_local_long  (const void *sendbuf, int count, void *recvbuf, const int recvcounts[], const int displs[], MPI_Comm comm);
  int MPI_Gatherv_local_ulong (const void *sendbuf, int count, void *recvbuf, const int recvcounts[], const int displs[], MPI_Comm comm);
  int MPI_Gatherv_local_char  (const void *sendbuf, int count, void *recvbuf, const int recvcounts[], const int displs[], MPI_Comm comm);

  int MPI_Scatter_local       (const void *sendbuf, int count, MPI_Datatype datatype, void *recvbuf, MPI_Comm comm);
  int MPI_Scatter_local_int   (const void *sendbuf, int count, void *recvbuf, MPI_Comm comm);
  int MPI_Scatter_local_float (const void *sendbuf, int count, void *recvbuf, MPI_Comm comm);
  int MPI_Scatter_local_double(const void *sendbuf, int count, void *recvbuf, MPI_Comm comm);
  int MPI_Scatter_local_long  (const void *sendbuf, int count, void *recvbuf, MPI_Comm comm);
  int MPI_Scatter_local_ulong (const void *sendbuf, int count, void *recvbuf, MPI_Comm comm);
  int MPI_Scatter_local_char  (const void *sendbuf, int count, void *recvbuf, MPI_Comm comm);

  int MPI_Scatterv_local       (const void *sendbuf, const int sendcounts[], const int displs[], MPI_Datatype datatype, void *recvbuf, MPI_Comm comm);
  int MPI_Scatterv_local_int   (const void *sendbuf, const int sendcounts[], const int displs[], void *recvbuf, MPI_Comm comm);
  int MPI_Scatterv_local_float (const void *sendbuf, const int sendcounts[], const int displs[], void *recvbuf, MPI_Comm comm);
  int MPI_Scatterv_local_double(const void *sendbuf, const int sendcounts[], const int displs[], void *recvbuf, MPI_Comm comm);
  int MPI_Scatterv_local_long  (const void *sendbuf, const int sendcounts[], const int displs[], void *recvbuf, MPI_Comm comm);
  int MPI_Scatterv_local_ulong (const void *sendbuf, const int sendcounts[], const int displs[], void *recvbuf, MPI_Comm comm);
  int MPI_Scatterv_local_char  (const void *sendbuf, const int sendcounts[], const int displs[], void *recvbuf, MPI_Comm comm);

  int innode_memcpy(int sender, const void* sendbuf, int receiver, void* recvbuf, int count, MPI_Datatype datatype, MPI_Comm comm);

  int MPI_Barrier_local(MPI_Comm comm);

}

#endif // EP_LIB_LOCAL_HPP_INCLUDED
