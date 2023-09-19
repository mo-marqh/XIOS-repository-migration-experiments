#ifndef __XIOS_MPI_HPP__
#define __XIOS_MPI_HPP__

/* skip C++ Binding for mpich , intel MPI */
#define MPICH_SKIP_MPICXX

/* skip C++ Binding for SGI MPI library */
#define MPI_NO_CPPBIND

/* skip C++ Binding for OpenMPI */
#define OMPI_SKIP_MPICXX

#include <mpi.h>
#include <climits> 
#include <cstdint>

#if SIZE_MAX == UCHAR_MAX
   #define MPI_SIZE_T MPI_UNSIGNED_CHAR
#elif SIZE_MAX == USHRT_MAX
   #define MPI_SIZE_T MPI_UNSIGNED_SHORT
#elif SIZE_MAX == UINT_MAX
   #define MPI_SIZE_T MPI_UNSIGNED
#elif SIZE_MAX == ULONG_MAX
   #define MPI_SIZE_T MPI_UNSIGNED_LONG
#elif SIZE_MAX == ULLONG_MAX
   #define MPI_SIZE_T MPI_UNSIGNED_LONG_LONG
#else
   #error "Unable to find MPI_SIZE_T equivalent type"
#endif

#include "mpi_tools.hpp"

inline int MPI_Comm_dup(MPI_Comm comm, MPI_Comm * newcomm)
{
  int ret=PMPI_Comm_dup(comm, newcomm) ;
  xios::CCommTrack::registerComm(*newcomm) ;
  return ret ;
}

inline int MPI_Intercomm_create(MPI_Comm local_comm, int local_leader,
                         MPI_Comm peer_comm, int remote_leader, int tag, MPI_Comm * newintercomm)
{
  int ret=PMPI_Intercomm_create(local_comm, local_leader, peer_comm, remote_leader, tag, newintercomm) ;
  xios::CCommTrack::registerComm(*newintercomm) ;
  return ret ;
}

inline int MPI_Intercomm_merge(MPI_Comm intercomm, int high, MPI_Comm * newintracomm)
{
  int ret=PMPI_Intercomm_merge(intercomm, high, newintracomm) ;
  xios::CCommTrack::registerComm(*newintracomm) ;
  return ret ;
}

inline int MPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm * newcomm)
{
  int ret=PMPI_Comm_split(comm, color, key,newcomm) ;
  xios::CCommTrack::registerComm(*newcomm) ;
  return ret ;
}

inline int MPI_Comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm *newcomm)
{
  int ret=PMPI_Comm_create(comm, group, newcomm) ;
  xios::CCommTrack::registerComm(*newcomm) ;
  return ret ;
}

inline int MPI_Comm_free(MPI_Comm *comm) 
{ 
  xios::CCommTrack::releaseComm(*comm) ;
  return PMPI_Comm_free(comm) ;
}


#endif
