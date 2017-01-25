#include "ep_lib.hpp"
#include <mpi.h>
//#include "ep_declaration.hpp"

namespace ep_lib
{



  int MPI_Alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
  {
    ::MPI_Aint typesize, llb;
    ::MPI_Type_get_extent(sendtype, &llb, &typesize);

    for(int i=0; i<comm.ep_comm_ptr->size_rank_info[0].second; i++)
    {
      MPI_Gather((char*)sendbuf+i*sendcount*typesize, sendcount, sendtype, recvbuf, recvcount, recvtype, i, comm);
    }

    return 0;
  }

}


