#include "ep_lib.hpp"
#include <mpi.h>


using namespace std;

::MPI_Comm MPI_COMM_WORLD_STD = MPI_COMM_WORLD;
//#undef MPI_COMM_WORLD


::MPI_Comm MPI_COMM_NULL_STD = MPI_COMM_NULL;
//#undef MPI_COMM_NULL


::MPI_Datatype MPI_INT_STD = MPI_INT;
::MPI_Datatype MPI_FLOAT_STD = MPI_FLOAT;
::MPI_Datatype MPI_DOUBLE_STD = MPI_DOUBLE;
::MPI_Datatype MPI_LONG_STD = MPI_LONG;
::MPI_Datatype MPI_CHAR_STD = MPI_CHAR;
::MPI_Datatype MPI_UNSIGNED_LONG_STD = MPI_UNSIGNED_LONG;
::MPI_Datatype MPI_UNSIGNED_CHAR_STD = MPI_UNSIGNED_CHAR;

#undef MPI_INT
#undef MPI_FLOAT
#undef MPI_DOUBLE
#undef MPI_LONG
#undef MPI_CHAR
#undef MPI_UNSIGNED_LONG
#undef MPI_UNSIGNED_CHAR


//ep_lib::MPI_Datatype MPI_INT = MPI_INT_STD;
//ep_lib::MPI_Datatype MPI_FLOAT = MPI_FLOAT_STD;
//ep_lib::MPI_Datatype MPI_DOUBLE = MPI_DOUBLE_STD;
//ep_lib::MPI_Datatype MPI_LONG = MPI_LONG_STD;
//ep_lib::MPI_Datatype MPI_CHAR = MPI_CHAR_STD;
//ep_lib::MPI_Datatype MPI_UNSIGNED_LONG = MPI_UNSIGNED_LONG_STD;




::MPI_Op MPI_SUM_STD = MPI_SUM;
::MPI_Op MPI_MAX_STD = MPI_MAX;
::MPI_Op MPI_MIN_STD = MPI_MIN;

#undef MPI_SUM
#undef MPI_MAX
#undef MPI_MIN

//ep_lib::MPI_Op MPI_SUM = MPI_SUM_STD;
//ep_lib::MPI_Op MPI_MAX = MPI_MAX_STD;
//ep_lib::MPI_Op MPI_MIN = MPI_MIN_STD;

//  ep_lib::MPI_Comm::MPI_Comm(const MPI_Comm & comm)
//  {
//    printf("calling MPI_Comm copy constructor\n");
//    is_ep = comm.is_ep;
//    is_intercomm = comm.is_intercomm;

//    int my_rank = comm.ep_comm_ptr->size_rank_info[1].first;
//    int num_ep  = comm.ep_comm_ptr->size_rank_info[1].second;


//    if(0 == my_rank)
//    {
//      MPI_Info info;
//      MPI_Comm *out_comm;
//      ::MPI_Comm mpi_dup;

//      ::MPI_Comm in_comm = static_cast< ::MPI_Comm>(comm.mpi_comm);

//      ::MPI_Comm_dup(in_comm, &mpi_dup);

//      MPI_Comm_create_endpoints(mpi_dup, num_ep, info, out_comm);
//      comm.ep_comm_ptr->comm_list->mem_bridge = out_comm;
//    }

//    MPI_Barrier(comm);

//    *this = (comm.ep_comm_ptr->comm_list->mem_bridge[my_rank]);
// //       // my_buffer = NULL;
// //       // ep_barrier = NULL;
// //       // rank_map = NULL;
// //       // ep_comm_ptr = NULL;
// //       // mem_bridge = NULL;
// //       // mpi_bridge = NULL;
// //       // mpi_comm = comm;
// }










