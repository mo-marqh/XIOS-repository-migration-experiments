

#include "ep_lib.hpp"

#include <mpi.h>
//#include "ep_declaration.hpp"

#undef MPI_INT
#undef MPI_FLOAT
#undef MPI_DOUBLE
#undef MPI_CHAR
#undef MPI_LONG
#undef MPI_UNSIGNED_LONG
#undef MPI_UNSIGNED_CHAR

#undef MPI_SUM
#undef MPI_MAX
#undef MPI_MIN

#undef MPI_COMM_WORLD
#undef MPI_COMM_NULL

// _STD defined in ep_type.cpp

extern ::MPI_Datatype MPI_INT_STD;
extern ::MPI_Datatype MPI_FLOAT_STD;
extern ::MPI_Datatype MPI_DOUBLE_STD;
extern ::MPI_Datatype MPI_LONG_STD;
extern ::MPI_Datatype MPI_CHAR_STD;
extern ::MPI_Datatype MPI_UNSIGNED_LONG_STD;
extern ::MPI_Datatype MPI_UNSIGNED_CHAR_STD;

extern ::MPI_Op MPI_SUM_STD;
extern ::MPI_Op MPI_MAX_STD;
extern ::MPI_Op MPI_MIN_STD;

extern ::MPI_Comm MPI_COMM_WORLD_STD;
extern ::MPI_Comm MPI_COMM_NULL_STD;

ep_lib::MPI_Datatype MPI_INT = MPI_INT_STD;
ep_lib::MPI_Datatype MPI_FLOAT = MPI_FLOAT_STD;
ep_lib::MPI_Datatype MPI_DOUBLE = MPI_DOUBLE_STD;
ep_lib::MPI_Datatype MPI_CHAR = MPI_CHAR_STD;
ep_lib::MPI_Datatype MPI_LONG = MPI_LONG_STD;
ep_lib::MPI_Datatype MPI_UNSIGNED_LONG = MPI_UNSIGNED_LONG_STD;
ep_lib::MPI_Datatype MPI_UNSIGNED_CHAR = MPI_UNSIGNED_CHAR_STD;

ep_lib::MPI_Op MPI_SUM = MPI_SUM_STD;
ep_lib::MPI_Op MPI_MAX = MPI_MAX_STD;
ep_lib::MPI_Op MPI_MIN = MPI_MIN_STD;

ep_lib::MPI_Comm MPI_COMM_WORLD(MPI_COMM_WORLD_STD);
ep_lib::MPI_Comm MPI_COMM_NULL(MPI_COMM_NULL_STD);

//ep_lib::MPI_Comm EP_COMM_WORLD;
//ep_lib::MPI_Comm EP_COMM_NULL;



