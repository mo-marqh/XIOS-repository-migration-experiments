#ifndef EP_DECLARATION_HPP_INCLUDED
#define EP_DECLARATION_HPP_INCLUDED

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

extern ep_lib::MPI_Datatype MPI_INT;
extern ep_lib::MPI_Datatype MPI_FLOAT;
extern ep_lib::MPI_Datatype MPI_DOUBLE;
extern ep_lib::MPI_Datatype MPI_CHAR;
extern ep_lib::MPI_Datatype MPI_LONG;
extern ep_lib::MPI_Datatype MPI_UNSIGNED_LONG;
extern ep_lib::MPI_Datatype MPI_UNSIGNED_CHAR;

extern ep_lib::MPI_Op MPI_SUM;
extern ep_lib::MPI_Op MPI_MAX;
extern ep_lib::MPI_Op MPI_MIN;

extern ep_lib::MPI_Comm MPI_COMM_WORLD;
extern ep_lib::MPI_Comm MPI_COMM_NULL;




#endif // EP_DECLARATION_HPP_INCLUDED

