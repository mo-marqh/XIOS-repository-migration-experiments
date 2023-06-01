#include "mpi.hpp"
#include "mpi_tools.hpp"


#include <string>

namespace xios  
{
  int MPI_Bcast_string(std::string& str, int root, MPI_Comm comm) 
  {
    int commRank ;
    int ret ;
    MPI_Comm_rank(comm,&commRank) ;
    int strSize ;
    if (commRank==root) strSize=str.size() ;
    MPI_Bcast(&strSize,1,MPI_INT,root,comm) ;
  
    if (commRank==root) ret=MPI_Bcast((char*)str.data(), strSize, MPI_CHAR, root, comm) ;
    else
    {
      char* tmp=new char[strSize] ;
      ret=MPI_Bcast(tmp, strSize, MPI_CHAR, root, comm) ;
      str=std::string(tmp,strSize) ;
      delete [] tmp ;
    }
    return ret ;
  }

  template<>
  MPI_Datatype MPI_GetType<bool>(void) { return MPI_CXX_BOOL ;}

  template<>
  MPI_Datatype MPI_GetType<char>(void) { return MPI_CHAR ;}

  template<>
  MPI_Datatype MPI_GetType<short int>(void) { return MPI_SHORT ;}

  template<>
  MPI_Datatype MPI_GetType<int>(void) { return MPI_INT ;}

  template<>
  MPI_Datatype MPI_GetType<size_t>(void) { return MPI_SIZE_T ;}

  template<>
  MPI_Datatype MPI_GetType<float>(void) { return MPI_FLOAT ;}

  template<>
  MPI_Datatype MPI_GetType<double>(void) { return MPI_DOUBLE ;}

  template<>
  MPI_Datatype MPI_GetType<long double>(void) { return MPI_LONG_DOUBLE ;}

}
