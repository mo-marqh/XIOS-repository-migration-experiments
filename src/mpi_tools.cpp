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

}