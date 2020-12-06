#ifndef __XIOS_MPI_TOOLS_HPP__
#define __XIOS_MPI_TOOLS_HPP__

#include <string>

namespace xios
{

  int MPI_Bcast_string(std::string& str, int root, MPI_Comm comm) ;

  template<typename T> 
  MPI_Datatype MPI_GetType(void) ;

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
#endif