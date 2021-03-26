#ifndef __XIOS_MPI_TOOLS_HPP__
#define __XIOS_MPI_TOOLS_HPP__

#include <string>

namespace xios
{

  int MPI_Bcast_string(std::string& str, int root, MPI_Comm comm) ;

  template<typename T> 
  MPI_Datatype MPI_GetType(void) ;

  template<>
  MPI_Datatype MPI_GetType<char>(void);

  template<>
  MPI_Datatype MPI_GetType<short int>(void);

  template<>
  MPI_Datatype MPI_GetType<int>(void);

  template<>
  MPI_Datatype MPI_GetType<size_t>(void);

  template<>
  MPI_Datatype MPI_GetType<float>(void);

  template<>
  MPI_Datatype MPI_GetType<double>(void);

  template<>
  MPI_Datatype MPI_GetType<long double>(void);


} 
#endif
