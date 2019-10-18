#ifndef __XIOS_MPI_TOOLS_HPP__

#include <string>

namespace xios
{

  int MPI_Bcast_string(std::string& str, int root, MPI_Comm comm) ;
} 
#endif