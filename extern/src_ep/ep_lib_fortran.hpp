#ifndef EP_LIB_FORTRAN_HPP_INCLUDED
#define EP_LIB_FORTRAN_HPP_INCLUDED

#include "ep_type.hpp"

namespace ep_lib
{
  

  MPI_Fint MPI_Comm_c2f(MPI_Comm comm);
  //int MPI_Comm_c2f(MPI_Comm comm);


  MPI_Comm MPI_Comm_f2c(MPI_Fint comm);
  //void MPI_Comm_f2c(MPI_Fint comm);

}


#endif // EP_LIB_FORTRAN_HPP_INCLUDED
