#include "oasis_cinterface.hpp"
#include <string>
#include <mpi.h>

namespace xmlioserver
{ 
 
  void oasis_init(const std::string& server_id)
  {
    fxios_oasis_init(server_id.data(),server_id.size()) ;
  }
  
  void oasis_finalize(void)
  {
    fxios_oasis_finalize() ;
  }
  

  void oasis_get_intracomm(MPI_Comm& comm_client_server,const std::string& server_id)
  {
    MPI_Fint f_comm ;
    
    fxios_oasis_get_intracomm(&f_comm,server_id.data(),server_id.size()) ;
    comm_client_server=MPI_Comm_f2c(f_comm) ;
  }

}
