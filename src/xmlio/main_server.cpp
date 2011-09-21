// XmlIOServer
#include "xmlioserver.hpp"

#include "attribute_template_impl.hpp"
#include "group_template_impl.hpp"

#include "client.hpp"
#include "server.hpp"
extern "C"
{
   void xios_dtreatment_start(tree::CContext * context_hdl, int filetype, int comm_client_server, int comm_server);
   void xios_set_timestep(double ts_year, double ts_month,  double ts_day,
                          double ts_hour, double ts_minute, double ts_second);
   void xios_write_data_k83(const char * fieldid, int fieldid_size, double * data_k8, int data_Xsize, int data_Ysize, int data_Zsize);
   void xios_update_calendar(int step);
}
// Point d'entrée du programme principal

int main (int argc, char ** argv, char ** UNUSED (env))
{
   try
   {
      MPIComm comm_client, comm_client_server, comm_server;
      CXIOSManager::Initialise (CXIOSManager::CLIENT_SERVER, &argc, &argv);
      CMPIManager::DispatchClient(true, comm_client, comm_client_server,  comm_server);
      CXIOSManager::RunServer("Nemo", comm_client_server, comm_server);
      CXIOSManager::Finalize ();         
   }
   catch (CException & exc)
   {
      std::cerr << exc.getMessage () << std::endl;
      CMPIManager::Finalize ();
      return (EXIT_FAILURE);
   }
   return (EXIT_SUCCESS);
}

