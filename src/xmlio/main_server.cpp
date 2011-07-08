// XmlIOServer
#include "xmlioserver.hpp"

#include "attribute_template_impl.hpp"
#include "group_template_impl.hpp"

#include "client.hpp"
#include "server.hpp"

#include "fake.hpp"

// Point d'entrée du programme principal

int main (int argc, char ** argv, char ** UNUSED (env))
{
   try
   {
      MPIComm comm_client, comm_client_server, comm_server;
      //comm::CMPIManager::Initialise(&argc, &argv); // < seulement en mode connecté
      //comm::CMPIManager::Finalize();               // < seulement en mode connecté

      CXIOSManager::Initialise (CXIOSManager::CLIENT_SERVER, &argc, &argv);
      //CXIOSManager::AddClient ("nemo"    , 4, 2, &nemo_fake_entry);
      //CXIOSManager::AddClient("orchidee", 1, 1, &orchidee_fake_entry);
      //CXIOSManager::AddClient("lmdz"    , 4, 2, &lmdz_fake_entry);
      CMPIManager::DispatchClient(true, comm_client, comm_client_server,  comm_server);
      CMPIManager::RunServer("Nemo", comm_client_server, comm_server);
      //CXIOSManager::RunClientServer (comm::CMPIManager::GetCommWorld ());
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

