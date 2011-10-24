// XmlIOServer
#include "xmlioserver.hpp"

#include "attribute_template_impl.hpp"
#include "group_template_impl.hpp"

#include "client.hpp"
#include "server.hpp"
#include "oasis_cinterface.hpp"
#include <string>
#include <iostream>

#include <unistd.h>


// Point d'entrée du programme principal

int main (int argc, char ** argv, char ** UNUSED (env))
{
   try
   {
      MPI_Comm comm_client=MPI_COMM_NULL, comm_client_server=MPI_COMM_NULL, comm_server=MPI_COMM_NULL, comm_parent=MPI_COMM_NULL;
//      sleep(60) ;
      CTreeManager::ParseFile("iodef.xml");

      CTreeManager::SetCurrentContextId(StdString("xios"));
      boost::shared_ptr<CVariable> var = CObjectFactory::GetObject<CVariable>("buffer_client_size"); 
      std::cout<<CObjectFactory::GetObject<CVariable>("xios","using_oasis")->getData<bool>()<<std::endl;
     
//      oasis_init(std::string("ionemo")) ;
//      std::cout<<"Initialisation OASIS Ok"<<std::endl ;
//      oasis_get_intracomm(comm_parent,std::string("oceanx")) ;
//      std::cout<<"Appel Oasis intracom Ok"<<std::endl ;
      CXIOSManager::Initialise(CXIOSManager::CLIENT_SERVER, &argc, &argv);
     
      std::cout<<"Appel Dispatch client ?"<<std::endl ;
      std::cout<<comm_client<<" "<<comm_client_server<<" "<<comm_server<<"  "<<comm_parent<<std::endl ;
      CMPIManager::DispatchClient(true, comm_client, comm_client_server, comm_server, comm_parent);
      std::cout<<"Appel Dispatch client Ok"<<std::endl ;
      CXIOSManager::RunServer("Nemo", comm_client_server, comm_server);
      CTreeManager::SetCurrentContextId(StdString("nemo"));
      CServer::CreateServer(comm_client_server)->run();
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

