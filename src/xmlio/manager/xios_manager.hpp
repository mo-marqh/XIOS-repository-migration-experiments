#ifndef __XMLIO_CXMLIOManager__
#define __XMLIO_CXMLIOManager__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "mpi_manager.hpp"

namespace xmlioserver
{
   /// ////////////////////// Déclarations ////////////////////// ///
   class CXIOSManager
   {
      public :

         /// Définition de type ///
         typedef enum _xios_type
         { CLIENT_SERVER = 0 , CLIENT, SERVER } XIOSType;
         
         typedef enum _xios_status
         { LOC_UNUSED = 0, LOC_CLIENT, LOC_SERVER, LOC_CLIENT_SERVER }  XIOSStatus;

         typedef struct
         {
            StdSize nbClient;
            StdSize nbClientPServer;
            void (*entry)(comm::MPIComm, comm::MPIComm, comm::MPIComm) ;
         } XIOSClient;
         
      public:

         /// Initialisation et finalisation de la bibliothèque ///
         static void Initialise(XIOSType type = CLIENT,
                                int    * argc = NULL  ,
                                char *** argv = NULL  );
         static void Finalize(void);

         /// Ajout de client en mode CLIENT_SERVER et CLIENT///
         static void AddClient(StdString clientName, StdSize nbClient, StdSize nbClientPServer,
                               void (*entry_point)(comm::MPIComm, comm::MPIComm, comm::MPIComm));

         /// Lancement des serveurs et/ou des clients ///
         static void RunServer(StdString clientName,
                               comm::MPIComm comm_client_server, comm::MPIComm comm_server);
         static void RunClient(bool launch = false, comm::MPIComm comm_client =
                                                    comm::CMPIManager::GetCommWorld());
         static void RunClientServer(comm::MPIComm comm_client_server =
                                     comm::CMPIManager::GetCommWorld());

      public :

         /// Accesseurs statiques ///
         static XIOSType   GetType(void);
         static XIOSStatus GetStatus(void);
         static StdString  GetClientName(void);
         
      public :
         
         static StdSize GetNbClient(void);
         static StdSize GetNbLocClient(const StdString & clientName = CXIOSManager::ClientName);

         static StdSize GetNbServer(void);
         static StdSize GetNbLocServer(const StdString & clientName = CXIOSManager::ClientName);
         
      public :
      
         /// Information ///
         static void ShowInformation_CS(comm::MPIComm comm_client_server);

         /// Variables statiques privées ///
         static StdString ExeName;
         static std::vector<StdString> ExeOptions;

         static XIOSType   Type;
         static XIOSStatus Status;
         static StdString  ClientName;
         static comm::MPIComm Comm_Client_Server, Comm_Server;
         static xios_map<StdString, XIOSClient> Clients;

   }; // class CXIOSManager
} // namespace xmlioserver

#endif //__XMLIO_CXMLIOManager__
