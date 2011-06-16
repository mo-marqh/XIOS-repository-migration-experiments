#include "xios_manager.hpp"

#include "tree_manager.hpp"
#include "data_treatment.hpp"
#include "nc4_data_output.hpp"
#include "attribute_template_impl.hpp"

namespace xmlioserver
{
      /// ////////////////////// Définitions ////////////////////// ///
      
      void CXIOSManager::Initialise(XIOSType type, int * argc, char *** argv)
      {
         CXIOSManager::Type = type;
         if (type != CLIENT)
         {
            // Initialisation de la biliothèque MPI si nécessaire
            comm::CMPIManager::Initialise(argc, argv);
            ExeName = StdString((*argv)[0]);
            for (int i = 1; i < *argc; i++)
               ExeOptions.push_back(StdString((*argv)[i]));
         }
      }

      void CXIOSManager::Finalize(void)
      {
         if (CXIOSManager::Type != CLIENT)
         {
            // Finalisation de la biliothèque MPI si nécessaire
            comm::CMPIManager::Finalize();
         }
      }

      ///-------------------------------------------------------------

      StdString CXIOSManager::ExeName("unknown");
      std::vector<StdString> CXIOSManager::ExeOptions;

      CXIOSManager::XIOSType   CXIOSManager::Type    = CLIENT;
      CXIOSManager::XIOSStatus CXIOSManager::Status  = LOC_UNUSED;

      StdString     CXIOSManager::ClientName("unknown name");
      comm::MPIComm CXIOSManager::Comm_Client_Server = -1;
      comm::MPIComm CXIOSManager::Comm_Server = -1;

      xios_map<StdString, CXIOSManager::XIOSClient> CXIOSManager::Clients;

      ///--------------------------------------------------------------
      
      void CXIOSManager::RunServer
         (StdString clientName, comm::MPIComm comm_client_server, comm::MPIComm comm_server)
      {
         using namespace comm;
         
         // Reconstruction de l'arborescence d'objet à l'aide des données envoyées par chacun des
         // clients associés à ce serveur.
         std::vector<boost::shared_ptr<CLinearBuffer> > clientBuffer;
         for (int i = 1; i < CMPIManager::GetCommSize(comm_client_server); i++)
         {
            while (!CMPIManager::HasReceivedData(comm_client_server, i)){}
            clientBuffer.push_back(CMPIManager::ReceiveLinearBuffer(comm_client_server, i));
         }
         
         // La quasi-totalité de l'arborescence est obtenue depuis les informations
         // fournies par le client 1 du sous-groupe.
         StdString main_data_tree = clientBuffer[0]->getString(0);        
         tree::CTreeManager::FromBinary(main_data_tree);
         
         // Obtention des sous-domaines clients.
         for (StdSize j = 1; j < clientBuffer.size(); j++)
         {
            main_data_tree = clientBuffer[j]->getString(0);
            tree::CTreeManager::DomainsFromBinary(main_data_tree);
         }

         {  // Traitement de tous les contextes
            StdString currentContextId = CObjectFactory::GetCurrentContextId();
            std::vector<boost::shared_ptr<CContext> > def_vector =
                                   CContext::GetContextGroup()->getChildList();
            std::vector<boost::shared_ptr<CContext> >::iterator
                               it = def_vector.begin(), end = def_vector.end();

            for (; it != end; it++)
            {
               boost::shared_ptr<CContext> context = *it;
               boost::shared_ptr<data::CDataTreatment> dt(new data::CDataTreatment (context));
               context->setDataTreatment(dt);
               dt->createDataOutput<io::CNc4DataOutput>();
            }
            CTreeManager::SetCurrentContextId(currentContextId);
         }
        
         StdOStringStream oss;
         oss << StdString("data/def/def_server_end.")
             << CMPIManager::GetCommRank(CMPIManager::GetCommWorld());
         CTreeManager::PrintTreeToFile(oss.str());      
 
      }
      
      //--------------------------------------------------------------
      
      void CXIOSManager::ShowInformation_CS(comm::MPIComm comm_client_server)
      {
         using namespace comm;
         typedef std::pair<StdString, XIOSClient> StdPairStrClient;
         if (CMPIManager::IsMaster(comm_client_server))
         {
            std::cout << " *************************************** " << std::endl
                      << " *     XmlIOServer (Client/Server)     * " << std::endl
                      << " *************************************** " << std::endl;
            std::cout << " > Nombre de processus : "
                      << CMPIManager::GetCommSize(comm_client_server) << std::endl;
            std::cout << " > Nombre de processus utilisés : "
                      << (CXIOSManager::GetNbClient() + CXIOSManager::GetNbServer()) << std::endl;
            std::cout << " > Nombre de clients : "
                      << CXIOSManager::GetNbClient() << std::endl;
            std::cout << " > Nombre de serveurs : "
                      << CXIOSManager::GetNbServer() << std::endl;

            xios_map<StdString, XIOSClient>::iterator
               iit  = CXIOSManager::Clients.begin(),
               eend = CXIOSManager::Clients.end();

            for (;iit != eend; iit++)
            {
               const StdPairStrClient & elem = *iit;
               std::cout << " - " << elem.first
                         << " > nombre de clients : "             
                         << elem.second.nbClient
                         << " , nombre de clients par serveur : " 
                         << elem.second.nbClientPServer
                         << " , nombre de serveurs : "            
                         << elem.second.nbClient/elem.second.nbClientPServer
                         << std::endl;
            }

            std::cout << " *************************************** " << std::endl;
         }

         comm::CMPIManager::Barrier();
         
      }
      
      //--------------------------------------------------------------
      
      void CXIOSManager::RunClientServer(comm::MPIComm comm_client_server)
      {
         using namespace comm;
         typedef std::pair<StdString, XIOSClient> StdPairStrClient;
         
         CXIOSManager::ShowInformation_CS(comm_client_server);
         
         xios_map<StdString, XIOSClient>::iterator
               iit  = CXIOSManager::Clients.begin(),
               eend = CXIOSManager::Clients.end();
         
         StdSize start = 0, end   = 0;
         
         bool isClient = true, isIncl = false, isIncl_ = false;
         MPIComm comm_client = 0, comm_client_grp = 0; comm_client_server = 0;

         for (;iit != eend; iit++)
         {
            const StdPairStrClient & elem = *iit;

            std::vector<int> clieindex, servindex ;
            StdSize   currentRank      = CMPIManager::GetCommRank();
            StdString clientName       = elem.first;
            StdSize   nbClient         = elem.second.nbClient;
            StdSize   nbClientPServer  = elem.second.nbClientPServer;
            StdSize   nbServer         = (elem.second.nbClient)/(elem.second.nbClientPServer);

            for (StdSize i = 0; i<nbServer; i++)
            {
               end = start + nbClientPServer;
               MPIComm comm_  =  CMPIManager::CreateComm
                  (CMPIManager::CreateSubGroup(CMPIManager::GetGroupWorld(), start, end));
               MPIComm comm__ =  CMPIManager::CreateComm
                  (CMPIManager::CreateSubGroup(CMPIManager::GetGroupWorld(), start+1, end));
                  
               servindex.push_back(start);
               for (StdSize j = start+1; j <= end; j++)
                  clieindex.push_back(j);
                                
               if ((currentRank >= start) && (currentRank <= end))
               {
                  comm_client_server = comm_;
                  comm_client_grp    = comm__;
                  isIncl = true;
                  CXIOSManager::ClientName = clientName;
                  CXIOSManager::Status = LOC_CLIENT;
               }
               if (currentRank == start)
               {
                  isClient = false; isIncl_  = true;
                  CXIOSManager::Comm_Client_Server = comm_;
                  CXIOSManager::Status = LOC_SERVER;
               }               
               if (clieindex.size() == nbClient)
               {
                  MPIComm comm___ = CMPIManager::CreateComm
                  (CMPIManager::CreateSubGroup(CMPIManager::GetGroupWorld(), clieindex));
                  if (isIncl) comm_client = comm___;
                  clieindex.clear();
                  isIncl = false;
               }
               
               start = start + nbClientPServer + 1;
            }
            MPIComm comm____ = CMPIManager::CreateComm
               (CMPIManager::CreateSubGroup(CMPIManager::GetGroupWorld(), servindex));
            if (isIncl_) CXIOSManager::Comm_Server = comm____;               
            servindex.clear();
            isIncl_ = false;            
         }
         
         if (isClient && (CXIOSManager::Clients.find(CXIOSManager::ClientName) !=
                          CXIOSManager::Clients.end()))
         {
            CXIOSManager::Clients[CXIOSManager::ClientName].entry
               (comm_client, comm_client_grp, comm_client_server);
         }
         else if(CXIOSManager::Clients.find(CXIOSManager::ClientName) !=
                 CXIOSManager::Clients.end())
         {
            CXIOSManager::RunServer(CXIOSManager::ClientName, 
                                    CXIOSManager::Comm_Client_Server,
                                    CXIOSManager::Comm_Server);
         }
      }
      
      //---------------------------------------------------------------
      
      void CXIOSManager::RunClient(comm::MPIComm comm_client)
      {
         CXIOSManager::Status  = LOC_CLIENT_SERVER;
         (CXIOSManager::Clients.begin()->second.entry)(comm_client, comm_client, comm_client);
      }

      //---------------------------------------------------------------

      void CXIOSManager::AddClient(StdString clientName, StdSize nbClient, StdSize nbClientPServer,
                                   void (*entry_point)(comm::MPIComm, comm::MPIComm, comm::MPIComm))
      {
         StdSize nbprocess  = comm::CMPIManager::GetCommSize(comm::CMPIManager::GetCommWorld());
         StdSize nbprocess_used = CXIOSManager::GetNbClient() + CXIOSManager::GetNbServer();

         if (nbClient < nbClientPServer)
            ERROR("CXIOSManager::AddClient(...)", 
                  << "nbClient < nbClientPServer");
                  
         if ((nbClient % nbClientPServer) != 0)
            ERROR("CXIOSManager::AddClient(...)",
                  << " (nbClient % nbClientPServer) != 0 !");
                  
         if ((nbprocess-nbprocess_used) < (nbClient + nbClient/nbClientPServer))
            ERROR("CXIOSManager::AddClient(...)",
                  << " Pas assez de processus disponibles !");
                  
         if (CXIOSManager::Clients.find(clientName) != CXIOSManager::Clients.end())
            ERROR("CXIOSManager::AddClient(...)",
                  << " Un client portant le même nom existe déjà !");

         XIOSClient client = {nbClient, nbClientPServer, entry_point };
         CXIOSManager::Clients[clientName] = client;
      }

      //---------------------------------------------------------------

      StdSize CXIOSManager::GetNbClient(void)
      {
         switch (CXIOSManager::Type)
         {
            case (CLIENT_SERVER):
            {
               StdSize retvalue = 0;
               typedef std::pair<StdString, XIOSClient> StdPairStrClient;
               xios_map<StdString, XIOSClient>::iterator
                  iit  = CXIOSManager::Clients.begin(),
                  eend = CXIOSManager::Clients.end();

               for (;iit != eend; iit++)
               {
                  const StdPairStrClient & elem = *iit;
                  retvalue += CXIOSManager::GetNbLocClient(elem.first);
               }
               
               return (retvalue);
            }
            case (CLIENT):
               return (comm::CMPIManager::GetCommSize(CXIOSManager::Comm_Server));
            case (SERVER):
               return (comm::CMPIManager::GetCommSize(CXIOSManager::Comm_Client_Server) - 1);
            default:
               return (0);
         }
      }
      
      //---------------------------------------------------------------

      StdSize CXIOSManager::GetNbLocClient(const StdString & clientName)
      {
         switch (CXIOSManager::Type)
         {
            case (CLIENT_SERVER):
               return (CXIOSManager::Clients[clientName].nbClient);
            case (CLIENT):
            case (SERVER):
               return (CXIOSManager::GetNbClient());
            default:
               return (0);
         }
      }

      //---------------------------------------------------------------

      StdSize CXIOSManager::GetNbServer(void)
      {
         switch (CXIOSManager::Type)
         {
            case (CLIENT_SERVER):
            {
               StdSize retvalue = 0;
               typedef std::pair<StdString, XIOSClient> StdPairStrClient;              

               xios_map<StdString, XIOSClient>::iterator
                  iit  = CXIOSManager::Clients.begin(),
                  eend = CXIOSManager::Clients.end();

               for (;iit != eend; iit++)
               {
                  const StdPairStrClient & elem = *iit;
                  retvalue += CXIOSManager::GetNbLocServer(elem.first);
               }                  
                  
               return (retvalue);
            }
            case (CLIENT):
               return (comm::CMPIManager::GetCommSize(CXIOSManager::Comm_Server));
            case (SERVER):
               return (1);
            default:
               return (0);
         }
      }

      //---------------------------------------------------------------

      StdSize CXIOSManager::GetNbLocServer(const StdString & clientName)
      {
         switch (CXIOSManager::Type)
         {
            case (CLIENT_SERVER):
               return (CXIOSManager::Clients[clientName].nbClient /
                       CXIOSManager::Clients[clientName].nbClientPServer);
            case (CLIENT):
            case (SERVER):
               return (CXIOSManager::GetNbServer());
            default:
               return (0);
         }
      }
      
      //---------------------------------------------------------------
      
      CXIOSManager::XIOSType   CXIOSManager::GetType(void)
      {
         return (CXIOSManager::Type);
      }
      
      //---------------------------------------------------------------
      
      CXIOSManager::XIOSStatus CXIOSManager::GetStatus(void)
      {
         return (CXIOSManager::Status);
      }
      
      //---------------------------------------------------------------
      
      StdString  CXIOSManager::GetClientName(void)
      {
         return (CXIOSManager::ClientName);
      }

      ///--------------------------------------------------------------

} // namespace xmlioserver
