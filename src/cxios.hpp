#ifndef __XIOS_HPP__
#define __XIOS_HPP__

#include "xios_spl.hpp"
#include "mpi.hpp"
#include "registry.hpp"
#include "ressources_manager.hpp"
#include "services_manager.hpp"
#include "contexts_manager.hpp"
#include "daemons_manager.hpp"
#include "coupler_manager.hpp"
#include "registry_manager.hpp"
#include "thread_manager.hpp"
#include "mpi_garbage_collector.hpp"

namespace xios
{
  /*!
  \class CXios
  */
  class CXios
  {
    public:
     static void initialize(void) ;
     static void initClientSide(const string & codeId, MPI_Comm& localComm, MPI_Comm& returnComm) ;
     static void initServerSide(void) ;
     static void clientFinalize(void) ;
     static void parseFile(const string& filename) ;

     template <typename T>
     static T getin(const string& id,const T& defaultValue) ;

     template <typename T>
     static T getin(const string& id) ;

    public:
     static string rootFile ; //!< Configuration filename
     static string xiosCodeId ; //!< Identity for XIOS
     static string clientFile;        //!< Filename template for client
     static string serverFile;        //!< Filename template for server
     static string serverPrmFile;  //!< Filename template for primary server in case of two server levels
     static string serverSndFile;  //!< Filename template for secondary server in case of two server levels

     static bool xiosStack;    //!< Exception handling
     static bool systemStack;  //!< Exception handling

     static bool isClient ; //!< Check if xios is client
     static bool isServer ; //!< Check if xios is server

     static MPI_Comm globalComm ; //!< Global communicator
     static MPI_Comm xiosComm ; //!< Global communicator

     static bool printLogs2Files; //!< Printing out logs into files
     static bool usingOasis ;     //!< Using Oasis
     static bool usingServer ;    //!< Using server (server mode)
     static bool usingServer2 ;   //!< Using secondary server (server mode). IMPORTANT: Use this variable ONLY in CServer::initialize().
     static int ratioServer2 ;    //!< Percentage of server processors dedicated to secondary server
     static int nbPoolsServer2 ;  //!< Number of pools created on the secondary server
     static double bufferSizeFactor; //!< Factor used to tune the buffer size
     static const double defaultBufferSizeFactor; //!< Default factor value
     static StdSize minBufferSize; //!< Minimum buffer size
     static StdSize maxBufferSize; //!< Maximum buffer size
     static bool isOptPerformance; //!< Check if buffer size is for performance (as large as possible)
     static CRegistry* globalRegistry ; //!< global registry which is wrote by the root process of the servers
     static double recvFieldTimeout; //!< Time to wait for data before issuing an error when receiving a field
     static bool checkEventSync; //!< For debuuging, check if event are coherent and synchrone on client side

     static bool checkSumSend; //!< For debugging, compute a checksum of fields sent by the model to the XIOS client (very expensive !)
     static bool checkSumRecv; //!< For debugging, compute a checksum of fields received by the model through the XIOS client

     static bool logMemory; //!< Activate memory monitoring for all XIOS process (generate CSV file for https://forge.ipsl.jussieu.fr/ioserver/chrome/site/XIOS_TOOLS/xios_memory.html)
     static bool reportMemory; //!< Activate memory reporting for all XIOS process (report in log files)

     static const string defaultPoolId ;
     static const string defaultServerId ;
     static const string defaultGathererId ;
     static const string defaultWriterId ;
     static const string defaultReaderId ;
     static const string defaultServicesId ;

     static CRegistryManager* registryManager_ ;
     static CRessourcesManager* ressourcesManager_ ;
     static CCouplerManager* couplerManager_ ;
     static CServicesManager* servicesManager_ ;
     static CContextsManager* contextsManager_ ;
     static CDaemonsManager* daemonsManager_ ;
     static CMpiGarbageCollector MpiGarbageCollector_  ;
    public:
     static CMpiGarbageCollector& getMpiGarbageCollector(void) { return MpiGarbageCollector_ ; }
    public:
     //! Setting xios to use server mode
     static void setUsingServer();

     //! Setting xios NOT to use server mode
     static void setNotUsingServer();
     
     //! is using server mode
     static bool isUsingServer() {return usingServer;}

     //! Initialize server (if any)
     static  void initServer();

     static void launchServicesManager( bool isXiosServer) ;
     static void launchContextsManager(bool isXiosServer) ;
     static void launchDaemonsManager(bool isXiosServer) ;
     static void launchRessourcesManager(bool isXiosServer) ;
     static void launchCouplerManager(bool isXiosServer) ;
     static void launchRegistryManager(bool isXiosServer) ;
     static void launchThreadManager(bool isXiosServer) ;
    
     static void finalizeServicesManager() ;
     static void finalizeContextsManager() ;
     static void finalizeDaemonsManager() ;
     static void finalizeRessourcesManager() ;
     static void finalizeCouplerManager() ;
     static void finalizeRegistryManager() ;
     static void finalizeThreadManager() ;

     static CRegistryManager*   getRegistryManager(void) { return registryManager_ ;}
     static CRessourcesManager* getRessourcesManager(void) { return ressourcesManager_ ;}
     static CCouplerManager*    getCouplerManager(void) { return couplerManager_ ;}
     static CServicesManager*   getServicesManager(void) { return servicesManager_ ;}
     static CContextsManager*   getContextsManager(void) { return contextsManager_ ;}
     static CDaemonsManager*    getDaemonsManager(void) { return daemonsManager_ ;}
     static CPoolRessource*     getPoolRessource(void) ;
     
     static MPI_Comm getGlobalComm(void) { return globalComm ;}
     static MPI_Comm getXiosComm(void) { return xiosComm ;}
     static void setXiosComm(MPI_Comm comm) { xiosComm=comm ;}
     static CRegistry* getGlobalRegistry(void) { return globalRegistry ;}
     static void setGlobalRegistry(CRegistry* registry) { globalRegistry=registry ;}

    private:
      //! Parse only Xios part of configuration file
      static void parseXiosConfig();
  } ;
}

//#include "cxios_impl.hpp"
#endif // __XIOS_HPP__
