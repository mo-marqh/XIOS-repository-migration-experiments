
#include "xios_spl.hpp"
#include "cxios.hpp"
#include "client.hpp"
#include "server.hpp"
#include "xml_parser.hpp"
#include <boost/functional/hash.hpp>
#include "mpi.hpp"
#include "memory.hpp"
#include <new>
#include "memtrack.hpp"
#include "registry.hpp"
#include "ressources_manager.hpp"
#include "services_manager.hpp"
#include "servers_ressource.hpp"
#include "mem_checker.hpp"

namespace xios
{
  string CXios::rootFile="./iodef.xml" ;
  string CXios::xiosCodeId="xios.x" ;
  string CXios::clientFile="./xios_client";
  string CXios::serverFile="./xios_server";
  string CXios::serverPrmFile="./xios_server1";
  string CXios::serverSndFile="./xios_server2";
  const string CXios::defaultPoolId="default_pool_id" ;
  const string CXios::defaultServerId="default_server_id" ;
  const string CXios::defaultWriterId="default_writer_id" ;
  const string CXios::defaultReaderId="default_reader_id" ;
  const string CXios::defaultGathererId="default_gatherer_id" ;
  const string CXios::defaultServicesId="default_services_id" ;
  
  bool CXios::xiosStack = true;
  bool CXios::systemStack = false;

  bool CXios::isClient ;
  bool CXios::isServer ;
  
  MPI_Comm CXios::globalComm ;
  MPI_Comm CXios::xiosComm ;

  bool CXios::usingOasis ;
  bool CXios::usingServer = false;
  bool CXios::usingServer2 = false;
  int CXios::ratioServer2 = 50;
  int CXios::nbPoolsServer2 = 1;
  double CXios::bufferSizeFactor = 1.0;
  const double CXios::defaultBufferSizeFactor = 1.0;
  StdSize CXios::minBufferSize = 64 * sizeof(double);
  StdSize CXios::maxBufferSize = std::numeric_limits<int>::max() ;
  bool CXios::printLogs2Files;
  bool CXios::isOptPerformance = true;
  CRegistry* CXios::globalRegistry = 0;
  double CXios::recvFieldTimeout = 300.0;
  bool CXios::checkEventSync=false ;
  bool CXios::checkSumRecv=false ;
  bool CXios::checkSumSend=false ;

  CDaemonsManager*    CXios::daemonsManager_=nullptr ;
  CRessourcesManager* CXios::ressourcesManager_=nullptr ;
  CServicesManager*   CXios::servicesManager_=nullptr ;
  CContextsManager*   CXios::contextsManager_=nullptr ;
  CCouplerManager*    CXios::couplerManager_=nullptr ;
  CRegistryManager*   CXios::registryManager_=nullptr ;

  CMpiGarbageCollector CXios::MpiGarbageCollector_  ;

  //! Parse configuration file and create some objects from it
  void CXios::initialize()
  {
    set_new_handler(noMemory);
    parseFile(rootFile);
    parseXiosConfig();
  }

  /*!
  \brief Parse xios part of configuration file (.iodef.xml)
   Both client and server need information returned from this function
  */
  void CXios::parseXiosConfig()
  {
    usingOasis=getin<bool>("using_oasis",false) ;
    usingServer=getin<bool>("using_server",false) ;
    usingServer2=getin<bool>("using_server2",false) ;
    ratioServer2=getin<int>("ratio_server2",50);
    nbPoolsServer2=getin<int>("number_pools_server2",0);
    info.setLevel(getin<int>("info_level",0)) ;
    report.setLevel(getin<int>("info_level",50));
    printLogs2Files=getin<bool>("print_file",false);

    xiosStack=getin<bool>("xios_stack",true) ;
    systemStack=getin<bool>("system_stack",false) ;
    if (xiosStack && systemStack)
    {
      xiosStack = false;
    }

    StdString bufMemory("memory");
    StdString bufPerformance("performance");
    StdString bufOpt = getin<StdString>("optimal_buffer_size", bufPerformance);
    std::transform(bufOpt.begin(), bufOpt.end(), bufOpt.begin(), ::tolower);
    if (0 == bufOpt.compare(bufMemory)) isOptPerformance = false;
    else if (0 != bufOpt.compare(bufPerformance))
    {
      ERROR("CXios::parseXiosConfig()", << "optimal_buffer_size must be memory or performance "<< endl );
    }

    bufferSizeFactor = getin<double>("buffer_size_factor", defaultBufferSizeFactor);
    minBufferSize = getin<int>("min_buffer_size", 1024 * sizeof(double));
    maxBufferSize = getin<int>("max_buffer_size", std::numeric_limits<int>::max());
    recvFieldTimeout = getin<double>("recv_field_timeout", recvFieldTimeout);
    if (recvFieldTimeout < 0.0)
      ERROR("CXios::parseXiosConfig()", "recv_field_timeout cannot be negative.");

    checkEventSync = getin<bool>("check_event_sync", checkEventSync);
    
    checkSumSend = getin<bool>("checksum_send_fields", false);
    checkSumRecv = getin<bool>("checksum_recv_fields", false);

    globalComm=MPI_COMM_WORLD ;
  }

  /*!
  Initialize client
  \param [in] codeId identity of context
  \param [in] localComm local communicator
  \param [in/out] returnComm communicator corresponding to group of client with same codeId
  */
  void CXios::initClientSide(const string& codeId, MPI_Comm& localComm, MPI_Comm& returnComm)
  TRY
  {
    initialize() ;

    isClient = true;

    //CClient::initialize(codeId,localComm,returnComm) ;
    CClient::initialize(codeId,localComm,returnComm) ;
    
    // If there are no server processes then we are in attached mode
    // and the clients are also servers
    isServer = !usingServer;

    if (printLogs2Files)
    {
      CClient::openInfoStream(clientFile);
      CClient::openErrorStream(clientFile);
    }
    else
    {
      CClient::openInfoStream();
      CClient::openErrorStream();
    }
    CMemChecker::logMem("CXios::initClientSide");
  }
  CATCH

  void CXios::clientFinalize(void)
  {
     CMemChecker::logMem("CXios::clientFinalize", true);

     CClient::finalize() ;
          
#ifdef XIOS_MEMTRACK

#ifdef XIOS_MEMTRACK_LIGHT
       report(10) << " Memory report : current memory used by XIOS : "<<  MemTrack::getCurrentMemorySize()*1.0/(1024*1024)<<" Mbyte" << endl ;
       report(10) << " Memory report : maximum memory used by XIOS : "<<  MemTrack::getMaxMemorySize()*1.0/(1024*1024)<<" Mbyte" << endl ;
#endif

#ifdef XIOS_MEMTRACK_FULL
      report(0) << " Memory report : current memory used by XIOS : "<<  MemTrack::getCurrentMemorySize()*1.0/(1024*1024)<<" Mbyte" << endl ;
      report(0) << " Memory report : maximum memory used by XIOS : "<<  MemTrack::getMaxMemorySize()*1.0/(1024*1024)<<" Mbyte" << endl ;
      
      ofstream memReport ;
      std::filebuf* fb = memReport.rdbuf();
      CClient::openStream(clientFile, ".mem", fb);
      
      MemTrack::TrackListMemoryUsage() ;
      size_t memtrack_blocks=0 ;
      memtrack_blocks=xios::CXios::getin("memtrack_blocks",memtrack_blocks) ;
      size_t memtrack_size=0 ;
      memtrack_size=xios::CXios::getin("memtrack_size",memtrack_size) ;
      MemTrack::TrackDumpBlocks(memReport, memtrack_blocks,memtrack_size);
      memReport.close();
#endif

     CClient::closeInfoStream();

#endif
  }

  //! Init server by parsing only xios part of config file
  void CXios::initServer()
  {
    set_new_handler(noMemory);
    std::set<StdString> parseList;
    parseList.insert("xios");
    xml::CXMLParser::ParseFile(rootFile, parseList);
    parseXiosConfig();
  }

  //! Initialize server then put it into listening state
  void CXios::initServerSide(void)
  {
    CMemChecker::get("xios").resume() ;
    initServer();
    isClient = false;
    isServer = true;

    // Initialize all aspects MPI
    CServer::initialize();
    CServer::finalize();

#ifdef XIOS_MEMTRACK

#ifdef XIOS_MEMTRACK_LIGHT
       report(10) << " Memory report : current memory used by XIOS : "<<  MemTrack::getCurrentMemorySize()*1.0/(1024*1024)<<" Mbyte" << endl ;
       report(10) << " Memory report : maximum memory used by XIOS : "<<  MemTrack::getMaxMemorySize()*1.0/(1024*1024)<<" Mbyte" << endl ;
#endif

#ifdef XIOS_MEMTRACK_FULL
      report(0) << " Memory report : current memory used by XIOS : "<<  MemTrack::getCurrentMemorySize()*1.0/(1024*1024)<<" Mbyte" << endl ;
      report(0) << " Memory report : maximum memory used by XIOS : "<<  MemTrack::getMaxMemorySize()*1.0/(1024*1024)<<" Mbyte" << endl ;
      ofstream memReport ;
      std::filebuf* fb = memReport.rdbuf();
      CClient::openStream(serverFile, ".mem", fb);
      
      MemTrack::TrackListMemoryUsage() ;
      size_t memtrack_blocks=0 ;
      memtrack_blocks=xios::CXios::getin("memtrack_blocks",memtrack_blocks) ;
      size_t memtrack_size=0 ;
      memtrack_size=xios::CXios::getin("memtrack_size",memtrack_size) ;
      MemTrack::TrackDumpBlocks(memReport,memtrack_blocks,memtrack_size);
      memReport.close() ;
#endif
#endif
    CMemChecker::get("xios").suspend() ;
    CServer::closeInfoStream();
  }

  //! Parse configuration file
  void CXios::parseFile(const string& filename)
  {
    xml::CXMLParser::ParseFile(filename);
  }

  //! Set using server
  void CXios::setUsingServer()
  {
    usingServer = true;
  }

  //! Unset using server
  void CXios::setNotUsingServer()
  {
    usingServer = false;
  }

  void CXios::launchRegistryManager(bool isXiosServer)
  {
    registryManager_ = new CRegistryManager(isXiosServer) ;
  }

  void CXios::launchRessourcesManager(bool isXiosServer)
  {
    ressourcesManager_ = new CRessourcesManager(isXiosServer) ;
  }

  void CXios::launchCouplerManager(bool isXiosServer)
  {
    couplerManager_ = new CCouplerManager(isXiosServer) ;
  }

  void CXios::launchServicesManager(bool isXiosServer)
  {
    servicesManager_ = new CServicesManager(isXiosServer) ;
  }

  void CXios::launchContextsManager(bool isXiosServer)
  {
    contextsManager_ = new CContextsManager(isXiosServer) ;
  }
  
  void CXios::launchDaemonsManager(bool isXiosServer)
  {
    daemonsManager_ = new CDaemonsManager(isXiosServer) ;
  }

  
  void CXios::finalizeRegistryManager()
  {
    delete registryManager_;
  }

  void CXios::finalizeRessourcesManager()
  {
    delete ressourcesManager_;
  }

  void CXios::finalizeCouplerManager()
  {
    delete couplerManager_;
  }

  void CXios::finalizeServicesManager()
  {
    delete servicesManager_  ;
  }

  void CXios::finalizeContextsManager()
  {
    delete contextsManager_  ;
  }
  
  void CXios::finalizeDaemonsManager()
  {
    delete daemonsManager_  ;
  }
  
  CPoolRessource* CXios::getPoolRessource(void)
  {
    if (isClient) return CClient::getPoolRessource() ;
    else if (isServer) return CServer::getServersRessource()->getPoolRessource() ;
    
    MISSING_RETURN( "CPoolRessource* CXios::getPoolRessource()" );
    return nullptr;
  }
}

