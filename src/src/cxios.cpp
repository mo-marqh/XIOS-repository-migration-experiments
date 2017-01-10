
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

namespace xios
{
  string CXios::rootFile="./iodef.xml" ;
  string CXios::xiosCodeId="xios.x" ;
//  string CXios::xiosCodeIdPrm="xios.x.1" ;
//  string CXios::xiosCodeIdSnd="xios.x.2" ;
  string CXios::clientFile="./xios_client";
  string CXios::serverFile="./xios_server";
  string CXios::serverPrmFile="./xios_server1";
  string CXios::serverSndFile="./xios_server2";

  bool CXios::isClient ;
  bool CXios::isServer ;
//  int CXios::serverLevel = 0 ;
  MPI_Comm CXios::globalComm ;
  bool CXios::usingOasis ;
  bool CXios::usingServer = false;
  bool CXios::usingServer2 = false;
  int CXios::ratioServer2 = 50;
  double CXios::bufferSizeFactor = 1.0;
  const double CXios::defaultBufferSizeFactor = 1.0;
  StdSize CXios::minBufferSize = 1024 * sizeof(double);
  bool CXios::printLogs2Files;
  bool CXios::isOptPerformance = true;
  CRegistry* CXios::globalRegistry = 0;

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
    info.setLevel(getin<int>("info_level",0)) ;
    report.setLevel(getin<int>("info_level",50));
    printLogs2Files=getin<bool>("print_file",false);

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

    globalComm=MPI_COMM_WORLD ;
  }

  /*!
  Initialize client
  \param [in] codeId identity of context
  \param [in] localComm local communicator
  \param [in/out] returnComm communicator corresponding to group of client with same codeId
  */
  void CXios::initClientSide(const string& codeId, MPI_Comm& localComm, MPI_Comm& returnComm)
  {
    initialize() ;

    isClient = true;

    CClient::initialize(codeId,localComm,returnComm) ;
    if (CClient::getRank()==0) globalRegistry = new CRegistry(returnComm) ;

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
  }

  void CXios::clientFinalize(void)
  {
     CClient::finalize() ;
     if (CClient::getRank()==0)
     {
       info(80)<<"Write data base Registry"<<endl<<globalRegistry->toString()<<endl ;
       globalRegistry->toFile("xios_registry.bin") ;
       delete globalRegistry ;
     }
     CClient::closeInfoStream();
  

#ifdef XIOS_MEMTRACK
     MemTrack::TrackListMemoryUsage() ;
     MemTrack::TrackDumpBlocks();
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
  void CXios::initServerSide(int serverLvl)
  {
    initServer();

//    if (serverLvl == 1)
//      isClient = true;
//    else
//      isClient = false;
//
//    isServer = true;
//    serverLevel = serverLvl;



    // Initialize all aspects MPI
    CServer::initialize();
    isServer = true;
    if (CServer::serverLevel == 1)
      isClient = true;
    else
      isClient = false;

    if (CServer::getRank()==0) globalRegistry = new CRegistry(CServer::intraComm) ;
    
    if (printLogs2Files)
    {
      if (CServer::serverLevel == 0)
//      if (CXios::serverLevel == 0)
      {
        CServer::openInfoStream(serverFile);
        CServer::openErrorStream(serverFile);
      }
      else if (CServer::serverLevel == 1)
//      else if (CXios::serverLevel == 1)
      {
        CServer::openInfoStream(serverPrmFile);
        CServer::openErrorStream(serverPrmFile);
      }
      else
      {
        CServer::openInfoStream(serverSndFile);
        CServer::openErrorStream(serverSndFile);
      }
    }
    else
    {
      CServer::openInfoStream();
      CServer::openErrorStream();
    }

    // Enter the loop to listen message from Client
    CServer::eventLoop();

    // Finalize
     if (CServer::getRank()==0)
     {
       info(80)<<"Write data base Registry"<<endl<<globalRegistry->toString()<<endl ;
       globalRegistry->toFile("xios_registry.bin") ;
       delete globalRegistry ;
     }
    CServer::finalize();
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
}
