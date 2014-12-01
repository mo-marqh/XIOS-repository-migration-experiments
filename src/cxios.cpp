
#include "xmlioserver_spl.hpp"
#include "cxios.hpp"
#include "client.hpp"
#include "server.hpp"
#include "xml_parser.hpp"
#include <boost/functional/hash.hpp>
#include "mpi.hpp"
#include "memory.hpp"
#include <new>
#include "memtrack.hpp"

namespace xios
{
  string CXios::rootFile="./iodef.xml" ;
  string CXios::xiosCodeId="xios.x" ;
  string CXios::clientFile="./xios_client";
  string CXios::serverFile="./xios_server";

  bool CXios::isClient ;
  bool CXios::isServer ;
  MPI_Comm CXios::globalComm ;
  bool CXios::usingOasis ;
  bool CXios::usingServer = false;
  double CXios::bufferServerFactorSize=1.0 ;
  double CXios::defaultBufferServerFactorSize=1.0 ;
  bool CXios::printLogs2Files;
  bool CXios::isOptPerformance = true;

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

    bufferServerFactorSize=getin<double>("buffer_factor_size",defaultBufferServerFactorSize) ;
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

    isClient=true;

    CClient::initialize(codeId,localComm,returnComm) ;

    if (usingServer) isServer=false;
    else isServer=true;

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
  void CXios::initServerSide(void)
  {
    initServer();
    isClient=true;
    isServer=false ;

    // Initialize all aspects MPI
    CServer::initialize();

    if (printLogs2Files)
    {
      CServer::openInfoStream(serverFile);
      CServer::openErrorStream(serverFile);
    }
    else
    {
      CServer::openInfoStream();
      CServer::openErrorStream();
    }

    // Enter the loop to listen message from Client
    CServer::eventLoop();

    // Finalize
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
