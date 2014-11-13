
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
  size_t CXios::bufferSize ;
  double CXios::bufferServerFactorSize=2 ;
  size_t CXios::defaultBufferSize=1024*1024*100 ; // 100Mo
  double CXios::defaultBufferServerFactorSize=2 ;
  bool CXios::printInfo2File;
  bool CXios::isServerSide;


  void CXios::initialize()
  {
    set_new_handler(noMemory);
    parseFile(rootFile);
    usingOasis=getin<bool>("using_oasis",false) ;
    usingServer=getin<bool>("using_server",false) ;
    info.setLevel(getin<int>("info_level",0)) ;
    printInfo2File=getin<bool>("print_file",false);
    bufferSize=getin<size_t>("buffer_size",defaultBufferSize) ;
    bufferServerFactorSize=getin<double>("buffer_server_factor_size",defaultBufferServerFactorSize) ;
    globalComm=MPI_COMM_WORLD ;
  }


  void CXios::initClientSide(const string& codeId, MPI_Comm& localComm, MPI_Comm& returnComm)
  {
    initialize() ;

    isClient=true;

    CClient::initialize(codeId,localComm,returnComm) ;

    if (usingServer) isServerSide = isServer=false;
    else isServerSide = isServer=true;

    if (printInfo2File)
      CClient::openInfoStream(clientFile);
    else
      CClient::openInfoStream();
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

  void CXios::initServer()
  {
    set_new_handler(noMemory);
    std::set<StdString> parseList;
    parseList.insert("xios");
    xml::CXMLParser::ParseFile(rootFile, parseList);
//    parseFile(rootFile);
    usingOasis=getin<bool>("using_oasis",false) ;
    info.setLevel(getin<int>("info_level",0)) ;
    printInfo2File=getin<bool>("print_file",false);
    bufferSize=getin<size_t>("buffer_size",defaultBufferSize) ;
    bufferServerFactorSize=getin<double>("buffer_server_factor_size",defaultBufferServerFactorSize) ;
    globalComm=MPI_COMM_WORLD ;

  }

  void CXios::initServerSide(void)
  {
//    initialize();
    initServer();
    isClient=true;
    isServer=false ;

    isServerSide = true;

    // Initialize all aspects MPI
    CServer::initialize();

    if (printInfo2File)
      CServer::openInfoStream(serverFile);
    else
      CServer::openInfoStream();

    // Enter the loop to listen message from Client
    CServer::eventLoop();

    // Finalize
    CServer::finalize();
    CServer::closeInfoStream();
  }

  void CXios::parseFile(const string& filename)
  {
    xml::CXMLParser::ParseFile(filename);
  }

  void CXios::setUsingServer()
  {
    usingServer = true;
  }

  void CXios::setNotUsingServer()
  {
    usingServer = false;
  }
}
