
#include "xmlioserver_spl.hpp"
#include "cxios.hpp"
#include "client_ym.hpp"
#include "server_ym.hpp"
#include "tree_manager.hpp"
#include <boost/functional/hash.hpp>
#include <mpi.h>

namespace xios
{
  string CXios::rootFile="./iodef.xml" ;
  string CXios::xiosCodeId="xios.x" ;
  
  bool CXios::isClient ;
  bool CXios::isServer ;
  MPI_Comm CXios::globalComm ;
  bool CXios::usingOasis ;
  bool CXios::usingServer ;   
  size_t CXios::bufferSize ;
  double CXios::bufferServerFactorSize=2 ;
  size_t CXios::defaultBufferSize=1024*1024*100 ; // 100Mo
  double CXios::defaultBufferServerFactorSize=2 ;
  
  void CXios::initialize()
  {
    tree::CTreeManager::ParseFile(rootFile);
    usingServer=getin<bool>("using_server",false) ;
    usingOasis=getin<bool>("using_oasis",false) ;
    info.setLevel(getin<int>("info_level",0)) ;
    bufferSize=getin<size_t>("buffer_size",defaultBufferSize) ;
    bufferServerFactorSize=getin<double>("buffer_server_factor_size",defaultBufferServerFactorSize) ;
    globalComm=MPI_COMM_WORLD ;
  }


  void CXios::initClientSide(const string& codeId, MPI_Comm& localComm, MPI_Comm& returnComm)
  {
     
    initialize() ;
    
    isClient=true;
    if (usingServer) isServer=false;
    else isServer=true ;
    
    ym::CClient::initialize(codeId,localComm,returnComm) ;

  }   

  void CXios::clientFinalize(void)
  {
     ym::CClient::finalize() ;    
  }   
  
  
  void CXios::initServerSide(void)
  {
    initialize();

    if (!usingServer) ERROR("void CXios::initServerSide(void)",<<"using_server is set to <false> and server initialization is called") ;
    isClient=true;
    isServer=false ;

    ym::CServer::initialize() ;
  } 
  

}
