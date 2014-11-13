#ifndef __BUFFER_CLIENT_HPP__
#define __BUFFER_CLIENT_HPP__

#include "xmlioserver_spl.hpp"
#include "buffer_out.hpp"
#include "mpi.hpp"
#include "cxios.hpp"

namespace xios
{
  extern size_t maxRequestSize ;

  class CClientBuffer
  {

    public:

    CClientBuffer(MPI_Comm intercomm,int serverRank, StdSize bfSize = CXios::bufferSize) ;
    ~CClientBuffer() ;
    bool isBufferFree(int size) ;
    CBufferOut*  getBuffer(int size) ;
    bool checkBuffer(void) ;
    bool hasPendingRequest(void) ;

    char* buffer[2] ;
    int remain(void) ;

    int current ;
    int count ;
    int bufferSize ;
    int serverRank ;
    bool pending ;

    size_t bufferSizeByServer ;

    MPI_Request request ;

    CBufferOut* retBuffer;
    MPI_Comm interComm ;
  } ;

}

#endif

