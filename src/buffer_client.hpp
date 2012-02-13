#ifndef __BUFFER_CLIENT_HPP__
#define __BUFFER_CLIENT_HPP__

#include "xmlioserver_spl.hpp"
#include "buffer_out.hpp"
#include <mpi.h>

namespace xmlioserver
{
  class CClientBuffer
  {
    
    public:
    
    CClientBuffer(MPI_Comm intercomm,int serverRank) ;
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
    
    static int bufferSizeByServer ;
    
    MPI_Request request ;
    
    CBufferOut* retBuffer;    
    MPI_Comm interComm ;
  } ;

}

#endif

