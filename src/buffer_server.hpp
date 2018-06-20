#ifndef __BUFFER_SERVER_HPP__
#define __BUFFER_SERVER_HPP__

#include "xios_spl.hpp"
#include "buffer.hpp"
#include "mpi.hpp"
#include "cxios.hpp"

namespace xios
{
  class CServerBuffer
  {
    public:
      CServerBuffer(StdSize bufSize) ;
      ~CServerBuffer() ;

      bool isBufferFree(size_t count) ;
      void* getBuffer(size_t count) ;
      void freeBuffer(size_t count) ;
      void createWindows(MPI_Comm oneSidedComm) ;
      bool freeWindows(void) ;
      bool getBufferFromClient(size_t timeLine, char* & buffer, size_t& count) ;
      bool isBufferEmpty(void) ;
      void updateCurrentWindows(void) ;
    private:
      char* buffer;
      size_t first;   // first occupied element
      size_t current; // first free element
      size_t end;
      size_t size;
      size_t used ;  // count of element occupied
      MPI_Win windows[2] ;
      int currentWindows ;
      bool hasWindows ;
  };
}

#endif
