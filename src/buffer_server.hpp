#ifndef __BUFFER_SERVER_HPP__
#define __BUFFER_SERVER_HPP__

#include "buffer_cs_base.hpp"
#include "xios_spl.hpp"
#include "buffer.hpp"
#include "mpi.hpp"
#include "cxios.hpp"

namespace xios
{
  class CServerBuffer : public CBufferClientServerBase
  {
    public:
      CServerBuffer(vector<MPI_Win>& windows, vector<MPI_Aint>& winAddress, int windowsRank, StdSize bufSize) ;
      ~CServerBuffer() ;

      bool isBufferFree(size_t count) ;
      bool isResizing(void) { return resizingBuffer_ ;}
      void* getBuffer(size_t count) ;
      void freeBuffer(size_t count) ;
      void createWindows(MPI_Comm oneSidedComm) ;
      bool getBufferFromClient(size_t timeLine, char* & buffer, size_t& count) ;
      bool isBufferEmpty(void) ;
      void updateCurrentWindows(void) ;
      void lockBuffer(void) ;
      void unlockBuffer(void) ;
      void notifyClientFinalize(void) ;
      void notifyBufferResizing(void) ;
    private:
      char* buffer;
      size_t first;   // first occupied element
      size_t current; // first free element
      size_t end;
      size_t size;
      size_t used ;  // count of element occupied
      std::vector<MPI_Win> windows_ ;
      std::vector<MPI_Aint> winAddress_ ;
      bool resizingBuffer_ = false ;
      int currentWindows ;
      bool hasWindows ;
      int windowsRank_ ;
      double bufferFromClientLatency_=0e-1 ;
      double bufferFromClientTime_ = 0;

  };
}

#endif
