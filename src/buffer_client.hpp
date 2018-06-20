#ifndef __BUFFER_CLIENT_HPP__
#define __BUFFER_CLIENT_HPP__

#include "xios_spl.hpp"
#include "buffer_out.hpp"
#include "mpi.hpp"
#include "cxios.hpp"

namespace xios
{
  class CClientBuffer
  {
    public:
      static size_t maxRequestSize;

      CClientBuffer(MPI_Comm intercomm, int serverRank, StdSize bufferSize, StdSize estimatedMaxEventSize);
      ~CClientBuffer();
      void createWindows(MPI_Comm oneSidedComm) ;
      void freeWindows(void) ;
      void lockBuffer(void) ;
      void unlockBuffer(void) ;
         
      bool isBufferFree(StdSize size);
      CBufferOut* getBuffer(size_t timeLine, StdSize size);
      bool checkBuffer(bool send=false);
      bool hasPendingRequest(void);
      StdSize remain(void);

    private:
      char* buffer[2];
      char* bufferHeader[2];
      size_t* firstTimeLine[2] ;
      size_t* bufferCount[2] ;
      size_t* control[2] ;
      bool winState[2] ;
      int current;

      StdSize count;
      StdSize maxEventSize;
      const StdSize bufferSize;
      const StdSize estimatedMaxEventSize;


      const int serverRank;
      bool pending;

      MPI_Request request;

      CBufferOut* retBuffer;
      const MPI_Comm interComm;
      MPI_Win windows[2] ;
      bool hasWindows ;
      static const int headerSize=3*sizeof(size_t);
  };
}
#endif
