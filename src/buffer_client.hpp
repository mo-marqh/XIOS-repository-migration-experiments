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

      CClientBuffer(MPI_Comm intercomm, vector<MPI_Win>& windows, int clientRank, int serverRank, StdSize bufferSize, StdSize estimatedMaxEventSize);
      ~CClientBuffer();
//      void createWindows(MPI_Comm oneSidedComm) ;
      void freeWindows(void) ;
      void lockBuffer(void) ;
      void unlockBuffer(void) ;
      
      bool isBufferFree(StdSize size);
      CBufferOut* getBuffer(size_t timeLine, StdSize size);
      bool checkBuffer(bool send=false);
      bool hasPendingRequest(void);
      StdSize remain(void);
      MPI_Aint getWinAddress(int numWindows) ;
      void infoBuffer(void) ;
      bool isNotifiedFinalized(void) ;
    private:
      char* buffer[2];
      char* bufferHeader[2];
      size_t* firstTimeLine[2] ;
      size_t* bufferCount[2] ;
      size_t* control[2] ;
      size_t* finalize[2] ;
      bool winState[2] ;
      int current;

      StdSize count;
      StdSize maxEventSize;
      const StdSize bufferSize;
      const StdSize estimatedMaxEventSize;


      const int serverRank;
      const int clientRank_;
      bool pending;

      MPI_Request request;

      CBufferOut* retBuffer;
      const MPI_Comm interComm;
      std::vector<MPI_Win> windows_ ;
      bool hasWindows ;
      static const int headerSize=4*sizeof(size_t);
  };
}
#endif
