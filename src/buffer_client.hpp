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

      CClientBuffer(MPI_Comm intercomm,int serverRank, StdSize bfSize = 0);
      ~CClientBuffer();

      bool isBufferFree(int size);
      CBufferOut* getBuffer(int size);
      bool checkBuffer(void);
      bool hasPendingRequest(void);
      int remain(void);

    private:
      char* buffer[2];

      int current;
      int count;
      int bufferSize;
      int serverRank;
      bool pending;

      MPI_Request request;

      CBufferOut* retBuffer;
      MPI_Comm interComm;
  };
}

#endif
