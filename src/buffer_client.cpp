#include "xios_spl.hpp"
#include "exception.hpp"
#include "log.hpp"
#include "buffer_out.hpp"
#include "buffer_client.hpp"
#include "cxios.hpp"
#include "mpi.hpp"
#include "tracer.hpp"

namespace xios
{
  size_t CClientBuffer::maxRequestSize = 0;

  CClientBuffer::CClientBuffer(MPI_Comm interComm, int serverRank, StdSize bufferSize, StdSize estimatedMaxEventSize)
    : interComm(interComm)
    , serverRank(serverRank)
    , bufferSize(bufferSize)
    , estimatedMaxEventSize(estimatedMaxEventSize)
    , maxEventSize(0)
    , current(0)
    , count(0)
    , pending(false)
    , hasWindows(false) 
  {
      MPI_Alloc_mem(bufferSize+headerSize, MPI_INFO_NULL, &bufferHeader[0]) ;
      MPI_Alloc_mem(bufferSize+headerSize, MPI_INFO_NULL, &bufferHeader[1]) ;


    buffer[0] = bufferHeader[0]+headerSize ;
    buffer[1] = bufferHeader[1]+headerSize ;
    firstTimeLine[0]=(size_t*)bufferHeader[0] ;
    firstTimeLine[1]=(size_t*)bufferHeader[1] ;
    bufferCount[0]=(size_t*)bufferHeader[0] +1 ;
    bufferCount[1]=(size_t*)bufferHeader[1] +1 ;
    control[0]=(size_t*)bufferHeader[0] +2 ;
    control[1]=(size_t*)bufferHeader[1] +2 ;

    *firstTimeLine[0]=0 ;
    *firstTimeLine[1]=0 ;
    *bufferCount[0]=0 ;
    *bufferCount[1]=0 ;
    *control[0]=0 ;
    *control[1]=0 ;
    winState[0]=false ;
    winState[1]=false ;
    retBuffer = new CBufferOut(buffer[current], bufferSize);
    info(10) << "CClientBuffer: allocated 2 x " << bufferSize << " bytes for server " << serverRank << endl;
  }

  CClientBuffer::~CClientBuffer()
  {
     freeWindows() ;
     MPI_Free_mem(bufferHeader[0]) ;
     MPI_Free_mem(bufferHeader[1]) ;
     delete retBuffer;
  }

  void CClientBuffer::createWindows(MPI_Comm oneSidedComm)
  {
    MPI_Barrier(oneSidedComm) ;
    MPI_Win_create(bufferHeader[0], bufferSize+headerSize, 1, MPI_INFO_NULL, oneSidedComm, &(windows[0])) ;
    MPI_Win_create(bufferHeader[1], bufferSize+headerSize, 1, MPI_INFO_NULL, oneSidedComm, &(windows[1])) ;

    MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, windows[0]) ;
    *firstTimeLine[0]=0 ;
    *bufferCount[0]=0 ;
    *control[0]=0 ;
    MPI_Win_unlock(0, windows[0]) ;

    MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, windows[1]) ;
    *firstTimeLine[1]=0 ;
    *bufferCount[1]=0 ;
    *control[1]=0 ;
    MPI_Win_unlock(0, windows[1]) ;
    winState[0]=false ;
    winState[1]=false ;
    MPI_Barrier(oneSidedComm) ;
    hasWindows=true ;
  }

  void CClientBuffer::freeWindows()
  {
    if (hasWindows)
    {
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, windows[0]) ;
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, windows[1]) ;
      *control[0]=2 ;
      *control[1]=2 ;
      MPI_Win_unlock(0, windows[1]) ;
      MPI_Win_unlock(0, windows[0]) ;
      
      MPI_Win_free(&windows[0]) ;
      MPI_Win_free(&windows[1]) ;
      hasWindows=false ;
    }
  }
 
  void CClientBuffer::lockBuffer(void)
  {
    if (hasWindows)
    {
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, windows[current]) ;
      winState[current]=true ;
    }
  }

  void CClientBuffer::unlockBuffer(void)
  {
    if (hasWindows)
    {
      MPI_Win_unlock(0, windows[current]) ;
      winState[current]=false ;
    }
  }

  StdSize CClientBuffer::remain(void)
  {
    return bufferSize - count;
  }

  bool CClientBuffer::isBufferFree(StdSize size)
  {
    bool loop=true ;
    while (loop) 
    {
      lockBuffer();
      if (*control[current]==0) loop=false ; // attemp to read from server ?
      else unlockBuffer() ;
    }
    
    if (size > bufferSize)
      ERROR("bool CClientBuffer::isBufferFree(StdSize size)",
            << "The requested size (" << size << " bytes) is too big to fit the buffer (" << bufferSize << " bytes), please increase the client buffer size." << endl);

    if (size > maxEventSize)
    {
      maxEventSize = size;

      if (size > estimatedMaxEventSize)
        error(0) << "WARNING: Unexpected event of size " << size << " for server " << serverRank
                 << " (estimated max event size = " << estimatedMaxEventSize << ")" << std::endl;

      if (size > maxRequestSize) maxRequestSize = size;
    }

      count=*bufferCount[current] ;
      return (size <= remain());
  }


  CBufferOut* CClientBuffer::getBuffer(size_t timeLine, StdSize size)
  {
    if (size <= remain())
    {
      info(100)<<"count "<<count<<"   bufferCount[current]  "<<*bufferCount[current]<<endl ;
      retBuffer->realloc(buffer[current] + count, size);
      count += size;
      if (*firstTimeLine[current]==0) *firstTimeLine[current]=timeLine ;
      *bufferCount[current]=count ;
      return retBuffer;
    }
    else
    {
      ERROR("CBufferOut* CClientBuffer::getBuffer(StdSize size)",
            << "Not enough space in buffer, this should not have happened...");
      return NULL;
    }
  }

  bool CClientBuffer::checkBuffer(bool send)
  {
    MPI_Status status;
    int flag;

    if (pending)
    {
      traceOff();
      MPI_Test(&request, &flag, &status);
      traceOn();
      if (flag == true) pending = false;
    }

    if (!pending)
    {
      if (!send) return false ;
      if (count > 0)
      {
        lockBuffer() ;
        if (*control[current]==0 && bufferCount[current] > 0)
        {
          MPI_Issend(buffer[current], count, MPI_CHAR, serverRank, 20, interComm, &request);
          pending = true;
          *control[current]=0 ;
          *firstTimeLine[current]=0 ;
          *bufferCount[current]=0 ;

           unlockBuffer() ;

          if (current == 1) current = 0;
          else current = 1;
          count = 0;
        }
        else unlockBuffer() ;
      }
    }

    return pending;
  }

  bool CClientBuffer::hasPendingRequest(void)
  {
   
    lockBuffer() ;
    count=*bufferCount[current] ;
    unlockBuffer() ;

    return (pending || count > 0);
  }


}
