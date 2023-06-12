#ifndef __WINDOW_MANAGER_HPP__
#define __WINDOW_MANAGER_HPP__

#include <map>
#include "mpi.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "message.hpp"
#include "window_base.hpp"

namespace xios
{


  class CWindowManager : public CWindowBase 
  {

    private :
    static const MPI_Aint OFFSET_LOCK=0 ;
    static const int SIZE_LOCK=sizeof(int) ;
    static const MPI_Aint OFFSET_BUFFER_SIZE=OFFSET_LOCK+SIZE_LOCK ;
    static const int SIZE_BUFFER_SIZE=sizeof(size_t) ;
    static const MPI_Aint OFFSET_BUFFER=OFFSET_BUFFER_SIZE+SIZE_BUFFER_SIZE ;
    static const int WINDOWS_LOCKED=-1 ;

    MPI_Win window_ ;
    void * winBuffer_ ;
    map<int,double> lastTimeLock_ ;
    const double latency_=0e-2 ; 

    public :

    CWindowManager(MPI_Comm winComm, size_t bufferSize) : CWindowBase(winComm, bufferSize + OFFSET_BUFFER_SIZE)
    {
      int lock=0 ;
      size_t size=0 ;
      int commRank ;
      MPI_Comm_rank(winComm, &commRank) ;
      lockExclusive(commRank) ;
      put(&lock, SIZE_LOCK, MPI_CHAR, commRank, OFFSET_LOCK, SIZE_LOCK, MPI_CHAR) ;
      put(&size, SIZE_BUFFER_SIZE, MPI_CHAR, commRank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR) ;
      unlockExclusive(commRank) ;
      MPI_Barrier(winComm) ;
    }
    
    void lockWindow(int rank, int state )
    {
      int lock=state ;
      double time ;
      auto it=lastTimeLock_.find(rank) ;
      if (it == lastTimeLock_.end()) 
      { 
        lastTimeLock_[rank] = 0. ; 
        it=lastTimeLock_.find(rank) ;
      }
      double& lastTime = it->second ;

      do 
      {
        time=MPI_Wtime() ;
        while(time-lastTime < latency_) time=MPI_Wtime() ;
        int flag ;
        MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
        compareAndSwap(&WINDOWS_LOCKED, &state, &lock, MPI_INT, rank, OFFSET_LOCK) ;
        flush(rank) ;
        lastTime=MPI_Wtime() ;
      } while (lock!=state) ;
      
      
    }

    void lockWindowExclusive(int rank, int state )
    {
      int lock=state ;
      double time ;
      auto it=lastTimeLock_.find(rank) ;
      if (it == lastTimeLock_.end()) 
      { 
        lastTimeLock_[rank] = 0. ; 
        it=lastTimeLock_.find(rank) ;
      }
      double& lastTime = it->second ;

      do 
      {
        time=MPI_Wtime() ;
        while(time-lastTime < latency_) time=MPI_Wtime() ;
        int flag ;
        MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
        lockExclusive(rank) ;
        compareAndSwap(&WINDOWS_LOCKED, &state, &lock, MPI_INT, rank, OFFSET_LOCK) ;
        unlockExclusive(rank) ;
        lastTime=MPI_Wtime() ;
      } while (lock!=state) ;
    }

    void lockWindowExclusive(int rank)
    {
      lockExclusive(rank) ;
    }

    void lockWindowShared(int rank)
    {
      lockShared(rank) ;
    }

    void unlockWindowExclusive(int rank)
    {
      unlockExclusive(rank) ;
    }

    void unlockWindowShared(int rank)
    {
      unlockShared(rank) ;
    }

    void lockWindow(int rank)
    {
      lockWindowExclusive(rank) ;
    }
    
    void unlockWindow(int rank)
    {
      unlockWindowExclusive(rank) ;
    }

    void flushWindow(int rank)
    {
      flush(rank) ;
    }

    void unlockWindow(int rank, int state )
    {
      int lock ;
      compareAndSwap(&state, &WINDOWS_LOCKED, &lock, MPI_INT, rank, OFFSET_LOCK) ;
      flush(rank) ;
    }
    
    template< class T >
    void updateToWindow(int rank, T* object, void (T::*dumpOut)(CBufferOut&) )
    {
      updateToExclusiveWindow(rank, object, dumpOut) ;
    }

    template< class T >
    void updateToExclusiveWindow(int rank, T* object, void (T::*dumpOut)(CBufferOut&) )
    {
      lockExclusive(rank) ;
      updateToLockedWindow(rank, object, dumpOut) ;
      unlockExclusive(rank) ;
     }

    template< class T >
    void updateTosharedWindow(int rank, T* object, void (T::*dumpOut)(CBufferOut&) )
    {
      lockShared(rank) ;
      updateToLockedWindow(rank, object, dumpOut) ;
      unlockShared(rank) ;
    }

    template< class T >
    void updateToLockedWindow(int rank, T* object, void (T::*dumpOut)(CBufferOut&) )
    {
      CBufferOut buffer ;
      (object->*dumpOut)(buffer) ;
      size_t size=buffer.count() ;

      put(&size, SIZE_BUFFER_SIZE, MPI_CHAR, rank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR) ;
      put(buffer.start(), size, MPI_CHAR, rank,OFFSET_BUFFER, size, MPI_CHAR) ;
      flush(rank) ;
    }

    template< typename T >
    void updateFromWindow(int rank, T* object, void (T::*dumpIn)(CBufferIn&) ) 
    {
      updateFromExclusiveWindow(rank,object, dumpIn) ;
    }

    template< typename T >
    void updateFromExclusiveWindow(int rank, T* object, void (T::*dumpIn)(CBufferIn&) ) 
    {
      size_t size ;
      lockExclusive(rank) ;
      updateFromLockedWindow(rank,object, dumpIn) ;
      unlockExclusive(rank) ;
    }

    template< typename T >
    void updateFromSharedWindow(int rank, T* object, void (T::*dumpIn)(CBufferIn&) ) 
    {
      size_t size ;
      lockShared(rank) ;
      updateFromLockedWindow(rank,object, dumpIn) ;
      unlockShared(rank) ;
    }

    template< typename T >
    void updateFromLockedWindow(int rank, T* object, void (T::*dumpIn)(CBufferIn&) ) 
    {
      size_t size ;

      get(&size, SIZE_BUFFER_SIZE, MPI_CHAR, rank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR) ;
      flush(rank) ;
      CBufferIn buffer(size) ;
      get(buffer.start(), size, MPI_CHAR, rank,OFFSET_BUFFER, size, MPI_CHAR) ;
      flush(rank) ;

      (object->*dumpIn)(buffer) ;
    }



    template< class T >
    void pushToWindow(int rank, T* object, void (T::*dumpOut)(CBufferOut&) )
    {
      pushToExclusiveWindow(rank, object, dumpOut) ;
    }

    template< class T >
    void pushToExclusiveWindow(int rank, T* object, void (T::*dumpOut)(CBufferOut&) )
    {
      lockExclusive(rank) ;
      pushToLockedWindow(rank, object, dumpOut) ;
      unlockExclusive(rank) ;
    }

    template< class T >
    void pushToSharedWindow(int rank, T* object, void (T::*dumpOut)(CBufferOut&) )
    {
      lockShared(rank) ;
      pushToLockedWindow(rank, object, dumpOut) ;
      unlockShared(rank) ;
    }

    template< class T >
    void pushToLockedWindow(int rank, T* object, void (T::*dumpOut)(CBufferOut&) )
    {
      size_t size ;
      get(&size, SIZE_BUFFER_SIZE, MPI_CHAR, rank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR) ;
      flush(rank) ;
      CBufferOut buffer ;
      (object->*dumpOut)(buffer) ;
      size_t bufferSize=buffer.count() ;
      size_t newSize = size + bufferSize;
      put(&newSize, SIZE_BUFFER_SIZE, MPI_CHAR, rank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR) ;
      put(buffer.start(), bufferSize, MPI_CHAR, rank, OFFSET_BUFFER+size, bufferSize, MPI_CHAR) ;
      flush(rank) ;
    }



    template< typename T >
    void popFromWindow(int rank, T* object, void (T::*dumpIn)(CBufferIn&) ) 
    {
      popFromExclusiveWindow(rank,object, dumpIn) ; 
    }

    template< typename T >
    void popFromExclusiveWindow(int rank, T* object, void (T::*dumpIn)(CBufferIn&) ) 
    {
      lockExclusive(rank) ;
      popFromLockedWindow(rank,object, dumpIn) ; 
      unlockExclusive(rank) ;
      
    }

    template< typename T >
    void popFromSharedWindow(int rank, T* object, void (T::*dumpIn)(CBufferIn&) ) 
    {
      lockShared(rank) ;
      popFromLockedWindow(rank,object, dumpIn) ; 
      unlockShared(rank) ;
    }

    template< typename T >
    void popFromLockedWindow(int rank, T* object, void (T::*dumpIn)(CBufferIn&) ) 
    {
      size_t size ;
      get(&size, SIZE_BUFFER_SIZE, MPI_CHAR, rank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR) ;
      flush(rank) ;
      CBufferIn buffer(size) ;
      get(buffer.start(), size, MPI_CHAR, rank,OFFSET_BUFFER, size, MPI_CHAR) ;
      flush(rank) ;
      (object->*dumpIn)(buffer) ;
      
      size=buffer.remain() ;
      put(&size, SIZE_BUFFER_SIZE, MPI_CHAR, rank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR) ;
      put(buffer.ptr(),buffer.remain(), MPI_CHAR, rank, OFFSET_BUFFER, buffer.remain(), MPI_CHAR) ;
      flush(rank) ;
    }

    ~CWindowManager()
    {
    }
  } ;
}



#endif