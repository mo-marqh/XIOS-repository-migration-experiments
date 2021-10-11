#ifndef __WINDOW_MANAGER_HPP__
#define __WINDOW_MANAGER_HPP__

#include <map>
#include "mpi.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "message.hpp"

namespace xios
{


  class CWindowManager
  {

    private :
    const MPI_Aint OFFSET_LOCK=0 ;
    const int SIZE_LOCK=sizeof(int) ;
    const MPI_Aint OFFSET_BUFFER_SIZE=OFFSET_LOCK+SIZE_LOCK ;
    const int SIZE_BUFFER_SIZE=sizeof(size_t) ;
    const MPI_Aint OFFSET_BUFFER=OFFSET_BUFFER_SIZE+SIZE_BUFFER_SIZE ;
    const int WINDOWS_LOCKED=-1 ;

    MPI_Win window_ ;
    void * winBuffer_ ;
    map<int,double> lastTimeLock_ ;
    const double latency_=0e-2 ; 

    public :

    CWindowManager(MPI_Comm winComm, size_t bufferSize)
    {
      const MPI_Aint windowSize=bufferSize+OFFSET_BUFFER ;
      MPI_Win_allocate(windowSize, 1, MPI_INFO_NULL, winComm, &winBuffer_, &window_) ;
      int lock=0 ;
      size_t size=0 ;
      int commRank ;
      MPI_Comm_rank(winComm, &commRank) ;
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, commRank, 0, window_) ;
      MPI_Put(&lock, SIZE_LOCK, MPI_CHAR, commRank, OFFSET_LOCK, SIZE_LOCK, MPI_CHAR, window_) ;
      MPI_Put(&size, SIZE_BUFFER_SIZE, MPI_CHAR, commRank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR, window_) ;
      MPI_Win_unlock(commRank, window_) ;
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
        MPI_Win_lock(MPI_LOCK_EXCLUSIVE, rank, 0, window_) ;
        MPI_Compare_and_swap(&WINDOWS_LOCKED, &state, &lock, MPI_INT, rank, OFFSET_LOCK, window_) ;
        MPI_Win_unlock(rank, window_) ;
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
        MPI_Win_lock(MPI_LOCK_EXCLUSIVE, rank, 0, window_) ;
        MPI_Compare_and_swap(&WINDOWS_LOCKED, &state, &lock, MPI_INT, rank, OFFSET_LOCK, window_) ;
        MPI_Win_unlock(rank, window_) ;
        lastTime=MPI_Wtime() ;
      } while (lock!=state) ;
    }

    void lockWindowExclusive(int rank)
    {
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, rank, 0, window_) ;
    }

    void lockWindowShared(int rank)
    {
      MPI_Win_lock(MPI_LOCK_SHARED, rank, 0, window_) ;
    }

    void unlockWindow(int rank)
    {
      MPI_Win_unlock(rank, window_) ;
    }

    void flushWindow(int rank)
    {
      MPI_Win_flush(rank, window_) ;
    }

    void unlockWindow(int rank, int state )
    {
      int lock ;
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, rank, 0, window_) ;
      MPI_Compare_and_swap(&state, &WINDOWS_LOCKED, &lock, MPI_INT, rank, OFFSET_LOCK, window_) ;
      MPI_Win_unlock(rank, window_) ;
    }
    
    template< class T >
    void updateToWindow(int rank, T* object, void (T::*dumpOut)(CBufferOut&) )
    {
      CBufferOut buffer ;
      (object->*dumpOut)(buffer) ;
      size_t size=buffer.count() ;
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, rank, 0, window_) ;
      MPI_Put(&size, SIZE_BUFFER_SIZE, MPI_CHAR, rank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR, window_) ;
      MPI_Put(buffer.start(), size, MPI_CHAR, rank,OFFSET_BUFFER, size, MPI_CHAR, window_) ;
      MPI_Win_unlock(rank, window_) ;
    }

    template< class T >
    void updateToLockedWindow(int rank, T* object, void (T::*dumpOut)(CBufferOut&) )
    {
      CBufferOut buffer ;
      (object->*dumpOut)(buffer) ;
      size_t size=buffer.count() ;
//      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, rank, 0, window_) ;
      MPI_Put(&size, SIZE_BUFFER_SIZE, MPI_CHAR, rank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR, window_) ;
      MPI_Put(buffer.start(), size, MPI_CHAR, rank,OFFSET_BUFFER, size, MPI_CHAR, window_) ;
//      MPI_Win_unlock(rank, window_) ;
    }

    template< typename T >
    void updateFromWindow(int rank, T* object, void (T::*dumpIn)(CBufferIn&) ) 
    {
      size_t size ;
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, rank, 0, window_) ;
      MPI_Get(&size, SIZE_BUFFER_SIZE, MPI_CHAR, rank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR, window_) ;
      MPI_Win_flush(rank,window_) ;
      CBufferIn buffer(size) ;
      MPI_Get(buffer.start(), size, MPI_CHAR, rank,OFFSET_BUFFER, size, MPI_CHAR, window_) ;
      MPI_Win_unlock(rank, window_) ;
      (object->*dumpIn)(buffer) ;
    }

    template< typename T >
    void updateFromLockedWindow(int rank, T* object, void (T::*dumpIn)(CBufferIn&) ) 
    {
      size_t size ;
//      MPI_Win_lock(MPI_LOCK_SHARED, rank, 0, window_) ;
      MPI_Get(&size, SIZE_BUFFER_SIZE, MPI_CHAR, rank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR, window_) ;
      MPI_Win_flush(rank,window_) ;
      CBufferIn buffer(size) ;
      MPI_Get(buffer.start(), size, MPI_CHAR, rank,OFFSET_BUFFER, size, MPI_CHAR, window_) ;
//      MPI_Win_unlock(rank, window_) ;
      MPI_Win_flush(rank, window_) ;
      (object->*dumpIn)(buffer) ;
    }


    template< class T >
    void pushToWindow(int rank, T* object, void (T::*dumpOut)(CBufferOut&) )
    {
      size_t size ;
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, rank, 0, window_) ;
      MPI_Get(&size, SIZE_BUFFER_SIZE, MPI_CHAR, rank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR, window_) ;
      MPI_Win_flush(rank,window_) ;
      CBufferOut buffer ;
      (object->*dumpOut)(buffer) ;
      size_t bufferSize=buffer.count() ;
      size_t newSize = size + bufferSize;
      MPI_Put(&newSize, SIZE_BUFFER_SIZE, MPI_CHAR, rank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR, window_) ;
      MPI_Put(buffer.start(), bufferSize, MPI_CHAR, rank, OFFSET_BUFFER+size, bufferSize, MPI_CHAR, window_) ;
      MPI_Win_unlock(rank, window_) ;
    }

    template< typename T >
    void popFromWindow(int rank, T* object, void (T::*dumpIn)(CBufferIn&) ) 
    {
      size_t size ;
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, rank, 0, window_) ;
      MPI_Get(&size, SIZE_BUFFER_SIZE, MPI_CHAR, rank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR, window_) ;
      MPI_Win_flush(rank,window_) ;
      CBufferIn buffer(size) ;
      MPI_Get(buffer.start(), size, MPI_CHAR, rank,OFFSET_BUFFER, size, MPI_CHAR, window_) ;
      MPI_Win_flush(rank,window_) ;
      (object->*dumpIn)(buffer) ;
      
      size=buffer.remain() ;
      MPI_Put(&size, SIZE_BUFFER_SIZE, MPI_CHAR, rank, OFFSET_BUFFER_SIZE, SIZE_BUFFER_SIZE, MPI_CHAR, window_) ;
      MPI_Put(buffer.ptr(),buffer.remain(), MPI_CHAR, rank, OFFSET_BUFFER, buffer.remain(), MPI_CHAR, window_) ;
      MPI_Win_unlock(rank, window_) ;
      
    }

    ~CWindowManager()
    {
      MPI_Win_free(&window_) ;
    }
  } ;
}



#endif