#ifndef __WINDOW_DYNAMIC_HPP__
#define __WINDOW_DYNAMIC_HPP__

#include <map>
//#include "exception.hpp"
#include "mpi.hpp"
#include "cxios.hpp"
#include <iostream>

namespace xios
{


  class CWindowDynamic
  {
    private:
      void * winBuffer_ ;   
      const MPI_Aint OFFSET_LOCK=0 ;
      const int SIZE_LOCK=sizeof(long) ;
      const MPI_Aint OFFSET_BUFFER =  SIZE_LOCK ;
      const MPI_Aint OFFSET_BUFFER_SIZE = SIZE_LOCK   ;
      MPI_Aint bufferSize_ ;
      const double maxLatency_ = 1e-3 ; // 1ms latency maximum
      MPI_Win window_ ;
      MPI_Aint windowSize_ ;
      int winCommRank_ ;
      map<int,MPI_Aint> winBufferAddress_ ;


    public :

    void allocateBuffer(MPI_Aint size)
    {
      bufferSize_ = size ;
      windowSize_ = size+OFFSET_BUFFER_SIZE ;
      MPI_Alloc_mem(windowSize_, MPI_INFO_NULL, &winBuffer_) ;
      MPI_Aint& lock = *((MPI_Aint*)(static_cast<char*>(winBuffer_)+OFFSET_LOCK)) ;
      lock=0 ;
    }  

    void create(MPI_Comm winComm)
    {
      MPI_Win_create_dynamic(MPI_INFO_NULL, winComm, &window_);
      CXios::getMpiGarbageCollector().registerWindow(window_) ;
      MPI_Barrier(winComm) ;
      MPI_Comm_rank(winComm, &winCommRank_) ;
      MPI_Win_lock_all(0, window_) ;
    }
    
    void lockAll()
    {
      MPI_Win_lock_all(0, window_) ;
    }

    void unlockAll()
    {
      MPI_Win_unlock_all(window_) ;
    }

    void setWinBufferAddress(MPI_Aint addr, int rank)
    {
      winBufferAddress_[rank]=addr ;
    }
    
    MPI_Aint getWinBufferAddress()
    {
      MPI_Aint ret ;
      MPI_Get_address(winBuffer_, &ret) ;
      return ret ;
    }

    void* getBufferAddress()
    {
      return static_cast<char*>(winBuffer_)+OFFSET_BUFFER_SIZE ;
    }
    
    bool tryLockExclusive(int rank)
    {
      long lock = 1;
      long unlock = 0;
      long state;

      int flag ;
      if (rank==winCommRank_) MPI_Win_sync(window_) ;
      MPI_Compare_and_swap(&lock, &unlock, &state, MPI_LONG, rank, winBufferAddress_[rank]+OFFSET_LOCK, window_) ;
      MPI_Win_flush(rank, window_);
//      if (rank==winCommRank_) MPI_Win_sync(window_) ;
      bool locked = (state == unlock) ;
      return locked ;
    }

    bool tryLockShared(int rank, MPI_Op op)
    {
      long one = 0x100000000;
      long res;

      MPI_Fetch_and_op(&one, &res, MPI_LONG, rank, winBufferAddress_[rank]+OFFSET_LOCK, op, window_);
      MPI_Win_flush(rank, window_);
      
      bool locked =  ! (res & 1) ;
      return locked ;
    }

    void unlockExclusive(int rank)
    {
      int lock = 1;
      int unlock = 0;
      int state;
      
      if (rank==winCommRank_) MPI_Win_sync(window_) ;
      MPI_Win_flush(rank, window_);
      MPI_Compare_and_swap(&unlock, &lock, &state, MPI_INT, rank, winBufferAddress_[rank]+OFFSET_LOCK, window_) ;
      MPI_Win_flush(rank, window_);
//      if (rank==winCommRank_) MPI_Win_sync(window_) ;
      if (lock != state) 
      {
        info(100)<<"Bad State : "<<((long*)winBuffer_)[0]<<endl ;
        ERROR("CWindowBase::unlockWindowExclusive",<<"unlockWindow failed: bad state"<<endl) ; 
      }
    }

    void unlockShared(int rank)
    {
      int minusone = -1;
      int res;
      MPI_Fetch_and_op(&minusone, &res, MPI_INT, rank, winBufferAddress_[rank]+OFFSET_LOCK+4, MPI_SUM, window_);
      MPI_Win_flush(rank, window_);
    }

    void lockExclusive(int rank)
    {
      double time =  MPI_Wtime() ;
      bool locked = tryLockExclusive(rank);
      double lastTime = MPI_Wtime() ;
      double delta = lastTime-time ;
      
      while (!locked)
      {
        time = MPI_Wtime() ;
        if (delta > maxLatency_) delta = maxLatency_ ;
        if (time >= lastTime+delta)
        { 
          locked = tryLockExclusive(rank);
          delta=delta*2.;
          lastTime = time ;      
        }
      }  
    }

    void lockShared(int rank)
    {
      double time =  MPI_Wtime() ;
      bool locked = tryLockShared(rank, MPI_SUM);
      double lastTime = MPI_Wtime() ;
      double delta = lastTime-time ;
      
      while (!locked)
      {
        time = MPI_Wtime() ;
        if (delta > maxLatency_) delta = maxLatency_ ;
        if (time >= lastTime+delta)
        { 
          locked = tryLockShared(rank, MPI_NO_OP);
          delta=delta*2.;
          lastTime = time ;      
        }
      }  
    }
    
    int attach(MPI_Aint size) 
    {
      windowSize_ = size+OFFSET_BUFFER_SIZE ;
      MPI_Alloc_mem(windowSize_, MPI_INFO_NULL, &winBuffer_) ;
      MPI_Aint& lock = *((MPI_Aint*)(static_cast<char*>(winBuffer_)+OFFSET_LOCK)) ;
      lock=0 ;
      MPI_Win_attach(window_, winBuffer_, size+OFFSET_BUFFER_SIZE) ;
      setWinBufferAddress(getWinBufferAddress(),winCommRank_) ;
    }
    
    int attach() 
    {
      MPI_Win_attach(window_, winBuffer_, windowSize_) ;
      setWinBufferAddress(getWinBufferAddress(),winCommRank_) ;
    }

    int detach() 
    {
      MPI_Win_detach(window_, winBuffer_) ;
      MPI_Free_mem(winBuffer_) ;        
    }

    int flush(int rank)
    {
      return MPI_Win_flush(rank, window_) ;
    }

    int put(const void *origin_addr, int origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
            int target_count, MPI_Datatype target_datatype)
    {
      return MPI_Put(origin_addr, origin_count, origin_datatype, target_rank,  target_disp + OFFSET_BUFFER, target_count, target_datatype, window_) ;
    }

    int get(void *origin_addr, int origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
            int target_count, MPI_Datatype target_datatype)
    {
      return MPI_Get(origin_addr, origin_count, origin_datatype, target_rank, target_disp + OFFSET_BUFFER, target_count, target_datatype, window_) ;
    }

    int compareAndSwap(const void *origin_addr, const void *compare_addr, void *result_addr, MPI_Datatype datatype,
                       int target_rank, MPI_Aint target_disp)
    {
      return MPI_Compare_and_swap(origin_addr, compare_addr, result_addr, datatype, target_rank, target_disp + OFFSET_BUFFER, window_) ;
    }

    ~CWindowDynamic()
    {
      MPI_Win_unlock_all(window_) ;
    }

  } ;
}



#endif
