#ifndef __WINDOW_BASE_HPP__
#define __WINDOW_BASE_HPP__

#include <map>
#include <string>

#include "exception.hpp"
#include "mpi.hpp"
#include <string>

namespace xios
{


  class CWindowBase
  {
    private:
      void * winBuffer_ ;   
      const MPI_Aint OFFSET_LOCK=0 ;
      const int SIZE_LOCK=sizeof(long) ;
      const MPI_Aint OFFSET_BUFFER =  SIZE_LOCK ;
      MPI_Aint bufferSize_ ;
      MPI_Aint windowSize_ ;
      const double maxLatency_ = 1e-3 ; // 1ms latency maximum
      MPI_Win window_ ;
      std::string name_ ;

    public :

    CWindowBase(MPI_Comm winComm, size_t bufferSize, const string name);

    bool tryLockExclusive(int rank)
    {
      long lock = 1;
      long unlock = 0;
      long state;

      int flag ;
      MPI_Compare_and_swap(&lock, &unlock, &state, MPI_LONG, rank, OFFSET_LOCK, window_) ;
      MPI_Win_flush(rank, window_);

      bool locked = (state == unlock) ;
      return locked ;
    }

    bool tryLockShared(int rank, MPI_Op op)
    {
      long one = 0x100000000;
      long res;

      MPI_Fetch_and_op(&one, &res, MPI_LONG, rank, OFFSET_LOCK, op, window_);
      MPI_Win_flush(rank, window_);
      
      bool locked =  ! (res & 1) ;
      return locked ;
    }

    void unlockExclusive(int rank)
    {
      int lock = 1;
      int unlock = 0;
      int state;

      MPI_Win_flush(rank, window_);
      MPI_Compare_and_swap(&unlock, &lock, &state, MPI_INT, rank, OFFSET_LOCK, window_) ;
      MPI_Win_flush(rank, window_);
      if (lock != state) ERROR("CWindowBase::unlockWindowExclusive",<<"unlockWindow failed: bad state"<<endl) ; 
    }

    void unlockShared(int rank)
    {
      int minusone = -1;
      int res;
      MPI_Fetch_and_op(&minusone, &res, MPI_INT, rank, OFFSET_LOCK+4, MPI_SUM, window_);
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
    
    int fetchAndOp(const void *origin_addr, void *result_addr, MPI_Datatype datatype, int target_rank, MPI_Aint target_disp, MPI_Op op)
    {
      return MPI_Fetch_and_op(origin_addr, result_addr, datatype, target_rank, target_disp + OFFSET_BUFFER, op, window_ ) ;
    }
    
    int compareAndSwap(const void *origin_addr, const void *compare_addr, void *result_addr, MPI_Datatype datatype,
                       int target_rank, MPI_Aint target_disp)
    {
      return MPI_Compare_and_swap(origin_addr, compare_addr, result_addr, datatype, target_rank, target_disp + OFFSET_BUFFER, window_) ;
    }

    ~CWindowBase()
    {
      MPI_Win_unlock_all(window_);
      info(100)<<"CWindowBase destructor : "<<name_<<endl ;
    }

  } ;
}



#endif
