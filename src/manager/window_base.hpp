#ifndef __WINDOW_BASE_HPP__
#define __WINDOW_BASE_HPP__

#include <map>
#include <string>

#include "exception.hpp"
#include "mpi.hpp"
#include <string>
#include <cstdint>
#include "timer.hpp"

namespace xios
{


  class CWindowBase
  {
    private:
      void * winBuffer_ ;   
      const MPI_Aint OFFSET_LOCK=0 ;
      const int SIZE_LOCK=2*sizeof(uint64_t) ;
      const MPI_Aint OFFSET_BUFFER =  SIZE_LOCK ;
      MPI_Aint bufferSize_ ;
      MPI_Aint windowSize_ ;
      const double maxLatency_ = 1e-3 ; // 1ms latency maximum
      const double minLatency_ = 1e-5 ;
      const uint32_t maxStackValue_ = 1073741824 ; //2^30 
      double timeLastLock_=0 ;
      MPI_Win window_ ;
      std::string name_ ;
      int myRank_ ;

    public :

    CWindowBase(MPI_Comm winComm, size_t bufferSize, const string name);

    void rescaleStack(int rank)
    {
      int32_t inc = -maxStackValue_  ;
      int32_t state ;
      int32_t state_before[4] ;
      int32_t state_after[4] ;
            
      
      lockExclusive(rank) ;
      MPI_Fetch_and_op(&inc, state_before, MPI_INT64_T, rank, OFFSET_LOCK, MPI_NO_OP, window_) ;
      MPI_Fetch_and_op(&inc, state_before+2, MPI_INT64_T, rank, OFFSET_LOCK+sizeof(uint64_t), MPI_NO_OP, window_) ;
      MPI_Win_flush(rank, window_);


      MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, OFFSET_LOCK, MPI_NO_OP, window_) ;
      MPI_Win_flush(rank, window_);
      if (state>=maxStackValue_) 
      {
        MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, OFFSET_LOCK, MPI_SUM, window_) ;
        MPI_Win_flush(rank, window_);
      }
      MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, OFFSET_LOCK+sizeof(int32_t), MPI_NO_OP, window_) ;
      MPI_Win_flush(rank, window_);
      if (state>=maxStackValue_) 
      {
        MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, OFFSET_LOCK+sizeof(int32_t), MPI_SUM, window_) ;
        MPI_Win_flush(rank, window_);
      }

      MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, OFFSET_LOCK+2*sizeof(int32_t), MPI_NO_OP, window_) ;
      MPI_Win_flush(rank, window_);
      if (state>=maxStackValue_) 
      {
        MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, OFFSET_LOCK+2*sizeof(int32_t), MPI_SUM, window_) ;
        MPI_Win_flush(rank, window_);
      }

      MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, OFFSET_LOCK+3*sizeof(int32_t), MPI_NO_OP, window_) ;
      MPI_Win_flush(rank, window_);
      if (state>=maxStackValue_)
      {
        MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, OFFSET_LOCK+3*sizeof(int32_t), MPI_SUM, window_) ;
        MPI_Win_flush(rank, window_);
      }

      MPI_Fetch_and_op(&inc, state_after, MPI_INT64_T, rank, OFFSET_LOCK, MPI_NO_OP, window_) ;
      MPI_Fetch_and_op(&inc, state_after+2, MPI_INT64_T, rank, OFFSET_LOCK+sizeof(uint64_t), MPI_NO_OP, window_) ;
      MPI_Win_flush(rank, window_);

      if (state_after[0]>=maxStackValue_ || state_after[1]>=maxStackValue_ || state_after[2]>=maxStackValue_ || state_after[3]>=maxStackValue_ )
      {
        info(100)<<"CWindowBase::rescaleStack : new stack state for windows rank ; before "<<rank<<" => "<<state_before[0]<<"  "<<state_before[1]<<"  "<<state_before[2]<<"  "<<state_before[3]<<endl ;
        info(100)<<"CWindowBase::rescaleStack : new stack state for windows rank ; after "<<rank<<" => "<<state_after[0]<<"  "<<state_after[1]<<"  "<<state_after[2]<<"  "<<state_after[3]<<endl ;
        info(100)<<"Warning Bad state value !!!!"<< endl ;
      }
      unlockExclusive(rank) ;
    }

    void unlockExclusive(int rank)
    {
      CTimer::get("unlock exclusive").resume();
      uint32_t inc[2] = {1 , 1} ;
      uint32_t state[2] ;
      
      MPI_Fetch_and_op(inc, state, MPI_UINT64_T, rank, OFFSET_LOCK+sizeof(uint64_t), MPI_SUM, window_) ;
      MPI_Win_flush(rank, window_);
      CTimer::get("unlock exclusive").suspend();
      if (state[0] > maxStackValue_ || state[1] > maxStackValue_ ) rescaleStack(rank) ;
    }

    void unlockShared(int rank)
    {
      double t0 = CTimer::getTime(); 
      CTimer::get("unlock shared").resume();
      
      uint32_t inc[2] = {1 , 0} ;
      uint32_t state[2] ;
      
      MPI_Fetch_and_op(inc, state, MPI_UINT64_T, rank, OFFSET_LOCK+sizeof(uint64_t), MPI_SUM, window_) ;
      MPI_Win_flush(rank, window_);
      
      CTimer::get("unlock shared").suspend();

      if (state[0] > maxStackValue_ || state[1] > maxStackValue_ ) rescaleStack(rank) ;
    }

    void lockExclusive(int rank)
    {
      CTimer::get("lock exclusive").resume();
      uint32_t inc[2] = {1 , 1} ;
      uint32_t state[2] ;
      uint32_t res[2] ;
      bool locked ;
      int flag ;

      double time = MPI_Wtime() ;
      while (time-timeLastLock_<minLatency_)
      {
        MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
        time = MPI_Wtime() ;
      }
      timeLastLock_ = time ; 

      locked=false; 
      MPI_Fetch_and_op(inc, state, MPI_UINT64_T, rank, OFFSET_LOCK, MPI_SUM, window_) ;
      MPI_Win_flush(rank, window_);
      
      time =  MPI_Wtime() ;
      MPI_Fetch_and_op(inc, res, MPI_UINT64_T, rank, OFFSET_LOCK+sizeof(uint64_t), MPI_NO_OP, window_) ;
      MPI_Win_flush(rank, window_);
      double lastTime = MPI_Wtime() ;
      double delta = lastTime-time ;
      if (res[0] % maxStackValue_ == state[0] % maxStackValue_) locked=true ;

      while (!locked)
      {
        time = MPI_Wtime() ;
        if (delta > maxLatency_) delta = maxLatency_ ;
        if (time >= lastTime+delta)
        { 
          MPI_Fetch_and_op(inc, res, MPI_UINT64_T, rank, OFFSET_LOCK+sizeof(uint64_t), MPI_NO_OP, window_) ;
          MPI_Win_flush(rank, window_);
          if (res[0] % maxStackValue_ == state[0] % maxStackValue_) locked=true ;
          else
          {
            delta=delta*2.;
            lastTime = time ;
          }      
        }
        else MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);

      }  
      CTimer::get("lock exclusive").suspend();
    }

    void lockShared(int rank)
    {
      double t0 ;
      {
        t0 = CTimer::getTime(); 
        CTimer::get("lock shared").resume();
      }
      uint32_t inc[2] = {1 , 0} ;
      uint32_t state[2] ;
      uint32_t res[2] ;
      bool locked ;

      double time = MPI_Wtime() ;
      int flag ;
      while (time-timeLastLock_<minLatency_)
      {
        MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
        time = MPI_Wtime() ;
      }
      timeLastLock_ = time ; 


      locked=false; 

      MPI_Fetch_and_op(inc, state, MPI_UINT64_T, rank, OFFSET_LOCK, MPI_SUM, window_) ;
      MPI_Win_flush(rank, window_);

      MPI_Fetch_and_op(inc, res, MPI_UINT64_T, rank, OFFSET_LOCK+sizeof(uint64_t), MPI_NO_OP, window_) ;
      MPI_Win_flush(rank, window_);

      double lastTime = MPI_Wtime() ;
      double delta = lastTime-time ;
      if (res[1] % maxStackValue_ == state[1] % maxStackValue_ ) locked=true ;

      while (!locked)
      {
        double time = MPI_Wtime() ;
        if (delta > maxLatency_) delta = maxLatency_ ;
        if (time >= lastTime+delta)
        { 
          MPI_Fetch_and_op(inc, res, MPI_UINT64_T, rank, OFFSET_LOCK+sizeof(uint64_t), MPI_NO_OP, window_) ;
          MPI_Win_flush(rank, window_);
          if (res[1] % maxStackValue_ == state[1] % maxStackValue_ ) locked=true ;
          else
          {
            delta=delta*2.;
            lastTime = time ;
          }      
        }
        else MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
      }

      CTimer::get("lock shared").suspend();
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
