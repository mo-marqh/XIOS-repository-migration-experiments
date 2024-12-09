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
      std::map<size_t, void *> winBuffer_ ;   
      const MPI_Aint OFFSET_LOCK=0 ;
      const int SIZE_LOCK=2*sizeof(uint64_t) ;
      const MPI_Aint OFFSET_BUFFER =  SIZE_LOCK ;
      const MPI_Aint OFFSET_BUFFER_SIZE = SIZE_LOCK   ;
      std::map<size_t, MPI_Aint> bufferSize_ ;
      const double maxLatency_ = 1e-3 ; // 1ms latency maximum
      const double minLatency_ = 1e-5 ;
      const uint32_t maxStackValue_ = 1073741824 ; //2^30 
      double timeLastLock_=0 ;
      MPI_Win window_ ;
      std::map<size_t, MPI_Aint> windowSize_ ;
      int winCommRank_ ;
      std::map< std::pair<int, size_t>, MPI_Aint> winBufferAddress_ ;
      
    public:

    void allocateBuffer(MPI_Aint size, size_t tag)
    {
      bufferSize_[tag] = size ;
      windowSize_[tag] = size+OFFSET_BUFFER_SIZE ;
      MPI_Alloc_mem(windowSize_[tag], MPI_INFO_NULL, &winBuffer_[tag]) ;
      uint64_t* lock = (uint64_t*)((char*)(winBuffer_[tag])+OFFSET_LOCK) ;
      lock[0]=0 ; lock[1]=0 ; 
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
      MPI_Win_lock_all(MPI_MODE_NOCHECK, window_) ;
    }

    void unlockAll()
    {
      MPI_Win_unlock_all(window_) ;
    }

    void setWinBufferAddress(MPI_Aint addr, int rank, size_t tag)
    {
      winBufferAddress_[{rank,tag}]=addr ;
    }
    
    MPI_Aint getWinBufferAddress(size_t tag)
    {
      MPI_Aint ret ;
      MPI_Get_address(winBuffer_[tag], &ret) ;
      return ret ;
    }

    void* getBufferAddress(size_t tag)
    {
      return static_cast<char*>(winBuffer_[tag])+OFFSET_BUFFER_SIZE ;
    }
    
    
    void rescaleStack(int rank, size_t tag)
    {
      int32_t inc = -maxStackValue_  ;
      int32_t state ;
      int32_t state_before[4] ;
      int32_t state_after[4] ;
            
      
      lockExclusive(rank, tag) ;
      MPI_Fetch_and_op(&inc, state_before, MPI_INT64_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK, MPI_NO_OP, window_) ;
      MPI_Fetch_and_op(&inc, state_before+2, MPI_INT64_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK+sizeof(uint64_t), MPI_NO_OP, window_) ;
      MPI_Win_flush(rank, window_);


      MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK, MPI_NO_OP, window_) ;
      MPI_Win_flush(rank, window_);
      if (state>=maxStackValue_) 
      {
        MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK, MPI_SUM, window_) ;
        MPI_Win_flush(rank, window_);
      }
      MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK+sizeof(int32_t), MPI_NO_OP, window_) ;
      MPI_Win_flush(rank, window_);
      if (state>=maxStackValue_) 
      {
        MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK+sizeof(int32_t), MPI_SUM, window_) ;
        MPI_Win_flush(rank, window_);
      }

      MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK+2*sizeof(int32_t), MPI_NO_OP, window_) ;
      MPI_Win_flush(rank, window_);
      if (state>=maxStackValue_) 
      {
        MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK+2*sizeof(int32_t), MPI_SUM, window_) ;
        MPI_Win_flush(rank, window_);
      }

      MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK+3*sizeof(int32_t), MPI_NO_OP, window_) ;
      MPI_Win_flush(rank, window_);
      if (state>=maxStackValue_)
      {
        MPI_Fetch_and_op(&inc, &state, MPI_INT32_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK+3*sizeof(int32_t), MPI_SUM, window_) ;
        MPI_Win_flush(rank, window_);
      }

      MPI_Fetch_and_op(&inc, state_after, MPI_INT64_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK, MPI_NO_OP, window_) ;
      MPI_Fetch_and_op(&inc, state_after+2, MPI_INT64_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK+sizeof(uint64_t), MPI_NO_OP, window_) ;
      MPI_Win_flush(rank, window_);

      if (state_after[0]>=maxStackValue_ || state_after[1]>=maxStackValue_ || state_after[2]>=maxStackValue_ || state_after[3]>=maxStackValue_ )
      {
        info(100)<<"CWindowBase::rescaleStack : new stack state for windows rank ; before "<<rank<<" => "<<state_before[0]<<"  "<<state_before[1]<<"  "<<state_before[2]<<"  "<<state_before[3]<<endl ;
        info(100)<<"CWindowBase::rescaleStack : new stack state for windows rank ; after "<<rank<<" => "<<state_after[0]<<"  "<<state_after[1]<<"  "<<state_after[2]<<"  "<<state_after[3]<<endl ;
        info(100)<<"Warning Bad state value !!!!"<< endl ;
      }
      unlockExclusive(rank, tag) ;
    }

    void unlockExclusive(int rank, size_t tag)
    {
      CTimer::get("unlock exclusive").resume();
      uint32_t inc[2] = {1 , 1} ;
      uint32_t state[2] ;
      
      MPI_Fetch_and_op(inc, state, MPI_UINT64_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK+sizeof(uint64_t), MPI_SUM, window_) ;
      MPI_Win_flush(rank, window_);
      CTimer::get("unlock exclusive").suspend();
      if (state[0] > maxStackValue_ || state[1] > maxStackValue_ ) rescaleStack(rank, tag) ;
    }

    void unlockShared(int rank, size_t tag)
    {
      double t0 = CTimer::getTime(); 
      CTimer::get("unlock shared").resume();
      
      uint32_t inc[2] = {1 , 0} ;
      uint32_t state[2] ;
      
      MPI_Fetch_and_op(inc, state, MPI_UINT64_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK+sizeof(uint64_t), MPI_SUM, window_) ;
      MPI_Win_flush(rank, window_);
      
      CTimer::get("unlock shared").suspend();

      if (state[0] > maxStackValue_ || state[1] > maxStackValue_ ) rescaleStack(rank, tag) ;
    }

    void lockExclusive(int rank, size_t tag)
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
      MPI_Fetch_and_op(inc, state, MPI_UINT64_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK, MPI_SUM, window_) ;
      MPI_Win_flush(rank, window_);
      
      time =  MPI_Wtime() ;
      MPI_Fetch_and_op(inc, res, MPI_UINT64_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK+sizeof(uint64_t), MPI_NO_OP, window_) ;
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
          MPI_Fetch_and_op(inc, res, MPI_UINT64_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK+sizeof(uint64_t), MPI_NO_OP, window_) ;
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

    void lockShared(int rank, size_t tag)
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

      MPI_Fetch_and_op(inc, state, MPI_UINT64_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK, MPI_SUM, window_) ;
      MPI_Win_flush(rank, window_);

      MPI_Fetch_and_op(inc, res, MPI_UINT64_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK+sizeof(uint64_t), MPI_NO_OP, window_) ;
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
          MPI_Fetch_and_op(inc, res, MPI_UINT64_T, rank, winBufferAddress_[{rank,tag}]+OFFSET_LOCK+sizeof(uint64_t), MPI_NO_OP, window_) ;
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

  
    void attach(MPI_Aint size, size_t tag) 
    {
      
      windowSize_[tag] = size+OFFSET_BUFFER_SIZE ;
      MPI_Alloc_mem(windowSize_[tag], MPI_INFO_NULL, &winBuffer_[tag]) ;
      uint64_t* lock = (uint64_t*)((char*)(winBuffer_[tag])+OFFSET_LOCK) ;
      lock[0]=0 ; lock[1]=0 ; 

      info(100)<<"Attach Buffer  =>  "<<winBuffer_[tag]<<endl ;
      MPI_Win_attach(window_, winBuffer_[tag], windowSize_[tag]) ;
      setWinBufferAddress(getWinBufferAddress(tag),winCommRank_,tag) ;
    }
    
    void attach(size_t tag) 
    {
      info(100)<<"Attach Buffer  =>  "<<winBuffer_[tag]<<endl ;
      MPI_Win_attach(window_, winBuffer_[tag], windowSize_[tag]) ;
      setWinBufferAddress(getWinBufferAddress(tag),winCommRank_, tag) ;
    }

    void detach(size_t tag) 
    {
      info(100)<<"Detach Buffer  =>  "<<winBuffer_[tag]<<endl ;
      MPI_Win_detach(window_, winBuffer_[tag]) ;
      MPI_Free_mem(winBuffer_[tag]) ;        
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
