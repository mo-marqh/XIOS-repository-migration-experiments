#include "window_base.hpp"
#include "cxios.hpp"

namespace xios
{
    CWindowBase::CWindowBase(MPI_Comm winComm, size_t bufferSize, const string name)  : name_(name)
    {
      bufferSize_ = bufferSize ;
      windowSize_ = bufferSize_ + OFFSET_BUFFER ;
      MPI_Win_allocate(windowSize_, 1, MPI_INFO_NULL, winComm, &winBuffer_, &window_) ;
      CXios::getMpiGarbageCollector().registerWindow(window_) ;
      uint64_t* lock = (uint64_t*)((char*)winBuffer_+OFFSET_LOCK) ;
      lock[0]=0 ; lock[1]=0 ;
      MPI_Win_lock_all(MPI_MODE_NOCHECK, window_) ;
      info(100)<<"CWindowBase constructor : "<<name_<<endl ;
      MPI_Comm_rank(winComm, &myRank_) ;
      MPI_Barrier(winComm) ;
    }
  
  
}
