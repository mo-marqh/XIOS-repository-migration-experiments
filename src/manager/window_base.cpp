#include "window_base.hpp"
#include "cxios.hpp"

namespace xios
{
    CWindowBase::CWindowBase(MPI_Comm winComm, size_t bufferSize)
    {
      bufferSize_ = bufferSize ;
      windowSize_ = bufferSize_ + OFFSET_BUFFER ;
      MPI_Win_allocate(windowSize_, 1, MPI_INFO_NULL, winComm, &winBuffer_, &window_) ;
      CXios::getMpiGarbageCollector().registerWindow(window_) ;
      MPI_Aint& lock = *((MPI_Aint*)((char*)winBuffer_+OFFSET_LOCK)) ;
      lock=0 ;
      MPI_Win_lock_all(0, window_) ;
      MPI_Barrier(winComm) ;
    }
  
  
}
