#ifndef __WINDOW_DYNAMIC_VIEW_HPP__
#define __WINDOW_DYNAMIC_VIEW_HPP__

#include <map>
//#include "exception.hpp"
#include "mpi.hpp"
#include "cxios.hpp"
#include <iostream>
#include "window_dynamic.hpp"

namespace xios
{


  class CWindowDynamicView
  {
    private:
      CWindowDynamic* winDynamic_ ;   
      size_t tag_ ;

    public :
    
    CWindowDynamicView(CWindowDynamic* winDynamic, size_t tag) : winDynamic_(winDynamic), tag_(tag) {}
    
    void allocateBuffer(MPI_Aint size)
    {
      winDynamic_->allocateBuffer(size, tag_) ;
    }  

    void create(MPI_Comm winComm)
    {
      winDynamic_->create(winComm);
    }
    
    void lockAll()
    {
      winDynamic_->lockAll();
    }

    void unlockAll()
    {
      winDynamic_->unlockAll();
    }

    void setWinBufferAddress(MPI_Aint addr, int rank)
    {
      winDynamic_->setWinBufferAddress(addr, rank, tag_);
    }
    
    MPI_Aint getWinBufferAddress()
    {
      return winDynamic_->getWinBufferAddress(tag_);
    }

    void* getBufferAddress()
    {
      return winDynamic_->getBufferAddress(tag_);
    }
    
    void unlockExclusive(int rank)
    {
      winDynamic_->unlockExclusive(rank, tag_);
    }

    void unlockShared(int rank)
    {
      winDynamic_->unlockShared(rank, tag_);
    }

    void lockExclusive(int rank)
    {
      winDynamic_->lockExclusive(rank, tag_);
    }

    void lockShared(int rank)
    {
      winDynamic_->lockShared(rank, tag_);
    }
    
    void attach(MPI_Aint size) 
    {
      winDynamic_->attach(size, tag_);
    }
    
    void attach() 
    {
      winDynamic_->attach(tag_) ;
    }

    void detach() 
    {
      winDynamic_->detach(tag_); 
    }

    int flush(int rank)
    {
      return winDynamic_->flush(rank);
    }

    int put(const void *origin_addr, int origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
            int target_count, MPI_Datatype target_datatype)
    {
      return winDynamic_->put(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype);
    }

    int get(void *origin_addr, int origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
            int target_count, MPI_Datatype target_datatype)
    {
      return winDynamic_->get(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype);
    }

    int compareAndSwap(const void *origin_addr, const void *compare_addr, void *result_addr, MPI_Datatype datatype,
                       int target_rank, MPI_Aint target_disp)
    {
      return winDynamic_->compareAndSwap(origin_addr, compare_addr, result_addr, datatype, target_rank, target_disp);
    }

    ~CWindowDynamicView()
    {

    }

  } ;
}



#endif
