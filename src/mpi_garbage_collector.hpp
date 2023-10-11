#ifndef __MPI_GARBAGE_COLLECTOR_HPP__
#define __MPI_GARBAGE_COLLECTOR_HPP__
#include "mpi.hpp"
#include "backtrace.hpp"
namespace xios
{
  class CMpiGarbageCollector
  {
    
    private:

      struct SType
      {
        enum {COMM, WIN} type ;
        MPI_Comm comm ;
        MPI_Win win ;
        std::string str;
      } ;
      std::list<SType> stack_ ;
    
    public:

     void registerCommunicator(MPI_Comm& comm, std::string str) { stack_.push_front(SType{SType::COMM, comm, MPI_WIN_NULL, str}) ;}
     void registerCommunicator(MPI_Comm& comm) { stack_.push_front(SType{SType::COMM, comm, MPI_WIN_NULL, MemCppTrack::backTrace(2)}) ;}
     void registerWindow(MPI_Win& win, std::string str) { stack_.push_front(SType{SType::WIN, MPI_COMM_NULL, win, str}) ;}
     void registerWindow(MPI_Win& win) { stack_.push_front(SType{SType::WIN, MPI_COMM_NULL, win, MemCppTrack::backTrace(2)}) ;}
      void release(void)
      {
        for( auto& it : stack_) 
          if (it.type==SType::COMM) xios::MPI_Comm_free(&it.comm);
          else if (it.type==SType::WIN) MPI_Win_free(&it.win);
        stack_.clear();
      }
  } ;

}

#endif
