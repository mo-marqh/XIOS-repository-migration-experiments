#include "xios_spl.hpp"
#include "exception.hpp"
#include "buffer_server.hpp"


namespace xios
{

  CServerBuffer::CServerBuffer(vector<MPI_Win>& windows, vector<MPI_Aint>& winAddress, int windowsRank, StdSize buffSize) 
  : hasWindows(true), windows_(windows), windowsRank_(windowsRank), winAddress_(winAddress)
  {
    size = 3 * buffSize;
    first = 0;
    current = 1;
    end = size;
    used=0 ;
    MPI_Alloc_mem(size, MPI_INFO_NULL, &buffer) ;
    currentWindows=1 ;
    if (windows[0]==MPI_WIN_NULL && windows[1]==MPI_WIN_NULL) hasWindows=false ;
  }

  CServerBuffer::~CServerBuffer()
  {
    MPI_Free_mem(buffer) ;
  }

  void CServerBuffer::updateCurrentWindows(void)
  {
    if (currentWindows==0) currentWindows=1 ;
    else currentWindows=0 ;
  }

/*
  void CServerBuffer::createWindows(MPI_Comm oneSidedComm)
  {
    MPI_Barrier(oneSidedComm) ;
    MPI_Win_create(NULL, 0, 1, MPI_INFO_NULL, oneSidedComm, &(windows[0])) ;
    MPI_Win_create(NULL, 0, 1, MPI_INFO_NULL, oneSidedComm, &(windows[1])) ;
    hasWindows=true ;
    updateCurrentWindows() ;
    MPI_Barrier(oneSidedComm) ;
  }
*/

/*
  bool CServerBuffer::freeWindows()
  {
    if (hasWindows)
    {
      size_t header[3] ;
      size_t& control=header[2] ;
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE,0,0,windows_[0]) ;
      MPI_Get(&control, 1, MPI_LONG_LONG_INT, windowsRank , 2*sizeof(size_t), 1, MPI_LONG_LONG_INT,windows[0]) ;
      MPI_Win_unlock(0,windows[0]) ;
      if (control==2)  // ok for free windows
      {
        MPI_Win_free( &(windows[0])) ;
        MPI_Win_free( &(windows[1])) ;
        hasWindows=false ;
        return true ;
      }
      else return false ;
    }
    else return true ;
  }
*/

  bool CServerBuffer::isBufferFree(size_t count)
  {
    bool ret ;

    if (count==0) return true ;

    if (current>first)
    {
      if (current+count<size)
      {
        ret=true ;
      }
      else if (current+count==size)
      {
        if (first>0)
        {
          ret=true ;
        }
        else
        {
          ret=false ;
        }
      }
      else
      {
        if (count<first)
        {
          ret=true ;
        }
        else
        {
          ret=false ;
        }
      }
    }
    else
    {
      if (current+count<first)
      {
        ret=true ;
      }
      else
      {
         ret=false ;
      }
    }

    return ret ;
  }

  bool CServerBuffer::isBufferEmpty(void)
  {
    if (used==0) return true ;
    else return false;
  }

  void* CServerBuffer::getBuffer(size_t count)
  {
    char* ret ;

    if (count==0) return buffer+current ;

    if (current>first)
    {
      if (current+count<size)
      {
        ret=buffer+current ;
        current+=count ;
      }
      else if (current+count==size)
      {
        if (first>0)
        {
          ret=buffer+current ;
          current=0 ;
        }
        else
        {
          ERROR("void* CServerBuffer::getBuffer(size_t count)",
                 <<"cannot allocate required size in buffer") ;
        }
      }
      else
      {
        end=current ;
        if (count<first)
        {
          ret=buffer ;
          current=count ;
        }
        else
        {
          ERROR("void* CServerBuffer::getBuffer(size_t count)",
                 <<"cannot allocate required size in buffer") ;
        }
      }
    }
    else
    {
      if (current+count<first)
      {
        ret=buffer+current ;
        current+=count ;
      }
      else
      {
          ERROR("void* CServerBuffer::getBuffer(size_t count)",
                 <<"cannot allocate required size in buffer") ;
      }
    }

    used+=count ;
    return ret ;
  }

  void CServerBuffer::freeBuffer(size_t count)
  {
    if (count==0) return ;

    if (first==end-1)
    {
      first=0 ;
      count-- ;
      end=size ;
    }

    if (first<=current)
    {
      if (first+count <current)
      {
        first+=count ;
      }
      else
      {
          ERROR("void CServerBuffer::freeBuffer(size_t count)",
                 <<"cannot free required size in buffer") ;
      }

    }
    else
    {
      if (first+count<end)
      {
        first+=count ;
      }
      else
      {
          ERROR("void CServerBuffer::freeBuffer(size_t count)",
                 <<"cannot free required size in buffer") ;
      }
    }
    used-=count ;
  }

  bool CServerBuffer::getBufferFromClient(size_t timeLine, char*& buffer, size_t& count)
  {
    if (!hasWindows || resizingBuffer_) return false ;

    
    size_t header[3] ;
    size_t& clientTimeline=header[0] ;
    size_t& clientCount=header[1] ;
    size_t& control=header[2] ;
    bool ok=false ;
    
    MPI_Group group ;
    int groupSize,groupRank ;
    MPI_Win_get_group(windows_[currentWindows], &group) ;
    MPI_Group_size(group, &groupSize) ;
    MPI_Group_rank(group, &groupRank) ;
    
    lockBuffer(); 

// lock is acquired

    MPI_Get(&clientTimeline, 1, MPI_LONG_LONG_INT, windowsRank_ , MPI_Aint_add(winAddress_[currentWindows],0), 1, MPI_LONG_LONG_INT,windows_[currentWindows]) ;
    MPI_Get(&clientCount, 1, MPI_LONG_LONG_INT, windowsRank_ , MPI_Aint_add(winAddress_[currentWindows],sizeof(size_t)), 1, MPI_LONG_LONG_INT,windows_[currentWindows]) ;
    MPI_Win_flush(windowsRank_, windows_[currentWindows]) ;

//    control=1 ;
//    MPI_Put(&control, 1, MPI_LONG_LONG_INT, windowsRank_ , MPI_Aint_add(winAddress_[currentWindows],2*sizeof(size_t)), 1, MPI_LONG_LONG_INT,windows_[currentWindows]) ;
   
//    MPI_Win_unlock(windowsRank_, windows_[currentWindows]) ;
    MPI_Win_flush(windowsRank_, windows_[currentWindows]) ;
//    info(100)<<"getBufferFromClient : windowsRank "<<windowsRank_<<" timeline "<<timeLine<<" clientTimeline "<<clientTimeline<<" clientCount "<<clientCount<<endl ;
    if (timeLine==clientTimeline)
    {
//      info(50)<<"getBufferFromClient timeLine==clientTimeLine: windowsRank "<<windowsRank_<<" timeline "<<timeLine<<" clientTimeline "<<clientTimeline<<" clientCount "<<clientCount<<endl ;
 
//      MPI_Win_lock(MPI_LOCK_EXCLUSIVE,windowsRank_,0,windows_[currentWindows]) ;
      buffer=(char*)getBuffer(clientCount) ;
      count=clientCount ;
      MPI_Get(buffer, clientCount, MPI_CHAR, windowsRank_, MPI_Aint_add(winAddress_[currentWindows],4*sizeof(size_t)) , clientCount, MPI_CHAR, windows_[currentWindows]) ;
      clientTimeline = 0 ;
      clientCount = 0 ;
//      control=0 ;
      MPI_Put(&header[0], 2, MPI_LONG_LONG_INT, windowsRank_, MPI_Aint_add(winAddress_[currentWindows],0) , 2, MPI_LONG_LONG_INT,windows_[currentWindows]) ;

// release lock
     unlockBuffer() ;

      ok=true ;
      char checksum=0 ;
      for(size_t i=0;i<count;i++) checksum=checksum+buffer[i] ;
      char checksumFirst=0 ;
      for(size_t i=5; i<10 && i<count ;i++) checksumFirst=checksumFirst+buffer[i] ;
      char checksumLast=0 ;
      for(size_t i=(count<10)?0:count-10; i<count ; i++) checksumLast=checksumLast+buffer[i] ;
      
      info(40)<<"getBufferFromClient timeLine==clientTimeLine: windowsRank "<<windowsRank_<<" timeline "<<timeLine<<" clientTimeline "
              <<clientTimeline<<" clientCount "<<count<<" checksum "<<(int)checksum<<" "
              <<(int)buffer[0]<<" "<<(int)buffer[1]<<" "<<(int)buffer[2]<<" "<<(int)buffer[3]<<" "<<(int)buffer[4]<<" "<<(int)buffer[5]<<" " 
              <<(int)buffer[6]<<" "<<(int)buffer[7]<<" "<<(int)buffer[8]<<" "<<(int)buffer[9]<<" "<<(int)buffer[10]<<" "<<(int)buffer[11]<<endl ;

    }
    else
    {
      //MPI_Win_lock(MPI_LOCK_EXCLUSIVE,windowsRank_,0,windows_[currentWindows]) ;
      //control=0 ;
      //MPI_Put(&control, 1, MPI_LONG_LONG_INT, windowsRank_ , MPI_Aint_add(winAddress_[currentWindows],2*sizeof(size_t)), 1, MPI_LONG_LONG_INT,windows_[currentWindows]) ;
 
 // release lock
      unlockBuffer() ;
    }

    if (ok) return true ;

    return false ;
  }
  
  void CServerBuffer::lockBuffer(void)
  {
    if (!hasWindows) return ;

    long long int lock=1 ;
    long long int zero=0, one=1 ;
//    control=1 ;
    MPI_Win_lock(MPI_LOCK_EXCLUSIVE,windowsRank_,0,windows_[currentWindows]) ;
    while(lock!=0)
    {
      MPI_Compare_and_swap(&one, &zero, &lock, MPI_LONG_LONG_INT, windowsRank_, MPI_Aint_add(winAddress_[currentWindows],2*sizeof(size_t)),
                           windows_[currentWindows]) ;
      MPI_Win_flush(windowsRank_, windows_[currentWindows]) ;
    }
  }

  void CServerBuffer::unlockBuffer(void)
  {
    if (!hasWindows) return ;
    long long int lock=1 ;
    long long int zero=0, one=1 ;
    
    MPI_Compare_and_swap(&zero, &one, &lock, MPI_LONG_LONG_INT, windowsRank_, MPI_Aint_add(winAddress_[currentWindows],2*sizeof(size_t)),
                          windows_[currentWindows]) ;
    MPI_Win_flush(windowsRank_, windows_[currentWindows]) ; 
    MPI_Win_unlock(windowsRank_,windows_[currentWindows]) ;
  }
  
  void CServerBuffer::notifyClientFinalize(void)
  {
    if (!hasWindows) return ;
    size_t finalize=1 ;
    lockBuffer(); 
// lock is acquired
    MPI_Put(&finalize, 1, MPI_LONG_LONG_INT, windowsRank_ , MPI_Aint_add(winAddress_[currentWindows],3*sizeof(size_t)), 1, MPI_LONG_LONG_INT,windows_[currentWindows]) ;
    unlockBuffer() ;
  }
}
