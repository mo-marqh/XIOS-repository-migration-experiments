#include "xios_spl.hpp"
#include "exception.hpp"
#include "buffer_server.hpp"


namespace xios
{

  CServerBuffer::CServerBuffer(StdSize buffSize) : hasWindows(false)
  {
    size = 3 * buffSize;
    first = 0;
    current = 1;
    end = size;
    used=0 ;
    buffer = new char[size]; // use MPI_ALLOC_MEM later?
    currentWindows=0 ;
  }

  CServerBuffer::~CServerBuffer()
  {
    delete [] buffer ;
  }

  void CServerBuffer::updateCurrentWindows(void)
  {
    if (currentWindows==0) currentWindows=1 ;
    else currentWindows=0 ;
  }
  
  void CServerBuffer::createWindows(MPI_Comm oneSidedComm)
  {
    MPI_Barrier(oneSidedComm) ;
    MPI_Win_create(NULL, 0, 1, MPI_INFO_NULL, oneSidedComm, &(windows[0])) ;
    MPI_Win_create(NULL, 0, 1, MPI_INFO_NULL, oneSidedComm, &(windows[1])) ;
    hasWindows=true ;
    MPI_Barrier(oneSidedComm) ;
  }

  bool CServerBuffer::freeWindows()
  {
    if (hasWindows)
    {
      size_t header[3] ;
      size_t& control=header[2] ;
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE,0,0,windows[0]) ;
      MPI_Get(&control, 1, MPI_LONG_LONG_INT, 0 , 2*sizeof(size_t), 1, MPI_LONG_LONG_INT,windows[0]) ;
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
    if (!hasWindows) return false ;

    
    size_t header[3] ;
    size_t& clientTimeline=header[0] ;
    size_t& clientCount=header[1] ;
    size_t& control=header[2] ;
    bool ok=false ;
    
    MPI_Win_lock(MPI_LOCK_EXCLUSIVE,0,0,windows[currentWindows]) ;

    MPI_Get(&clientTimeline, 1, MPI_LONG_LONG_INT, 0 , 0, 1, MPI_LONG_LONG_INT,windows[currentWindows]) ;
    MPI_Get(&clientCount, 1, MPI_LONG_LONG_INT, 0 , 1*sizeof(size_t), 1, MPI_LONG_LONG_INT,windows[currentWindows]) ;
    control=1 ;
    MPI_Put(&control, 1, MPI_LONG_LONG_INT, 0 , 2*sizeof(size_t), 1, MPI_LONG_LONG_INT,windows[currentWindows]) ;
   
    MPI_Win_unlock(0,windows[currentWindows]) ;

    if (timeLine==clientTimeline)
    {

      MPI_Win_lock(MPI_LOCK_EXCLUSIVE,0,0,windows[currentWindows]) ;
      buffer=(char*)getBuffer(clientCount) ;
      count=clientCount ;
      MPI_Get(buffer, clientCount, MPI_CHAR, 0, 3*sizeof(size_t) , clientCount, MPI_CHAR, windows[currentWindows]) ;
      clientTimeline = 0 ;
      clientCount = 0 ;
      control=0 ;
      MPI_Put(&header[0], 3, MPI_LONG_LONG_INT, 0, 0 , 3, MPI_LONG_LONG_INT,windows[currentWindows]) ;
  
      MPI_Win_unlock(0,windows[currentWindows]) ;
      ok=true ;
    }
    else
    {
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE,0,0,windows[currentWindows]) ;
      control=0 ;
      MPI_Put(&control, 1, MPI_LONG_LONG_INT, 0 , 2*sizeof(size_t), 1, MPI_LONG_LONG_INT,windows[currentWindows]) ;
      MPI_Win_unlock(0,windows[currentWindows]) ;
    }

    if (ok) return true ;

    return false ;
  }
    
   
}
