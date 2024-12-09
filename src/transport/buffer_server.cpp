#include "xios_spl.hpp"
#include "exception.hpp"
#include "buffer_server.hpp"
#include "timer.hpp"
#include "window_dynamic.hpp"


namespace xios
{

  CServerBuffer::CServerBuffer(int clientRank, vector<CWindowDynamicView*>& windows, vector<MPI_Aint>& winAddress, int windowsRank, StdSize buffSize) 
  : hasWindows(true), clientRank_(clientRank), windows_(windows), windowsRank_(windowsRank), winAddress_(winAddress)
  {
    size = 3 * buffSize;
    first = 0;
    current = 1;
    end = size;
    used=0 ;
    MPI_Alloc_mem(size, MPI_INFO_NULL, &buffer) ;
    currentWindows=1 ;
    if (windows[0]==nullptr && windows[1]==nullptr) hasWindows=false ;
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
      used-- ;
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

  void CServerBuffer::popBuffer(size_t count)
  {
    if (count==0) return ;

    if (current==0)  
    {
      current = end ;
      end=size ;
    }
    

    if (first<=current)
    {
      if (current-count >first)
      {
        current-=count ;
      }
      else
      {
          ERROR("void CServerBuffer::popBuffer(size_t count)",
                 <<"cannot pop required size in buffer") ;
      }

    }
    else
    {
      if (current-count>=0)
      {
        current-=count ;
      }
      else
      {
          ERROR("void CServerBuffer::freeBuffer(size_t count)",
                 <<"cannot pop required size in buffer") ;
      }
    }
    used-=count ;
  }

  bool CServerBuffer::getBufferFromClient(size_t timeLine, char*& buffer, size_t& count)
  {
    count = -1 ;
    if (!hasWindows || resizingBuffer_) return false ;
    double time=MPI_Wtime() ;
    if (time-bufferFromClientTime_ < bufferFromClientLatency_ ) return false;
    bufferFromClientTime_ = time ;
    CTimer::get("getBufferFromClient").resume() ;   
    size_t clientTimeline ;
    size_t clientCount ;
    bool ok=false ;
    
    info(100)<<"getBufferFromClient : check data in client buffer  : clientRank "<<clientRank_<<" timeline "<<timeLine<<" ??"<< endl ;
    lockBuffer(); 
    CTimer::get("getBufferFromClient_locked").resume() ;   
// lock is acquired

    windows_[currentWindows]->get(&clientTimeline, 1, MPI_LONG_LONG_INT, windowsRank_ , MPI_Aint_add(winAddress_[currentWindows],timeLineOffset_*sizeof(size_t)), 1, MPI_LONG_LONG_INT) ;
    windows_[currentWindows]->get(&clientCount, 1, MPI_LONG_LONG_INT, windowsRank_ , MPI_Aint_add(winAddress_[currentWindows],countOffset_*sizeof(size_t)), 1, MPI_LONG_LONG_INT) ;
    windows_[currentWindows]->flush(windowsRank_) ;
   
    if (timeLine==clientTimeline)
    {
      buffer=(char*)getBuffer(clientCount) ;
      count=clientCount ;
      windows_[currentWindows]->get(buffer, clientCount, MPI_CHAR, windowsRank_, MPI_Aint_add(winAddress_[currentWindows],4*sizeof(size_t)) , clientCount, MPI_CHAR) ;
      
      clientTimeline = 0 ;
      clientCount = 0 ;
      windows_[currentWindows]->put(&clientTimeline, 1, MPI_LONG_LONG_INT, windowsRank_ , MPI_Aint_add(winAddress_[currentWindows],timeLineOffset_*sizeof(size_t)), 1, MPI_LONG_LONG_INT) ;
      windows_[currentWindows]->put(&clientCount, 1, MPI_LONG_LONG_INT, windowsRank_ , MPI_Aint_add(winAddress_[currentWindows],countOffset_*sizeof(size_t)), 1, MPI_LONG_LONG_INT) ;

// release lock
      CTimer::get("getBufferFromClient_locked").suspend() ;   
      unlockBuffer() ;

      ok=true ;
      char checksum=0 ;
      for(size_t i=0;i<count;i++) checksum=checksum+buffer[i] ;
      char checksumFirst=0 ;
      for(size_t i=5; i<10 && i<count ;i++) checksumFirst=checksumFirst+buffer[i] ;
      char checksumLast=0 ;
      for(size_t i=(count<10)?0:count-10; i<count ; i++) checksumLast=checksumLast+buffer[i] ;
      
      info(40)<<"getBufferFromClient timeLine==clientTimeLine: clientRank "<<clientRank_<<" timeline "<<timeLine<<" clientTimeline "
              <<clientTimeline<<" clientCount "<<count<<" checksum "<<(int)checksum<<" "
              <<(int)buffer[0]<<" "<<(int)buffer[1]<<" "<<(int)buffer[2]<<" "<<(int)buffer[3]<<" "<<(int)buffer[4]<<" "<<(int)buffer[5]<<" " 
              <<(int)buffer[6]<<" "<<(int)buffer[7]<<" "<<(int)buffer[8]<<" "<<(int)buffer[9]<<" "<<(int)buffer[10]<<" "<<(int)buffer[11]<<endl ;

    }
    else
    {
      count=0 ;
 
 // release lock
      CTimer::get("getBufferFromClient_locked").suspend() ; 
      unlockBuffer() ;
    }
    CTimer::get("getBufferFromClient").suspend() ;  
    info(100)<<"getBufferFromClient : check data in client buffer ==> done"<<endl ;

    if (ok) return true ;

    return false ;
  }
  
  void CServerBuffer::lockBuffer(void)
  {
    if (!hasWindows) return ;
    //MPI_Win_lock(MPI_LOCK_EXCLUSIVE,windowsRank_,0,windows_[currentWindows]) ;
    windows_[currentWindows]->lockExclusive(windowsRank_) ;
  }

  void CServerBuffer::unlockBuffer(void)
  {
    if (!hasWindows) return ;
    windows_[currentWindows]->unlockExclusive(windowsRank_) ;
  }
  
  void CServerBuffer::notifyClientFinalize(void)
  {
    if (!hasWindows) return ;
    size_t notify=notifyFinalize_ ;
    lockBuffer(); 
// lock is acquired
    windows_[currentWindows]->put(&notify, 1, MPI_LONG_LONG_INT, windowsRank_ , MPI_Aint_add(winAddress_[currentWindows], notifyOffset_*sizeof(size_t)), 1, MPI_LONG_LONG_INT) ;
    unlockBuffer() ;
  }

  void CServerBuffer::notifyBufferResizing(void)
  {
    resizingBuffer_=true ;
    if (!hasWindows) return ;
    size_t notify=notifyResizeBuffer_ ;
    lockBuffer(); 
// lock is acquired
    windows_[currentWindows]->put(&notify, 1, MPI_LONG_LONG_INT, windowsRank_ , MPI_Aint_add(winAddress_[currentWindows], notifyOffset_*sizeof(size_t)), 1, MPI_LONG_LONG_INT) ;
    unlockBuffer() ;
  }
}
