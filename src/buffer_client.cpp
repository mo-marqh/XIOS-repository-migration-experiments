#include "xios_spl.hpp"
#include "exception.hpp"
#include "log.hpp"
#include "buffer_out.hpp"
#include "buffer_client.hpp"
#include "cxios.hpp"
#include "mpi.hpp"
#include "tracer.hpp"
#include "timeline_events.hpp"
#include "timer.hpp"

namespace xios
{
  size_t CClientBuffer::maxRequestSize = 0;

  CClientBuffer::CClientBuffer(MPI_Comm interComm, vector<MPI_Win>& windows, int clientRank, int serverRank, StdSize bufferSize, StdSize estimatedMaxEventSize)
    : interComm(interComm)
    , clientRank_(clientRank)
    , serverRank(serverRank)
    , bufferSize(bufferSize)
    , estimatedMaxEventSize(estimatedMaxEventSize)
    , maxEventSize(0)
    , current(0)
    , count(0)
    , pending(false)
    , hasWindows(false) 
    , windows_(windows)
  {
    if (windows[0]==MPI_WIN_NULL && windows[1]==MPI_WIN_NULL) hasWindows=false ;
    else hasWindows=true ;

      MPI_Alloc_mem(bufferSize+headerSize_, MPI_INFO_NULL, &bufferHeader[0]) ;
      MPI_Alloc_mem(bufferSize+headerSize_, MPI_INFO_NULL, &bufferHeader[1]) ;
      buffer[0] = bufferHeader[0]+headerSize_ ;
      buffer[1] = bufferHeader[1]+headerSize_ ;
      firstTimeLine[0]=(size_t*)bufferHeader[0] + timeLineOffset_ ;
      firstTimeLine[1]=(size_t*)bufferHeader[1] + timeLineOffset_ ;
      bufferCount[0]=(size_t*)bufferHeader[0] + countOffset_ ;
      bufferCount[1]=(size_t*)bufferHeader[1] + countOffset_ ;
      control[0]=(size_t*)bufferHeader[0] + controlOffset_ ;
      control[1]=(size_t*)bufferHeader[1] + controlOffset_ ;
      notify[0]=(size_t*)bufferHeader[0] + notifyOffset_ ;
      notify[1]=(size_t*)bufferHeader[1] + notifyOffset_ ;

      *firstTimeLine[0]=0 ;
      *firstTimeLine[1]=0 ;
      *bufferCount[0]=0 ;
      *bufferCount[1]=0 ;
      *control[0]=0 ;
      *control[1]=0 ;
      *notify[0]=notifyNothing_ ;
      *notify[1]=notifyNothing_ ;
      winState[0]=false ;
      winState[1]=false ;


    if (hasWindows)
    {  
    
      MPI_Aint buffSize=bufferSize+headerSize_ ;
      MPI_Win_attach(windows_[0], bufferHeader[0], buffSize) ;
      MPI_Win_attach(windows_[1], bufferHeader[1], buffSize) ;
    
      MPI_Group group ;
      int groupSize,groupRank ;
      MPI_Win_get_group(windows_[0], &group) ;
      MPI_Group_size(group, &groupSize) ;
      MPI_Group_rank(group, &groupRank) ;
      if (groupRank!=clientRank_) ERROR("CClientBuffer::CClientBuffer",<< " ClientRank != groupRank "<<clientRank_<<" "<<groupRank);

      MPI_Win_get_group(windows_[1], &group) ;
      MPI_Group_size(group, &groupSize) ;
      MPI_Group_rank(group, &groupRank) ;
      if (groupRank!=clientRank_) ERROR("CClientBuffer::CClientBuffer",<< " ClientRank != groupRank "<<clientRank_<<" "<<groupRank);

      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, clientRank_, 0, windows_[0]) ;
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, clientRank_, 0, windows_[1]) ;

      MPI_Win_unlock(clientRank_, windows_[1]) ;
      MPI_Win_unlock(clientRank_, windows_[0]) ;
    } 
    retBuffer = new CBufferOut(buffer[current], bufferSize);
    info(10) << "CClientBuffer: allocated 2 x " << bufferSize << " bytes for server " << serverRank << endl;
  }

  MPI_Aint CClientBuffer::getWinAddress(int i)
  {
     MPI_Aint address ;
     
     if (hasWindows) MPI_Get_address(bufferHeader[i], &address) ;
     else address=0 ;

     return address ;
  }

  CClientBuffer::~CClientBuffer()
  {
     //freeWindows() ;
     if (hasWindows)
     {
       MPI_Win_detach(windows_[0],bufferHeader[0]);
       MPI_Win_detach(windows_[1],bufferHeader[1]);
       MPI_Free_mem(bufferHeader[0]) ;
       MPI_Free_mem(bufferHeader[1]) ;
     }
     delete retBuffer;
  }

  void CClientBuffer::lockBuffer(void)
  {
    CTimer::get("lock buffer").resume();
    if (hasWindows)
    {
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE,clientRank_, 0, windows_[current]) ;
      winState[current]=true ;
    }
    CTimer::get("lock buffer").suspend();
  }

  void CClientBuffer::unlockBuffer(void)
  {
    CTimer::get("unlock buffer").resume();
    if (hasWindows)
    {
      MPI_Win_unlock(clientRank_, windows_[current]) ;
      winState[current]=false ;
    }
    CTimer::get("unlock buffer").suspend();
  }

  StdSize CClientBuffer::remain(void)
  {
    return bufferSize - count;
  }

  bool CClientBuffer::isBufferFree(StdSize size)
  {
  
    lockBuffer();
    count=*bufferCount[current] ;
    
    if (resizingBufferStep_ > 0 ) return false ;

    if (size > bufferSize)
    {
      resizingBufferStep_=1 ;
      *firstTimeLine[current]=0 ;
      newBufferSize_=size ;
      return false ;
    }

    if (size > maxEventSize)
    {
      maxEventSize = size;

      if (size > estimatedMaxEventSize)
        error(0) << "WARNING: Unexpected event of size " << size << " for server " << serverRank
                 << " (estimated max event size = " << estimatedMaxEventSize << ")" << std::endl;

      if (size > maxRequestSize) maxRequestSize = size;
    }
    
    if (size > remain())
    {
      if (isGrowableBuffer_)
      {
        resizingBufferStep_ = 1 ;
        *firstTimeLine[current]=0 ;
        newBufferSize_ = (count+size)*growFactor_ ;
      }  
      return false ;
    }
    else return true ;
  }


  CBufferOut* CClientBuffer::getBuffer(size_t timeLine, StdSize size)
  {
    if (size <= remain())
    {
      retBuffer->realloc(buffer[current] + count, size);
      count += size;
      if (*firstTimeLine[current]==0) *firstTimeLine[current]=timeLine ;
      *bufferCount[current]=count ;
      return retBuffer;
    }
    else
    {
      ERROR("CBufferOut* CClientBuffer::getBuffer(StdSize size)",
            << "Not enough space in buffer, this should not have happened...");
      return NULL;
    }
  }

  void CClientBuffer::infoBuffer(void)
  {
      
      char checksum=0 ;
      for(size_t i=0;i<*bufferCount[current];i++) checksum=checksum+buffer[current][i] ;
 
      char checksumFirst=0 ;
      for(size_t i=5; i<10 && i<*bufferCount[current] ;i++) checksumFirst=checksumFirst+buffer[current][i] ;
 
      char checksumLast=0 ;
      for(size_t i=(*bufferCount[current]<10)?0:*bufferCount[current]-10; i<*bufferCount[current] ; i++) checksumLast=checksumLast+buffer[current][i] ;
 
      info(45)<<"CClientBuffer::infoBuffer "<<" clientRank_ "<<clientRank_<<" serverRank "<<serverRank <<" current "<<current<<" WinState "<<winState[current]
              <<" firstTimeLine "<<*firstTimeLine[current]<<" count "<<*bufferCount[current]<<" checksum "<<(int)checksum<<" "
              <<(int)buffer[current][0]<<" "<<(int)buffer[current][1]<<" "<<(int)buffer[current][2]<<" "<<(int)buffer[current][3]<<" "<<(int)buffer[current][4]<<" "<<(int)buffer[current][5]<<" "
              <<(int)buffer[current][6]<<" "<<(int)buffer[current][7]<<" "<<(int)buffer[current][8]<<" "<<(int)buffer[current][9]<<" "<<(int)buffer[current][10]<<" "<<(int)buffer[current][11]<<endl ;

  }

  bool CClientBuffer::checkBuffer(bool send)
  {
    MPI_Status status;
    int flag;
    
    MPI_Win_lock(MPI_LOCK_EXCLUSIVE, clientRank_, 0, windows_[0]) ;
    MPI_Win_unlock(clientRank_, windows_[0]) ;

    MPI_Win_lock(MPI_LOCK_EXCLUSIVE, clientRank_, 0, windows_[1]) ;
    MPI_Win_unlock(clientRank_, windows_[1]) ;

    if (pending)
    {
      traceOff();
      MPI_Test(&request, &flag, &status);
      traceOn();
      if (flag == true) pending = false;
    }

    if (!pending)
    {
      if (!send && resizingBufferStep_==0 ) return false ;

      if (count > 0)
      {
        double time=MPI_Wtime() ;
        if (time - lastCheckedWithNothing_ > latency_)
        {
          lockBuffer() ;
          if (*bufferCount[current] > 0)
          {
            MPI_Issend(buffer[current], count, MPI_CHAR, serverRank, 20, interComm, &request);
            if (resizingBufferStep_==4) resizingBufferStep_=0 ;
            pending = true;
            *firstTimeLine[current]=0 ;
            *bufferCount[current]=0 ;

             unlockBuffer() ;

            if (current == 1) current = 0;
            else current = 1;
            count = 0;
          }
          else 
          {
            unlockBuffer() ;
            lastCheckedWithNothing_ = time ;
          }
        }
      }
      else
      {
        if (resizingBufferStep_==1) resizeBufferNotify() ;
        else if (resizingBufferStep_==2) isNotifiedChangeBufferSize() ;
        else if (resizingBufferStep_==3) resizeBuffer(newBufferSize_) ;
      }
    }

    return pending;
  }

  void CClientBuffer::resizeBufferNotify(void)
  {
    // notify server of changing buffers size
    lockBuffer() ;
    int size=sizeof(int)+sizeof(size_t) ;
    CBufferOut* bufOut = this->getBuffer(timelineEventNotifyChangeBufferSize, size);
    bufOut->put(size);
    bufOut->put(timelineEventNotifyChangeBufferSize);
    resizingBufferStep_ = 2 ;
    unlockBuffer() ;
  }

  void CClientBuffer::resizeBuffer(size_t newSize)
  {

    if (hasWindows)
    { 
      MPI_Win_detach(windows_[0], bufferHeader[0]) ;
      MPI_Win_detach(windows_[1], bufferHeader[1]) ;
    }
    MPI_Free_mem(bufferHeader[0]) ;
    MPI_Free_mem(bufferHeader[1]) ;

    bufferSize=newSize ;
    MPI_Alloc_mem(bufferSize+headerSize_, MPI_INFO_NULL, &bufferHeader[0]) ;
    MPI_Alloc_mem(bufferSize+headerSize_, MPI_INFO_NULL, &bufferHeader[1]) ;
    buffer[0] = bufferHeader[0]+headerSize_ ;
    buffer[1] = bufferHeader[1]+headerSize_ ;
    firstTimeLine[0]=(size_t*)bufferHeader[0] + timeLineOffset_;
    firstTimeLine[1]=(size_t*)bufferHeader[1] + timeLineOffset_;
    bufferCount[0]=(size_t*)bufferHeader[0] + countOffset_ ;
    bufferCount[1]=(size_t*)bufferHeader[1] + countOffset_ ;
    control[0]=(size_t*)bufferHeader[0] + controlOffset_ ;  // control=0 => nothing ; control=1 => changeBufferSize
    control[1]=(size_t*)bufferHeader[1] + controlOffset_ ;
    notify[0]=(size_t*)bufferHeader[0] + notifyOffset_ ;
    notify[1]=(size_t*)bufferHeader[1] + notifyOffset_ ;

    *firstTimeLine[0]=0 ;
    *firstTimeLine[1]=0 ;
    *bufferCount[0]=0 ;
    *bufferCount[1]=0 ;
    *control[0]=0 ;
    *control[1]=0 ;
    *notify[0] = notifyNothing_ ;
    *notify[1] = notifyNothing_ ;
    winState[0]=false ;
    winState[1]=false ;
    current=0 ;
    
    if (hasWindows)
    {  
    
      MPI_Win_attach(windows_[0], bufferHeader[0], bufferSize+headerSize_) ;
      MPI_Win_attach(windows_[1], bufferHeader[1], bufferSize+headerSize_) ;
          
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, clientRank_, 0, windows_[0]) ;
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, clientRank_, 0, windows_[1]) ;

      MPI_Win_unlock(clientRank_, windows_[1]) ;
      MPI_Win_unlock(clientRank_, windows_[0]) ;
    } 

    lockBuffer() ;
 
    int size=sizeof(int)+2*sizeof(size_t)+2*sizeof(MPI_Aint) ;
    CBufferOut* bufOut = this->getBuffer(timelineEventChangeBufferSize, size);
    bufOut->put(size);
    bufOut->put(timelineEventChangeBufferSize);
    bufOut->put(newBufferSize_);
    bufOut->put(this->getWinAddress(0));
    bufOut->put(this->getWinAddress(1));

    resizingBufferStep_=4;
    unlockBuffer() ;
    info(100)<<"CClientBuffer::resizeBuffer(size_t newSize) : resizing buffer of server "<<serverRank<<" ; new size : "<<newSize<<" ; winAdress[0] "<<this->getWinAddress(0)<<" winAdress[1] "<<this->getWinAddress(1)<<endl;
  }

  bool CClientBuffer::hasPendingRequest(void)
  {
   
    lockBuffer() ;
    count=*bufferCount[current] ;
    unlockBuffer() ;

    return (pending || count > 0);
  }

  bool CClientBuffer::isNotifiedChangeBufferSize(void)
  {
   
    bool ret ;
    lockBuffer() ;
    ret=*notify[current] == notifyResizeBuffer_ ? true : false ;
    if (ret) 
    {
      *notify[current] = notifyNothing_ ;
      resizingBufferStep_=3;  
    }
    unlockBuffer() ;

    return ret;
  }

  bool CClientBuffer::isNotifiedFinalized(void)
  {
   
    bool ret ;
    lockBuffer() ;
    ret=*notify[current] == notifyFinalize_ ? true : false ;
    unlockBuffer() ;

    return ret;
  }

}
