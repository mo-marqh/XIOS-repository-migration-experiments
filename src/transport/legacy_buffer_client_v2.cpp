#include "xios_spl.hpp"
#include "exception.hpp"
#include "log.hpp"
#include "buffer_out.hpp"
#include "legacy_buffer_client_v2.hpp"
#include "cxios.hpp"
#include "mpi.hpp"
#include "tracer.hpp"
#include "timeline_events.hpp"
#include "timer.hpp"

namespace xios
{
  size_t CLegacyClientBufferV2::maxRequestSize = 0;

  CLegacyClientBufferV2::CLegacyClientBufferV2(MPI_Comm interComm, int serverRank, StdSize bufferSize, bool hasWindows, CWindowDynamic* winDynamic_)
    : interComm(interComm)
    , serverRank(serverRank)
    , bufferSize(bufferSize)
    , maxEventSize(0)
    , current(0)
    , count(0)
    , pending(false)
    , hasWindows_(hasWindows) 
  {
      if (hasWindows_)
      { 
        int intercommRank, intercommSize ;
        MPI_Comm_rank(interComm, &clientRank_) ; 
        MPI_Comm_size(interComm, &intercommSize) ; 
        //winDynamic_= new CWindowDynamic() ;
        windows_.resize(2) ;
        windows_[0] = new CWindowDynamicView(winDynamic_, serverRank) ;
        windows_[0]->allocateBuffer(bufferSize+headerSize_) ;
        bufferHeader[0] = (char*) windows_[0]->getBufferAddress() ;
        windows_[1] = new CWindowDynamicView(winDynamic_, intercommSize + serverRank) ;
        windows_[1]->allocateBuffer(bufferSize+headerSize_) ;
        bufferHeader[1] = (char*) windows_[1]->getBufferAddress() ;
        attachWindows() ;
      }
      else
      {
        MPI_Alloc_mem(bufferSize+headerSize_, MPI_INFO_NULL, &bufferHeader[0]) ;
        MPI_Alloc_mem(bufferSize+headerSize_, MPI_INFO_NULL, &bufferHeader[1]) ;
      }

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

    
      retBuffer = new CBufferOut(buffer[current], bufferSize);
      info(10) << "CLegacyClientBufferV2: allocated 2 x " << bufferSize << " bytes for server " << serverRank << endl;
  }

  MPI_Aint CLegacyClientBufferV2::getWinAddress(int i)
  {
    MPI_Aint address ;
    MPI_Get_address(bufferHeader[i], &address) ;
    return address ;
  }
  
  MPI_Aint CLegacyClientBufferV2::getWinBufferAddress(int i)
  {
    return windows_[i]->getWinBufferAddress() ;
  }

  void CLegacyClientBufferV2::attachWindows()
  {
    isAttachedWindows_=true ;

    if (hasWindows_)
    {  
      MPI_Aint buffSize=bufferSize+headerSize_ ;
      //winDynamic_->create(winComm) ;
      windows_[0]->attach() ;
      windows_[0]->lockExclusive(clientRank_) ;

      windows_[1]->attach() ;
      windows_[1]->lockExclusive(clientRank_) ;
      
      windows_[0]->unlockExclusive(clientRank_) ;
      windows_[1]->unlockExclusive(clientRank_) ;

      

    } 

  }


  CLegacyClientBufferV2::~CLegacyClientBufferV2()
  {
    if (hasWindows_)
    {
      windows_[0]->detach() ;
      windows_[1]->detach() ;
      delete windows_[0] ;
      delete windows_[1] ;
      //delete winDynamic_ ;
    }
    else 
    {
      MPI_Free_mem(bufferHeader[0]) ;
      MPI_Free_mem(bufferHeader[1]) ;
    }
    delete retBuffer;
  }

  void CLegacyClientBufferV2::lockBuffer(void)
  {
    CTimer::get("lock buffer").resume();
    if (isAttachedWindows_)    
    {
      if (winState[current]==true) ERROR("CLegacyClientBufferV2::lockBuffer(void)",<<"Try lo lock client buffer but winState said it is already locked") ;
      //MPI_Win_lock(MPI_LOCK_EXCLUSIVE,clientRank_, 0, windows_[current]) ;
      windows_[current]->lockExclusive(clientRank_) ;
      winState[current]=true ;
    }
    CTimer::get("lock buffer").suspend();
  }

  void CLegacyClientBufferV2::unlockBuffer(void)
  {
    CTimer::get("unlock buffer").resume();
    if (isAttachedWindows_)
    {
      if (winState[current]==false) ERROR("CLegacyClientBufferV2::lockBuffer(void)",<<"Try lo unlock client buffer but winState said it is already unlocked") ;
      //MPI_Win_unlock(clientRank_, windows_[current]) ;
      windows_[current]->unlockExclusive(clientRank_) ;
      winState[current]=false ;
    }
    CTimer::get("unlock buffer").suspend();
  }

  StdSize CLegacyClientBufferV2::remain(void)
  {
    return bufferSize - count;
  }

  bool CLegacyClientBufferV2::isBufferFree(StdSize size)
  {
    if (!isAttachedWindows_) return false;

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


  CBufferOut* CLegacyClientBufferV2::getBuffer(size_t timeLine, StdSize size)
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
      ERROR("CBufferOut* CLegacyClientBufferV2::getBuffer(StdSize size)",
            << "Not enough space in buffer, this should not have happened...");
      return NULL;
    }
  }

  void CLegacyClientBufferV2::infoBuffer(void)
  {
      
      char checksum=0 ;
      for(size_t i=0;i<*bufferCount[current];i++) checksum=checksum+buffer[current][i] ;
 
      char checksumFirst=0 ;
      for(size_t i=5; i<10 && i<*bufferCount[current] ;i++) checksumFirst=checksumFirst+buffer[current][i] ;
 
      char checksumLast=0 ;
      for(size_t i=(*bufferCount[current]<10)?0:*bufferCount[current]-10; i<*bufferCount[current] ; i++) checksumLast=checksumLast+buffer[current][i] ;
 
      info(45)<<"CLegacyClientBufferV2::infoBuffer "<<" clientRank_ "<<clientRank_<<" serverRank "<<serverRank <<" current "<<current<<" WinState "<<winState[current]
              <<" firstTimeLine "<<*firstTimeLine[current]<<" count "<<*bufferCount[current]<<" checksum "<<(int)checksum<<" "
              <<(int)buffer[current][0]<<" "<<(int)buffer[current][1]<<" "<<(int)buffer[current][2]<<" "<<(int)buffer[current][3]<<" "<<(int)buffer[current][4]<<" "<<(int)buffer[current][5]<<" "
              <<(int)buffer[current][6]<<" "<<(int)buffer[current][7]<<" "<<(int)buffer[current][8]<<" "<<(int)buffer[current][9]<<" "<<(int)buffer[current][10]<<" "<<(int)buffer[current][11]<<endl ;

  }

  bool CLegacyClientBufferV2::checkBuffer(bool send)
  {
    MPI_Status status;
    int flag;
    MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
 
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

  void CLegacyClientBufferV2::resizeBufferNotify(void)
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

  void CLegacyClientBufferV2::resizeBuffer(size_t newSize)
  {

    bufferSize=newSize ;

    if (hasWindows_)
    { 
      windows_[0]->detach();
      windows_[1]->detach();
      
      windows_[0]->attach(bufferSize+headerSize_) ;
      bufferHeader[0] = (char*) windows_[0] -> getBufferAddress() ;
      windows_[1]->attach(bufferSize+headerSize_) ;
      bufferHeader[1] = (char*) windows_[1] -> getBufferAddress() ;
    }
    
    
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
    
    if (hasWindows_)
    {  

      windows_[0]->lockExclusive(clientRank_) ;
      windows_[1]->lockExclusive(clientRank_) ;
      
      windows_[1]->unlockExclusive(clientRank_) ;
      windows_[0]->unlockExclusive(clientRank_) ;
    } 

    lockBuffer() ;
 
    int size=sizeof(int)+2*sizeof(size_t)+2*sizeof(MPI_Aint) ;
    CBufferOut* bufOut = this->getBuffer(timelineEventChangeBufferSize, size);
    bufOut->put(size);
    bufOut->put(timelineEventChangeBufferSize);
    bufOut->put(newBufferSize_);
    
    bufOut->put(this->getWinBufferAddress(0));
    bufOut->put(this->getWinBufferAddress(1));

    resizingBufferStep_=4;
    unlockBuffer() ;
    info(100)<<"CLegacyClientBufferV2::resizeBuffer(size_t newSize) : resizing buffer of server "<<serverRank<<" ; new size : "<<newSize<<" ; winAdress[0] "<<this->getWinBufferAddress(0)<<" winAdress[1] "<<this->getWinBufferAddress(1)<<endl;
  }

  bool CLegacyClientBufferV2::hasPendingRequest(void)
  {
   
    lockBuffer() ;
    count=*bufferCount[current] ;
    unlockBuffer() ;

    return (pending || count > 0);
  }

  bool CLegacyClientBufferV2::isNotifiedChangeBufferSize(void)
  {
   
    bool ret ;
    lockBuffer() ;
    ret=*notify[current] == notifyResizeBuffer_ ? true : false ;
    if (ret || !hasWindows_) 
    {
      *notify[current] = notifyNothing_ ;
      resizingBufferStep_=3;  
    }
    unlockBuffer() ;

    return ret;
  }

  bool CLegacyClientBufferV2::isNotifiedFinalized(void)
  {
    if (!isFinalized_)
    {
      double time=MPI_Wtime() ;
//      if (time - lastCheckedNotify_ > latency_)
      {
        int flag ;
        MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
        lockBuffer() ;
        isFinalized_=*notify[current] == notifyFinalize_ ? true : false ;
        unlockBuffer() ;
        lastCheckedNotify_=time ;
      }
    }
    return isFinalized_ ;
  }

}
