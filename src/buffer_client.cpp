#include "xios_spl.hpp"
#include "exception.hpp"
#include "log.hpp"
#include "buffer_out.hpp"
#include "buffer_client.hpp"
#include "cxios.hpp"
#include "mpi.hpp"
#include "tracer.hpp"
#include "timeline_events.hpp"

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

      MPI_Alloc_mem(bufferSize+headerSize, MPI_INFO_NULL, &bufferHeader[0]) ;
      MPI_Alloc_mem(bufferSize+headerSize, MPI_INFO_NULL, &bufferHeader[1]) ;
      buffer[0] = bufferHeader[0]+headerSize ;
      buffer[1] = bufferHeader[1]+headerSize ;
      firstTimeLine[0]=(size_t*)bufferHeader[0] ;
      firstTimeLine[1]=(size_t*)bufferHeader[1] ;
      bufferCount[0]=(size_t*)bufferHeader[0] +1 ;
      bufferCount[1]=(size_t*)bufferHeader[1] +1 ;
      control[0]=(size_t*)bufferHeader[0] +2 ;
      control[1]=(size_t*)bufferHeader[1] +2 ;
      finalize[0]=(size_t*)bufferHeader[0] +3 ;
      finalize[1]=(size_t*)bufferHeader[1] +3 ;

      *firstTimeLine[0]=0 ;
      *firstTimeLine[1]=0 ;
      *bufferCount[0]=0 ;
      *bufferCount[1]=0 ;
      *control[0]=0 ;
      *control[1]=0 ;
      *finalize[0]=0 ;
      *finalize[1]=0 ;
      winState[0]=false ;
      winState[1]=false ;


    if (hasWindows)
    {  
    
      MPI_Aint buffSize=bufferSize+headerSize ;
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

/*  void CClientBuffer::createWindows(MPI_Comm oneSidedComm)
  {
    MPI_Barrier(oneSidedComm) ;
    MPI_Win_create(bufferHeader[0], bufferSize+headerSize, 1, MPI_INFO_NULL, oneSidedComm, &(windows[0])) ;
    MPI_Win_create(bufferHeader[1], bufferSize+headerSize, 1, MPI_INFO_NULL, oneSidedComm, &(windows[1])) ;

    MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, windows[0]) ;
    *firstTimeLine[0]=0 ;
    *bufferCount[0]=0 ;
    *control[0]=0 ;
    MPI_Win_unlock(0, windows[0]) ;

    MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, windows[1]) ;
    *firstTimeLine[1]=0 ;
    *bufferCount[1]=0 ;
    *control[1]=0 ;
    MPI_Win_unlock(0, windows[1]) ;
    winState[0]=false ;
    winState[1]=false ;
    MPI_Barrier(oneSidedComm) ;
    hasWindows=true ;
  }
*/

/*  
  void CClientBuffer::freeWindows()
  {
    if (hasWindows)
    {
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, windows_[0]) ;
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, windows_[1]) ;
      *control[0]=2 ;
      *control[1]=2 ;
      MPI_Win_unlock(0, windows_[1]) ;
      MPI_Win_unlock(0, windows_[0]) ;
      
      MPI_Win_free(&windows_[0]) ;
      MPI_Win_free(&windows_[1]) ;
      hasWindows=false ;
    }
  }
*/ 
  void CClientBuffer::lockBuffer(void)
  {
    if (hasWindows)
    {
   //   MPI_Win_lock(MPI_LOCK_EXCLUSIVE, clientRank_, 0, windows_[current]) ;
      long long int lock=1 ;
      long long int zero=0, one=1 ;
     
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE,clientRank_, 0, windows_[current]) ;
     
      while(lock!=0)
      {
        MPI_Compare_and_swap(&one, &zero, &lock, MPI_LONG_LONG_INT, clientRank_, MPI_Aint_add(getWinAddress(current),2*sizeof(size_t)),
                             windows_[current]) ;
        MPI_Win_flush(clientRank_, windows_[current]) ;
      }

//      info(100)<<"Buffer locked "<<&windows_<<"  "<<current<<endl ;
      winState[current]=true ;
    }
  }

  void CClientBuffer::unlockBuffer(void)
  {
    if (hasWindows)
    {
      long long int lock=1 ;
      long long int zero=0, one=1 ;

      MPI_Compare_and_swap(&zero, &one, &lock, MPI_LONG_LONG_INT, clientRank_, MPI_Aint_add(getWinAddress(current),2*sizeof(size_t)),
                             windows_[current]) ;
      MPI_Win_unlock(clientRank_, windows_[current]) ;

 //     info(100)<<"Buffer unlocked "<<&windows_<<"  "<<current<<endl ;
      winState[current]=false ;
    }
  }

  StdSize CClientBuffer::remain(void)
  {
    return bufferSize - count;
  }

  bool CClientBuffer::isBufferFree(StdSize size)
  {
//    bool loop=true ;
//    while (loop) 
//    {
//      lockBuffer();
//      if (*control[current]==0) loop=false ; // attemp to read from server ?
//      else unlockBuffer() ;
//    }
  
    lockBuffer();
    count=*bufferCount[current] ;
    
    if (resizingBufferStep_ > 0 ) return false ;

    if (size > bufferSize)
    {
      // ERROR("bool CClientBuffer::isBufferFree(StdSize size)",
      //      << "The requested size (" << size << " bytes) is too big to fit the buffer (" << bufferSize << " bytes), please increase the client buffer size." << endl);
      resizingBufferStep_=1 ;
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
/*      info(50)<<"CClientBuffer::getBuffer "<<" clientRank_ "<<clientRank_<<" serverRank "<<serverRank <<" current "<<current
              <<" size "<<size<<" timeLine "<< timeLine <<" firstTimeLine "<<*firstTimeLine[current]<<" count "<<*bufferCount[current]<<endl ;
      if (!winState[current]) info(40)<<"CClientBuffer::getBuffer "<<" Windows Not Locked... "<<" clientRank_ "<<clientRank_<<" serverRank "<<serverRank <<" current "<<current
              <<" size "<<size<<" timeLine "<< timeLine <<" firstTimeLine "<<*firstTimeLine[current]<<" count "<<*bufferCount[current]<<endl ;*/
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
        lockBuffer() ;
 //       if (*control[current]==0 && bufferCount[current] > 0)
        if (*bufferCount[current] > 0)
        {
          MPI_Issend(buffer[current], count, MPI_CHAR, serverRank, 20, interComm, &request);
          if (resizingBufferStep_==3) resizingBufferStep_=0 ;
          pending = true;
//          *control[current]=0 ;
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
        }
      }
      else
      {
        if (resizingBufferStep_==2) resizeBuffer(newBufferSize_) ;
        if (resizingBufferStep_==1) resizeBufferNotify() ;
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
    MPI_Alloc_mem(bufferSize+headerSize, MPI_INFO_NULL, &bufferHeader[0]) ;
    MPI_Alloc_mem(bufferSize+headerSize, MPI_INFO_NULL, &bufferHeader[1]) ;
    buffer[0] = bufferHeader[0]+headerSize ;
    buffer[1] = bufferHeader[1]+headerSize ;
    firstTimeLine[0]=(size_t*)bufferHeader[0] ;
    firstTimeLine[1]=(size_t*)bufferHeader[1] ;
    bufferCount[0]=(size_t*)bufferHeader[0] +1 ;
    bufferCount[1]=(size_t*)bufferHeader[1] +1 ;
    control[0]=(size_t*)bufferHeader[0] +2 ;
    control[1]=(size_t*)bufferHeader[1] +2 ;
    finalize[0]=(size_t*)bufferHeader[0] +3 ;
    finalize[1]=(size_t*)bufferHeader[1] +3 ;

    *firstTimeLine[0]=0 ;
    *firstTimeLine[1]=0 ;
    *bufferCount[0]=0 ;
    *bufferCount[1]=0 ;
    *control[0]=0 ;
    *control[1]=0 ;
    *finalize[0]=0 ;
    *finalize[1]=0 ;
    winState[0]=false ;
    winState[1]=false ;
    current=0 ;
    
    if (hasWindows)
    {  
    
      MPI_Win_attach(windows_[0], bufferHeader[0], bufferSize+headerSize) ;
      MPI_Win_attach(windows_[1], bufferHeader[1], bufferSize+headerSize) ;
          
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

    resizingBufferStep_=3;
    unlockBuffer() ;
  }

  bool CClientBuffer::hasPendingRequest(void)
  {
   
    lockBuffer() ;
    count=*bufferCount[current] ;
    unlockBuffer() ;

    return (pending || count > 0);
  }

  bool CClientBuffer::isNotifiedFinalized(void)
  {
   
    bool ret ;
    lockBuffer() ;
    ret=*finalize[current] == 1 ? true : false ;
    unlockBuffer() ;

    return ret;
  }

}
