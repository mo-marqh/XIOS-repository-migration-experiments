#include "one_sided_client_buffer.hpp"
#include "event_client.hpp"
#include "timer.hpp"

namespace xios
{
  
  static CLogType logProtocol("log_protocol") ;

  COneSidedClientBuffer::COneSidedClientBuffer(MPI_Comm& interComm, int serverRank, MPI_Comm& commSelf, MPI_Comm& interCommMerged, int intraServerRank) : interComm_(interComm), serverRank_(serverRank)
  {
    
    MPI_Alloc_mem(controlSize_*sizeof(MPI_Aint), MPI_INFO_NULL, &control_) ;
    control_[CONTROL_ADDR] = 0 ;
    control_[CONTROL_FINALIZE] = 0 ;
    sendNewBuffer() ;
    createWindow(commSelf, interCommMerged, intraServerRank ) ;
  }

  void COneSidedClientBuffer::createWindow(MPI_Comm& commSelf, MPI_Comm& interCommMerged, int intraServerRank )
  {
    CTimer::get("create Windows").resume() ;
    MPI_Comm interComm ;
    MPI_Intercomm_create(commSelf, 0, interCommMerged, intraServerRank, 0, &interComm) ;
    MPI_Intercomm_merge(interComm, false, &winComm_) ;
    int rank ;
    MPI_Comm_rank(winComm_,&rank) ;
    info(logProtocol)<<"Windows rank="<<rank<<endl ;
    CXios::getMpiGarbageCollector().registerCommunicator(winComm_) ;
    MPI_Comm_free(&interComm) ;
    
    maxWindows_=MAX_WINDOWS ;    
    windows_.resize(maxWindows_) ;
    usedWindows_.resize(maxWindows_,false) ;
    for(int i=0;i<maxWindows_;++i) 
    {
      MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_, &windows_[i]);
      CXios::getMpiGarbageCollector().registerWindow(windows_[i]) ;
    }
    currentWindow_=-1 ;
    
    MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_, &winControl_);
    CXios::getMpiGarbageCollector().registerWindow(winControl_) ;

    

    CTimer::get("create Windows").suspend() ;

    MPI_Barrier(winComm_) ;
    MPI_Win_attach(winControl_, control_, controlSize_*sizeof(MPI_Aint)) ;
    MPI_Barrier(winComm_) ;
 //   MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, winControl_) ;
 //   MPI_Win_unlock(0,winControl_) ;

  } 



  void COneSidedClientBuffer::newBuffer(size_t size, bool fixed)
  { 
    currentWindow_=(currentWindow_+1)%maxWindows_ ;
    if (usedWindows_[currentWindow_]) 
    {
      ERROR("void COneSidedClientBuffer::newBuffer(size_t size, bool fixed)",<<"Try to alloc buffer to a window already in use"<<endl ) ;
    }
    else usedWindows_[currentWindow_]=true ;
    buffers_.push_back(new CBuffer(windows_[currentWindow_], size, fixed)); 
    currentBuffer_=buffers_.back() ;
    info(logProtocol)<<"   Nb attached memory blocs="<<buffers_.size()<<endl ;
  }

  bool COneSidedClientBuffer::isBufferFree(size_t size)
  {
    if (buffers_.size()>maxWindows_-1) return false ; 
    CBuffer* buffer ;
    if (buffers_.size()==0) return true ;
    else if (!fixed) return true ;
    else 
    {
      buffer = buffers_.back() ;
      if (buffer->remain()>=size) return true ;
      else
      {
        if (buffer->isFixed()) 
        {
          if ( size > buffer->getSize()) return true ;
          else return false ;
        }
        else return true ;
      }
    }
  }
  
  int COneSidedClientBuffer::writeBuffer(char* buffer, size_t size)
  {
    MPI_Aint addr ;
    size_t start ;
    size_t count ;
    int nbBlocs=0 ;

    if (isBufferFree(size))
    {
      while (size > 0)
      {
        if (buffers_.empty())
        {
          if (fixed_) newBuffer(fixedSize_,fixed_) ;
          else
          { 
            if (currentBufferSize_==0) currentBufferSize_=size ;
            newBuffer(currentBufferSize_, fixed_) ;
          }
        }
        CBuffer* currentBuffer = buffers_.back() ;

        MPI_Win_lock(MPI_LOCK_SHARED, 0, 0, windows_[currentWindow_]) ;
        currentBuffer->write(&buffer, size, addr, start, count) ;
        if (count > 0) 
        {
          blocs_.push_back({addr,currentBuffer_, start, count, currentWindow_}) ;
          nbBlocs++ ; 
        }

        currentBuffer->write(&buffer, size, addr, start, count) ;
        MPI_Win_unlock(0,windows_[currentWindow_]) ;

        if (count > 0) 
        {
          blocs_.push_back({addr,currentBuffer_, start, count, currentWindow_}) ;
          nbBlocs++ ; 
        }

        if (size>0) 
        {
          if (fixed_) 
          {
            currentBufferSize_ = fixedSize_ ;
            newBuffer(currentBufferSize_,fixed_) ;
          }
          else
          {
            currentBufferSize_ = max((size_t)(currentBufferSize_*growingFactor_), size) ;
            newBuffer(currentBufferSize_,fixed_) ;
          }
        } 
      }      
      // send message here ?
      return nbBlocs ;
    }
    else return 0 ;
  }

  void COneSidedClientBuffer::freeBuffer(MPI_Aint addr)
  {
//    if (addr != lastFreedBloc_)
    if (addr != 0)
    {
      while(freeBloc(addr)) ;
//      lastFreedBloc_ = addr ;
    }
    
    if (isFinalized_ && !buffers_.empty() && buffers_.front()->getCount()==0) 
    {
      delete buffers_.front() ;
      buffers_.pop_front() ;
    }
  }
  
  bool COneSidedClientBuffer::freeBloc(MPI_Aint addr)
  {
    SBloc& bloc = blocs_.front() ;
    bloc.buffer->free(bloc.start, bloc.count) ;
    if (bloc.buffer->getCount()==0) 
      if (buffers_.size()>1) 
      {  
        usedWindows_[bloc.window]=false ;
        delete buffers_.front() ;
        buffers_.pop_front() ;
      }
    
    if (addr != bloc.addr) 
    {
      blocs_.pop_front() ;
      return true ;
    }
    else 
    {
      blocs_.pop_front() ;
      return false ;
    }

  }

  bool COneSidedClientBuffer::writeEvent(size_t timeline, CEventClient& event)
  {
    size_t size = event.getSize() ;
    if (isBufferFree(size))
    {
      CBufferOut buffer(size) ;
      event.send(timeline, size, &buffer) ;
      size_t bufferSizeBefore = currentBufferSize_ ; 
      int nbBlocs = writeBuffer((char*)buffer.start(),buffer.count()) ;
      if (currentBufferSize_!=bufferSizeBefore) sendResizeBufferEvent(timeline,currentBufferSize_) ;
      sendTimelineEvent(timeline, event.getNbSender(), nbBlocs) ;
      return true ;
    }
    else return false ;
  }

  void COneSidedClientBuffer::eventLoop(void)
  {
    // check to free requests
    int flag ;
    bool out = true;
    SRequest request ; 
    while (!requests_.empty() && out) 
    {
      request = requests_.front() ;
      MPI_Test(&request.mpiRequest, &flag, MPI_STATUS_IGNORE) ;
      if (flag==true)
      {
        delete request.buffer ;
        requests_.pop_front() ;
      }
      else out=false; 
    }
    
    // check to free blocs
    MPI_Aint addr ;
    MPI_Aint finalize ;
    MPI_Win_lock(MPI_LOCK_SHARED, 0, 0, winControl_) ;
    addr = control_[CONTROL_ADDR] ;
    control_[CONTROL_ADDR] = 0 ;
    finalize = control_[CONTROL_FINALIZE] ;
    MPI_Win_unlock(0, winControl_) ;
    freeBuffer(addr) ;
    if (finalize==1) isFinalized_=true ;


  }

  void COneSidedClientBuffer::sendTimelineEvent(size_t timeline, int nbSenders, int nbBlocs)
  {
    ostringstream outStr ;
    SRequest request ;
    request.buffer = new CBufferOut(sizeof(timeline)+sizeof(nbSenders)+sizeof(nbBlocs)+(sizeof(MPI_Aint)+sizeof(int)+sizeof(int))*nbBlocs) ; 
    *(request.buffer)<<timeline<<nbSenders<<nbBlocs ;
    if (info.isActive(logProtocol))  outStr<<"New timeline event sent to server rank "<<serverRank_<<" : timeLine="<<timeline<<"  nbSenders="<<nbSenders<<"  nbBlocs="<<nbBlocs<<endl ;
    auto it = blocs_.end() ;
    for(int i=0 ; i<nbBlocs; ++i,--it) ;
    for(int i=0 ; i<nbBlocs; ++i,++it) 
    {
      *(request.buffer) << it->addr << it->count << it->window;
    
      if (info.isActive(logProtocol))
      {
        size_t checksum=0 ;
        for(size_t j=0;j<it->count;j++) checksum+=((unsigned char*)(it->addr))[j] ;
        outStr<<"Bloc "<<i<<"  addr="<<it->addr<<"  count="<<it->count<<"  checksum="<<checksum<<"  ;  " ;
      }
    }
    MPI_Isend(request.buffer->start(),request.buffer->count(), MPI_CHAR, serverRank_, 20, interComm_, &request.mpiRequest ) ;
    info(logProtocol)<<outStr.str()<<endl ;
    requests_.push_back(request) ;
  }

  void COneSidedClientBuffer::sendResizeBufferEvent(size_t timeline, size_t size)
  {
    SRequest request ;
    request.buffer = new CBufferOut(sizeof(EVENT_BUFFER_RESIZE)+sizeof(timeline)+sizeof(size)) ; 
    *(request.buffer)<<EVENT_BUFFER_RESIZE<<timeline<<size ;
    MPI_Isend(request.buffer->start(),request.buffer->count(), MPI_CHAR, serverRank_, 20, interComm_, &request.mpiRequest ) ;
    requests_.push_back(request) ;
  }

  void COneSidedClientBuffer::sendNewBuffer(void)
  {
    MPI_Aint controlAddr ;
    MPI_Get_address(control_, &controlAddr) ;
    MPI_Send(&controlAddr, 1, MPI_AINT, serverRank_, 20, interComm_) ;
  }

}
