#include "p2p_client_buffer.hpp"
#include "event_client.hpp"
#include "timer.hpp"

namespace xios
{
  
  extern CLogType logProtocol;
  extern CLogType logTimers ;

  CP2pClientBuffer::CP2pClientBuffer(MPI_Comm& interComm, int serverRank, MPI_Comm& commSelf, MPI_Comm& interCommMerged, int intraServerRank) : interComm_(interComm), serverRank_(serverRank), interCommMerged_(interCommMerged), intraServerRank_(intraServerRank)
  {
    
    //MPI_Alloc_mem(controlSize_*sizeof(MPI_Aint), MPI_INFO_NULL, &control_) ;
    //control_[CONTROL_ADDR] = 0 ;
    //control_[CONTROL_FINALIZE] = 0 ;
    sendNewBuffer() ;
    createWindow(commSelf, interCommMerged, intraServerRank ) ;
    char dummy ;
    MPI_Irecv(&dummy, 0, MPI_CHAR, intraServerRank, 22, interCommMerged, &finalizeRequest_) ;
  }

  void CP2pClientBuffer::createWindow(MPI_Comm& commSelf, MPI_Comm& interCommMerged, int intraServerRank )
  {
    if (info.isActive(logTimers)) CTimer::get("create Windows").resume() ;
    //MPI_Comm interComm ;
    //xios::MPI_Intercomm_create(commSelf, 0, interCommMerged, intraServerRank, 0, &interComm) ;
    //xios::MPI_Intercomm_merge(interComm, false, &winComm_) ;
    //int rank ;
    //MPI_Comm_rank(winComm_,&rank) ;
    //info(logProtocol)<<"Windows rank="<<rank<<endl ;
    //CXios::getMpiGarbageCollector().registerCommunicator(winComm_) ;
    //xios::MPI_Comm_free(&interComm) ;
    
    maxWindows_=MAX_WINDOWS ;    
    windows_.resize(maxWindows_) ;
    usedWindows_.resize(maxWindows_,false) ;
    //for(int i=0;i<maxWindows_;++i) 
    //{
    //  MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_, &windows_[i]);
    //  CXios::getMpiGarbageCollector().registerWindow(windows_[i]) ;
    //}
    currentWindow_=-1 ;
    currentMirror_=-1 ;
    
    //MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_, &winControl_);
    //CXios::getMpiGarbageCollector().registerWindow(winControl_) ;

    //MPI_Barrier(winComm_) ;
    //MPI_Win_attach(winControl_, control_, controlSize_*sizeof(MPI_Aint)) ;
    //MPI_Barrier(winComm_) ;
    if (info.isActive(logTimers)) CTimer::get("create Windows").suspend() ;
 
 //   MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, winControl_) ;
 //   MPI_Win_unlock(0,winControl_) ;

  } 



  void CP2pClientBuffer::newBuffer(size_t size, bool fixed)
  { 
    currentMirror_++;
    currentWindow_=(currentWindow_+1)%maxWindows_ ;
    //if (usedWindows_[currentWindow_]) 
    //{
    //  ERROR("void CP2pClientBuffer::newBuffer(size_t size, bool fixed)",<<"Try to alloc buffer to a window already in use"<<endl ) ;
    //}
    //else usedWindows_[currentWindow_]=true ;
    buffers_.push_back(new CBuffer(windows_[currentMirror_], size, fixed)); 
    currentBuffer_=buffers_.back() ;
    info(logProtocol)<<"   Nb attached memory blocs="<<buffers_.size()<<", size of the last buffer = " << size << endl ;
  }

  bool CP2pClientBuffer::isBufferFree(size_t size)
  {
    if (sentBlocRequest_.size()> maxSentBlocRequests_) return false ;
    if (buffers_.size()>maxWindows_-1) return false ; 
    CBuffer* buffer ;
    if (buffers_.size()==0) return true ;
    else if (!fixed_) return true ;
    else 
    {
      buffer = buffers_.back() ;
      if (buffer->remain()>=size) return true ;
      else
      {
        if (buffer->isFixed()) 
        {
          if ( size > buffer->getSize()) return true ;
	  else if ( currentBufferSize_ < fixedSize_ ) return true ;
          else return false ;
        }
        else return true ;
      }
    }
  }
  
  int CP2pClientBuffer::writeBuffer(char* buffer, size_t size)
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
          if (fixed_) {
            currentBufferSize_=fixedSize_;
            newBuffer(fixedSize_,fixed_) ;
          }
          else
          { 
            if (currentBufferSize_==0) currentBufferSize_=size ;
            newBuffer(currentBufferSize_, fixed_) ;
          }
        }
        else if ((currentBufferSize_ < fixedSize_)&&(fixed_))
        {
          // Forces to allocate the fixed buffer if defined
          //   without this test, could be not done if each field size is < currentBufferSize_
          currentBufferSize_ = fixedSize_ ;
          newBuffer(currentBufferSize_, fixed_) ;
        }
        CBuffer* currentBuffer = buffers_.back() ;

        //MPI_Win_lock(MPI_LOCK_SHARED, 0, 0, windows_[currentWindow_]) ;
        currentBuffer->write(&buffer, size, addr, start, count) ;
        if (count > 0) 
        {
          //info(logProtocol) << "Using currentMirror_ 1 : "<<currentMirror_ << endl;
          blocs_.push_back({addr,currentBuffer_, start, static_cast<int>(count), currentMirror_}) ;
          nbBlocs++ ; 
        }

        currentBuffer->write(&buffer, size, addr, start, count) ;
        //MPI_Win_unlock(0,windows_[currentWindow_]) ;

        if (count > 0) 
        {
          //info(logProtocol) << "Using currentMirror_ 2 : "<<currentMirror_ << endl;
          blocs_.push_back({addr,currentBuffer_, start, static_cast<int>(count), currentMirror_}) ;
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

  void CP2pClientBuffer::freeBuffer(MPI_Aint addr)
  {
    if (addr != 0)
    {
      while(freeBloc(addr)) ;
    }
    
    if (isFinalized_ && !buffers_.empty() && buffers_.front()->getCount()==0) 
    {
      delete buffers_.front() ;
      buffers_.pop_front() ;
    }
  }
  
  bool CP2pClientBuffer::freeBloc(MPI_Aint addr)
  {
    SBloc& bloc = blocs_.front() ;
    
    if (info.isActive(logProtocol))
    {
      size_t checksum=0 ;
      for(size_t j=0;j<bloc.count;j++) checksum+=((unsigned char*)(bloc.addr))[j] ;
      info(logProtocol)<<"free bloc sent to server rank "<<serverRank_<<" : addr="<<bloc.addr<<"  start="<<bloc.start<<"  count="<<bloc.count<<"  checksum="<<checksum<<endl ;
    }

    bloc.buffer->free(bloc.start, bloc.count) ;
    if (bloc.buffer->getCount()==0) 
      if (buffers_.size()>1) 
      {  
        //usedWindows_[bloc.window]=false ;
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

  bool CP2pClientBuffer::writeEvent(size_t timeline, CEventClient& event)
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

  void CP2pClientBuffer::eventLoop(void)
  {
    // check to free requests
    int flag ;
    bool out = true;
    SRequest request ; 
    while (!requests_.empty() && out) 
    {
      request = requests_.front() ;
      if (info.isActive(logProtocol)) CTimer::get("sendTimelineEvent : MPI_Test").resume() ;
      MPI_Test(&request.mpiRequest, &flag, MPI_STATUS_IGNORE) ;
      if (info.isActive(logProtocol)) CTimer::get("sendTimelineEvent : MPI_Test").suspend() ;
      if (flag==true)
      {
        delete request.buffer ;
        requests_.pop_front() ;
      }
      else out=false; 
    }
    
    // check to free blocs
//    MPI_Aint addr ;
//    MPI_Aint finalize ;
//    MPI_Win_lock(MPI_LOCK_SHARED, 0, 0, winControl_) ;
//    addr = control_[CONTROL_ADDR] ;
//    control_[CONTROL_ADDR] = 0 ;
//    finalize = control_[CONTROL_FINALIZE] ;
//    MPI_Win_unlock(0, winControl_) ;
    MPI_Aint addr=0 ;
    auto it=sentBlocRequest_.begin() ;
    for( ; it!=sentBlocRequest_.end() ; ++it)
    {
      int flag=true ;
      MPI_Status status ; 
      MPI_Test(&it->mpiRequest, &flag, &status) ;
      if (flag==false) 
      {
//        ++it ;
        break ;
      }
      else addr = it->addr;
    }
    if (addr!=0) sentBlocRequest_.erase(sentBlocRequest_.begin(), it) ;
    freeBuffer(addr) ;

    // if (finalize==1) isFinalized_=true ;
    listenFinalize() ;
  }

  void CP2pClientBuffer::listenFinalize(void)
  {
    if (!isFinalized_)
    {
      int flag ;
      MPI_Status status ;
      MPI_Test(&finalizeRequest_,&flag, &status) ;
      if (flag) isFinalized_=true;
    }
  }

  void CP2pClientBuffer::sendTimelineEvent(size_t timeline, int nbSenders, int nbBlocs)
  {
    ostringstream outStr ;
    SRequest request ;
    request.buffer = new CBufferOut(sizeof(timeline)+sizeof(nbSenders)+sizeof(nbBlocs)+(sizeof(MPI_Aint)+sizeof(int)+sizeof(int)+sizeof(size_t))*nbBlocs) ; 
    *(request.buffer)<<timeline<<nbSenders<<nbBlocs ;
    if (info.isActive(logProtocol))  outStr<<"New timeline event sent to server rank "<<serverRank_<<" : timeLine="<<timeline<<"  nbSenders="<<nbSenders<<"  nbBlocs="<<nbBlocs<<endl ;
    auto it = blocs_.end() ;
    for(int i=0 ; i<nbBlocs; ++i,--it) ;
    for(int i=0 ; i<nbBlocs; ++i,++it) 
    {
      *(request.buffer) << it->addr << it->count << it->window << it->start;
    
      if (info.isActive(logProtocol))
      {
        size_t checksum=0 ;
        for(size_t j=0;j<it->count;j++) checksum+=((unsigned char*)(it->addr))[j] ;
        outStr<<"Bloc "<<i<<"  addr="<<it->addr<<"  count="<<it->count<<"  checksum="<<checksum<<"  window="<<it->window<<"  start="<<it->start<<"  ;  " ;
      }

      sentBlocRequest_.emplace_back() ;
      sentBlocRequest_.back().addr = it->addr ; 
      MPI_Issend((void*)(it->addr), it->count, MPI_CHAR, intraServerRank_, 21, interCommMerged_, &sentBlocRequest_.back().mpiRequest) ;
    }
    if (info.isActive(logProtocol)) CTimer::get("sendTimelineEvent : MPI_Isend").resume() ;
    //info(logProtocol) << "Send event : " << request.buffer->count() << endl;
    MPI_Isend(request.buffer->start(),request.buffer->count(), MPI_CHAR, intraServerRank_, 20, interCommMerged_, &request.mpiRequest ) ;
    if (info.isActive(logProtocol)) CTimer::get("sendTimelineEvent : MPI_Isend").suspend() ;
    info(logProtocol)<<outStr.str()<<endl ;
    requests_.push_back(request) ;
  }

  void CP2pClientBuffer::sendResizeBufferEvent(size_t timeline, size_t size)
  {
    SRequest request ;
    request.buffer = new CBufferOut(sizeof(EVENT_BUFFER_RESIZE)+sizeof(timeline)+sizeof(size)) ; 
    *(request.buffer)<<EVENT_BUFFER_RESIZE<<timeline<<size ;
    //info(logProtocol) << "Send resize : " << request.buffer->count() << endl;
    MPI_Isend(request.buffer->start(),request.buffer->count(), MPI_CHAR, intraServerRank_, 20, interCommMerged_, &request.mpiRequest ) ;
    requests_.push_back(request) ;
  }

  void CP2pClientBuffer::sendNewBuffer(void)
  {
    MPI_Aint controlAddr ;
//    MPI_Get_address(control_, &controlAddr) ;
    MPI_Send(&controlAddr, 1, MPI_AINT, intraServerRank_, 20, interCommMerged_) ;
  }

}
