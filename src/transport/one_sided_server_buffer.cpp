#include "one_sided_server_buffer.hpp"
#include "xios_spl.hpp"
#include "mpi.hpp"
#include "timer.hpp"
#include "buffer_in.hpp"



namespace xios
{
  CLogType logProtocol("log_protocol") ;

  COneSidedServerBuffer::COneSidedServerBuffer(int clientRank, const MPI_Comm& commSelf, const MPI_Comm& interCommMerged, map<size_t, SPendingEvent>& pendingEvents, 
                                               map<size_t, SPendingEvent>& completedEvents, vector<char>& buffer) 
                        : clientRank_(clientRank), pendingFullEvents_(pendingEvents), completedFullEvents_(completedEvents)
  {
    MPI_Alloc_mem(controlSize_*sizeof(MPI_Aint), MPI_INFO_NULL, &control_) ;
    CBufferIn bufferIn(buffer.data(),buffer.size()) ;
    bufferIn >> controlAddr_;
    createWindow(commSelf, interCommMerged) ;
  }

  void COneSidedServerBuffer::createWindow(const MPI_Comm& commSelf, const MPI_Comm& interCommMerged)
  {
    CTimer::get("create Windows").resume() ;
    MPI_Comm interComm ;
    MPI_Intercomm_create(commSelf, 0, interCommMerged, clientRank_, 0 , &interComm) ;
    MPI_Intercomm_merge(interComm, true, &winComm_) ;
    CXios::getMpiGarbageCollector().registerCommunicator(winComm_) ;
    MPI_Comm_free(&interComm) ;
    
    maxWindows_=MAX_WINDOWS ;
    windows_.resize(maxWindows_) ;
   
    for(int i=0;i<maxWindows_;++i) 
    {
      MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_, &windows_[i]);
      CXios::getMpiGarbageCollector().registerWindow(windows_[i]) ;
    }
    MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_, &winControl_);
    CXios::getMpiGarbageCollector().registerWindow(winControl_) ;
    CTimer::get("create Windows").suspend() ;
    MPI_Barrier(winComm_) ;
    MPI_Barrier(winComm_) ;

  }

  void COneSidedServerBuffer::receivedRequest(vector<char>& buffer)
  {
    size_t timeline ;
    int nbSenders ;
    CBufferIn bufferIn(buffer.data(),buffer.size()) ;
    bufferIn >> timeline ;
    if (timeline==EVENT_BUFFER_RESIZE)
    {
      size_t AssociatedTimeline ;
      size_t newSize ;
      bufferIn >>AssociatedTimeline>>newSize ;
      bufferResize_.push_back({AssociatedTimeline,newSize}) ;
    }
    else // receive standard event
    {

      bufferIn>> nbSenders ;
      nbSenders_[timeline] = nbSenders ;
      auto pendingFullEvent=pendingFullEvents_.find(timeline) ;
      if (pendingFullEvent==pendingFullEvents_.end()) 
      {
        SPendingEvent pendingEvent = {nbSenders,1,{this}} ;
        pendingFullEvents_[timeline]=pendingEvent ;
      }
      else 
      {  
        pendingFullEvent->second.currentNbSenders++ ;
        pendingFullEvent->second.buffers.push_back(this) ;
      }
    
      int nbBlocs ; 
      int count ;
      int window ;
      bufferIn >> nbBlocs ;
      MPI_Aint bloc ;
      auto& blocs = pendingBlocs_[timeline] ;
      for(int i=0;i<nbBlocs;++i) 
      {
        bufferIn >> bloc >> count >> window;
        blocs.push_back({bloc, count, window}) ;
      }
    }
  }

  void COneSidedServerBuffer::eventLoop(void)
  {
    int flag ;
    if (!pendingRmaRequests_.empty()) testPendingRequests() ;
    if (pendingRmaRequests_.empty()) transferEvents() ;

    if (!isLocked_)
    {
      if (lastBlocToFree_!=0)
      {
        info(logProtocol)<<"Send bloc to free : "<<lastBlocToFree_<<endl ;
        MPI_Win_lock(MPI_LOCK_EXCLUSIVE, windowRank_, 0, winControl_) ;
        MPI_Aint target=MPI_Aint_add(controlAddr_, CONTROL_ADDR*sizeof(MPI_Aint)) ;
        MPI_Put(&lastBlocToFree_, 1, MPI_AINT, windowRank_, target, 1, MPI_AINT, winControl_) ;
        MPI_Win_unlock(windowRank_,winControl_) ; 
        lastBlocToFree_ = 0 ;        
      }
    }

    if (buffers_.size()>1) 
     if (buffers_.front()->getCount()==0) buffers_.pop_front() ; // if buffer is empty free buffer
  }

  void COneSidedServerBuffer::notifyClientFinalize(void)
  {
    eventLoop() ; // to free the last bloc
    MPI_Aint finalize=1 ;
    MPI_Win_lock(MPI_LOCK_EXCLUSIVE, windowRank_, 0, winControl_) ;
    MPI_Aint target=MPI_Aint_add(controlAddr_, CONTROL_FINALIZE*sizeof(MPI_Aint)) ;
    MPI_Put(&finalize, 1, MPI_AINT, windowRank_, target, 1, MPI_AINT, winControl_) ;
    MPI_Win_unlock(windowRank_,winControl_) ; 
  }
  void COneSidedServerBuffer::testPendingRequests(void)
  {
    if (!pendingRmaRequests_.empty())
    {
      int flag ;
      MPI_Testall(pendingRmaRequests_.size(), pendingRmaRequests_.data(), &flag, pendingRmaStatus_.data()) ;
      if (flag==true) 
      {
        if (!isLocked_) ERROR("void COneSidedServerBuffer::testPendingRequests(void)",<<"windows is not Locked");
        for(auto& win : windowsLocked_) 
        {
          info(logProtocol)<<"unlock window "<<win<<endl ;
          MPI_Win_unlock(windowRank_,windows_[win]) ; 
        }
        windowsLocked_.clear() ;
          
        isLocked_=false ;
        pendingRmaRequests_.clear() ;
        pendingRmaStatus_.clear() ;
        completedEvents_.insert(onTransferEvents_.begin(),onTransferEvents_.end()) ;
        
        for(auto & event : onTransferEvents_) 
        {
          size_t timeline = event.first ;

          auto pendingFullEvent=pendingFullEvents_.find(timeline) ;
          pendingFullEvent->second.nbSenders-- ;
          pendingFullEvent->second.currentNbSenders-- ;
          

          auto completedFullEvent=completedFullEvents_.find(timeline) ;
          if (completedFullEvent==completedFullEvents_.end()) 
          {
            SPendingEvent pendingEvent = {nbSenders_[timeline],1,{this}} ;
            completedFullEvents_[timeline]=pendingEvent ;
          }
          else 
          {
            completedFullEvent->second.currentNbSenders++ ;
            completedFullEvent->second.buffers.push_back(this) ;
          }
          nbSenders_.erase(timeline) ;
        }  
        onTransferEvents_.clear() ;
      }
    }

  }
  
  size_t COneSidedServerBuffer::remainSize(void)
  {
    if (!fixed_) return std::numeric_limits<size_t>::max() ;
    else
    {
      if (currentBuffer_ == nullptr) return fixedSize_ ;
      else return currentBuffer_->remain() ;
    }
  }

  void COneSidedServerBuffer::transferEvents(void)
  {
    if (pendingRmaRequests_.empty() && !pendingBlocs_.empty())
    {
      size_t remain=remainSize() ;
      size_t transferedSize=0 ;

      size_t timeline =  pendingBlocs_.begin()->first ;
      auto& blocs = pendingBlocs_.begin()->second ;
      
      if (!bufferResize_.empty()) 
      {
        if (bufferResize_.front().first==timeline)
        {
          currentBufferSize_=bufferResize_.front().second ;
          info(logProtocol)<<"Received new buffer size="<<currentBufferSize_<<"  at timeline="<<timeline<<endl ;
          bufferResize_.pop_front() ;
          newBuffer(currentBufferSize_,fixed_) ;
        }
      }

      size_t eventSize=0 ;
      for(auto& bloc : blocs) eventSize+=get<1>(bloc) ;
      
      if (eventSize > remain) 
      {
        if ( eventSize <= currentBufferSize_) return ; // wait for free storage ;
        else 
        {
          if (currentBuffer_==nullptr) remain = eventSize ;
          else remain = currentBuffer_->remain() + fixedSize_ ;
        }
      }
      
      if (isLocked_) ERROR("void COneSidedServerBuffer::transferEvents(void)",<<"windows is Locked");
      for(auto& bloc : blocs) 
      {
        int win=get<2>(bloc) ;
        if (windowsLocked_.count(win)==0) 
        {
          MPI_Win_lock(MPI_LOCK_SHARED, windowRank_, 0, windows_[win]) ;
          windowsLocked_.insert(win) ;
        }
      }
      isLocked_=true ;
      do
      {
        transferEvent() ; // ok enough storage for this bloc
        
        transferedSize += eventSize ;
        pendingBlocs_.erase(pendingBlocs_.begin()) ;
        
//        break ; // transfering just one event temporary => to remove
        
        if (pendingBlocs_.empty()) break ; // no more blocs to tranfer => exit loop

        timeline =  pendingBlocs_.begin()->first ;
        auto& blocs=pendingBlocs_.begin()->second ;
        
        if (!bufferResize_.empty()) 
        {
          if (bufferResize_.front().first==timeline)
          {
            currentBufferSize_=bufferResize_.front().second ;
            info(logProtocol)<<"Received new buffer size="<<currentBufferSize_<<"  at timeline="<<timeline<<endl ;
            bufferResize_.pop_front() ;
            newBuffer(currentBufferSize_,fixed_) ;
          }
        }

        for(auto& bloc : blocs) eventSize+=get<1>(bloc) ;
        if (transferedSize+eventSize<=remain)
        {
          for(auto& bloc : blocs) 
          {
            int win=get<2>(bloc) ;
            if (windowsLocked_.count(win)==0) 
            {
              MPI_Win_lock(MPI_LOCK_SHARED, windowRank_, 0, windows_[win]) ;
              windowsLocked_.insert(win) ;
            }
          }
        }
      }
      while(transferedSize+eventSize<=remain) ;
      
    }
  }
  
  void COneSidedServerBuffer::transferEvent(void)
  {
    MPI_Aint addr;
    MPI_Aint offset ;

    size_t size;
    size_t start;
    size_t count;
    int window ;

    auto& blocs=pendingBlocs_.begin()->second ;
    size_t timeline = pendingBlocs_.begin() -> first ;
  

    for(auto& bloc : blocs)
    {
      addr = std::get<0>(bloc) ;
      size = std::get<1>(bloc) ;
      window = std::get<2>(bloc) ;

      offset=0 ;

      do
      {
        if (currentBuffer_!=nullptr)
        {
          currentBuffer_->reserve(size, start, count) ;
      
          if ( count > 0)
          {
            transferRmaRequest(timeline, addr, offset, currentBuffer_, start, count, window) ;
            offset=MPI_Aint_add(offset, count) ;
          }
          currentBuffer_->reserve(size, start, count) ;
      
          if ( count > 0)
          {
            transferRmaRequest(timeline, addr, offset, currentBuffer_, start, count, window) ;
            offset=MPI_Aint_add(offset, count) ;
          }
        }

        if (size>0) 
        {
          if (fixed_) newBuffer(fixedSize_,fixed_) ;
          else
          {
            currentBufferSize_ = std::max((size_t)(currentBufferSize_*growingFactor_), size) ;
            newBuffer(currentBufferSize_,fixed_) ;
          }
        }
      } while (size > 0 ) ;
    }

    pendingRmaStatus_.resize(pendingRmaRequests_.size()) ;
  }

  void COneSidedServerBuffer::transferRmaRequest(size_t timeline, MPI_Aint addr, MPI_Aint offset, CBuffer* buffer, size_t start, int count, int window)
  {
    MPI_Request request ;
    MPI_Aint offsetAddr=MPI_Aint_add(addr, offset) ;
    info(logProtocol)<<"receive Bloc from client "<<clientRank_<<" : timeline="<<timeline<<"  addr="<<addr<<"  count="<<count<<" buffer="<<buffer<<"  start="<<start<<endl ;
    info(logProtocol)<<"check dest buffers ; start_buffer="<<static_cast<void*>(buffer->getBuffer())<<"  end_buffer="<<static_cast<void*>(buffer->getBuffer()+buffer->getSize()-1)
             <<"  start="<<static_cast<void*>(buffer->getBuffer()+start)<<"   end="<<static_cast<void*>(buffer->getBuffer()+start+count-1)<<endl ;
    MPI_Rget(buffer->getBuffer()+start, count, MPI_CHAR, windowRank_, offsetAddr, count, MPI_CHAR, windows_[window], &request) ;
    pendingRmaRequests_.push_back(request) ;
    onTransferEvents_[timeline].push_back({buffer,start,count,addr}) ;
  }

  void COneSidedServerBuffer::fillEventServer(size_t timeline, CEventServer& event)
  {
    auto &completedEvent=completedEvents_[timeline] ;
    size_t size=0 ;
    for(auto& bloc : completedEvent) size+=bloc.count ;
    char* buffer = new char[size] ;
    size=0 ;
    
    ostringstream outStr ;
    outStr<<"Received Event from client "<<clientRank_<<"  timeline="<<timeline<<"  nbBlocs="<<completedEvent.size()<<endl ;
    int i=0 ;
    MPI_Aint addr ;
    for(auto& bloc : completedEvent) 
    {
      memcpy(&buffer[size], bloc.buffer->getBuffer()+bloc.start, bloc.count) ;
      
      if (info.isActive(logProtocol))
      {
        size_t checksum=0 ;
        for(size_t j=0;j<bloc.count;j++) checksum += (unsigned char) buffer[size+j] ;
        outStr<<"bloc "<<i<<"  count="<<bloc.count<<" checksum="<<checksum<<"  ;  " ;
        i++ ;
      }

      size+=bloc.count ;
      bloc.buffer->free(bloc.start, bloc.count) ; // free bloc
      addr=bloc.addr ;
      if (bloc.buffer->getCount()==0) if (buffers_.size() > 1) buffers_.pop_front() ; // if buffer is empty free buffer
    }
    event.push(clientRank_, nullptr, buffer, size) ;
    if (info.isActive(logProtocol)) outStr<<" ==> nbSenders="<<event.getNbSender() ;
    info(logProtocol)<<outStr.str()<<endl ;
    
    lastBlocToFree_=addr ;
    /*
    if (!isLocked_) MPI_Win_lock(MPI_LOCK_SHARED, windowRank_, 0, window_) ;
    MPI_Aint target=MPI_Aint_add(controlAddr_, CONTROL_ADDR) ;
    MPI_Put(&addr, 1, MPI_AINT, windowRank_, target, 1, MPI_AINT, window_) ;
    if (!isLocked_) MPI_Win_unlock(windowRank_,window_) ;
   */
    completedEvents_.erase(timeline) ;
    eventLoop() ;
  }

  

}
