#include "p2p_server_buffer.hpp"
#include "xios_spl.hpp"
#include "mpi.hpp"
#include "timer.hpp"
#include "buffer_in.hpp"



namespace xios
{
  extern CLogType logProtocol ;
  extern CLogType logTimers ;

  CP2pServerBuffer::CP2pServerBuffer(int clientRank, const MPI_Comm& commSelf, const MPI_Comm& interCommMerged, map<size_t, SPendingEvent>& pendingEvents, 
                                               map<size_t, SPendingEvent>& completedEvents, vector<char>& buffer) 
                        : clientRank_(clientRank), interCommMerged_(interCommMerged), pendingFullEvents_(pendingEvents), completedFullEvents_(completedEvents)
  {
    //MPI_Alloc_mem(controlSize_*sizeof(MPI_Aint), MPI_INFO_NULL, &control_) ;
    //CBufferIn bufferIn(buffer.data(),buffer.size()) ;
    //bufferIn >> controlAddr_;
    createWindow(commSelf, interCommMerged) ;
    countDeletedBuffers_ = 0;
  }

  void CP2pServerBuffer::createWindow(const MPI_Comm& commSelf, const MPI_Comm& interCommMerged)
  {
    if (info.isActive(logTimers)) CTimer::get("create Windows").resume() ;
    //MPI_Comm interComm ;
    //xios::MPI_Intercomm_create(commSelf, 0, interCommMerged, clientRank_, 0 , &interComm) ;
    //xios::MPI_Intercomm_merge(interComm, true, &winComm_) ;
    //CXios::getMpiGarbageCollector().registerCommunicator(winComm_) ;
    //xios::MPI_Comm_free(&interComm) ;
    
    //maxWindows_=MAX_WINDOWS ;
    //windows_.resize(maxWindows_) ;
   
    //for(int i=0;i<maxWindows_;++i) 
    //{
    //  MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_, &windows_[i]);
    //  CXios::getMpiGarbageCollector().registerWindow(windows_[i]) ;
    //}
    //MPI_Win_create_dynamic(MPI_INFO_NULL, winComm_, &winControl_);
    //CXios::getMpiGarbageCollector().registerWindow(winControl_) ;
    if (info.isActive(logTimers)) CTimer::get("create Windows").suspend() ;
    //MPI_Barrier(winComm_) ;
    //MPI_Barrier(winComm_) ;

  }

  void CP2pServerBuffer::receivedRequest(vector<char>& buffer)
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
      info(logProtocol)<<"received request from rank : "<<clientRank_<<"  with timeline : "<<timeline
                                                        <<"   at time : "<<CTimer::get("XIOS server").getTime()<<endl ;
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
      size_t start ; 
      bufferIn >> nbBlocs ;
      MPI_Aint bloc ;
      auto& blocs = pendingBlocs_[timeline] ;
      for(int i=0;i<nbBlocs;++i) 
      {
        bufferIn >> bloc >> count >> window >> start;
        //info(logProtocol) << "Receiving window : "<<window << endl;
        blocs.push_back({bloc, count, window,start}) ;
      }
    }
  }

  void CP2pServerBuffer::eventLoop(void)
  {
    int flag ;
    if (!pendingRmaRequests_.empty()) testPendingRequests() ;
    if (pendingRmaRequests_.empty()) transferEvents() ;

    //if (!isLocked_)
    //{
      if (lastBlocToFree_!=0)
      {
        info(logProtocol)<<"Send bloc to free : "<<lastBlocToFree_<<endl ;
        //if (info.isActive(logProtocol)) CTimer::get("Send bloc to free").resume() ;
        //MPI_Win_lock(MPI_LOCK_EXCLUSIVE, windowRank_, 0, winControl_) ;
        //MPI_Aint target=MPI_Aint_add(controlAddr_, CONTROL_ADDR*sizeof(MPI_Aint)) ;
        //MPI_Put(&lastBlocToFree_, 1, MPI_AINT, windowRank_, target, 1, MPI_AINT, winControl_) ;
        //MPI_Win_unlock(windowRank_,winControl_) ; 
        //if (info.isActive(logProtocol)) CTimer::get("Send bloc to free").suspend() ;
        lastBlocToFree_ = 0 ;        
      }
    //}

    if (buffers_.size()>1) 
    {
      if (buffers_.front()->getCount()==0) {
        // If the front buffer is empty and if another buffer become the active one (buffers_.size()>1)
        //     the front buffer can be deleted, no new message will be sent through the front buffer
        delete buffers_.front();
        buffers_.erase(buffers_.begin()) ; // if buffer is empty free buffer
        //info(logProtocol) << "Deleting win : " << countDeletedBuffers_  << endl;
        countDeletedBuffers_++;
      }
    }
  }

  void CP2pServerBuffer::notifyClientFinalize(void)
  {
    eventLoop() ; // to free the last bloc
    //MPI_Aint finalize=1 ;
    //MPI_Win_lock(MPI_LOCK_EXCLUSIVE, windowRank_, 0, winControl_) ;
    //MPI_Aint target=MPI_Aint_add(controlAddr_, CONTROL_FINALIZE*sizeof(MPI_Aint)) ;
    //MPI_Put(&finalize, 1, MPI_AINT, windowRank_, target, 1, MPI_AINT, winControl_) ;
    //MPI_Win_unlock(windowRank_,winControl_) ; 
    int dummy ;
    MPI_Send(&dummy, 0, MPI_CHAR, clientRank_, 22, interCommMerged_) ;
  }
  
  void CP2pServerBuffer::testPendingRequests(void)
  {
    if (!pendingRmaRequests_.empty())
    {
      int flag ;    

      if (info.isActive(logProtocol)) CTimer::get("transfer MPI_Testall").resume() ;
      MPI_Testall(pendingRmaRequests_.size(), pendingRmaRequests_.data(), &flag, pendingRmaStatus_.data()) ;
      if (info.isActive(logProtocol)) CTimer::get("transfer MPI_Testall").suspend() ;
      
      if (flag==true) 
      {
        //if (!isLocked_) ERROR("void COneSidedServerBuffer::testPendingRequests(void)",<<"windows is not Locked");
        //for(auto& win : windowsLocked_) 
        //{
        //  info(logProtocol)<<"unlock window "<<win<<endl ;
        //  if (info.isActive(logProtocol)) CTimer::get("transfer unlock").resume() ;
        //  MPI_Win_unlock(windowRank_,windows_[win]) ; 
        //  if (info.isActive(logProtocol)) CTimer::get("transfer unlock").suspend() ;
        //}
        //windowsLocked_.clear() ;
        

        if (info.isActive(logProtocol)) CTimer::get("transfer MPI_Rget from "+std::to_string(clientRank_)).suspend() ;
        if (info.isActive(logProtocol)) CTimer::get("lastTransfer from "+std::to_string(clientRank_)).suspend() ;
        
        size_t transferedSize = 0 ;
        for(auto& count : pendingRmaCount_) transferedSize+=count ;

        if (info.isActive(logProtocol))
        {
          double time = CTimer::get("lastTransfer from "+std::to_string(clientRank_)).getCumulatedTime() ;
          info(logProtocol)<<"Tranfer message from rank : "<<clientRank_<<"  nbBlocs : "<< pendingRmaStatus_.size()
                           << "  total count = "<<transferedSize<<"  duration : "<<time<<" s"
                           << "  Bandwith : "<< transferedSize/time<< "byte/s"<<endl ;
          CTimer::get("lastTransfer from "+std::to_string(clientRank_)).reset() ;
          for(int i=0;i<pendingRmaAddr_.size();i++)
          {
            size_t checksum=0 ;
            unsigned char* buffer = (unsigned char*) pendingRmaAddr_[i] ;
            for(size_t j=0;j<pendingRmaCount_[i];j++) checksum += buffer[j] ;
            info(logProtocol)<<"Bloc transfered to adrr="<<(void*) buffer<<"  count="<<pendingRmaCount_[i]<<"  checksum="<<checksum<<endl ;
          }

         }

        //isLocked_=false ;
        pendingRmaRequests_.clear() ;
        pendingRmaStatus_.clear() ;
        pendingRmaCount_.clear() ;
        pendingRmaAddr_.clear() ;
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
  
  size_t CP2pServerBuffer::remainSize(void)
  {
    if (!fixed_) return std::numeric_limits<size_t>::max() ;
    else
    {
      if (currentBuffer_ == nullptr) return fixedSize_ ;
      else return currentBuffer_->remain() ;
    }
  }
  
  size_t CP2pServerBuffer::remainSize(int bufferId)
  {
    if (bufferId-countDeletedBuffers_>=buffers_.size())
    {
      //info(logProtocol) << "The buffer " << bufferId << " is not yet allocated" << endl;
      return 0;
    }      
    return buffers_[bufferId-countDeletedBuffers_]->remain() ;
  }


  void CP2pServerBuffer::transferEvents(void)
  {
    if (pendingRmaRequests_.empty() && !pendingBlocs_.empty())
    {
      size_t remain=remainSize() ;
      size_t transferedSize=0 ;

      size_t timeline =  pendingBlocs_.begin()->first ;
      auto& blocs = pendingBlocs_.begin()->second ; // map<size_t  , list<tuple<MPI_Aint,int ,int,size_t>>> pendingBlocs_;
                                                    //     timeline,            addr    ,size,win,start
      // addr   = std::get<0>(bloc) ;
      // size   = std::get<1>(bloc) ;
      // window = std::get<2>(bloc) ;
      // start  = std::get<3>(bloc) ; // start : used to check mirror behavior

      size_t eventSize=0 ;
      
      //if (isLocked_) ERROR("void COneSidedServerBuffer::transferEvents(void)",<<"windows is Locked");
      
      if (info.isActive(logProtocol)) CTimer::get("transfer MPI_Rget from "+std::to_string(clientRank_)).resume() ;
      if (info.isActive(logProtocol)) CTimer::get("lastTransfer from "+std::to_string(clientRank_)).resume() ;
      //for(auto& bloc : blocs) 
      //{
      //  int win=get<2>(bloc) ;
      //  if (windowsLocked_.count(win)==0) 
      //  {
      //    info(logProtocol)<<"lock window "<<win<<endl ;
      //    if (info.isActive(logProtocol)) CTimer::get("transfer lock").resume() ;
      //    MPI_Win_lock(MPI_LOCK_SHARED, windowRank_, 0, windows_[win]) ;
      //    if (info.isActive(logProtocol)) CTimer::get("transfer lock").suspend() ;
      //    windowsLocked_.insert(win) ;
      //  }
      //}
      //isLocked_=true ;
//      do

      bool spaceForAllblocks = true;
      int lastBufferUsed = -1;
      if (blocs.size()==0) spaceForAllblocks = false;
      else
      {
        for(auto& bloc : blocs)
        {
          //info(logProtocol) << "blocSize = " << get<1>(bloc)
          //                  << " - remain in win : " << get<2>(bloc) << " : " << remainSize( get<2>(bloc) )
          //                  << "; bufferResize_ = " <<  bufferResize_.size() << endl;
          
          // if the active buffer change, the new buffer must be considered as empty
          if (lastBufferUsed != get<2>(bloc) ) eventSize = 0;

          // if the targeted buffer does not exist
          if ( get<2>(bloc)-countDeletedBuffers_>=buffers_.size() )
          {
            if ( bufferResize_.empty() ) // no resize order
            {
              spaceForAllblocks = false;
              break;
            }
          }
          else if ( ( get<1>(bloc) > (remainSize(get<2>(bloc))-eventSize) ) )  // if there is no enough place in the targeted bloc
          {
            spaceForAllblocks = false;
            break;
          }
          else
          {
            // if there is enough place in the targeted bloc, store the 
            lastBufferUsed = get<2>(bloc);
            eventSize += get<1>(bloc);
          }
        }
      }      

      if (spaceForAllblocks)
      {
        transferEvent() ; // ok enough storage for this bloc
        
        transferedSize += eventSize ;
        pendingBlocs_.erase(pendingBlocs_.begin()) ;
        
        //  break ; // transfering just one event temporary => to remove
        
//        if (pendingBlocs_.empty()) break ; // no more blocs to tranfer => exit loop
//
//        timeline =  pendingBlocs_.begin()->first ;
//        auto& blocs=pendingBlocs_.begin()->second ;
//        
//
//        for(auto& bloc : blocs) eventSize+=get<1>(bloc) ;
//        if (transferedSize+eventSize<=remain)
        {
          //for(auto& bloc : blocs) 
          //{
          //  int win=get<2>(bloc) ;
          //  if (windowsLocked_.count(win)==0) 
          //  {
          //    info(logProtocol)<<"lock window "<<win<<endl ;
          //    if (info.isActive(logProtocol)) CTimer::get("transfer lock").resume() ;
          //    MPI_Win_lock(MPI_LOCK_SHARED, windowRank_, 0, windows_[win]) ;
          //    if (info.isActive(logProtocol)) CTimer::get("transfer lock").suspend() ;
          //    windowsLocked_.insert(win) ;
          //  }
          //}
        }
      }
//      while(transferedSize+eventSize<=remain) ;
      
    }
  }
  
  void CP2pServerBuffer::transferEvent(void)
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
      start = std::get<3>(bloc) ; // start : used to check mirror behavior

      offset=0 ;

      // Need to keep loop even if a given bloc will not be split.
      // To mimic client behavior, especially if (size_==end_) reset end_ = 0 ;
      do
      {
        //if ( (currentBuffer_!=nullptr) || (window-countDeletedBuffers_ == buffers_.size() ) )
        {
          if (window-countDeletedBuffers_ >= buffers_.size())
            {
              if (!bufferResize_.empty()) 
                {
                  if (bufferResize_.front().first==timeline)
                    {
                      currentBufferSize_=bufferResize_.front().second * bufferServerFactor_ ;
                      //info(logProtocol)<<"Received new buffer size="<<currentBufferSize_<<"  at timeline="<<timeline<<endl ;
                      bufferResize_.pop_front() ;
                      newBuffer(currentBufferSize_,fixed_) ;
                    }
                }
            }
          
          buffers_[window-countDeletedBuffers_]->reserve(size, start, count) ;
      
          if ( count > 0)
          {
            transferRmaRequest(timeline, addr, offset, buffers_[window-countDeletedBuffers_], start, count, window) ;
            offset=MPI_Aint_add(offset, count) ;
            
          }
        }

      } while (size > 0 ) ;
    }

    pendingRmaStatus_.resize(pendingRmaRequests_.size()) ;
  }

  void CP2pServerBuffer::transferRmaRequest(size_t timeline, MPI_Aint addr, MPI_Aint offset, CBuffer* buffer, size_t start, int count, int window)
  {
    MPI_Request request ;
    MPI_Aint offsetAddr=MPI_Aint_add(addr, offset) ;
    if (info.isActive(logProtocol))
    {
      info(logProtocol)<<"receive Bloc from client "<<clientRank_<<" : timeline="<<timeline<<"  addr="<<addr<<"  count="<<count<<" buffer="<<buffer<<"  start="<<start<<"  window="<<window<<endl ;
      info(logProtocol)<<"check dest buffers ; start_buffer="<<static_cast<void*>(buffer->getBuffer())<<"  end_buffer="<<static_cast<void*>(buffer->getBuffer()+buffer->getSize()-1)
               <<"  start="<<static_cast<void*>(buffer->getBuffer()+start)<<"   end="<<static_cast<void*>(buffer->getBuffer()+start+count-1)<<endl ;
    }
    if (info.isActive(logProtocol)) CTimer::get("MPI_Rget").resume() ;
    //MPI_Rget(buffer->getBuffer()+start, count, MPI_CHAR, windowRank_, offsetAddr, count, MPI_CHAR, windows_[window], &request) ;
    MPI_Irecv(buffer->getBuffer()+start, count, MPI_CHAR, clientRank_, 21, interCommMerged_, &request) ;
    if (info.isActive(logProtocol)) CTimer::get("MPI_Rget").suspend() ;
    pendingRmaRequests_.push_back(request) ;
    pendingRmaCount_.push_back(count) ;
    pendingRmaAddr_.push_back(buffer->getBuffer()+start) ;
    onTransferEvents_[timeline].push_back({buffer,start,count,addr}) ;
  }

  void CP2pServerBuffer::fillEventServer(size_t timeline, CEventServer& event)
  {
    auto &completedEvent=completedEvents_[timeline] ;
    size_t size=0 ;
    for(auto& bloc : completedEvent) size+=bloc.count ;
    char* buffer = new char[size] ;
    size=0 ;
    
    ostringstream outStr ;
    if (info.isActive(logProtocol)) outStr<<"Received Event from client "<<clientRank_<<"  timeline="<<timeline
                                          <<"  nbBlocs="<<completedEvent.size()<<endl ;
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
      //info(logProtocol) << "Free from : " << bloc.start << ", size : " << bloc.count<< endl;
      bloc.buffer->free(bloc.start, bloc.count) ; // free bloc
      addr=bloc.addr ;
      if (bloc.buffer->getCount()==0)
      {
        if (buffers_.size() > 1)
        {
          // If the front buffer is empty and if another buffer become the active one (buffers_.size()>1)
          //     the front buffer can be deleted, no new message will be sent through the front buffer
          delete buffers_.front();
          buffers_.erase(buffers_.begin()) ; // if buffer is empty free buffer
          //info(logProtocol) << "Deleting win : " << countDeletedBuffers_  << endl;
          countDeletedBuffers_++;
        }
      }
    }
    event.push(clientRank_, nullptr, buffer, size) ;
    if (info.isActive(logProtocol)) outStr<<" ==> nbSenders="<<event.getNbSender() ;
    info(logProtocol)<<outStr.str()<<endl ;
    
    lastBlocToFree_=addr ;

    completedEvents_.erase(timeline) ;
    eventLoop() ;
  }

  

}
