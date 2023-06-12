#include "event_scheduler.hpp"
#include "xios_spl.hpp"
#include "mpi.hpp"
#include "tracer.hpp"

namespace xios
{
 
 
  CEventScheduler::CEventScheduler(const MPI_Comm& comm) 
  {
    MPI_Comm_dup(comm, &communicator_) ;
    MPI_Comm_size(communicator_,&mpiSize_) ;
    MPI_Comm_rank(communicator_,&mpiRank_);


    int maxChild=1 ;

    int m ;
    do
    {
      m=1 ;
      maxChild=maxChild+1 ;
      for(int i=0;i<maxChild;i++) m=m*maxChild ;
     } while(m<mpiSize_) ;
    
    
    int maxLevel=0 ;
    for(int size=1; size<=mpiSize_; size*=maxChild) maxLevel++ ; 

    int begin, end, nb ;
    int pos, n ;
 
    parent_=vector<int>(maxLevel+1) ;
    child_=vector<vector<int> >(maxLevel+1,vector<int>(maxChild)) ;
    nbChild_=vector<int> (maxLevel+1) ;
   
    level_=0 ;
    begin=0 ;
    end=mpiSize_-1 ;     
    nb=end-begin+1 ;
     
    do
    {
      n=0 ;
      pos=begin ;
      nbChild_[level_]=0 ;
      parent_[level_+1]=begin ;
      for(int i=0;i<maxChild && i<nb ;i++)
      {
        if (i<nb%maxChild) n = nb/maxChild + 1 ;
        else n = nb/maxChild ;
      
        if (mpiRank_>=pos && mpiRank_<pos+n)
        {
          begin=pos ;
          end=pos+n-1 ;
        }
        child_[level_][i]=pos ;
        pos=pos+n ;
        nbChild_[level_]++ ;
      } 
      nb=end-begin+1 ;
      level_=level_+1 ;
    } while (nb>1) ;

    
  }

  CEventScheduler::~CEventScheduler()
  {
    while (!pendingSentParentRequest_.empty() || !pendingRecvParentRequest_.empty() || !pendingRecvChildRequest_.empty() ||  !pendingSentChildRequest_.empty())
    {
      checkEvent() ;
    } 
  } 

  void CEventScheduler::registerEvent(const size_t timeLine, const size_t contextHashId)
  {
    registerEvent(timeLine, contextHashId, level_) ;
    checkEvent() ;
  }
  
  void CEventScheduler::registerEvent(const size_t timeLine, const size_t contextHashId, const size_t lev)
  {
       
    traceOff() ;
    SPendingRequest* sentRequest=new SPendingRequest ;
    sentRequest->buffer[0]=timeLine ;
    sentRequest->buffer[1]=contextHashId ;
    sentRequest->buffer[2]=lev-1 ;

    pendingSentParentRequest_.push(sentRequest) ;
    MPI_Isend(sentRequest->buffer,3, MPI_UNSIGNED_LONG, parent_[lev], 0, communicator_, &sentRequest->request) ;
    traceOn() ;
  } 

  bool CEventScheduler::queryEvent(const size_t timeLine, const size_t contextHashId)
  {
    checkEvent() ;
    if (! eventStack_.empty() && eventStack_.front().first==timeLine && eventStack_.front().second==contextHashId)
    {
      //eventStack_.pop() ;
      return true ;
    }
    else return false ; 
  } 
 
  void CEventScheduler::checkEvent(void)
  {
    traceOff() ;
    checkChildRequest() ;
    checkParentRequest() ;
    traceOn() ;
    
  }
  
  void CEventScheduler::checkParentRequest(void)
  {
    int completed ;
    MPI_Status status ;
    int received ;
    SPendingRequest* recvRequest ;
    completed=true ;
    
    // check sent request to parent
    while (! pendingSentParentRequest_.empty() && completed)
    {
      MPI_Test( & pendingSentParentRequest_.front()->request, &completed, &status) ;
      if (completed) 
      {
        delete pendingSentParentRequest_.front() ;
        pendingSentParentRequest_.pop() ;
      }
    }
    
    // probe if a message is coming from parent
    received=true ;
    while(received)
    {
      MPI_Iprobe(MPI_ANY_SOURCE,1,communicator_,&received, &status) ;
      if (received)
      {
        recvRequest=new SPendingRequest ;
        MPI_Irecv(recvRequest->buffer, 3, MPI_UNSIGNED_LONG, MPI_ANY_SOURCE, 1, communicator_, &(recvRequest->request)) ;
        pendingRecvParentRequest_.push(recvRequest) ;
      }
    }
    
     // check sent request from parent
    completed=true ;
    while (! pendingRecvParentRequest_.empty() && completed)
    {
      recvRequest=pendingRecvParentRequest_.front() ;
      MPI_Test( &(recvRequest->request), &completed, &status) ;
      if (completed) 
      {
        size_t timeLine=recvRequest->buffer[0] ;
        size_t hashId=recvRequest->buffer[1] ;
        size_t lev=recvRequest->buffer[2] ;
        delete recvRequest ;
        pendingRecvParentRequest_.pop() ;       
 
        if (lev==level_) eventStack_.push(pair<size_t,size_t>(timeLine,hashId)) ;
        else  bcastEvent(timeLine, hashId, lev) ;
      }
    }   
    
  }

  void CEventScheduler::checkChildRequest(void)
  {
// function call only by parent mpi process

    MPI_Status status ; 
    int received ;
    received=true ;
    SPendingRequest* recvRequest ;
    
    // check for posted requests and make the corresponding receive
    while(received)
    {
      MPI_Iprobe(MPI_ANY_SOURCE,0,communicator_,&received, &status) ;
      if (received)
      {
        recvRequest=new SPendingRequest ;
        MPI_Irecv(recvRequest->buffer, 3, MPI_UNSIGNED_LONG, MPI_ANY_SOURCE, 0, communicator_, &recvRequest->request) ;
        pendingRecvChildRequest_.push_back(recvRequest) ;
      }
    }
    
    // check if receive request is achieved
    
    for(list<SPendingRequest*>::iterator it=pendingRecvChildRequest_.begin(); it!=pendingRecvChildRequest_.end() ; )
    {
      MPI_Test(&((*it)->request),&received,&status) ;
      if (received)
      {
        size_t timeLine=(*it)->buffer[0] ;
        size_t hashId=(*it)->buffer[1] ;
        size_t lev=(*it)->buffer[2] ;
        
        SEvent event={timeLine,hashId,lev} ;
        delete *it ; // free mem
        it=pendingRecvChildRequest_.erase(it) ; // get out of the list
        
        map< SEvent,int>::iterator itEvent=recvEvent_.find(event) ;
        if (itEvent==recvEvent_.end()) 
        {
          itEvent=(recvEvent_.insert(pair< SEvent ,int > (event,1))).first ;
 
        }
        else (itEvent->second)++ ;
        if (itEvent->second==nbChild_[lev])
        {
          if (lev==0)
          {
            bcastEvent(timeLine,hashId,lev) ;
            recvEvent_.erase(itEvent) ;
          }
          else
          {
            registerEvent( timeLine,hashId,lev) ;
            recvEvent_.erase(itEvent) ;
          }
        }
      }
      else ++it ;
    }
    
    // check if bcast request is achieved

    for(list<SPendingRequest*>::iterator it=pendingSentChildRequest_.begin(); it!=pendingSentChildRequest_.end() ; )
    {
      MPI_Test(&(*it)->request,&received,&status) ;
      if (received)
      {
        delete *it ;    // free memory
        it = pendingSentChildRequest_.erase(it) ;          // get out of the list

      }
      else ++it ;
        
    }
  }
  
  void CEventScheduler::bcastEvent(const size_t timeLine, const size_t contextHashId, const size_t lev)
  {
    SPendingRequest* sentRequest ;
     
    
    for(int i=0; i<nbChild_[lev];i++)
    {
      sentRequest=new SPendingRequest ;
      sentRequest->buffer[0]=timeLine ;
      sentRequest->buffer[1]=contextHashId ;
      sentRequest->buffer[2]=lev+1 ;
      MPI_Isend(sentRequest->buffer,3, MPI_UNSIGNED_LONG, child_[lev][i], 1, communicator_, & sentRequest->request) ;
      pendingSentChildRequest_.push_back(sentRequest) ;
    }
  }
   

}
