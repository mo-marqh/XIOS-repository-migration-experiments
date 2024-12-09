#include "server_context.hpp"
#include "contexts_manager.hpp"
#include "cxios.hpp"
#include "mpi.hpp"
#include "context.hpp"
#include "register_context_info.hpp"
#include "services.hpp"
#include "thread_manager.hpp"
#include "timer.hpp"


namespace xios
{
  using namespace std ;
  extern CLogType logTimers ;

  map<string, tuple<bool,MPI_Comm,MPI_Comm> > CServerContext::overlapedComm_ ;

  CServerContext::CServerContext(CService* parentService, MPI_Comm contextComm, const std::string& poolId, const std::string& serviceId, 
                                 const int& partitionId, const std::string& contextId) : finalizeSignal_(false), parentService_(parentService),
                                 hasNotification_(false)
  {
   useWindowManager_ = CXios::servicesUseWindowManager ;

   info(40)<<"CCServerContext::CServerContext  : new context creation ; contextId : "<<contextId<<endl ;
   int localRank, globalRank, commSize ;

    xios::MPI_Comm_dup(contextComm, &contextComm_) ;
    CXios::getMpiGarbageCollector().registerCommunicator(contextComm_) ;
    xiosComm_=CXios::getXiosComm() ;
  
    MPI_Comm_rank(xiosComm_,&globalRank) ;
    MPI_Comm_rank(contextComm_,&localRank) ;
 
    if (useWindowManager_) winNotify_ = new CWindowManager(contextComm_, maxBufferSize_,"CServerContext::winNotify_") ;

    MPI_Barrier(contextComm_) ;
    
    if (localRank==localLeader_) 
    {
      globalLeader_=globalRank ;
      MPI_Comm_size(contextComm_,&commSize) ;
      
      CXios::getServicesManager()->getServiceType(poolId,serviceId, 0, type_) ;
      SRegisterContextInfo contextInfo = {poolId, serviceId, partitionId, type_, contextId, commSize, globalLeader_} ;
      name_ = CXios::getContextsManager()->getServerContextName(poolId, serviceId, partitionId, type_, contextId) ;
      CXios::getContextsManager()->registerContext(name_, contextInfo) ;
    }
    MPI_Bcast(&type_, 1, MPI_INT, localLeader_,contextComm_) ;
    name_ = CXios::getContextsManager()->getServerContextName(poolId, serviceId, partitionId, type_, contextId) ;
    context_=CContext::create(name_);

    std::hash<string> hashString ;
    hashNotify_ = hashString("CServerContext::"+name_);
  
    info(10)<<"Context "<< CXios::getContextsManager()->getServerContextName(poolId, serviceId, partitionId, type_, contextId)<<" created, on local rank "<<localRank
                        <<" and global rank "<<globalRank<<endl  ;
    
    if (CThreadManager::isUsingThreads()) CThreadManager::spawnThread(&CServerContext::threadEventLoop, this) ;
  }

  CServerContext::~CServerContext()
  {
    if (useWindowManager_)  delete winNotify_ ;
    cout<<"Server Context destructor"<<endl;
  } 

  bool CServerContext::createIntercomm(const string& poolId, const string& serviceId, const int& partitionId, const string& contextId, 
                                       const MPI_Comm& intraComm, MPI_Comm& interCommClient, MPI_Comm& interCommServer, bool wait)
  {
    info(40)<<"CServerContext::createIntercomm  : context intercomm creation ; contextId : "<<contextId<<endl ;
    int intraCommRank ;
    MPI_Comm_rank(intraComm, &intraCommRank) ;
    int contextLeader ;

    bool ok ;
    int type ;
    

    if (intraCommRank==0)
    {
      ok=CXios::getContextsManager()->createServerContextIntercomm(poolId, serviceId, partitionId, contextId, name_, wait) ;
      if (ok) 
      {
        CXios::getServicesManager()->getServiceType(poolId,serviceId, 0, type) ;
        string name=CXios::getContextsManager()->getServerContextName(poolId, serviceId, partitionId, type, contextId) ;
        CXios::getContextsManager()->getContextLeader(name, contextLeader) ;
      }
    }
    
    if (wait)
    {
      MPI_Request req ;
      MPI_Status status ;
      MPI_Ibarrier(intraComm,&req) ;
    
      int flag=false ;
      while(!flag) 
      {
        CXios::getDaemonsManager()->servicesEventLoop() ;
        MPI_Test(&req,&flag,&status) ;
      }
    }
    
    MPI_Bcast(&ok, 1, MPI_INT, 0, intraComm) ;

    if (ok)  
    {
      MPI_Comm newInterCommClient, newInterCommServer ;
      xios::MPI_Comm_dup(contextComm_,&newInterCommClient) ;
      xios::MPI_Comm_dup(contextComm_,&newInterCommServer) ;
      overlapedComm_[name_]=tuple<bool, MPI_Comm, MPI_Comm>(false, newInterCommClient, newInterCommServer) ;
      MPI_Barrier(contextComm_) ;

      int globalRank ;
      MPI_Comm_rank(xiosComm_,&globalRank) ;
      MPI_Bcast(&contextLeader, 1, MPI_INT, 0, intraComm) ;
      
      int overlap, nOverlap ;
      if (contextLeader==globalRank) overlap=1 ;
      else overlap=0 ;
      MPI_Allreduce(&overlap, &nOverlap, 1, MPI_INT, MPI_SUM, contextComm_) ;

      if (nOverlap==0)
      { 
        xios::MPI_Intercomm_create(intraComm, 0, xiosComm_, contextLeader, 3141, &interCommClient) ;
        CXios::getMpiGarbageCollector().registerCommunicator(interCommClient) ;
        xios::MPI_Comm_dup(interCommClient, &interCommServer) ;
        CXios::getMpiGarbageCollector().registerCommunicator(interCommServer) ;
        xios::MPI_Comm_free(&newInterCommClient) ;
        xios::MPI_Comm_free(&newInterCommServer) ;
      }
      else
      {
        ERROR("void CServerContext::createIntercomm(void)",<<"CServerContext::createIntercomm : overlap ==> not managed") ;
      }
    }
    overlapedComm_.erase(name_) ;
    return ok ;
  }

  void CServerContext::createIntercomm(int remoteLeader, const string& sourceContext)
  {
    info(40)<<"CServerContext::createIntercomm  : notify createContextIntercomm to all context members ; sourceContext : "<<sourceContext<<endl ;
    
    if (useWindowManager_)
    {
      int commSize ;
      MPI_Comm_size(contextComm_,&commSize) ;
      for(int rank=0; rank<commSize; rank++)
      {
        notifyOutType_=NOTIFY_CREATE_INTERCOMM ;
        notifyOutCreateIntercomm_ = make_tuple(remoteLeader, sourceContext) ;
        winNotify_->pushToExclusiveWindow(rank, this, &CServerContext::notificationsDumpOut) ;
      }      
    }
    else
    {  
      notifyOutType_=NOTIFY_CREATE_INTERCOMM ;
      notifyOutCreateIntercomm_ = make_tuple(remoteLeader, sourceContext) ;
      CXios::getNotificationsManager()->sendLockedNotification(hashNotify_, this, &CServerContext::notificationsDumpOut) ;
    }
  }

  void CServerContext::notificationsDumpOut(CBufferOut& buffer)
  {
    if (useWindowManager_) buffer.realloc(maxBufferSize_) ;

    if (notifyOutType_==NOTIFY_CREATE_INTERCOMM)
    {
      auto& arg=notifyOutCreateIntercomm_ ;
      buffer << notifyOutType_ << std::get<0>(arg)<<std::get<1>(arg) ;
    }
  }

  void CServerContext::notificationsDumpIn(CBufferIn& buffer)
  {
    if (buffer.bufferSize() == 0) notifyInType_= NOTIFY_NOTHING ;
    else
    {
      buffer>>notifyInType_;
      if (notifyInType_==NOTIFY_CREATE_INTERCOMM)
      {
        auto& arg=notifyInCreateIntercomm_ ;
        buffer >> std::get<0>(arg)>> std::get<1>(arg) ;
      }
    }
  }

  void CServerContext::checkNotifications(void)
  {
    if (!hasNotification_)
    {
      if (useWindowManager_)
      {
        if (winNotify_==nullptr) return ;
        int flag ;
        MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
        double time=MPI_Wtime() ;
        if (time-lastEventLoop_ > eventLoopLatency_) 
        {
          int commRank ;
          MPI_Comm_rank(contextComm_, &commRank) ;
          winNotify_->popFromExclusiveWindow(commRank, this, &CServerContext::notificationsDumpIn) ;
          if (notifyInType_!= NOTIFY_NOTHING)  hasNotification_=true ;
          lastEventLoop_=time ;
        }
      }
      else if (CXios::getNotificationsManager()->recvNotification(hashNotify_,   this, &CServerContext::notificationsDumpIn)) hasNotification_=true ;
     
      if (hasNotification_)
      {
        auto eventScheduler=parentService_->getEventScheduler() ;
        std::hash<string> hashString ;
        size_t hashId = hashString(name_) ;
        size_t currentTimeLine=0 ;
        eventScheduler->registerEvent(currentTimeLine,hashId); 
      }
    }
    
    if (hasNotification_)
    {
      auto eventScheduler=parentService_->getEventScheduler() ;
      std::hash<string> hashString ;
      size_t hashId = hashString(name_) ;
      size_t currentTimeLine=0 ;
      if (eventScheduler->queryEvent(currentTimeLine,hashId))
      {
        eventScheduler->popEvent() ;
        if (notifyInType_==NOTIFY_CREATE_INTERCOMM) createIntercomm() ;
        hasNotification_=false ;
      }
    }
  }


  bool CServerContext::eventLoop(bool serviceOnly)
  {
    bool finished=false ;
    if (!CXios::getContextsManager()->hasContext(name_)) return finished; // sure to be registered

    if (info.isActive(logTimers)) CTimer::get("CServerContext::eventLoop").resume();
    
    if (!isContextInitialized_)
    {
      context_->init(this, contextComm_, type_) ;
      isContextInitialized_=true ;
    }

    checkNotifications() ;


    if (!serviceOnly && context_!=nullptr)  
    {
      if (context_->eventLoop())
      {
        info(100)<<"Remove context server with id "<<context_->getId()<<endl ;
        CContext::removeContext(context_->getId()) ;
        context_=nullptr ;
        // destroy context ??? --> later
      }
    }
    if (info.isActive(logTimers)) CTimer::get("CServerContext::eventLoop").suspend();
    if (context_==nullptr && finalizeSignal_) finished=true ;
    return finished ;
  }

  void CServerContext::threadEventLoop(void)
  {
    
    if (info.isActive(logTimers)) CTimer::get("CServerContext::eventLoop").resume();
    info(100)<<"Launch Thread for CServerContext::threadEventLoop, context id = "<<context_->getId()<<endl ;
    CThreadManager::threadInitialize() ; 
    do
    {

      while(!CXios::getContextsManager()->hasContext(name_)) CThreadManager::yield() ; // sure to be registered)

      if (!isContextInitialized_)
      {
        context_->init(this, contextComm_, type_) ;
        isContextInitialized_=true ;
      }

      checkNotifications() ;


      if (context_!=nullptr)  
      {
        if (context_->eventLoop())
        {
          info(100)<<"Remove context server with id "<<context_->getId()<<endl ;
          CContext::removeContext(context_->getId()) ;
          context_=nullptr ;
          // destroy context ??? --> later
        }
      }
      if (context_==nullptr && finalizeSignal_) finished_=true ;
 
      if (!finished_) CThreadManager::yield() ;
    }
    while (!finished_) ;
    
    CThreadManager::threadFinalize() ;
    info(100)<<"Close thread for CServerContext::threadEventLoop"<<endl ;
    if (info.isActive(logTimers)) CTimer::get("CServerContext::eventLoop").suspend();
  }

  void CServerContext::createIntercomm(void)
  {
    info(40)<<"CServerContext::createIntercomm  : received createIntercomm notification"<<endl ;

     MPI_Comm interCommServer, interCommClient ;
     auto& arg=notifyInCreateIntercomm_ ;
     int remoteLeader=get<0>(arg) ;
     string sourceContext=get<1>(arg) ;

     auto it=overlapedComm_.find(sourceContext) ;
     int overlap=0 ;
     if (it!=overlapedComm_.end())
     {
       get<0>(it->second)=true ;
       overlap=1 ;
     }
     int nOverlap ;  
     MPI_Allreduce(&overlap, &nOverlap, 1, MPI_INT, MPI_SUM, contextComm_) ;
     int commSize ;
     MPI_Comm_size(contextComm_,&commSize ) ;

    if (nOverlap==0)
    { 
      info(10)<<"CServerContext::createIntercomm : No overlap ==> context in server mode"<<endl ;
      xios::MPI_Intercomm_create(contextComm_, 0, xiosComm_, remoteLeader, 3141, &interCommServer) ;
      CXios::getMpiGarbageCollector().registerCommunicator(interCommServer) ;
      xios::MPI_Comm_dup(interCommServer,&interCommClient) ;
      CXios::getMpiGarbageCollector().registerCommunicator(interCommClient) ;
      context_ -> createClientInterComm(interCommClient,interCommServer) ;
      clientsInterComm_.push_back(interCommClient) ;
      clientsInterComm_.push_back(interCommServer) ;
    }
    else
    {
      ERROR("void CServerContext::createIntercomm(void)",<<"CServerContext::createIntercomm : overlap ==> not managed") ;
    }
   
  }

  void CServerContext::freeComm(void)
  {
    //delete winNotify_ ;
    //winNotify_=nullptr ;
    //xios::MPI_Comm_free(&contextComm_) ;
    // don't forget intercomm -> later
  }
  
  void CServerContext::finalizeSignal(void)
  {
    finalizeSignal_=true ;
  }

}
