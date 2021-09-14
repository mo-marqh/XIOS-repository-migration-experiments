#include "server_context.hpp"
#include "contexts_manager.hpp"
#include "cxios.hpp"
#include "mpi.hpp"
#include "context.hpp"
#include "register_context_info.hpp"
#include "services.hpp"


namespace xios
{
  using namespace std ;

  map<string, tuple<bool,MPI_Comm,MPI_Comm> > CServerContext::overlapedComm_ ;

  CServerContext::CServerContext(CService* parentService, MPI_Comm contextComm, const std::string& poolId, const std::string& serviceId, 
                                 const int& partitionId, const std::string& contextId) : finalizeSignal_(false), parentService_(parentService),
                                 hasNotification_(false)
  {
   int localRank, globalRank, commSize ;

    MPI_Comm_dup(contextComm, &contextComm_) ;
    xiosComm_=CXios::getXiosComm() ;
  
    MPI_Comm_rank(xiosComm_,&globalRank) ;
    MPI_Comm_rank(contextComm_,&localRank) ;
 
    winNotify_ = new CWindowManager(contextComm_, maxBufferSize_) ;
    MPI_Barrier(contextComm_) ;
    
    int type;
    if (localRank==localLeader_) 
    {
      globalLeader_=globalRank ;
      MPI_Comm_rank(contextComm_,&commSize) ;
      
      CXios::getServicesManager()->getServiceType(poolId,serviceId, 0, type) ;
      SRegisterContextInfo contextInfo = {poolId, serviceId, partitionId, type, contextId, commSize, globalLeader_} ;
      name_ = CXios::getContextsManager()->getServerContextName(poolId, serviceId, partitionId, type, contextId) ;
      CXios::getContextsManager()->registerContext(name_, contextInfo) ;
    }
    MPI_Bcast(&type, 1, MPI_INT, localLeader_,contextComm_) ;
    name_ = CXios::getContextsManager()->getServerContextName(poolId, serviceId, partitionId, type, contextId) ;
    context_=CContext::create(name_);

    context_->init(this, contextComm, type) ;

    info(10)<<"Context "<< CXios::getContextsManager()->getServerContextName(poolId, serviceId, partitionId, type, contextId)<<" created, on local rank "<<localRank
                        <<" and global rank "<<globalRank<<endl  ;
  }

  CServerContext::~CServerContext()
  {

  } 

  bool CServerContext::createIntercomm(const string& poolId, const string& serviceId, const int& partitionId, const string& contextId, 
                                       const MPI_Comm& intraComm, MPI_Comm& interCommClient, MPI_Comm& interCommServer, bool wait)
  {
    int intraCommRank ;
    MPI_Comm_rank(intraComm, &intraCommRank) ;
    int contextLeader ;

    bool ok ;
    int type ;
    MPI_Comm newInterCommClient, newInterCommServer ;
    MPI_Comm_dup(contextComm_,&newInterCommClient) ;
    MPI_Comm_dup(contextComm_,&newInterCommServer) ;
    overlapedComm_[name_]=tuple<bool, MPI_Comm, MPI_Comm>(false, newInterCommClient, newInterCommServer) ;
    MPI_Barrier(contextComm_) ;

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
    
    MPI_Request req ;
    MPI_Status status ;
    MPI_Ibarrier(intraComm,&req) ;
    
    int flag=false ;
    while(!flag) 
    {
      CXios::getDaemonsManager()->servicesEventLoop() ;
      MPI_Test(&req,&flag,&status) ;
    }

    MPI_Bcast(&ok, 1, MPI_INT, 0, intraComm) ;

    if (ok)  
    {
      int globalRank ;
      MPI_Comm_rank(xiosComm_,&globalRank) ;
      MPI_Bcast(&contextLeader, 1, MPI_INT, 0, intraComm) ;
      
      int overlap, nOverlap ;
      if (contextLeader==globalRank) overlap=1 ;
      else overlap=0 ;
      MPI_Allreduce(&overlap, &nOverlap, 1, MPI_INT, MPI_SUM, contextComm_) ;
/*
      int overlap  ;
      if (get<0>(overlapedComm_[name_])) overlap=1 ;
      else overlap=0 ;

      int nOverlap ;  
      MPI_Allreduce(&overlap, &nOverlap, 1, MPI_INT, MPI_SUM, contextComm_) ;
      int commSize ;
      MPI_Comm_size(contextComm_,&commSize ) ;
*/
      if (nOverlap> 0 )
      {
        while (get<0>(overlapedComm_[name_])==false) CXios::getDaemonsManager()->servicesEventLoop() ;
        isAttachedMode_=true ;
        cout<<"CServerContext::createIntercomm : total overlap ==> context in attached mode"<<endl ;
        interCommClient=newInterCommClient ;
        interCommServer=newInterCommServer ;
      }
      else if (nOverlap==0)
      { 
        cout<<"CServerContext::createIntercomm : No overlap ==> context in server mode"<<endl ;
        isAttachedMode_=false ;
        MPI_Intercomm_create(intraComm, 0, xiosComm_, contextLeader, 3141, &interCommClient) ;
        MPI_Comm_dup(interCommClient, &interCommServer) ;
        MPI_Comm_free(&newInterCommClient) ;
        MPI_Comm_free(&newInterCommServer) ;
      }
      else
      {
        cout<<"CServerContext::createIntercomm : partial overlap ==> not managed"<<endl ;
      }
    }
    overlapedComm_.erase(name_) ;
    return ok ;
  }


  void CServerContext::createIntercomm(int remoteLeader, const string& sourceContext)
  {
     int commSize ;
     MPI_Comm_size(contextComm_,&commSize) ;
     for(int rank=0; rank<commSize; rank++)
     {
       notifyOutType_=NOTIFY_CREATE_INTERCOMM ;
       notifyOutCreateIntercomm_ = make_tuple(remoteLeader, sourceContext) ;
       sendNotification(rank) ;
     }
  }
  
  void CServerContext::sendNotification(int rank)
  {
    winNotify_->lockWindow(rank,0) ;
    winNotify_->pushToWindow(rank, this, &CServerContext::notificationsDumpOut) ;
    winNotify_->unlockWindow(rank,0) ;
  }

  
  void CServerContext::notificationsDumpOut(CBufferOut& buffer)
  {
    
    buffer.realloc(maxBufferSize_) ;
    
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
      int commRank ;
      MPI_Comm_rank(contextComm_, &commRank) ;
      winNotify_->lockWindow(commRank,0) ;
      winNotify_->popFromWindow(commRank, this, &CServerContext::notificationsDumpIn) ;
      winNotify_->unlockWindow(commRank,0) ;
      
      if (notifyInType_!= NOTIFY_NOTHING)
      {
        hasNotification_=true ;
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
    if (winNotify_!=nullptr) checkNotifications() ;
    if (!serviceOnly && context_!=nullptr)  
    {
      if (context_->eventLoop())
      {
        context_=nullptr ;
        // destroy context ??? --> later
      }
    }

    if (context_==nullptr && finalizeSignal_) finished=true ;
    return finished ;
  }

  void CServerContext::createIntercomm(void)
  {
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

    if (nOverlap==commSize)
    {
      info(10)<<"CServerContext::createIntercomm : total overlap ==> context in attached mode"<<endl ;
      isAttachedMode_=true ;
      interCommClient=get<2>(it->second) ;
      interCommServer=get<1>(it->second) ;
      context_ -> createClientInterComm(interCommClient, interCommServer ) ;
      clientsInterComm_.push_back(interCommClient) ;
      clientsInterComm_.push_back(interCommServer) ;
    }
    else if (nOverlap==0)
    { 
      info(10)<<"CServerContext::createIntercomm : No overlap ==> context in server mode"<<endl ;
      isAttachedMode_=false ;
      MPI_Intercomm_create(contextComm_, 0, xiosComm_, remoteLeader, 3141, &interCommServer) ;
      MPI_Comm_dup(interCommServer,&interCommClient) ;
      context_ -> createClientInterComm(interCommClient,interCommServer) ;
      clientsInterComm_.push_back(interCommClient) ;
      clientsInterComm_.push_back(interCommServer) ;
    }
    else
    {
      ERROR("void CServerContext::createIntercomm(void)",<<"CServerContext::createIntercomm : partial overlap ==> not managed") ;
    }
   
  }

  void CServerContext::freeComm(void)
  {
    delete winNotify_ ;
    winNotify_=nullptr ;
    MPI_Comm_free(&contextComm_) ;
    // don't forget intercomm -> later
  }
  
  void CServerContext::finalizeSignal(void)
  {
    finalizeSignal_=true ;
  }

}