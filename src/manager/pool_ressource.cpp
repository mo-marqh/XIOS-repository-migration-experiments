#include "pool_ressource.hpp"
#include "services.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "message.hpp"
#include "type.hpp"
#include "cxios.hpp"
#include "timer.hpp"
#include "event_scheduler.hpp"
#include "thread_manager.hpp"

namespace xios
{
  extern CLogType logTimers ;
  
  CPoolRessource::CPoolRessource(MPI_Comm poolComm, shared_ptr<CEventScheduler> eventScheduler, const std::string& Id, bool isServer) : Id_(Id), finalizeSignal_(false)
  {
    useWindowManager_ = CXios::servicesUseWindowManager ;

    int commRank, commSize ;
    xios::MPI_Comm_dup(poolComm, &poolComm_) ;
    CXios::getMpiGarbageCollector().registerCommunicator(poolComm_) ;
    std::hash<string> hashString ;
    hashId_ = hashString("CPoolRessource::"+Id) ;

    if (useWindowManager_) winNotify_ = new CWindowManager(poolComm_, maxBufferSize_,"CPoolRessource::winNotify_") ;
    hashNotify_ = hashString("CPoolRessource::"+Id) ;

    MPI_Comm_rank(poolComm, &commRank) ;
    MPI_Comm_size(poolComm, &commSize) ;
    info(40)<<"CPoolRessource::CPoolRessource  : creating new pool : "<<Id<<endl ;
    if (commRank==localLeader_)
    {
      for(int i=0; i<commSize;i++) occupancy_.insert(std::pair<char,int>(0,i)) ; 
      int globalLeaderRank ;
      MPI_Comm_rank(CXios::getXiosComm(),&globalLeaderRank) ;
      if (isServer) CXios::getRessourcesManager()->registerPoolServer(Id, commSize, globalLeaderRank) ;
    }
    
    notifyType_=NOTIFY_NOTHING;
     if (useWindowManager_) winNotify_->updateToExclusiveWindow(commRank, this, &CPoolRessource::notificationsDumpOut) ;
    MPI_Barrier(poolComm_) ;
    if (eventScheduler) eventScheduler_=eventScheduler ;
    else eventScheduler_= make_shared<CEventScheduler>(poolComm) ;
    freeRessourceEventScheduler_ = eventScheduler_ ;
    
    if (CThreadManager::isUsingThreads()) CThreadManager::spawnThread(&CPoolRessource::threadEventLoop, this) ;
  }

  void CPoolRessource::synchronize(void)
  {
    bool out=false ; 
    size_t timeLine=0 ;
          
    eventScheduler_->registerEvent(timeLine, hashId_) ;
    while (!out)
    {
      CThreadManager::yield() ;
      out = eventScheduler_->queryEvent(timeLine,hashId_) ;
      if (out) eventScheduler_->popEvent() ;
    }
  }


  void CPoolRessource::sendNotification(int rank)
  {
    winNotify_->pushToExclusiveWindow(rank, this, &CPoolRessource::notificationsDumpOut) ;
  }

  void CPoolRessource::createService(const std::string& serviceId, int type, int size, int nbPartitions)
  {
    info(40)<<"CPoolRessource::createService  : notify createService to all pool members ; serviceId : "<<serviceId<<endl ;
    notifyType_=NOTIFY_CREATE_SERVICE ;
    notifyCreateService_=make_tuple(serviceId, type, size, nbPartitions) ;
    if (useWindowManager_)
    {
      int commSize ;
      MPI_Comm_size(poolComm_, &commSize) ; 
      for(int rank=0; rank<commSize; rank++) sendNotification(rank) ;
    }
    else CXios::getNotificationsManager()->sendLockedNotification(hashNotify_, this, &CPoolRessource::notificationsDumpOut) ;
  }
  
  void CPoolRessource::createServiceOnto(const std::string& serviceId, int type, const std::string& onServiceId)
  {
    info(40)<<"CPoolRessource::createServiceOnto  : notify createServiceOnto to all pool members ; serviceId : "<<serviceId
            <<"  onto service Id  :"<< serviceId<<endl ;
    notifyType_=NOTIFY_CREATE_SERVICE_ONTO ;
    notifyCreateServiceOnto_=make_tuple(serviceId, type, onServiceId) ;
    if (useWindowManager_)
    {
      int commSize ;
      MPI_Comm_size(poolComm_, &commSize) ; 
      for(int rank=0; rank<commSize; rank++) sendNotification(rank) ;
    }
    CXios::getNotificationsManager()->sendLockedNotification(hashNotify_, this, &CPoolRessource::notificationsDumpOut) ;
  }


  void CPoolRessource::checkNotifications(void)
  {
    bool recv=false ;
    if (useWindowManager_)
    { 
      double time=MPI_Wtime() ;
      int flag ;
      MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
      int commRank ;
      MPI_Comm_rank(poolComm_, &commRank) ;
      if (time-lastEventLoop_ > eventLoopLatency_) 
      {
        winNotify_->popFromExclusiveWindow(commRank, this, &CPoolRessource::notificationsDumpIn) ;
        if (notifyType_==NOTIFY_CREATE_SERVICE || notifyType_==NOTIFY_CREATE_SERVICE_ONTO) recv=true ;
        lastEventLoop_=time ;
      }
    }
    else if (CXios::getNotificationsManager()->recvNotification(hashNotify_,   this, &CPoolRessource::notificationsDumpIn)) recv=true ;

    if (recv)
    {
      if (notifyType_==NOTIFY_CREATE_SERVICE) 
      {
        if (CThreadManager::isUsingThreads()) synchronize() ;
        createService() ;
      }
      else if (notifyType_==NOTIFY_CREATE_SERVICE_ONTO) 
      {
        if (CThreadManager::isUsingThreads()) synchronize() ;
        createServiceOnto() ;
      }
    }
  }

  void CPoolRessource::notificationsDumpOut(CBufferOut& buffer)
  {
    if (useWindowManager_) buffer.realloc(maxBufferSize_) ;
    
    if (notifyType_==NOTIFY_CREATE_SERVICE)
    {
      auto& arg=notifyCreateService_ ;
      buffer << notifyType_<< get<0>(arg) << get<1>(arg) << std::get<2>(arg) << get<3>(arg) ;
    }
    else if (notifyType_==NOTIFY_CREATE_SERVICE_ONTO)
    {
      auto& arg=notifyCreateServiceOnto_ ;
      buffer << notifyType_<< get<0>(arg) << get<1>(arg)<< get<2>(arg)  ;
    }
  }


  void CPoolRessource::notificationsDumpIn(CBufferIn& buffer)
  {
    if (buffer.bufferSize() == 0) notifyType_= NOTIFY_NOTHING ;
    else
    {
      buffer>>notifyType_;
      if (notifyType_==NOTIFY_CREATE_SERVICE)
      {
        auto& arg=notifyCreateService_ ;
        buffer >> get<0>(arg) >> get<1>(arg) >> std::get<2>(arg)>> get<3>(arg) ;
      }
      else if (notifyType_==NOTIFY_CREATE_SERVICE_ONTO)
      {
        auto& arg=notifyCreateServiceOnto_ ;
        buffer >> get<0>(arg) >> get<1>(arg) >> get<2>(arg) ;
      }
    }
  }  

  void CPoolRessource::createService(void)
  {
    auto& arg = notifyCreateService_ ;
    createNewService(get<0>(arg), get<1>(arg), get<2>(arg), get<3>(arg)) ;
  }
  
  void CPoolRessource::createServiceOnto(void)
  {
    auto& arg = notifyCreateServiceOnto_ ;
    createNewServiceOnto(get<0>(arg), get<1>(arg), get<2>(arg)) ;
  }

  bool CPoolRessource::eventLoop(bool serviceOnly)
  {
    if (info.isActive(logTimers)) CTimer::get("CPoolRessource::eventLoop").resume();
   
    checkNotifications() ;
    
    for (auto it=services_.begin(); it!=services_.end() ; ++it) 
    {
      if (it->second->eventLoop(serviceOnly))
      {
        delete it->second ;
        services_.erase(it) ;
        // don't forget to free service later
        break ;
      }
    }
    if (info.isActive(logTimers)) CTimer::get("CPoolRessource::eventLoop").suspend();
    if (services_.empty() && finalizeSignal_) finished_=true ;
    return finished_ ;
  }

  void CPoolRessource::threadEventLoop(void)
  {
    if (info.isActive(logTimers)) CTimer::get("CPoolRessource::eventLoop").resume();
    info(100)<<"Launch Thread for  CPoolRessource::threadEventLoop, pool id = "<<Id_<<endl ;
    CThreadManager::threadInitialize() ; 
    
    do
    {
      checkNotifications() ;
    
      for(auto it=services_.begin();it!=services_.end();++it) 
      {
        if (it->second->isFinished())
        {
          delete it->second ; 
          services_.erase(it) ;
          // destroy server_context -> to do later
          break ;
        } ;
      }

      if (services_.empty() && finalizeSignal_) finished_=true ;
      
      if (!finished_) CThreadManager::yield() ;
    
    } while (!finished_) ;

    CThreadManager::threadFinalize() ;
    if (info.isActive(logTimers)) CTimer::get("CPoolRessource::eventLoop").suspend();
    info(100)<<"Close thread for  CPoolRessource::threadEventLoop, pool id = "<<Id_<<endl ;
  }

  void CPoolRessource::createNewService(const std::string& serviceId, int type, int size, int nbPartitions)
  {
     
    info(40)<<"CPoolRessource::createNewService  : receive createService notification ; serviceId : "<<serviceId<<endl ;
    MPI_Comm serviceComm, newServiceComm, freeComm ;
    int commRank ;
     
    int color;
    if (!services_.empty()) color = 0 ;
    else color=1 ; 
    MPI_Comm_rank(poolComm_,&commRank) ;
    xios::MPI_Comm_split(poolComm_, color, commRank, &freeComm) ;  // workaround

    
    if (services_.empty()) 
    {
      int commSize ;
      MPI_Comm_size(freeComm,&commSize) ;
      if (commSize<size) ERROR("void CPoolRessource::createNewService(const std::string& serviceId, int type, int size, int nbPartitions)",
                               << "No enough free ressources on pool id="<<getId()<<" to launch service id="<<serviceId);


      MPI_Comm_rank(freeComm,&commRank) ;
      bool in ;
      if (commRank < size) in=true ;
      else in=false ;

      xios::MPI_Comm_split(freeComm, in, commRank, &serviceComm) ;
      
      
      // temporary for event scheduler, we must using hierarchical split of free ressources communicator.
      // we hope for now that spliting using occupancy make this match

      if (in)
      {
        int serviceCommSize ;
        int serviceCommRank ;
        MPI_Comm_size(serviceComm,&serviceCommSize) ;
        MPI_Comm_rank(serviceComm,&serviceCommRank) ;

        info(10)<<"Service  "<<serviceId<<" created "<<"  service size : "<<serviceCommSize<< "   service rank : "<<serviceCommRank 
                              <<" on rank pool "<<commRank<<endl ;
       
        int partitionId ; 
        if ( serviceCommRank >= (serviceCommSize/nbPartitions+1)*(serviceCommSize%nbPartitions) )
        {
          int rank =  serviceCommRank - (serviceCommSize/nbPartitions+1)*(serviceCommSize%nbPartitions) ;
          partitionId = serviceCommSize%nbPartitions +  rank / (serviceCommSize/nbPartitions) ;
        }
        else  partitionId = serviceCommRank / (serviceCommSize/nbPartitions + 1) ;

        xios::MPI_Comm_split(serviceComm, partitionId, commRank, &newServiceComm) ;

        MPI_Comm_size(newServiceComm,&serviceCommSize) ;
        MPI_Comm_rank(newServiceComm,&serviceCommRank) ;
        info(10)<<"Service  "<<serviceId<<" created "<<"  partition : " <<partitionId<<" service size : "<<serviceCommSize
                << " service rank : "<<serviceCommRank <<" on rank pool "<<commRank<<endl ;
      
        shared_ptr<CEventScheduler> parentScheduler, childScheduler ;
        freeRessourceEventScheduler_->splitScheduler(newServiceComm, parentScheduler, childScheduler) ;
        if (isFirstSplit_) eventScheduler_ = parentScheduler ;
        isFirstSplit_=false ;

        services_[std::make_tuple(serviceId,partitionId)] = new CService(newServiceComm, childScheduler, Id_, serviceId, partitionId, type, nbPartitions) ;
       
        xios::MPI_Comm_free(&newServiceComm) ;
      }
      else
      {
        shared_ptr<CEventScheduler> parentScheduler, childScheduler ;
        freeRessourceEventScheduler_->splitScheduler(serviceComm, parentScheduler, childScheduler) ;
        if (isFirstSplit_) eventScheduler_ = parentScheduler ;
        freeRessourceEventScheduler_ = childScheduler ;
        isFirstSplit_=false ;
      }
      xios::MPI_Comm_free(&serviceComm) ;
    }
    xios::MPI_Comm_free(&freeComm) ;
  }
  
  void CPoolRessource::createNewServiceOnto(const std::string& serviceId, int type, const std::string& onServiceId)
  {
     
    info(40)<<"CPoolRessource::createNewServiceOnto  : receive createServiceOnto notification ; serviceId : "
            <<serviceId<<"  ontoServiceId : "<<onServiceId<<endl ;
    for(auto& service : services_) 
    {
      if (std::get<0>(service.first)==onServiceId)
      {
        const MPI_Comm& serviceComm = service.second->getCommunicator() ;
        MPI_Comm newServiceComm ;
        xios::MPI_Comm_dup(serviceComm, &newServiceComm) ;
        CXios::getMpiGarbageCollector().registerCommunicator(newServiceComm) ;
        int nbPartitions = service.second->getNbPartitions() ;
        int partitionId = service.second->getPartitionId() ;
        shared_ptr<CEventScheduler>  eventScheduler = service.second->getEventScheduler() ;
        info(40)<<"CPoolRessource::createNewServiceOnto ; found onServiceId : "<<onServiceId<<endl  ;
        services_[std::make_tuple(serviceId,partitionId)] = new CService(newServiceComm, eventScheduler, Id_, serviceId, partitionId, type,
                                                                         nbPartitions) ;       
      }
    }
    
  }

  void CPoolRessource::createService(MPI_Comm serviceComm, shared_ptr<CEventScheduler> eventScheduler, const std::string& serviceId, int partitionId, int type, int nbPartitions) // for clients
  {
    services_[std::make_tuple(serviceId,partitionId)] = new CService(serviceComm, eventScheduler, Id_, serviceId, partitionId, type, nbPartitions) ;
  }


  void CPoolRessource::finalizeSignal(void)
  {
    finalizeSignal_=true ;
    for (auto it=services_.begin(); it!=services_.end() ; ++it) it->second->finalizeSignal() ;
  }  
  
  CPoolRessource::~CPoolRessource()
  {
    if (useWindowManager_) delete winNotify_ ;
    for(auto& service : services_) delete service.second ;
  }
}
