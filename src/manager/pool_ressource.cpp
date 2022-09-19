#include "pool_ressource.hpp"
#include "services.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "message.hpp"
#include "type.hpp"
#include "cxios.hpp"
#include "timer.hpp"

namespace xios
{
  CPoolRessource::CPoolRessource(MPI_Comm poolComm, const std::string& Id) : Id_(Id), finalizeSignal_(false)
  {
    int commRank, commSize ;
    MPI_Comm_dup(poolComm, &poolComm_) ;
    winNotify_ = new CWindowManager(poolComm_, maxBufferSize_) ;
    MPI_Comm_rank(poolComm, &commRank) ;
    MPI_Comm_size(poolComm, &commSize) ;
    info(40)<<"CPoolRessource::CPoolRessource  : creating new pool : "<<Id<<endl ;
    if (commRank==localLeader_)
    {
      for(int i=0; i<commSize;i++) occupancy_.insert(std::pair<char,int>(0,i)) ; 
      int globalLeaderRank ;
      MPI_Comm_rank(CXios::getXiosComm(),&globalLeaderRank) ;
      CXios::getRessourcesManager()->registerPool(Id, commSize, globalLeaderRank) ;
    }
    
    winNotify_->lockWindow(commRank,0) ;
    winNotify_->updateToWindow(commRank, this, &CPoolRessource::notificationsDumpOut) ;
    winNotify_->unlockWindow(commRank,0) ;       
    MPI_Barrier(poolComm_) ;
  }

  void CPoolRessource::createService(const std::string& serviceId, int type, int size, int nbPartitions)
  {
    // for now suppose nbPartitions=1
    
    auto it=occupancy_.begin() ;
    int commSize ;
    MPI_Comm_size(poolComm_, &commSize) ;
    vector<bool> procs_in(commSize,false) ;
    vector<pair<int,int>> procs_update ;

    for(int i=0; i<size; i++) 
    {
      procs_in[it->second]=true ;
      procs_update.push_back(std::pair<int,int>(it->first+1,it->second)) ;
      ++it ;
    }
    
    occupancy_.erase(occupancy_.begin(),it) ;
    occupancy_.insert(procs_update.begin(),procs_update.end()) ;
    
    info(40)<<"CPoolRessource::createService  : notify createService to all pool members ; serviceId : "<<serviceId<<endl ;
    for(int rank=0; rank<commSize; rank++)
    {
      if (procs_in[rank]) createServiceNotify(rank, serviceId, type, size, nbPartitions, true) ;
      else createServiceNotify(rank, serviceId, type, size, nbPartitions, false) ;
    }
  }

  void CPoolRessource::createServiceOnto(const std::string& serviceId, int type, const std::string& onServiceId)
  {
    // for now suppose nbPartitions=1
    
    auto it=occupancy_.begin() ;
    int commSize ;
    MPI_Comm_size(poolComm_, &commSize) ;
    
    info(40)<<"CPoolRessource::createService  : notify createServiceOnto to all pool members ; serviceId : "<<serviceId
            <<"  onto service Id  :"<< serviceId<<endl ;
    for(int rank=0; rank<commSize; rank++) createServiceOntoNotify(rank, serviceId, type, onServiceId) ;
  }

/*  
  void CPoolRessource::createServiceNotify(int rank, const std::string& serviceId, int type, int size, int nbPartitions, 
                                           bool in)
  {
    winNotify_->lockWindow(rank,0) ;
    winNotify_->updateFromWindow(rank, this, &CPoolRessource::createServiceDumpIn) ;
    notifications_.push_back(std::make_tuple(serviceId,type,size,nbPartitions,in)) ;
    winNotify_->updateToWindow(rank, this, &CPoolRessource::createServiceDumpOut) ;  
    winNotify_->unlockWindow(rank,0) ;   
  }
*/
  
  void CPoolRessource::createServiceNotify(int rank, const string& serviceId, int type, int size, int nbPartitions, bool in)
  {
    notifyType_=NOTIFY_CREATE_SERVICE ;
    notifyCreateService_=make_tuple(serviceId, type, size, nbPartitions, in ) ;
    sendNotification(rank) ;
  }


  void CPoolRessource::createServiceOntoNotify(int rank, const string& serviceId, int type, const string& onServiceId)
  {
    notifyType_=NOTIFY_CREATE_SERVICE_ONTO ;
    notifyCreateServiceOnto_=make_tuple(serviceId, type, onServiceId) ;
    sendNotification(rank) ;
  }


  void CPoolRessource::sendNotification(int rank)
  {
    winNotify_->lockWindowExclusive(rank) ;
    winNotify_->pushToLockedWindow(rank, this, &CPoolRessource::notificationsDumpOut) ;
    winNotify_->unlockWindow(rank) ;
  }

  void CPoolRessource::checkNotifications(void)
  {
    int commRank ;
    MPI_Comm_rank(poolComm_, &commRank) ;
    winNotify_->lockWindowExclusive(commRank) ;
    winNotify_->popFromLockedWindow(commRank, this, &CPoolRessource::notificationsDumpIn) ;
    winNotify_->unlockWindow(commRank) ;
    if (notifyType_==NOTIFY_CREATE_SERVICE) createService() ;
    else if (notifyType_==NOTIFY_CREATE_SERVICE_ONTO) createServiceOnto() ;
  }


  void CPoolRessource::notificationsDumpOut(CBufferOut& buffer)
  {

    buffer.realloc(maxBufferSize_) ;
    
    if (notifyType_==NOTIFY_CREATE_SERVICE)
    {
      auto& arg=notifyCreateService_ ;
      buffer << notifyType_<< get<0>(arg) << get<1>(arg) << std::get<2>(arg) << get<3>(arg) << get<4>(arg);
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
        buffer >> get<0>(arg) >> get<1>(arg) >> std::get<2>(arg)>> get<3>(arg)>> get<4>(arg) ;
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
    createNewService(get<0>(arg), get<1>(arg), get<2>(arg), get<3>(arg), get<4>(arg)) ;
  }
  
  void CPoolRessource::createServiceOnto(void)
  {
    auto& arg = notifyCreateServiceOnto_ ;
    createNewServiceOnto(get<0>(arg), get<1>(arg), get<2>(arg)) ;
  }

/*
  void CPoolRessource::createServiceDumpOut(CBufferOut& buffer)
  {
    buffer.realloc(maxBufferSize_) ;
   
    buffer << (int) (notifications_.size());
    
    for(auto it=notifications_.begin();it!=notifications_.end(); ++it) 
      buffer << std::get<0>(*it) << static_cast<int>(std::get<1>(*it))<< std::get<2>(*it)<< std::get<3>(*it) << std::get<4>(*it)  ;
  }

*/

/*
  void CPoolRessource::createServiceDumpIn(CBufferIn& buffer)
  {
    std::string serviceId ;
    int type ;
    int size; 
    int nbPartitions; 
    bool in ;

    notifications_.clear() ;
    int nbNotifications ;
    buffer>>nbNotifications ;
    for(int i=0;i<nbNotifications;i++) 
    {
      buffer>>serviceId>>type>>size>>nbPartitions>>in ;
      notifications_.push_back(std::make_tuple(serviceId,type,size,nbPartitions,in)) ;
    }
  }
*/

  bool CPoolRessource::eventLoop(bool serviceOnly)
  {
    CTimer::get("CPoolRessource::eventLoop").resume();
   
    double time=MPI_Wtime() ;
    int flag ;
    MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
    if (time-lastEventLoop_ > eventLoopLatency_) 
    {
      //checkCreateServiceNotification() ;
      checkNotifications() ;
      lastEventLoop_=time ;
    }
    
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
    CTimer::get("CPoolRessource::eventLoop").suspend();
    if (services_.empty() && finalizeSignal_) return true ;
    else return false ;
  }
/*
  void CPoolRessource::checkCreateServiceNotification(void)
  {
    int commRank ;
    MPI_Comm_rank(poolComm_, &commRank) ;
    winNotify_->lockWindow(commRank,0) ;
    winNotify_->updateFromWindow(commRank, this, &CPoolRessource::createServiceDumpIn) ;
    
    if (!notifications_.empty())
    {
      auto info = notifications_.front() ;
      createNewService(get<0>(info), get<1>(info), get<2>(info), get<3>(info), get<4>(info)) ;
      notifications_.pop_front() ;
      winNotify_->updateToWindow(commRank, this, &CPoolRessource::createServiceDumpOut) ;     
    }
    winNotify_->unlockWindow(commRank,0) ;

  }
*/

  void CPoolRessource::createNewService(const std::string& serviceId, int type, int size, int nbPartitions, bool in)
  {
     
     info(40)<<"CPoolRessource::createNewService  : receive createService notification ; serviceId : "<<serviceId<<endl ;
     MPI_Comm serviceComm, newServiceComm ;
     int commRank ;
     MPI_Comm_rank(poolComm_,&commRank) ;
     MPI_Comm_split(poolComm_, in, commRank, &serviceComm) ;
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

       MPI_Comm_split(serviceComm, partitionId, commRank, &newServiceComm) ;
       
       MPI_Comm_size(newServiceComm,&serviceCommSize) ;
       MPI_Comm_rank(newServiceComm,&serviceCommRank) ;
       info(10)<<"Service  "<<serviceId<<" created "<<"  partition : " <<partitionId<<" service size : "<<serviceCommSize
               << " service rank : "<<serviceCommRank <<" on rank pool "<<commRank<<endl ;
      
       services_[std::make_tuple(serviceId,partitionId)] = new CService(newServiceComm, Id_, serviceId, partitionId, type, nbPartitions) ;
       
       MPI_Comm_free(&newServiceComm) ;
     }
     MPI_Comm_free(&serviceComm) ;
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
        MPI_Comm_dup(serviceComm, &newServiceComm) ;
        int nbPartitions = service.second->getNbPartitions() ;
        int partitionId = service.second->getPartitionId() ;
        shared_ptr<CEventScheduler>  eventScheduler = service.second->getEventScheduler() ;
        info(40)<<"CPoolRessource::createNewServiceOnto ; found onServiceId : "<<onServiceId<<endl  ;
        services_[std::make_tuple(serviceId,partitionId)] = new CService(newServiceComm, Id_, serviceId, partitionId, type,
                                                                         nbPartitions, eventScheduler) ;       
      }
    }
    
  }

  void CPoolRessource::createService(MPI_Comm serviceComm, const std::string& serviceId, int partitionId, int type, int nbPartitions) // for clients & attached
  {
    services_[std::make_tuple(serviceId,partitionId)] = new CService(serviceComm, Id_, serviceId, partitionId, type, nbPartitions) ;
  }


  void CPoolRessource::finalizeSignal(void)
  {
    finalizeSignal_=true ;
    for (auto it=services_.begin(); it!=services_.end() ; ++it) it->second->finalizeSignal() ;
  }  
  
  CPoolRessource::~CPoolRessource()
  {
    delete winNotify_ ;
    for(auto& service : services_) delete service.second ;
  }
}