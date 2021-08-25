#include "pool_ressource.hpp"
#include "services.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "message.hpp"
#include "type.hpp"
#include "cxios.hpp"

namespace xios
{
  CPoolRessource::CPoolRessource(MPI_Comm poolComm, const std::string& Id) : Id_(Id), finalizeSignal_(false)
  {
    int commRank, commSize ;
    MPI_Comm_dup(poolComm, &poolComm_) ;
    winNotify_ = new CWindowManager(poolComm_, maxBufferSize_) ;
    MPI_Comm_rank(poolComm, &commRank) ;
    MPI_Comm_size(poolComm, &commSize) ;


    if (commRank==localLeader_)
    {
      for(int i=0; i<commSize;i++) occupancy_.insert(std::pair<char,int>(0,i)) ; 
      int globalLeaderRank ;
      MPI_Comm_rank(CXios::getXiosComm(),&globalLeaderRank) ;
      CXios::getRessourcesManager()->registerPool(Id, commSize, globalLeaderRank) ;
    }
    
    winNotify_->lockWindow(commRank,0) ;
    winNotify_->updateToWindow(commRank, this, &CPoolRessource::createServiceDumpOut) ;  
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

    for(int rank=0; rank<commSize; rank++)
    {
      if (procs_in[rank]) createServiceNotify(rank, serviceId, type, size, nbPartitions, true) ;
      else createServiceNotify(rank, serviceId, type, size, nbPartitions, false) ;
    }
  }

  
  void CPoolRessource::createServiceNotify(int rank, const std::string& serviceId, int type, int size, int nbPartitions, 
                                           bool in)
  {
    winNotify_->lockWindow(rank,0) ;
    winNotify_->updateFromWindow(rank, this, &CPoolRessource::createServiceDumpIn) ;
    notifications_.push_back(std::make_tuple(serviceId,type,size,nbPartitions,in)) ;
    winNotify_->updateToWindow(rank, this, &CPoolRessource::createServiceDumpOut) ;  
    winNotify_->unlockWindow(rank,0) ;   
  }


  void CPoolRessource::createServiceDumpOut(CBufferOut& buffer)
  {
    buffer.realloc(maxBufferSize_) ;
   
    buffer << (int) (notifications_.size());
    
    for(auto it=notifications_.begin();it!=notifications_.end(); ++it) 
      buffer << std::get<0>(*it) << static_cast<int>(std::get<1>(*it))<< std::get<2>(*it)<< std::get<3>(*it) << std::get<4>(*it)  ;
  }


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

  bool CPoolRessource::eventLoop(bool serviceOnly)
  {
    checkCreateServiceNotification() ;
    for (auto it=services_.begin(); it!=services_.end() ; ++it) 
    {
      if (it->second->eventLoop(serviceOnly))
      {
        services_.erase(it) ;
        // don't forget to free service later
        break ;
      }
    }

    if (services_.empty() && finalizeSignal_) return true ;
    else return false ;
  }

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

  void CPoolRessource::createNewService(const std::string& serviceId, int type, int size, int nbPartitions, bool in)
  {
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

  void CPoolRessource::createService(MPI_Comm serviceComm, const std::string& serviceId, int partitionId, int type, int nbPartitions) // for clients & attached
  {
    services_[std::make_tuple(serviceId,partitionId)] = new CService(serviceComm, Id_, serviceId, partitionId, type, nbPartitions) ;
  }


  void CPoolRessource::finalizeSignal(void)
  {
    finalizeSignal_=true ;
    for (auto it=services_.begin(); it!=services_.end() ; ++it) it->second->finalizeSignal() ;
  }

}