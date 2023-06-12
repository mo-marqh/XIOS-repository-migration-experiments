#include "servers_ressource.hpp"
#include "window_manager.hpp"
#include "ressources_manager.hpp"
#include "pool_ressource.hpp"
#include "cxios.hpp"
#include "mpi.hpp"
#include "timer.hpp"
#include <vector>
#include <string>




namespace xios
{
  using namespace std ;

  CServersRessource::CServersRessource(MPI_Comm serverComm) : poolRessource_(nullptr), finalizeSignal_(false)
  {

    MPI_Comm_dup(serverComm, &serverComm_) ;
    MPI_Comm xiosComm=CXios::getXiosComm() ;
  
    int localRank, globalRank ;
    MPI_Comm_rank(xiosComm,&globalRank) ;
    MPI_Comm_rank(serverComm_,&localRank) ;
    
    winNotify_ = new CWindowManager(serverComm_, maxBufferSize_) ;
    MPI_Barrier(serverComm_) ;
    if (localRank==localLeader_) 
    {
      int commSize ;
      MPI_Comm_size(serverComm_,&commSize) ;
      CXios::getRessourcesManager()->registerServerLeader(globalRank) ;
      CXios::getRessourcesManager()->registerRessourcesSize(commSize) ;
      freeRessourcesRank_.resize(commSize) ;
      for(int i=0;i<commSize;i++) freeRessourcesRank_[i]=i ;
    }

    MPI_Comm_dup(serverComm_, &freeRessourcesComm_) ; 

  }

  void CServersRessource::createPool(const string& poolId, const int size)
  {
    int commSize ;
    MPI_Comm_size(serverComm_,&commSize) ;
    vector<int> newFreeRessourcesRank(freeRessourcesRank_.size()-size) ;

    bool isPartOf ;

    for(int i=0, j=0; i<freeRessourcesRank_.size();i++) 
    {
       if (i<size) isPartOf=true ;
       else 
       {
         isPartOf=false ;
         newFreeRessourcesRank[j]=freeRessourcesRank_[i] ;
         j++ ;
       }
       
       notifyOutType_=NOTIFY_CREATE_POOL ;
       notifyOutCreatePool_ = make_tuple(poolId, isPartOf) ;
       sendNotification(freeRessourcesRank_[i]) ;
    }
    freeRessourcesRank_ = std::move(newFreeRessourcesRank) ;
  }

  void CServersRessource::finalize(void)
  {
    int commSize ;
    MPI_Comm_size(serverComm_,&commSize) ;

    for(int rank=0; rank<commSize;rank++)
    { 
      notifyOutType_=NOTIFY_FINALIZE ;
      sendNotification(rank) ;
    }
  }

  void CServersRessource::sendNotification(int rank)
  {
    winNotify_->pushToExclusiveWindow(rank, this, &CServersRessource::notificationsDumpOut) ;
  }


  void CServersRessource::notificationsDumpOut(CBufferOut& buffer)
  {
    
    buffer.realloc(maxBufferSize_) ;
    
    if (notifyOutType_==NOTIFY_CREATE_POOL)
    {
      auto& arg=notifyOutCreatePool_ ;
      buffer << notifyOutType_ << std::get<0>(arg) << std::get<1>(arg) ;
    }
    else if (notifyOutType_==NOTIFY_FINALIZE) buffer << notifyOutType_ ;
  }

  void CServersRessource::notificationsDumpIn(CBufferIn& buffer)
  {
    if (buffer.bufferSize() == 0) notifyInType_= NOTIFY_NOTHING ;
    else
    {
      buffer>>notifyInType_;
      if (notifyInType_==NOTIFY_CREATE_POOL)
      {
        auto& arg=notifyInCreatePool_ ;
        buffer >> std::get<0>(arg) >> std::get<1>(arg)  ;
      }
      else if (notifyInType_==NOTIFY_FINALIZE) { /*nothing to do*/}
    }
  }

  bool CServersRessource::eventLoop(bool serviceOnly)
  {
    CTimer::get("CServersRessource::eventLoop").resume();
    double time=MPI_Wtime() ;
    int flag ;
    MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);

    if (time-lastEventLoop_ > eventLoopLatency_) 
    {
      checkNotifications() ;
      lastEventLoop_=time ;
    }

    if (poolRessource_!=nullptr) 
    {
      if (poolRessource_->eventLoop(serviceOnly))
      {
        delete poolRessource_ ;
        poolRessource_=nullptr ;
        // don't forget to free pool ressource later
      } 
    }
    CTimer::get("CServersRessource::eventLoop").suspend();
    if (poolRessource_==nullptr && finalizeSignal_) return true ;
    else return false ;
  }

  void CServersRessource::checkNotifications(void)
  {
    int commRank ;
    MPI_Comm_rank(serverComm_, &commRank) ;
    winNotify_->popFromExclusiveWindow(commRank, this, &CServersRessource::notificationsDumpIn) ;
    if (notifyInType_==NOTIFY_CREATE_POOL) createPool() ;
    else if (notifyInType_==NOTIFY_FINALIZE) finalizeSignal() ;
  }

  void CServersRessource::createPool(void)
  {
    auto& arg=notifyInCreatePool_ ;
    string poolId=get<0>(arg) ;
    bool isPartOf=get<1>(arg) ;
    
    int commRank ;
    MPI_Comm poolComm ;
    MPI_Comm_rank(freeRessourcesComm_,&commRank) ;
    MPI_Comm_split(freeRessourcesComm_, isPartOf, commRank, &poolComm) ;
    
    if (isPartOf)
    {  
      poolRessource_ = new CPoolRessource(poolComm, poolId, true) ;
      MPI_Comm_free(&poolComm) ;
    }
    else 
    {
      MPI_Comm_free(&freeRessourcesComm_) ;
      freeRessourcesComm_=poolComm ;
    }

  }
  
  void CServersRessource::finalizeSignal(void)
  {
    finalizeSignal_=true ;
    if (poolRessource_!=nullptr) poolRessource_->finalizeSignal() ;
  }

  bool CServersRessource::isServerLeader(void)
  {
    int commRank ;
    MPI_Comm_rank(serverComm_,&commRank) ;
    if (commRank==localLeader_) return true ;
    else return false ;
  }

  CServersRessource::~CServersRessource()
  {
    delete winNotify_ ;
  }
}
