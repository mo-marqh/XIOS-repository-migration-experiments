#ifndef __CONTEXT_SERVER_HPP__
#define __CONTEXT_SERVER_HPP__
#include "xios_spl.hpp"
#include "event_server.hpp"
#include "buffer_server.hpp"
#include "mpi.hpp"
#include "event_scheduler.hpp"

namespace xios
{
  class CContext ;
  class CContextClient;

  class CContextServer
  {
    public:
    
    enum ETransport { generic, legacy, legacyV2, p2p, oneSided}  ;
      
    template<ETransport transport=generic> 
    static CContextServer* getNew(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm) ;
    
    CContextServer(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm) ;
    virtual ~CContextServer() {} 
    void setAssociatedClient(CContextClient* associatedClient) {associatedClient_=associatedClient ;}
    CContextClient* getAssociatedClient(void) { return associatedClient_ ;}
    int getIntraCommRank(void) { return intraCommRank_ ;}
    int getIntraCommSize(void) { return intraCommSize_ ;}

    virtual bool eventLoop(bool enableEventsProcessing = true) = 0 ;
    virtual void releaseBuffers(void)=0;
    virtual bool hasPendingEvent(void)=0;
    
    protected :

      MPI_Comm intraComm_ ;
      int intraCommSize_ ;
      int intraCommRank_ ;

      MPI_Comm interComm_ ;
      int commSize_ ;
      int clientSize_ ;

      CContext* context_ ;
      CContextClient* associatedClient_ ;

      size_t hashId_ ;
      shared_ptr<CEventScheduler> eventScheduler_=nullptr ;
  } ;
  
  template<>
  CContextServer* CContextServer::getNew<CContextServer::generic>(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm) ;
 
  template<>
  CContextServer* CContextServer::getNew<CContextServer::oneSided>(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm) ;

  template<>
  CContextServer* CContextServer::getNew<CContextServer::p2p>(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm) ;

  template<>
  CContextServer* CContextServer::getNew<CContextServer::legacyV2>(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm) ;

}

#endif
