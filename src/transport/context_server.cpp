#include "context_server.hpp"
#include "buffer_in.hpp"
#include "type.hpp"
#include "context.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "attribute_template.hpp"
#include "domain.hpp"
#include "field.hpp"
#include "file.hpp"
#include "grid.hpp"
#include "mpi.hpp"
#include "tracer.hpp"
#include "timer.hpp"
#include "cxios.hpp"
#include "event_scheduler.hpp"
#include "server.hpp"
#include "servers_ressource.hpp"
#include "pool_ressource.hpp"
#include "services.hpp"
#include "contexts_manager.hpp"
#include "timeline_events.hpp"
#include "one_sided_context_server.hpp"
#include "legacy_context_server.hpp"



#include <boost/functional/hash.hpp>
#include <random>
#include <chrono>


namespace xios
{
  using namespace std ;

  CContextServer::CContextServer(CContext* parent,MPI_Comm intraComm_,MPI_Comm interComm_) 
    : associatedClient_(nullptr)
  {
    context=parent;
    intraComm=intraComm_;
    MPI_Comm_size(intraComm,&intraCommSize);
    MPI_Comm_rank(intraComm,&intraCommRank);

    interComm=interComm_;
    MPI_Comm_remote_size(interComm,&clientSize_);
    
    SRegisterContextInfo contextInfo ;
    CXios::getContextsManager()->getContextInfo(context->getId(), contextInfo, intraComm) ;
    eventScheduler_=CXios::getPoolRessource()->getService(contextInfo.serviceId,contextInfo.partitionId)->getEventScheduler() ;

    // generate unique hash for server
    auto time=chrono::system_clock::now().time_since_epoch().count() ;
    std::default_random_engine rd(time); // not reproducible from a run to another
    std::uniform_int_distribution<size_t> dist;
    hashId=dist(rd) ;
    MPI_Bcast(&hashId,1,MPI_SIZE_T,0,intraComm) ; // Bcast to all server of the context
      
  }

  template<>
  CContextServer* CContextServer::getNew<CContextServer::generic>(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm) 
  { 
    string defaultProtocol = CXios::getin<string>("transport_protocol", "default") ;
    if (defaultProtocol=="one_sided") return new COneSidedContextServer(parent, intraComm, interComm) ;
    else if  (defaultProtocol=="legacy") return new CLegacyContextServer(parent, intraComm, interComm) ;
    else if  (defaultProtocol=="default") return new CLegacyContextServer(parent, intraComm, interComm) ;
    else ERROR("CContextServer* CContextServer::getNew<CContextServer::generic>(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm)",
              <<"Protocol name <"<<defaultProtocol<<"> is undefined,  must be <default>, <one_sided> or <legacy>" ) ;     
  }

  template<>
  CContextServer* CContextServer::getNew<CContextServer::oneSided>(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm) 
  { 
    return new COneSidedContextServer(parent, intraComm, interComm) ; 
  }

  template<>
  CContextServer* CContextServer::getNew<CContextServer::legacy>(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm) 
  { 
    return new COneSidedContextServer(parent, intraComm, interComm) ; 
  }

}
