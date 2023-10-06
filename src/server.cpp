#include "globalScopeData.hpp"
#include "xios_spl.hpp"
#include "cxios.hpp"
#include "server.hpp"
#include "client.hpp"
#include "type.hpp"
#include "context.hpp"
#include "object_template.hpp"
#include "oasis_cinterface.hpp"
#include <boost/functional/hash.hpp>
#include <boost/algorithm/string.hpp>
#include "mpi.hpp"
#include "tracer.hpp"
#include "timer.hpp"
#include "mem_checker.hpp"
#include "event_scheduler.hpp"
#include "string_tools.hpp"
#include "ressources_manager.hpp"
#include "services_manager.hpp"
#include "contexts_manager.hpp"
#include "servers_ressource.hpp"
#include "services.hpp"
#include "pool_node.hpp"
#include <cstdio>
#include "workflow_graph.hpp"
#include "release_static_allocation.hpp"
#include "thread_manager.hpp"
#include <sys/stat.h>
#include <unistd.h>



namespace xios
{
    MPI_Comm CServer::intraComm_ ;
    MPI_Comm CServer::serversComm_ ;
    std::list<MPI_Comm> CServer::interCommLeft ;
    std::list<MPI_Comm> CServer::interCommRight ;
    std::list<MPI_Comm> CServer::contextInterComms;
    std::list<MPI_Comm> CServer::contextIntraComms;
    int CServer::serverLevel = 0 ;
    int CServer::nbContexts = 0;
    bool CServer::isRoot = false ;
    int CServer::rank_ = INVALID_RANK;
    StdOFStream CServer::m_infoStream;
    StdOFStream CServer::m_errorStream;
    map<string,CContext*> CServer::contextList ;
    vector<int> CServer::sndServerGlobalRanks;
    bool CServer::finished=false ;
    bool CServer::is_MPI_Initialized ;
    CEventScheduler* CServer::eventScheduler = 0;
    CServersRessource* CServer::serversRessource_=nullptr ;
    CThirdPartyDriver* CServer::driver_ =nullptr ;

       
    void CServer::initialize(void)
    {
      
      MPI_Comm serverComm ;
      int initialized ;
      MPI_Initialized(&initialized) ;
      if (initialized) is_MPI_Initialized=true ;
      else is_MPI_Initialized=false ;
      MPI_Comm globalComm=CXios::getGlobalComm() ;
      /////////////////////////////////////////
      ///////////// PART 1 ////////////////////
      /////////////////////////////////////////
      // don't use OASIS
      if (!CXios::usingOasis)
      {
        if (!is_MPI_Initialized) 
        {
          int required = MPI_THREAD_SERIALIZED ;
          int provided ;
          MPI_Init_thread(NULL,NULL, required, &provided) ;
        }
       
        // split the global communicator
        // get hash from all model to attribute a unique color (int) and then split to get client communicator
        // every mpi process of globalComm (MPI_COMM_WORLD) must participate
         
        int commRank, commSize ;
        MPI_Comm_rank(globalComm,&commRank) ;
        MPI_Comm_size(globalComm,&commSize) ;

        std::hash<string> hashString ;
        size_t hashServer=hashString(CXios::xiosCodeId) ;
          
        size_t* hashAll = new size_t[commSize] ;
        MPI_Allgather(&hashServer,1,MPI_SIZE_T,hashAll,1,MPI_SIZE_T,globalComm) ;
          
        int color=0 ;
        map<size_t,int> listHash ;
        for(int i=0 ; i<=commSize ; i++) 
          if (listHash.count(hashAll[i])==0) 
          {
            listHash[hashAll[i]]=color ;
            color=color+1 ;
          }
        color=listHash[hashServer] ;
        delete[] hashAll ;

        xios::MPI_Comm_split(globalComm, color, commRank, &serverComm) ;
        CXios::getMpiGarbageCollector().registerCommunicator(serverComm) ;

      }
      else // using OASIS
      {
        
        if (!is_MPI_Initialized) 
        {
          int required = MPI_THREAD_SERIALIZED ;
          int provided ;
          MPI_Init_thread(NULL,NULL, required, &provided) ;
        }

        driver_ = new CThirdPartyDriver();

        driver_->getComponentCommunicator( serverComm );
      }
      xios::MPI_Comm_dup(serverComm, &intraComm_);
      CXios::getMpiGarbageCollector().registerCommunicator(intraComm_) ;
      
      CTimer::get("XIOS").resume() ;
      CTimer::get("XIOS server").resume() ;
      CTimer::get("XIOS initialize").resume() ;
 
      /////////////////////////////////////////
      ///////////// PART 2 ////////////////////
      /////////////////////////////////////////
      

      // Create the XIOS communicator for every process which is related
      // to XIOS, as well on client side as on server side
      MPI_Comm xiosGlobalComm ;
      string strIds=CXios::getin<string>("clients_code_id","") ;
      vector<string> clientsCodeId=splitRegex(strIds,"\\s*,\\s*") ;
      if (strIds.empty())
      {
        // no code Ids given, suppose XIOS initialisation is global            
        int commRank, commGlobalRank, serverLeader, clientLeader,serverRemoteLeader,clientRemoteLeader ;
        MPI_Comm splitComm,interComm ;
        MPI_Comm_rank(globalComm,&commGlobalRank) ;
        xios::MPI_Comm_split(globalComm, 1, commGlobalRank, &splitComm) ;
        MPI_Comm_rank(splitComm,&commRank) ;
        if (commRank==0) serverLeader=commGlobalRank ;
        else serverLeader=0 ;
        clientLeader=0 ;
        MPI_Allreduce(&clientLeader,&clientRemoteLeader,1,MPI_INT,MPI_SUM,globalComm) ;
        MPI_Allreduce(&serverLeader,&serverRemoteLeader,1,MPI_INT,MPI_SUM,globalComm) ;
        xios::MPI_Intercomm_create(splitComm, 0, globalComm, clientRemoteLeader,1341,&interComm) ;
        xios::MPI_Intercomm_merge(interComm,false,&xiosGlobalComm) ;
        CXios::setXiosComm(xiosGlobalComm) ;
      }
      else
      {

        xiosGlobalCommByFileExchange(serverComm) ;

      }
      
      /////////////////////////////////////////
      ///////////// PART 4 ////////////////////
      //  create servers intra communicator  //
      ///////////////////////////////////////// 
      
      int commRank ;
      MPI_Comm_rank(CXios::getXiosComm(), &commRank) ;
      xios::MPI_Comm_split(CXios::getXiosComm(),true,commRank,&serversComm_) ;
      CXios::getMpiGarbageCollector().registerCommunicator(serversComm_) ;
      
      CXios::setUsingServer() ;

      /////////////////////////////////////////
      ///////////// PART 5 ////////////////////
      //       redirect files output         //
      ///////////////////////////////////////// 
      
      CServer::openInfoStream(CXios::serverFile);
      CServer::openErrorStream(CXios::serverFile);

      CMemChecker::logMem( "CServer::initialize" );

      /////////////////////////////////////////
      ///////////// PART 4 ////////////////////
      /////////////////////////////////////////

      CXios::launchDaemonsManager(true) ;
     
      /////////////////////////////////////////
      ///////////// PART 5 ////////////////////
      /////////////////////////////////////////

      // create the services

      auto ressourcesManager=CXios::getRessourcesManager() ;
      auto servicesManager=CXios::getServicesManager() ;
      auto contextsManager=CXios::getContextsManager() ;
      auto daemonsManager=CXios::getDaemonsManager() ;
      auto serversRessource=CServer::getServersRessource() ;

      int rank;
      MPI_Comm_rank(intraComm_, &rank) ;
      if (rank==0) isRoot=true;
      else isRoot=false;

      if (serversRessource->isServerLeader())
      {
        // creating pool
        CPoolNodeGroup::get("xios","pool_definition")->solveDescInheritance(true) ;
        vector<CPoolNode*> pools = CPoolNodeGroup::get("xios","pool_definition")->getAllChildren();
        for(auto& pool : pools) pool->allocateRessources() ;
        
        int nbRessources = ressourcesManager->getFreeRessourcesSize() ;
        if (nbRessources>0)
        {
          if (!CXios::usingServer2)
          {
            ressourcesManager->createPool(CXios::defaultPoolId, nbRessources) ;
            if (CThreadManager::isUsingThreads()) 
              while(!ressourcesManager->hasPool(CXios::defaultPoolId)) 
              {
                daemonsManager->eventLoop() ;
                CThreadManager::yield() ;
              }
            else ressourcesManager->waitPoolRegistration(CXios::defaultPoolId) ;
          
            servicesManager->createServices(CXios::defaultPoolId, CXios::defaultWriterId, CServicesManager::WRITER,nbRessources,1) ;
            if (CThreadManager::isUsingThreads()) 
              while(!servicesManager->hasService(CXios::defaultPoolId, CXios::defaultWriterId,0)) 
              {
                daemonsManager->eventLoop() ;
                CThreadManager::yield() ;
              }
            else servicesManager->waitServiceRegistration(CXios::defaultPoolId, CXios::defaultWriterId) ;
            
            servicesManager->createServicesOnto(CXios::defaultPoolId, CXios::defaultReaderId, CServicesManager::READER, CXios::defaultWriterId) ;
            if (CThreadManager::isUsingThreads()) 
            {
              daemonsManager->eventLoop() ;
              while(!servicesManager->hasService(CXios::defaultPoolId, CXios::defaultReaderId, 0)) CThreadManager::yield() ;
            }
            else servicesManager->waitServiceRegistration(CXios::defaultPoolId, CXios::defaultReaderId) ;
          }
          else
          {
            int nprocsServer = nbRessources*CXios::ratioServer2/100.;
            int nprocsGatherer = nbRessources - nprocsServer ;
          
            int nbPoolsServer2 = CXios::nbPoolsServer2 ;
            if (nbPoolsServer2 == 0) nbPoolsServer2 = nprocsServer;
            ressourcesManager->createPool(CXios::defaultPoolId, nbRessources) ;
            if (CThreadManager::isUsingThreads()) 
              while(!ressourcesManager->hasPool(CXios::defaultPoolId)) 
              {
                daemonsManager->eventLoop() ;
                CThreadManager::yield() ;
              }
            else ressourcesManager->waitPoolRegistration(CXios::defaultPoolId) ;

            servicesManager->createServices(CXios::defaultPoolId,  CXios::defaultGathererId, CServicesManager::GATHERER, nprocsGatherer, 1) ;
            if (CThreadManager::isUsingThreads()) 
              while(!servicesManager->hasService(CXios::defaultPoolId, CXios::defaultGathererId,0)) 
              {
                daemonsManager->eventLoop() ;
                CThreadManager::yield() ;
              }
            else servicesManager->waitServiceRegistration(CXios::defaultPoolId, CXios::defaultGathererId) ;

            servicesManager->createServicesOnto(CXios::defaultPoolId, CXios::defaultReaderId, CServicesManager::READER, CXios::defaultGathererId) ;
            if (CThreadManager::isUsingThreads()) 
              while(!servicesManager->hasService(CXios::defaultPoolId, CXios::defaultReaderId, 0)) 
              {
                daemonsManager->eventLoop() ;
                CThreadManager::yield() ;
              }
            else servicesManager->waitServiceRegistration(CXios::defaultPoolId, CXios::defaultReaderId) ;
            
            servicesManager->createServices(CXios::defaultPoolId,  CXios::defaultWriterId, CServicesManager::WRITER, nprocsServer, nbPoolsServer2) ;
            if (CThreadManager::isUsingThreads())
              for(int i=0; i<nbPoolsServer2; i++)
                while(!servicesManager->hasService(CXios::defaultPoolId, CXios::defaultWriterId,i)) 
                {
                  daemonsManager->eventLoop() ;
                  CThreadManager::yield() ;
                }
            else servicesManager->waitServiceRegistration(CXios::defaultPoolId, CXios::defaultWriterId) ;
          }
        }
//        servicesManager->createServices(CXios::defaultPoolId,  CXios::defaultServicesId, CServicesManager::ALL_SERVICES, nbRessources, 1) ;
      }

      MPI_Request req ;
      MPI_Status status ;
      MPI_Ibarrier(getServersRessource()->getCommunicator(),&req) ; // be sure that all services are created now, could be remove later if more asynchronisity
      int ok=false ;
      while (!ok)
      {
        daemonsManager->eventLoop() ;
        if (CThreadManager::isUsingThreads()) CThreadManager::yield();
        MPI_Test(&req,&ok,&status) ;
      }


      //testingEventScheduler() ;
/*
      MPI_Request req ;
      MPI_Status status ;
      MPI_Ibarrier(CXios::getXiosComm(),&req) ; // be sure that all services are created now, could be remove later if more asynchronisity
      int ok=false ;
      while (!ok)
      {
        daemonsManager->eventLoop() ;
        MPI_Test(&req,&ok,&status) ;
      }
*/
      CTimer::get("XIOS initialize").suspend() ;

      /////////////////////////////////////////
      ///////////// PART 5 ////////////////////
      /////////////////////////////////////////
      // loop on event loop

      bool finished=false ;
      CTimer::get("XIOS event loop").resume() ;

      while (!finished)
      {
        finished=daemonsManager->eventLoop() ;
        if (CThreadManager::isUsingThreads()) CThreadManager::yield() ;
      }
      CTimer::get("XIOS event loop").suspend() ;

      // Delete CContext
      //CObjectTemplate<CContext>::cleanStaticDataStructure();
    }


    void CServer::testingEventScheduler(void)
    {
      CXios::getPoolRessource()->getEventScheduler()->registerEvent(1,10) ;
      CXios::getPoolRessource()->getEventScheduler()->registerEvent(2,10) ;
      if (CXios::getPoolRessource()->hasService(CXios::defaultGathererId,0))
      {
        CXios::getPoolRessource()->getService(CXios::defaultGathererId,0)-> getEventScheduler()->registerEvent(1,100) ;
        CXios::getPoolRessource()->getService(CXios::defaultGathererId,0)-> getEventScheduler()->registerEvent(2,100) ;
        CXios::getPoolRessource()->getService(CXios::defaultGathererId,0)-> getEventScheduler()->registerEvent(3,100) ;
      }
      if (CXios::getPoolRessource()->hasService(CXios::defaultWriterId,0))
      {
        CXios::getPoolRessource()->getService(CXios::defaultWriterId,0)-> getEventScheduler()->registerEvent(1,1000) ;
        CXios::getPoolRessource()->getService(CXios::defaultWriterId,0)-> getEventScheduler()->registerEvent(2,1000) ;
      }
      CXios::getPoolRessource()->getEventScheduler()->registerEvent(3,10) ;
      CXios::getPoolRessource()->getEventScheduler()->registerEvent(4,10) ;
      
      if (CXios::getPoolRessource()->hasService(CXios::defaultGathererId,0))
      {
        CXios::getPoolRessource()->getService(CXios::defaultGathererId,0)-> getEventScheduler()->registerEvent(4,100) ;
        CXios::getPoolRessource()->getService(CXios::defaultGathererId,0)-> getEventScheduler()->registerEvent(5,100) ;
      }
      if (CXios::getPoolRessource()->hasService(CXios::defaultWriterId,0))
      {
        CXios::getPoolRessource()->getService(CXios::defaultWriterId,0)-> getEventScheduler()->registerEvent(3,1000) ;
        CXios::getPoolRessource()->getService(CXios::defaultWriterId,0)-> getEventScheduler()->registerEvent(4,1000) ;
        CXios::getPoolRessource()->getService(CXios::defaultWriterId,0)-> getEventScheduler()->registerEvent(5,1000) ;
      }
      CXios::getPoolRessource()->getEventScheduler()->registerEvent(5,10) ;
      CXios::getPoolRessource()->getEventScheduler()->registerEvent(6,10) ;
      
      int numEvents=0 ;
      int poolEvent=1 ;
      int gatherEvent=1 ;
      int writerEvent=1 ;
      do
      {
        if (CXios::getPoolRessource()->getEventScheduler()->queryEvent(poolEvent,10))
        {
          CXios::getPoolRessource()->getEventScheduler()->popEvent() ;
          MPI_Barrier(CXios::getPoolRessource()->getCommunicator());
          poolEvent++ ;
          numEvents++;
        }
        
        if (CXios::getPoolRessource()->getEventScheduler()->queryEvent(gatherEvent,100))
        {
          CXios::getPoolRessource()->getEventScheduler()->popEvent() ;
          MPI_Barrier(CXios::getPoolRessource()->getService(CXios::defaultGathererId,0)->getCommunicator());
          gatherEvent++ ;
          numEvents++;
        }

        if (CXios::getPoolRessource()->getEventScheduler()->queryEvent(writerEvent,1000))
        {
          CXios::getPoolRessource()->getEventScheduler()->popEvent() ;
          MPI_Barrier(CXios::getPoolRessource()->getService(CXios::defaultWriterId,0)->getCommunicator());
          writerEvent++ ;
          numEvents++;
        }

        
      } while (numEvents!=11) ;

    }


    void  CServer::xiosGlobalCommByFileExchange(MPI_Comm serverComm)
    {
        
      MPI_Comm globalComm=CXios::getGlobalComm() ;
      MPI_Comm xiosGlobalComm ;
      
      string strIds=CXios::getin<string>("clients_code_id","") ;
      vector<string> clientsCodeId=splitRegex(strIds,"\\s*,\\s*") ;
      
      int commRank, globalRank ;
      MPI_Comm_rank(serverComm, &commRank) ;
      MPI_Comm_rank(globalComm, &globalRank) ;
      string serverFileName("__xios_publisher::"+CXios::xiosCodeId+"__to_remove__") ;

      if (commRank==0) // if root process publish name
      {  
        std::ofstream ofs (serverFileName, std::ofstream::out);
        ofs<<globalRank ;
        ofs.close();
      }
        
      vector<int> clientsRank(clientsCodeId.size()) ;
      for(int i=0;i<clientsRank.size();i++)
      {
        std::ifstream ifs ;
        string fileName=("__xios_publisher::"+clientsCodeId[i]+"__to_remove__") ;
        struct stat buffer;
        do {
        } while( stat(fileName.c_str(), &buffer) != 0 );
        sleep(1);
        ifs.open(fileName, ifstream::in) ;
        ifs>>clientsRank[i] ;
        //cout <<  "\t\t read: " << clientsRank[i] << " in " << fileName << endl;
        ifs.close() ; 
      }

      MPI_Comm intraComm ;
      xios::MPI_Comm_dup(serverComm,&intraComm) ;
      MPI_Comm interComm ;
      for(int i=0 ; i<clientsRank.size(); i++)
      {  
        xios::MPI_Intercomm_create(intraComm, 0, globalComm, clientsRank[i], 3141, &interComm);
        CXios::getMpiGarbageCollector().registerCommunicator(interComm) ;
        interCommLeft.push_back(interComm) ;
        xios::MPI_Comm_free(&intraComm) ;
        xios::MPI_Intercomm_merge(interComm,false, &intraComm ) ;
      }
      xiosGlobalComm=intraComm ;  
      MPI_Barrier(xiosGlobalComm);
      if (commRank==0) std::remove(serverFileName.c_str()) ;
      MPI_Barrier(xiosGlobalComm);

      CXios::setXiosComm(xiosGlobalComm) ;
      
    }


    void  CServer::xiosGlobalCommByPublishing(MPI_Comm serverComm)
    {
        // untested, need to be tested on a true MPI-2 compliant library

        // try to discover other client/server
/*
        // publish server name
        char portName[MPI_MAX_PORT_NAME];
        int ierr ;
        int commRank ;
        MPI_Comm_rank(serverComm, &commRank) ;
        
        if (commRank==0) // if root process publish name
        {  
          MPI_Open_port(MPI_INFO_NULL, portName);
          MPI_Publish_name(CXios::xiosCodeId.c_str(), MPI_INFO_NULL, portName);
        }

        MPI_Comm intraComm=serverComm ;
        MPI_Comm interComm ;
        for(int i=0 ; i<clientsCodeId.size(); i++)
        {  
          MPI_Comm_accept(portName, MPI_INFO_NULL, 0, intraComm, &interComm);
          xios::MPI_Intercomm_merge(interComm,false, &intraComm ) ;
        }
*/      
    }

   /*!
    * Root process is listening for an order sent by client to call "oasis_enddef".
    * The root client of a compound send the order (tag 5). It is probed and received.
    * When the order has been received from each coumpound, the server root process ping the order to the root processes of the secondary levels of servers (if any).
    * After, it also inform (asynchronous call) other processes of the communicator that the oasis_enddef call must be done
    */
    
     void CServer::listenOasisEnddef(void)
     {
        int flag ;
        MPI_Status status ;
        list<MPI_Comm>::iterator it;
        int msg ;
        static int nbCompound=0 ;
        int size ;
        static bool sent=false ;
        static MPI_Request* allRequests ;
        static MPI_Status* allStatus ;


        if (sent)
        {
          MPI_Comm_size(intraComm_,&size) ;
          MPI_Testall(size,allRequests, &flag, allStatus) ;
          if (flag==true)
          {
            delete [] allRequests ;
            delete [] allStatus ;
            sent=false ;
          }
        }
        

        for(it=interCommLeft.begin();it!=interCommLeft.end();it++)
        {
           MPI_Status status ;
           traceOff() ;
           MPI_Iprobe(0,5,*it,&flag,&status) ;  // tags oasis_endded = 5
           traceOn() ;
           if (flag==true)
           {
              MPI_Recv(&msg,1,MPI_INT,0,5,*it,&status) ; // tags oasis_endded = 5
              nbCompound++ ;
              if (nbCompound==interCommLeft.size())
              {
                MPI_Comm_size(intraComm_,&size) ;
                allRequests= new MPI_Request[size] ;
                allStatus= new MPI_Status[size] ;
                for(int i=0;i<size;i++) MPI_Isend(&msg,1,MPI_INT,i,5,intraComm_,&allRequests[i]) ; // tags oasis_endded = 5
                sent=true ;
              }
           }
        }
}
     
   /*!
    * Processes probes message from root process if oasis_enddef call must be done.
    * When the order is received it is scheduled to be treated in a synchronized way by all server processes of the communicator
    */
     void CServer::listenRootOasisEnddef(void)
     {
       int flag ;
       MPI_Status status ;
       const int root=0 ;
       int msg ;
       static bool eventSent=false ;

       if (eventSent)
       {
         boost::hash<string> hashString;
         size_t hashId = hashString("oasis_enddef");

         if (CXios::getPoolRessource()->getEventScheduler()->queryEvent(0,hashId))
         {
           CXios::getPoolRessource()->getEventScheduler()->popEvent() ;
           driver_->endSynchronizedDefinition() ;
           eventSent=false ;
         }
       }
         
       traceOff() ;
       MPI_Iprobe(root,5,intraComm_, &flag, &status) ;
       traceOn() ;
       if (flag==true)
       {
           MPI_Recv(&msg,1,MPI_INT,root,5,intraComm_,&status) ; // tags oasis_endded = 5
           boost::hash<string> hashString;
           size_t hashId = hashString("oasis_enddef");
           CXios::getPoolRessource()->getEventScheduler()->registerEvent(0,hashId);
           eventSent=true ;
       }
     }

    void CServer::finalize(void)
    {
      CTimer::get("XIOS").suspend() ;
      CTimer::get("XIOS server").suspend() ;
      delete eventScheduler ;

      for (std::list<MPI_Comm>::iterator it = contextInterComms.begin(); it != contextInterComms.end(); it++)
        xios::MPI_Comm_free(&(*it));

      for (std::list<MPI_Comm>::iterator it = contextIntraComms.begin(); it != contextIntraComms.end(); it++)
        xios::MPI_Comm_free(&(*it));

        for (std::list<MPI_Comm>::iterator it = interCommRight.begin(); it != interCommRight.end(); it++)
          xios::MPI_Comm_free(&(*it));

//      xios::MPI_Comm_free(&intraComm);
      CXios::finalizeDaemonsManager();
      finalizeServersRessource();
      
      CContext::removeAllContexts() ; // free memory for related context
      
      CXios::getMpiGarbageCollector().release() ; // release unfree MPI ressources
      MPI_Comm xiosComm=CXios::getXiosComm() ;
      xios::MPI_Comm_free(&xiosComm) ;
      CMemChecker::logMem( "CServer::finalize", true );
      
      CCommTrack::dumpComm() ;

      if (!is_MPI_Initialized)
      {
        if (CXios::usingOasis) delete driver_;
        else MPI_Finalize() ;
      }
      report(0)<<"Performance report : Time spent for XIOS : "<<CTimer::get("XIOS server").getCumulatedTime()<<endl  ;
      report(0)<<"Performance report : Time spent in processing events : "<<CTimer::get("Process events").getCumulatedTime()<<endl  ;
      report(0)<<"Performance report : Ratio : "<<CTimer::get("Process events").getCumulatedTime()/CTimer::get("XIOS server").getCumulatedTime()*100.<<"%"<<endl  ;
      report(100)<<CTimer::getAllCumulatedTime()<<endl ;
      if (CXios::reportMemory)
      {
        report(100)<<CMemChecker::getAllCumulatedMem()<<endl ;
      }
      
      CWorkflowGraph::drawWorkFlowGraph_server();
      xios::releaseStaticAllocation() ; // free memory from static allocation
    }

    /*!
    * Open a file specified by a suffix and an extension and use it for the given file buffer.
    * The file name will be suffix+rank+extension.
    * 
    * \param fileName[in] protype file name
    * \param ext [in] extension of the file
    * \param fb [in/out] the file buffer
    */
    void CServer::openStream(const StdString& fileName, const StdString& ext, std::filebuf* fb)
    {
      StdStringStream fileNameServer;
      int numDigit = 0;
      int commSize = 0;
      int commRank ;
      int id;
      
      MPI_Comm_size(CXios::getGlobalComm(), &commSize);
      MPI_Comm_rank(CXios::getGlobalComm(), &commRank);

      while (commSize)
      {
        commSize /= 10;
        ++numDigit;
      }
      id = commRank;

      fileNameServer << fileName << "_" << std::setfill('0') << std::setw(numDigit) << id << ext;
      fb->open(fileNameServer.str().c_str(), std::ios::out);
      if (!fb->is_open())
        ERROR("void CServer::openStream(const StdString& fileName, const StdString& ext, std::filebuf* fb)",
              << std::endl << "Can not open <" << fileNameServer.str() << "> file to write the server log(s).");
    }

    /*!
    * \brief Open a file stream to write the info logs
    * Open a file stream with a specific file name suffix+rank
    * to write the info logs.
    * \param fileName [in] protype file name
    */
    void CServer::openInfoStream(const StdString& fileName)
    {
      std::filebuf* fb = m_infoStream.rdbuf();
      openStream(fileName, ".out", fb);

      info.write2File(fb);
      report.write2File(fb);
    }

    //! Write the info logs to standard output
    void CServer::openInfoStream()
    {
      info.write2StdOut();
      report.write2StdOut();
    }

    //! Close the info logs file if it opens
    void CServer::closeInfoStream()
    {
      if (m_infoStream.is_open()) m_infoStream.close();
    }

    /*!
    * \brief Open a file stream to write the error log
    * Open a file stream with a specific file name suffix+rank
    * to write the error log.
    * \param fileName [in] protype file name
    */
    void CServer::openErrorStream(const StdString& fileName)
    {
      std::filebuf* fb = m_errorStream.rdbuf();
      openStream(fileName, ".err", fb);

      error.write2File(fb);
    }

    //! Write the error log to standard error output
    void CServer::openErrorStream()
    {
      error.write2StdErr();
    }

    //! Close the error log file if it opens
    void CServer::closeErrorStream()
    {
      if (m_errorStream.is_open()) m_errorStream.close();
    }

    void CServer::launchServersRessource(MPI_Comm serverComm)
    {
      serversRessource_ = new CServersRessource(serverComm) ;
    }

    void  CServer::finalizeServersRessource(void) 
    { 
      delete serversRessource_; serversRessource_=nullptr ;
    }
}
