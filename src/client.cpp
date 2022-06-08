#include "globalScopeData.hpp"
#include "xios_spl.hpp"
#include "cxios.hpp"
#include "client.hpp"
#include <boost/functional/hash.hpp>
#include "type.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "oasis_cinterface.hpp"
#include "mpi.hpp"
#include "timer.hpp"
#include "buffer_client.hpp"
#include "string_tools.hpp"
#include "ressources_manager.hpp"
#include "services_manager.hpp"
#include <functional>
#include <cstdio>
#include "workflow_graph.hpp"
#include "release_static_allocation.hpp"

namespace xios
{

    const double serverPublishDefaultTimeout=10;

    MPI_Comm CClient::intraComm_ ;
    MPI_Comm CClient::interComm_ ;
    MPI_Comm CClient::clientsComm_ ;

    std::list<MPI_Comm> CClient::contextInterComms;
    int CClient::serverLeader ;
    bool CClient::is_MPI_Initialized ;
    int CClient::rank_ = INVALID_RANK;
    StdOFStream CClient::m_infoStream;
    StdOFStream CClient::m_errorStream;
    CPoolRessource* CClient::poolRessource_=nullptr ;

    MPI_Comm& CClient::getInterComm(void)   { return (interComm_); }
     
///---------------------------------------------------------------
/*!
 * \fn void CClient::initialize(const string& codeId, MPI_Comm& localComm, MPI_Comm& returnComm)
 * Function creates intraComm (CClient::intraComm) for client group with id=codeId and interComm (CClient::interComm) between client and server groups.
 * \param [in] codeId identity of context.
 * \param [in/out] localComm local communicator.
 * \param [in/out] returnComm (intra)communicator of client group.
 */

    void CClient::initialize(const string& codeId, MPI_Comm& localComm, MPI_Comm& returnComm)
    {
    
       MPI_Comm clientComm ;
      // initialize MPI if not initialized
      int initialized ;
      MPI_Initialized(&initialized) ;
      if (initialized) is_MPI_Initialized=true ;
      else is_MPI_Initialized=false ;
      
      MPI_Comm globalComm=CXios::getGlobalComm() ;

      /////////////////////////////////////////
      ///////////// PART 1 ////////////////////
      /////////////////////////////////////////
      

      // localComm isn't given
      if (localComm == MPI_COMM_NULL)
      {
          
        // don't use OASIS
        if (!CXios::usingOasis)
        {

          if (!is_MPI_Initialized)
          {
            MPI_Init(NULL, NULL);
          }
          CTimer::get("XIOS").resume() ;
          CTimer::get("XIOS init/finalize",false).resume() ;
          
          // split the global communicator
          // get hash from all model to attribute a unique color (int) and then split to get client communicator
          // every mpi process of globalComm (MPI_COMM_WORLD) must participate

          int commRank, commSize ;
          MPI_Comm_rank(globalComm,&commRank) ;
          MPI_Comm_size(globalComm,&commSize) ;

          std::hash<string> hashString ;
          size_t hashClient=hashString(codeId) ;
          
          size_t* hashAll = new size_t[commSize] ;
          MPI_Allgather(&hashClient,1,MPI_SIZE_T,hashAll,1,MPI_SIZE_T,globalComm) ;
          
          int color=0 ;
          map<size_t,int> listHash ;
          for(int i=0 ; i<=commSize ; i++) 
            if (listHash.count(hashAll[i])==0) 
            {
              listHash[hashAll[i]]=color ;
              color=color+1 ;
            }
            color=listHash[hashClient] ;
          delete[] hashAll ;

          MPI_Comm_split(globalComm, color, commRank, &clientComm) ;
        }
        else // using oasis to split communicator
        {
          if (!is_MPI_Initialized) oasis_init(codeId) ;
          oasis_get_localcomm(clientComm) ;
        }
      }
      else // localComm is given
      {
        MPI_Comm_dup(localComm,&clientComm) ;
      }
      
     
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
         MPI_Comm_split(globalComm, 0, commGlobalRank, &splitComm) ;
         int splitCommSize, globalCommSize ;
        
         MPI_Comm_size(splitComm,&splitCommSize) ;
         MPI_Comm_size(globalComm,&globalCommSize) ;
         if (splitCommSize==globalCommSize) // no server
         {
           MPI_Comm_dup(globalComm,&xiosGlobalComm) ;
           CXios::setXiosComm(xiosGlobalComm) ;
         }
         else
         {
           MPI_Comm_rank(splitComm,&commRank) ;
           if (commRank==0) clientLeader=commGlobalRank ;
           else clientLeader=0 ;
           serverLeader=0 ;
           MPI_Allreduce(&clientLeader,&clientRemoteLeader,1,MPI_INT,MPI_SUM,globalComm) ;
           MPI_Allreduce(&serverLeader,&serverRemoteLeader,1,MPI_INT,MPI_SUM,globalComm) ;
           MPI_Intercomm_create(splitComm, 0, globalComm, serverRemoteLeader,1341,&interComm) ;
           MPI_Intercomm_merge(interComm,true,&xiosGlobalComm) ;
           CXios::setXiosComm(xiosGlobalComm) ;
         }
      }
      else
      {

        xiosGlobalCommByFileExchange(clientComm, codeId) ;
      
      }

      int commRank ;
      MPI_Comm_rank(CXios::getXiosComm(), &commRank) ;
      MPI_Comm_split(CXios::getXiosComm(),false,commRank, &clientsComm_) ;
      
      // is using server or not ?
      int xiosCommSize, clientsCommSize ; 
      MPI_Comm_size(CXios::getXiosComm(), &xiosCommSize) ;
      MPI_Comm_size(clientsComm_, &clientsCommSize) ;
      if (xiosCommSize==clientsCommSize) CXios::setUsingServer() ;
      else CXios::setNotUsingServer() ;

      /////////////////////////////////////////
      ///////////// PART 3 ////////////////////
      /////////////////////////////////////////
     
      CXios::launchDaemonsManager(false) ;
      poolRessource_ = new CPoolRessource(clientComm, codeId) ;

      /////////////////////////////////////////
      ///////////// PART 4 ////////////////////
      /////////////////////////////////////////      
      
      returnComm = clientComm ;
    }


    void CClient::xiosGlobalCommByFileExchange(MPI_Comm clientComm, const string& codeId)
    {
 
      MPI_Comm globalComm=CXios::getGlobalComm() ;
      MPI_Comm xiosGlobalComm ;

      string strIds=CXios::getin<string>("clients_code_id","") ;
      vector<string> clientsCodeId=splitRegex(strIds,"\\s*,\\s*") ;

      int commRank, globalRank, clientRank, serverRank ;
      MPI_Comm_rank(clientComm, &commRank) ;
      MPI_Comm_rank(globalComm, &globalRank) ;
      string clientFileName("__xios_publisher::"+codeId+"__to_remove__") ;
           
      int error ;

      if (commRank==0) // if root process publish name
      {  
        std::ofstream ofs (clientFileName, std::ofstream::out);
        ofs<<globalRank ;
        ofs.close();
        
  // get server root rank

        std::ifstream ifs ;
        string fileName=("__xios_publisher::"+CXios::xiosCodeId+"__to_remove__") ;
      
        double timeout = CXios::getin<double>("server_puplish_timeout",serverPublishDefaultTimeout) ;
        double time ;
          
        do
        {
          CTimer::get("server_publish_timeout").resume() ;  
          ifs.clear() ;
          ifs.open(fileName, std::ifstream::in) ;
          CTimer::get("server_publish_timeout").suspend() ;
        } while (ifs.fail() && CTimer::get("server_publish_timeout").getCumulatedTime()<timeout) ;
        
        if (CTimer::get("server_publish_timeout").getCumulatedTime()>=timeout || ifs.fail())
        {
          ifs.clear() ;
          ifs.close() ;
          ifs.clear() ;
          error=true ;            
        }
        else 
        {
          ifs>>serverRank ;
          ifs.close() ;
          error=false ;
        } 

      } 
      
      MPI_Bcast(&error,1,MPI_INT,0,clientComm) ;
      
      if (error==false)  // you have a server
      {
        MPI_Comm intraComm ;
        MPI_Comm_dup(clientComm,&intraComm) ;
        MPI_Comm interComm ;
        
        int pos=0 ;
        for(int i=0 ; codeId!=clientsCodeId[i]; i++) pos=pos+1 ;

        bool high=true ;
        for(int i=pos ; i<clientsCodeId.size(); i++)
        {  
          MPI_Intercomm_create(intraComm, 0, globalComm, serverRank, 3141, &interComm);
          MPI_Comm_free(&intraComm) ;
          MPI_Intercomm_merge(interComm,high, &intraComm ) ;
          high=false ;
        }
        xiosGlobalComm=intraComm ;
      }
      else  // no server detected
      {
        vector<int> clientsRank(clientsCodeId.size()) ;
        
        if (commRank==0)
        {  
          for(int i=0;i<clientsRank.size();i++)
          {
            std::ifstream ifs ;
            string fileName=("__xios_publisher::"+clientsCodeId[i]+"__to_remove__") ;
            do
            {
              ifs.clear() ;
              ifs.open(fileName, std::ifstream::in) ;
            } while (ifs.fail()) ;
            ifs>>clientsRank[i] ;
            ifs.close() ;
          }
        }
         
        int client ;
        MPI_Comm intraComm ;
        MPI_Comm_dup(clientComm,&intraComm) ;
        MPI_Comm interComm ;
        
        int pos=0 ;
        for(int i=0 ; codeId!=clientsCodeId[i]; i++) pos=pos+1 ;
        
        bool high=true ;
        for(int i=pos+1 ; i<clientsCodeId.size(); i++)
        {  
          if (codeId==clientsCodeId[0])   // first model play the server rule
          {          
            MPI_Intercomm_create(intraComm, 0, globalComm, clientsRank[i], 3141, &interComm);
            MPI_Intercomm_merge(interComm,false, &intraComm ) ;
          }
          else
          {          
            MPI_Intercomm_create(intraComm, 0, globalComm, clientsRank[0], 3141, &interComm);
            MPI_Intercomm_merge(interComm,high, &intraComm ) ;
            high=false ;
          }
        }
        xiosGlobalComm=intraComm ;
      }

      MPI_Barrier(xiosGlobalComm);
      if (commRank==0) std::remove(clientFileName.c_str()) ;         
      MPI_Barrier(xiosGlobalComm);
  
      CXios::setXiosComm(xiosGlobalComm) ;

      MPI_Comm commUnfree ;
      MPI_Comm_dup(clientComm, &commUnfree ) ;
 
    }

// to check on other architecture
    void CClient::xiosGlobalCommByPublishing(MPI_Comm clientComm, const string& codeId)
    {

      // untested. need to be developped an a true MPI compliant library

/*
        // try to discover other client/server
        // do you have a xios server ?
        char portName[MPI_MAX_PORT_NAME];
        int ierr ;
        int commRank ;
        MPI_Comm_rank(clientComm,&commRank) ;

        MPI_Barrier(globalComm) ;
        if (commRank==0)
        {
             
          MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN );
          const char* serviceName=CXios::xiosCodeId.c_str() ;
          ierr=MPI_Lookup_name(CXios::xiosCodeId.c_str(), MPI_INFO_NULL, portName);
          MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_ARE_FATAL );
        }
        ierr=MPI_SUCCESS ;
        MPI_Bcast(&ierr,1,MPI_INT,0,clientComm) ;

        if (ierr==MPI_SUCCESS) // you have a server
        {  
          MPI_Comm intraComm=clientComm ;
          MPI_Comm interComm ;
          for(int i=0 ; i<clientsCodeId.size(); i++)
          {  
            MPI_Comm_connect(portName, MPI_INFO_NULL, 0, intraComm, &interComm);
            MPI_Intercomm_merge(interComm, true, &intraComm ) ;
          }
          xiosGlobalComm=intraComm ;
        }
        else  // you don't have any server
        {
          if (codeId==clientsCodeId[0]) // first code will publish his name
          {

            if (commRank==0) // if root process publish name
            {  
              MPI_Open_port(MPI_INFO_NULL, portName);
              MPI_Publish_name(CXios::xiosCodeId.c_str(), MPI_INFO_NULL, portName);
            }

            MPI_Comm intraComm=clientComm ;
            MPI_Comm interComm ;
            for(int i=0 ; i<clientsCodeId.size()-1; i++)
            {  
              MPI_Comm_accept(portName, MPI_INFO_NULL, 0, intraComm, &interComm);
              MPI_Intercomm_merge(interComm,false, &intraComm ) ;
            }
          }
          else  // other clients are connecting to the first one
          {
            if (commRank==0)
            {

              MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN );
              ierr=MPI_Lookup_name(CXios::xiosCodeId.c_str(), MPI_INFO_NULL, portName);
              MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_ARE_FATAL );
             }

            MPI_Bcast(&ierr,1,MPI_INT,0,clientComm) ;

            if (ierr==MPI_SUCCESS) // you can connect
            {  
              MPI_Comm intraComm=clientComm ;
              MPI_Comm interComm ;
              for(int i=0 ; i<clientsCodeId.size()-1; i++)
              {  
                MPI_Comm_connect(portName, MPI_INFO_NULL, 0, intraComm, &interComm);
                MPI_Intercomm_merge(interComm, true, &intraComm ) ;
              }
              xiosGlobalComm=intraComm ;
            }
          }
        }  
      */
    }


///---------------------------------------------------------------
/*!
 * \fn void CClient::registerContext(const string& id, MPI_Comm contextComm)
 * \brief Sends a request to create a context to server. Creates client/server contexts.
 * \param [in] id id of context.
 * \param [in] contextComm.
 * Function is only called by client.
 */
    void CClient::registerContext(const string& id, MPI_Comm contextComm)
    {
      int commRank, commSize ;
      MPI_Comm_rank(contextComm,&commRank) ;
      MPI_Comm_size(contextComm,&commSize) ;

      getPoolRessource()->createService(contextComm, id, 0, CServicesManager::CLIENT, 1) ;
      getPoolRessource()->createService(contextComm, id+"_"+CXios::defaultServerId, 0, CServicesManager::IO_SERVER, 1) ;

      if (commRank==0) while (!CXios::getServicesManager()->hasService(getPoolRessource()->getId(), id, 0)) { CXios::getDaemonsManager()->eventLoop();}

      if (commRank==0) CXios::getContextsManager()->createServerContext(getPoolRessource()->getId(), id, 0, id) ;
      int type=CServicesManager::CLIENT ;
      string name = CXios::getContextsManager()->getServerContextName(getPoolRessource()->getId(), id, 0, type, id) ;
      double time ;
      double lastTime=0 ;
      double latency=0 ;
      bool out=false ;
      while (!out)
      {
        time=MPI_Wtime() ;
        if (time-lastTime > latency) 
        {
          out=CXios::getContextsManager()->hasContext(name, contextComm);
          lastTime=time ;
        }
        if (!out) CXios::getDaemonsManager()->eventLoop() ;
      }

    }



/*!
 * \fn void CClient::callOasisEnddef(void)
 * \brief Send the order to the servers to call "oasis_enddef". It must be done by each compound of models before calling oasis_enddef on client side
 * Function is only called by client.
 */
    void CClient::callOasisEnddef(void)
    {
      bool oasisEnddef=CXios::getin<bool>("call_oasis_enddef",true) ;
      if (!oasisEnddef) ERROR("void CClient::callOasisEnddef(void)", <<"Function xios_oasis_enddef called but variable <call_oasis_enddef> is set to false."<<endl
                                                                     <<"Variable <call_oasis_enddef> must be set to true"<<endl) ;
      if (CXios::isServer)
      // Attached mode
      {
        // nothing to do   
      }
      else
      {
        int rank ;
        int msg=0 ;

        MPI_Comm_rank(intraComm_,&rank) ;
        if (rank==0) 
        {
          MPI_Send(&msg,1,MPI_INT,0,5,interComm_) ; // tags oasis_endded = 5
        }

      }
    }

    void CClient::finalize(void)
    {
     
      MPI_Barrier(clientsComm_) ;
      int commRank ;
      MPI_Comm_rank(clientsComm_, &commRank) ;
      if (commRank==0) CXios::getRessourcesManager()->finalize() ;
      
      CTimer::get("XIOS init/finalize",false).suspend() ;
      CTimer::get("XIOS").suspend() ;
      CXios::finalizeDaemonsManager() ;
      finalizePoolRessource() ;
      CContext::removeAllContexts() ; // free memory for related context 

      CXios::getMpiGarbageCollector().release() ; // release unfree MPI ressources
      if (!is_MPI_Initialized)
      {
        if (CXios::usingOasis) oasis_finalize();
        else MPI_Finalize() ;
      }
      
      info(20) << "Client side context is finalized"<<endl ;
      report(0) <<" Performance report : Whole time from XIOS init and finalize: "<< CTimer::get("XIOS init/finalize").getCumulatedTime()<<" s"<<endl ;
      report(0) <<" Performance report : total time spent for XIOS : "<< CTimer::get("XIOS").getCumulatedTime()<<" s"<<endl ;
      report(0)<< " Performance report : time spent for waiting free buffer : "<< CTimer::get("Blocking time").getCumulatedTime()<<" s"<<endl ;
      report(0)<< " Performance report : Ratio : "<< CTimer::get("Blocking time").getCumulatedTime()/CTimer::get("XIOS init/finalize").getCumulatedTime()*100.<<" %"<<endl ;
      report(0)<< " Performance report : This ratio must be close to zero. Otherwise it may be usefull to increase buffer size or numbers of server"<<endl ;
//      report(0)<< " Memory report : Current buffer_size : "<<CXios::bufferSize<<endl ;
      report(0)<< " Memory report : Minimum buffer size required : " << CClientBuffer::maxRequestSize << " bytes" << endl ;
      report(0)<< " Memory report : increasing it by a factor will increase performance, depending of the volume of data wrote in file at each time step of the file"<<endl ;
      report(100)<<CTimer::getAllCumulatedTime()<<endl ;
      CWorkflowGraph::drawWorkFlowGraph_client();

      xios::releaseStaticAllocation() ;

    }
    
    void CClient::finalizePoolRessource() 
    { 
      delete poolRessource_ ; poolRessource_=nullptr ;
    }

    /*!
    * Return global rank without oasis and current rank in model intraComm in case of oasis
    */
   int CClient::getRank()
   {
     return rank_;
   }

    /*!
    * Open a file specified by a suffix and an extension and use it for the given file buffer.
    * The file name will be suffix+rank+extension.
    * 
    * \param fileName[in] protype file name
    * \param ext [in] extension of the file
    * \param fb [in/out] the file buffer
    */
    void CClient::openStream(const StdString& fileName, const StdString& ext, std::filebuf* fb)
    {
      StdStringStream fileNameClient;
      int numDigit = 0;
      int size = 0;
      int rank;
      MPI_Comm_size(CXios::getGlobalComm(), &size);
      MPI_Comm_rank(CXios::getGlobalComm(),&rank);
      while (size)
      {
        size /= 10;
        ++numDigit;
      }

      fileNameClient << fileName << "_" << std::setfill('0') << std::setw(numDigit) << rank << ext;

      fb->open(fileNameClient.str().c_str(), std::ios::out);
      if (!fb->is_open())
        ERROR("void CClient::openStream(const StdString& fileName, const StdString& ext, std::filebuf* fb)",
              << std::endl << "Can not open <" << fileNameClient.str() << "> file to write the client log(s).");
    }

    /*!
    * \brief Open a file stream to write the info logs
    * Open a file stream with a specific file name suffix+rank
    * to write the info logs.
    * \param fileName [in] protype file name
    */
    void CClient::openInfoStream(const StdString& fileName)
    {
      std::filebuf* fb = m_infoStream.rdbuf();
      openStream(fileName, ".out", fb);

      info.write2File(fb);
      report.write2File(fb);
    }

    //! Write the info logs to standard output
    void CClient::openInfoStream()
    {
      info.write2StdOut();
      report.write2StdOut();
    }

    //! Close the info logs file if it opens
    void CClient::closeInfoStream()
    {
      if (m_infoStream.is_open()) m_infoStream.close();
    }

    /*!
    * \brief Open a file stream to write the error log
    * Open a file stream with a specific file name suffix+rank
    * to write the error log.
    * \param fileName [in] protype file name
    */
    void CClient::openErrorStream(const StdString& fileName)
    {
      std::filebuf* fb = m_errorStream.rdbuf();
      openStream(fileName, ".err", fb);

      error.write2File(fb);
    }

    //! Write the error log to standard error output
    void CClient::openErrorStream()
    {
      error.write2StdErr();
    }

    //! Close the error log file if it opens
    void CClient::closeErrorStream()
    {
      if (m_errorStream.is_open()) m_errorStream.close();
    }
}
