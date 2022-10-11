#include "context.hpp"
#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "calendar_type.hpp"
#include "duration.hpp"

#include "legacy_context_client.hpp"
#include "legacy_context_server.hpp"
#include "one_sided_context_client.hpp"
#include "one_sided_context_server.hpp"
#include "nc4_data_output.hpp"
#include "node_type.hpp"
#include "message.hpp"
#include "type.hpp"
#include "xios_spl.hpp"
#include "timer.hpp"
#include "mem_checker.hpp"
#include "memtrack.hpp"
#include <limits>
#include <fstream>
#include "server.hpp"
#include "distribute_file_server2.hpp"
#include "services_manager.hpp"
#include "contexts_manager.hpp"
#include "cxios.hpp"
#include "client.hpp"
#include "coupler_in.hpp"
#include "coupler_out.hpp"
#include "servers_ressource.hpp"
#include "pool_ressource.hpp"
#include "services.hpp"
#include "contexts_manager.hpp"
#include <chrono>
#include <random>

namespace xios
{

  std::shared_ptr<CContextGroup> CContext::root;

   /// ////////////////////// Définitions ////////////////////// ///

   CContext::CContext(void)
      : CObjectTemplate<CContext>(), CContextAttributes()
      , calendar(), hasClient(false), hasServer(false)
      , isPostProcessed(false), finalized(false)
      , allProcessed(false), countChildContextFinalized_(0), isProcessingEvent_(false)

   { /* Ne rien faire de plus */  }

   CContext::CContext(const StdString & id)
      : CObjectTemplate<CContext>(id), CContextAttributes()
      , calendar(), hasClient(false), hasServer(false)
      , isPostProcessed(false), finalized(false)
      , allProcessed(false), countChildContextFinalized_(0), isProcessingEvent_(false)
   { /* Ne rien faire de plus */ }

   CContext::~CContext(void)
   {
     for(auto& client : writerClientOut_) delete client ;
     for(auto& server : writerServerOut_) delete server ;

     for(auto& client : writerClientIn_) delete client ;
     for(auto& server : writerServerIn_) delete server ;

     for(auto& client : readerClientOut_) delete client ;
     for(auto& server : readerServerOut_) delete server ;

     for(auto& client : readerClientIn_) delete client ;
     for(auto& server : readerServerIn_) delete server ;


     if (registryIn!=nullptr) delete registryIn ;
     if (registryOut!=nullptr) delete registryOut ;
   }

   //----------------------------------------------------------------
   //! Get name of context
   StdString CContext::GetName(void)   { return (StdString("context")); }
   StdString CContext::GetDefName(void){ return (CContext::GetName()); }
   ENodeType CContext::GetType(void)   { return (eContext); }

   //----------------------------------------------------------------

  void CContext::initEventScheduler(void)
  {
    SRegisterContextInfo contextInfo ;
    CXios::getContextsManager()->getContextInfo(this->getId(), contextInfo, getIntraComm()) ;

    eventScheduler_=CXios::getPoolRessource()->getService(contextInfo.serviceId,contextInfo.partitionId)->getEventScheduler() ;
 
    // generate unique hash for server
    auto time=chrono::system_clock::now().time_since_epoch().count() ;
    std::default_random_engine rd(time); // not reproducible from a run to another
    std::uniform_int_distribution<size_t> dist;
    hashId_=dist(rd) ;
    MPI_Bcast(&hashId_,1,MPI_SIZE_T,0,getIntraComm()) ; // Bcast to all server of the context
  }
   /*!
   \brief Get context group (context root)
   \return Context root
   */
   CContextGroup* CContext::getRoot(void)
   TRY
   {
      if (root.get()==NULL) root=std::shared_ptr<CContextGroup>(new CContextGroup(xml::CXMLNode::GetRootName()));
      return root.get();
   }
   CATCH

   void CContext::releaseStaticAllocation(void)
   TRY
   {
      CDomain::releaseStaticAllocation();
      CAxis::releaseStaticAllocation();
      CScalar::releaseStaticAllocation();
      if (root) root.reset() ;
   }
   CATCH
   
   //----------------------------------------------------------------

   /*!
   \brief Get calendar of a context
   \return Calendar
   */
   std::shared_ptr<CCalendar> CContext::getCalendar(void) const
   TRY
   {
      return (this->calendar);
   }
   CATCH

   //----------------------------------------------------------------

   /*!
   \brief Set a context with a calendar
   \param[in] newCalendar new calendar
   */
   void CContext::setCalendar(std::shared_ptr<CCalendar> newCalendar)
   TRY
   {
      this->calendar = newCalendar;
   }
   CATCH_DUMP_ATTR

   //----------------------------------------------------------------
   /*!
   \brief Parse xml file and write information into context object
   \param [in] node xmld node corresponding in xml file
   */
   void CContext::parse(xml::CXMLNode & node)
   TRY
   {
      CContext::SuperClass::parse(node);

      // PARSING POUR GESTION DES ENFANTS
      xml::THashAttributes attributes = node.getAttributes();

      if (attributes.end() != attributes.find("src"))
      {
         StdIFStream ifs ( attributes["src"].c_str() , StdIFStream::in );
         if ( (ifs.rdstate() & std::ifstream::failbit ) != 0 )
            ERROR("void CContext::parse(xml::CXMLNode & node)",
                  <<endl<< "Can not open <"<<attributes["src"].c_str()<<"> file" );
         if (!ifs.good())
            ERROR("CContext::parse(xml::CXMLNode & node)",
                  << "[ filename = " << attributes["src"] << " ] Bad xml stream !");
         xml::CXMLParser::ParseInclude(ifs, attributes["src"], *this);
      }

      if (node.getElementName().compare(CContext::GetName()))
         DEBUG("Le noeud is wrong defined but will be considered as a context !");

      if (!(node.goToChildElement()))
      {
         DEBUG("Le context ne contient pas d'enfant !");
      }
      else
      {
         do { // Parcours des contextes pour traitement.

            StdString name = node.getElementName();
            attributes.clear();
            attributes = node.getAttributes();

            if (attributes.end() != attributes.find("id"))
            {
              DEBUG(<< "Definition node has an id,"
                    << "it will not be taking account !");
            }

#define DECLARE_NODE(Name_, name_)    \
   if (name.compare(C##Name_##Definition::GetDefName()) == 0) \
   { C##Name_##Definition::create(C##Name_##Definition::GetDefName()) -> parse(node); continue; }
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"

            DEBUG(<< "The element \'"     << name
                  << "\' in the context \'" << CContext::getCurrent()->getId()
                  << "\' is not a definition !");

         } while (node.goToNextElement());

         node.goToParentElement(); // Retour au parent
      }
   }
   CATCH_DUMP_ATTR

   //----------------------------------------------------------------
   //! Show tree structure of context
   void CContext::ShowTree(StdOStream & out)
   TRY
   {
      StdString currentContextId = CContext::getCurrent() -> getId();
      std::vector<CContext*> def_vector =
         CContext::getRoot()->getChildList();
      std::vector<CContext*>::iterator
         it = def_vector.begin(), end = def_vector.end();

      out << "<? xml version=\"1.0\" ?>" << std::endl;
      out << "<"  << xml::CXMLNode::GetRootName() << " >" << std::endl;

      for (; it != end; it++)
      {
         CContext* context = *it;
         CContext::setCurrent(context->getId());
         out << *context << std::endl;
      }

      out << "</" << xml::CXMLNode::GetRootName() << " >" << std::endl;
      CContext::setCurrent(currentContextId);
   }
   CATCH

   //----------------------------------------------------------------

   //! Convert context object into string (to print)
   StdString CContext::toString(void) const
   TRY
   {
      StdOStringStream oss;
      oss << "<" << CContext::GetName()
          << " id=\"" << this->getId() << "\" "
          << SuperClassAttribute::toString() << ">" << std::endl;
      if (!this->hasChild())
      {
         //oss << "<!-- No definition -->" << std::endl; // fait planter l'incrémentation
      }
      else
      {

#define DECLARE_NODE(Name_, name_)    \
   if (C##Name_##Definition::has(C##Name_##Definition::GetDefName())) \
   oss << * C##Name_##Definition::get(C##Name_##Definition::GetDefName()) << std::endl;
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"

      }
      oss << "</" << CContext::GetName() << " >";
      return (oss.str());
   }
   CATCH

   //----------------------------------------------------------------

   /*!
   \brief Find all inheritace among objects in a context.
   \param [in] apply (true) write attributes of parent into ones of child if they are empty
                     (false) write attributes of parent into a new container of child
   \param [in] parent unused
   */
   void CContext::solveDescInheritance(bool apply, const CAttributeMap * const UNUSED(parent))
   TRY
   {
#define DECLARE_NODE(Name_, name_)    \
   if (C##Name_##Definition::has(C##Name_##Definition::GetDefName())) \
     C##Name_##Definition::get(C##Name_##Definition::GetDefName())->solveDescInheritance(apply);
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
   }
   CATCH_DUMP_ATTR

   //----------------------------------------------------------------

   //! Verify if all root definition in the context have child.
   bool CContext::hasChild(void) const
   TRY
   {
      return (
#define DECLARE_NODE(Name_, name_)    \
   C##Name_##Definition::has(C##Name_##Definition::GetDefName())   ||
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
      false);
}
   CATCH

   //----------------------------------------------------------------

   void CContext::CleanTree(void)
   TRY
   {
#define DECLARE_NODE(Name_, name_) C##Name_##Definition::ClearAllAttributes();
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
   }
   CATCH

void CContext::removeContext(const string& contextId)
{
  #define DECLARE_NODE(Name_, name_)     CObjectFactory::deleteContext< C##Name_ >(contextId);
  #define DECLARE_NODE_PAR(Name_, name_) CObjectFactory::deleteContext< C##Name_ >(contextId);
  #include "node_type.conf"
  #define DECLARE_NODE(Name_, name_)     CObjectFactory::deleteContext< C##Name_##Group >(contextId);
  #define DECLARE_NODE_PAR(Name_, name_) 
  #include "node_type.conf"

/*
  #define DECLARE_NODE(Name_, name_)     CObjectFactory::dumpObjects< C##Name_##Group >();
  #define DECLARE_NODE_PAR(Name_, name_) 
  #include "node_type.conf"

  #define DECLARE_NODE(Name_, name_)     CObjectFactory::dumpObjects< C##Name_>();
  #define DECLARE_NODE_PAR(Name_, name_) 
  #include "node_type.conf"
*/
}

void CContext::removeAllContexts(void)
{
  #define DECLARE_NODE(Name_, name_)     CObjectFactory::deleteAllContexts< C##Name_ >();
  #define DECLARE_NODE_PAR(Name_, name_) CObjectFactory::deleteAllContexts< C##Name_ >();
  #include "node_type.conf"
  #define DECLARE_NODE(Name_, name_)     CObjectFactory::deleteAllContexts< C##Name_##Group >();
  #define DECLARE_NODE_PAR(Name_, name_) 
  #include "node_type.conf"
/*
  #define DECLARE_NODE(Name_, name_)     CObjectFactory::dumpObjects< C##Name_##Group >();
  #define DECLARE_NODE_PAR(Name_, name_) 
  #include "node_type.conf"

  #define DECLARE_NODE(Name_, name_)     CObjectFactory::dumpObjects< C##Name_>();
  #define DECLARE_NODE_PAR(Name_, name_) 
  #include "node_type.conf"
*/
  CObjectFactory::deleteAllContexts<CContext>() ;
  CObjectFactory::deleteAllContexts<CContextGroup>() ;
  CObjectFactory::clearCurrentContextId();
  CGroupFactory::clearCurrentContextId();
}
   ///---------------------------------------------------------------


 /*!
    * Compute the required buffer size to send the fields data.
    * \param maxEventSize [in/out] the size of the bigger event for each connected server
    * \param [in] contextClient
    * \param [in] bufferForWriting True if buffers are used for sending data for writing
      This flag is only true for client and server-1 for communication with server-2
    */
   std::map<int, StdSize> CContext::getDataBufferSize(std::map<int, StdSize>& maxEventSize,
                                                      CContextClient* contextClient, bool bufferForWriting /*= "false"*/)
   TRY
   {
     std::map<int, StdSize> dataSize;

     // Find all reference domain and axis of all active fields
     std::vector<CFile*>& fileList = bufferForWriting ? this->enabledWriteModeFiles : this->enabledReadModeFiles;
     size_t numEnabledFiles = fileList.size();
     for (size_t i = 0; i < numEnabledFiles; ++i)
     {
       CFile* file = fileList[i];
       if (file->getContextClient() == contextClient)
       {
         std::vector<CField*> enabledFields = file->getEnabledFields();
         size_t numEnabledFields = enabledFields.size();
         for (size_t j = 0; j < numEnabledFields; ++j)
         {
           // const std::vector<std::map<int, StdSize> > mapSize = enabledFields[j]->getGridDataBufferSize(contextClient);
           const std::map<int, StdSize> mapSize = enabledFields[j]->getGridDataBufferSize(contextClient,bufferForWriting);
           std::map<int, StdSize>::const_iterator it = mapSize.begin(), itE = mapSize.end();
           for (; it != itE; ++it)
           {
             // If dataSize[it->first] does not exist, it will be zero-initialized
             // so we can use it safely without checking for its existance
           if (CXios::isOptPerformance)
               dataSize[it->first] += it->second;
             else if (dataSize[it->first] < it->second)
               dataSize[it->first] = it->second;

           if (maxEventSize[it->first] < it->second)
               maxEventSize[it->first] = it->second;
           }
         }
       }
     }
     return dataSize;
   }
   CATCH_DUMP_ATTR

/*!
    * Compute the required buffer size to send the attributes (mostly those grid related).
    * \param maxEventSize [in/out] the size of the bigger event for each connected server
    * \param [in] contextClient
    * \param [in] bufferForWriting True if buffers are used for sending data for writing
      This flag is only true for client and server-1 for communication with server-2
    */
   std::map<int, StdSize> CContext::getAttributesBufferSize(std::map<int, StdSize>& maxEventSize,
                                                           CContextClient* contextClient, bool bufferForWriting /*= "false"*/)
   TRY
   {
   // As calendar attributes are sent even if there are no active files or fields, maps are initialized according the size of calendar attributes
     std::map<int, StdSize> attributesSize = CCalendarWrapper::get(CCalendarWrapper::GetDefName())->getMinimumBufferSizeForAttributes(contextClient);
     maxEventSize = CCalendarWrapper::get(CCalendarWrapper::GetDefName())->getMinimumBufferSizeForAttributes(contextClient);

     std::vector<CFile*>& fileList = this->enabledFiles;
     size_t numEnabledFiles = fileList.size();
     for (size_t i = 0; i < numEnabledFiles; ++i)
     {
//         CFile* file = this->enabledWriteModeFiles[i];
        CFile* file = fileList[i];
        std::vector<CField*> enabledFields = file->getEnabledFields();
        size_t numEnabledFields = enabledFields.size();
        for (size_t j = 0; j < numEnabledFields; ++j)
        {
          const std::map<int, StdSize> mapSize = enabledFields[j]->getGridAttributesBufferSize(contextClient, bufferForWriting);
          std::map<int, StdSize>::const_iterator it = mapSize.begin(), itE = mapSize.end();
          for (; it != itE; ++it)
          {
         // If attributesSize[it->first] does not exist, it will be zero-initialized
         // so we can use it safely without checking for its existence
             if (attributesSize[it->first] < it->second)
         attributesSize[it->first] = it->second;

         if (maxEventSize[it->first] < it->second)
         maxEventSize[it->first] = it->second;
          }
        }
     }
     return attributesSize;
   }
   CATCH_DUMP_ATTR



   //! Verify whether a context is initialized
   bool CContext::isInitialized(void)
   TRY
   {
     return hasClient;
   }
   CATCH_DUMP_ATTR


   void CContext::init(CServerContext* parentServerContext, MPI_Comm intraComm, int serviceType)
   TRY
   {
     parentServerContext_ = parentServerContext ;
     if (serviceType==CServicesManager::CLIENT) 
       initClient(intraComm, serviceType) ;
     else
       initServer(intraComm, serviceType) ;
     initEventScheduler() ;
    }
    CATCH_DUMP_ATTR



//! Initialize client side
   void CContext::initClient(MPI_Comm intraComm, int serviceType)
   TRY
   {
      intraComm_=intraComm ;
      MPI_Comm_rank(intraComm_, &intraCommRank_) ;
      MPI_Comm_size(intraComm_, &intraCommSize_) ;

      serviceType_ = CServicesManager::CLIENT ;
      if (serviceType_==CServicesManager::CLIENT)
      {
        hasClient=true ;
        hasServer=false ;
      }
      contextId_ = getId() ;
      
      attached_mode=true ;
      if (!CXios::isUsingServer()) attached_mode=false ;


      string contextRegistryId=getId() ;
      registryIn=new CRegistry(CXios::getRegistryManager()->getRegistryIn());
      registryIn->setPath(contextRegistryId) ;
      
      registryOut=new CRegistry(intraComm_) ;
      registryOut->setPath(contextRegistryId) ;
     
   }
   CATCH_DUMP_ATTR

   
   void CContext::initServer(MPI_Comm intraComm, int serviceType)
   TRY
   {
     hasServer=true;
     intraComm_=intraComm ;
     MPI_Comm_rank(intraComm_, &intraCommRank_) ;
     MPI_Comm_size(intraComm_, &intraCommSize_) ;

     serviceType_=serviceType ;

     if (serviceType_==CServicesManager::GATHERER)
     {
       hasClient=true ;
       hasServer=true ;
     }
     else if (serviceType_==CServicesManager::WRITER || serviceType_==CServicesManager::READER)
     {
       hasClient=false ;
       hasServer=true ;
     }

     CXios::getContextsManager()->getContextId(getId(), contextId_, intraComm) ;
     
     string contextRegistryId=getId() ;
     registryIn=new CRegistry(CXios::getRegistryManager()->getRegistryIn());
     registryIn->setPath(contextRegistryId) ;
      
     registryOut=new CRegistry(intraComm_) ;
     registryOut->setPath(contextRegistryId) ;

   }
   CATCH_DUMP_ATTR


  void CContext::createClientInterComm(MPI_Comm interCommClient, MPI_Comm interCommServer) // for servers
  TRY
  {
    MPI_Comm intraCommClient ;
    MPI_Comm_dup(intraComm_, &intraCommClient);
    comms.push_back(intraCommClient);
    // attached_mode=parentServerContext_->isAttachedMode() ; //ym probably inherited from source context

    CContextServer* server ;
    CContextClient* client ;
    
    server = CContextServer::getNew(this,intraComm_, interCommServer); // check if we need to dupl. intraComm_ ?
    client = CContextClient::getNew(this,intraCommClient,interCommClient);
    client->setAssociatedServer(server) ;  
    server->setAssociatedClient(client) ;
    
    if (serviceType_ == CServicesManager::GATHERER || serviceType_ == CServicesManager::WRITER)
    {  
      writerServerIn_.push_back(server) ; 
      writerClientIn_.push_back(client) ; 
    }
    else if (serviceType_ == CServicesManager::READER)
    {
      readerServerIn_.push_back(server) ; 
      readerClientIn_.push_back(client) ; 
    }
  }
  CATCH_DUMP_ATTR

  
  void CContext::createServerInterComm(const string& poolId, const string& serverId, vector<pair<string, pair<CContextClient*,CContextServer*>>>& clientServers )
  TRY
  {
    MPI_Comm interCommClient, interCommServer ;
    int commRank ;
    MPI_Comm_rank(intraComm_,&commRank) ;
    
    int nbPartitions ;
    if (commRank==0)
    {
      CXios::getServicesManager()->getServiceNbPartitions(poolId, serverId, 0, nbPartitions) ;
      for(int i=0 ; i<nbPartitions; i++) CXios::getContextsManager()->createServerContext(poolId, serverId, i, getContextId()) ;
    }
    setCurrent(getId()) ; // getCurrent/setCurrent may be supress, it can cause a lot of trouble (attached ???)
    MPI_Bcast(&nbPartitions, 1, MPI_INT, 0, intraComm_) ;
      
    MPI_Comm interComm ;
    for(int i=0 ; i<nbPartitions; i++)
    {
      parentServerContext_->createIntercomm(poolId, serverId, i, getContextId(), intraComm_, interCommClient, interCommServer) ;
      int type ; 
      if (commRank==0) CXios::getServicesManager()->getServiceType(poolId, serverId, 0, type) ;
      MPI_Bcast(&type,1,MPI_INT,0,intraComm_) ;
      string fullServerId=CXios::getContextsManager()->getServerContextName(poolId, serverId, i, type, getContextId()) ;

      MPI_Comm intraCommClient, intraCommServer ;

      intraCommClient=intraComm_ ;
      MPI_Comm_dup(intraComm_, &intraCommServer) ;

      CContextClient* client = CContextClient::getNew(this, intraCommClient, interCommClient) ;
      CContextServer* server = CContextServer::getNew(this, intraCommServer, interCommServer) ;
      client->setAssociatedServer(server) ;
      server->setAssociatedClient(client) ;
      
      clientServers.push_back({fullServerId,{client,server}}) ;
      clientsId_[client] = fullServerId ;
      serversId_[server] = fullServerId ;
    }
    setCurrent(getId()) ; // Back on main context
  }
  CATCH_DUMP_ATTR
  
  void CContext::createServerInterComm(void) 
  TRY
  {
    vector<pair<string, pair<CContextClient*,CContextServer*>>> clientServers ;

    if (serviceType_ == CServicesManager::CLIENT)
    {
      if (attached_mode) createServerInterComm(CClient::getPoolRessource()->getId(), getContextId()+"_"+CXios::defaultWriterId, clientServers) ;
      else if (CXios::usingServer2) createServerInterComm(CXios::defaultPoolId, CXios::defaultGathererId, clientServers) ;
      else createServerInterComm(CXios::defaultPoolId, CXios::defaultWriterId, clientServers) ;
      
      writerClientOut_.push_back(clientServers[0].second.first) ; 
      writerServerOut_.push_back(clientServers[0].second.second) ;

      clientServers.clear() ;
   
      if (attached_mode) createServerInterComm(CClient::getPoolRessource()->getId(), getContextId()+"_"+CXios::defaultReaderId, clientServers) ;
      else createServerInterComm(CXios::defaultPoolId, CXios::defaultReaderId, clientServers) ;
      readerClientOut_.push_back(clientServers[0].second.first) ; 
      readerServerOut_.push_back(clientServers[0].second.second) ;


    }
    else if (serviceType_ == CServicesManager::GATHERER)
    {
      createServerInterComm(CXios::defaultPoolId, CXios::defaultWriterId, clientServers) ;
      for(auto& clientServer : clientServers)
      {
        writerClientOut_.push_back(clientServer.second.first) ; 
        writerServerOut_.push_back(clientServer.second.second) ;
      }
    }

  }
  CATCH_DUMP_ATTR

  void CContext::globalEventLoop(void)
  {
    lockContext() ;
    CXios::getDaemonsManager()->eventLoop() ;
    unlockContext() ;
    setCurrent(getId()) ;
  }

  bool CContext::scheduledEventLoop(bool enableEventsProcessing) 
  {
    bool out, finished; 
    size_t timeLine=timeLine_ ;
    if (serviceType_==CServicesManager::CLIENT)
    {
      timeLine_++ ;
      eventScheduler_->registerEvent(timeLine, hashId_) ;
    }

    do
    { 
      finished=eventLoop(enableEventsProcessing) ;
      if (serviceType_==CServicesManager::CLIENT) 
      {  
        out = eventScheduler_->queryEvent(timeLine,hashId_) ;
        if (out) eventScheduler_->popEvent() ;
      }

      else out=true ;
    }  while(!out) ;
    
    return finished ;
  }

  bool CContext::eventLoop(bool enableEventsProcessing)
  {
    bool  finished(true); 
    if (isLockedContext()) enableEventsProcessing=false;
    
    setCurrent(getId()) ;

    if (!finalized)
    {
      for(auto client : writerClientOut_) client->eventLoop();
      for(auto server : writerServerOut_) finished &= server->eventLoop(enableEventsProcessing);
      for(auto client : writerClientIn_) client->eventLoop();
      for(auto server : writerServerIn_) finished &= server->eventLoop(enableEventsProcessing);
      for(auto client : readerClientOut_) client->eventLoop();
      for(auto server : readerServerOut_) finished &= server->eventLoop(enableEventsProcessing);
      for(auto client : readerClientIn_) client->eventLoop();
      for(auto server : readerServerIn_) finished &= server->eventLoop(enableEventsProcessing);
      for(auto couplerOut : couplerOutClient_) couplerOut.second->eventLoop();
      for(auto couplerIn : couplerInClient_) couplerIn.second->eventLoop();
      for(auto couplerOut : couplerOutServer_) couplerOut.second->eventLoop(enableEventsProcessing);
      for(auto couplerIn : couplerInServer_) couplerIn.second->eventLoop(enableEventsProcessing);
    }
    setCurrent(getId()) ;
    return finalized && finished ;
  }

  void CContext::addCouplingChanel(const std::string& fullContextId, bool out)
  {
     int contextLeader ;
     
     if (out)
     { 
       if (couplerOutClient_.find(fullContextId)==couplerOutClient_.end()) 
       {
         bool ok=CXios::getContextsManager()->getContextLeader(fullContextId, contextLeader, getIntraComm()) ;
     
         MPI_Comm interComm, interCommClient, interCommServer  ;
         MPI_Comm intraCommClient, intraCommServer ;

         if (ok) MPI_Intercomm_create(getIntraComm(), 0, CXios::getXiosComm(), contextLeader, 0, &interComm) ;

        MPI_Comm_dup(intraComm_, &intraCommClient) ;
        MPI_Comm_dup(intraComm_, &intraCommServer) ;
        MPI_Comm_dup(interComm, &interCommClient) ;
        MPI_Comm_dup(interComm, &interCommServer) ;
        CContextClient* client = CContextClient::getNew(this, intraCommClient, interCommClient);
        CContextServer* server = CContextServer::getNew(this, intraCommServer, interCommServer);
        client->setAssociatedServer(server) ;
        server->setAssociatedClient(client) ;
        MPI_Comm_free(&interComm) ;
        couplerOutClient_[fullContextId] = client ;
        couplerOutServer_[fullContextId] = server ;
      }
    }
    else if (couplerInClient_.find(fullContextId)==couplerInClient_.end())
    {
      bool ok=CXios::getContextsManager()->getContextLeader(fullContextId, contextLeader, getIntraComm()) ;
     
       MPI_Comm interComm, interCommClient, interCommServer  ;
       MPI_Comm intraCommClient, intraCommServer ;

       if (ok) MPI_Intercomm_create(getIntraComm(), 0, CXios::getXiosComm(), contextLeader, 0, &interComm) ;

       MPI_Comm_dup(intraComm_, &intraCommClient) ;
       MPI_Comm_dup(intraComm_, &intraCommServer) ;
       MPI_Comm_dup(interComm, &interCommServer) ;
       MPI_Comm_dup(interComm, &interCommClient) ;
       CContextServer* server = CContextServer::getNew(this, intraCommServer, interCommServer);
       CContextClient* client = CContextClient::getNew(this, intraCommClient, interCommClient);
       client->setAssociatedServer(server) ;
       server->setAssociatedClient(client) ;
       MPI_Comm_free(&interComm) ;

       couplerInClient_[fullContextId] = client ;
       couplerInServer_[fullContextId] = server ;        
    }
  }
  
   void CContext::finalize(void)
   TRY
   {
      registryOut->hierarchicalGatherRegistry() ;
      if (intraCommRank_==0) CXios::getRegistryManager()->merge(*registryOut) ;

      if (serviceType_==CServicesManager::CLIENT)
      {
//ym        doPreTimestepOperationsForEnabledReadModeFiles(); // For now we only use server level 1 to read data
        triggerLateFields() ;

        // inform couplerIn that I am finished
        for(auto& couplerInClient : couplerInClient_) sendCouplerInContextFinalized(couplerInClient.second) ;

        // wait until received message from couplerOut that they have finished
        bool couplersInFinalized ;
        do
        {
          couplersInFinalized=true ;
          for(auto& couplerOutClient : couplerOutClient_) couplersInFinalized &= isCouplerInContextFinalized(couplerOutClient.second) ; 
          globalEventLoop() ;
        } while (!couplersInFinalized) ;

        CContextClient* client ;
        CContextServer* server ;

        if (writerClientOut_.size()!=0)
        {
          client=writerClientOut_[0] ;
          server=writerServerOut_[0] ;

          info(100)<<"DEBUG: context "<<getId()<<" Send client finalize to writer"<<endl ;
          client->finalize();
          info(100)<<"DEBUG: context "<<getId()<<" Client finalize sent to writer"<<endl ;
          while (client->havePendingRequests()) client->eventLoop();
          info(100)<<"DEBUG: context "<<getId()<<" no pending request on writer ok"<<endl ;
          bool notifiedFinalized=false ;
          do
          {
            notifiedFinalized = client->isNotifiedFinalized() ;
          } while (!notifiedFinalized) ;

          server->releaseBuffers();
          client->releaseBuffers();
          info(100)<<"DEBUG: context "<<getId()<<" release client writer ok"<<endl ;
        }

        if (readerClientOut_.size()!=0)
        {
          client=readerClientOut_[0] ;
          server=readerServerOut_[0] ;

          info(100)<<"DEBUG: context "<<getId()<<" Send client finalize to reader"<<endl ;
          client->finalize();
          info(100)<<"DEBUG: context "<<getId()<<" Client finalize sent to reader"<<endl ;
          while (client->havePendingRequests()) client->eventLoop();
          info(100)<<"DEBUG: context "<<getId()<<" no pending request on reader ok"<<endl ;
          bool notifiedFinalized=false ;
         do
         {
            notifiedFinalized = client->isNotifiedFinalized() ;
         }   while (!notifiedFinalized) ;

          server->releaseBuffers();
          client->releaseBuffers();
          info(100)<<"DEBUG: context "<<getId()<<" release client reader ok"<<endl ;
        }
      }
      else if (serviceType_==CServicesManager::GATHERER)
      {
         for(auto& client : writerClientOut_)
         {
           client->finalize();
           bool bufferReleased;
           do
           {
             client->eventLoop();
             bufferReleased = !client->havePendingRequests();
           } while (!bufferReleased);
           
           bool notifiedFinalized=false ;
           do
           {
             notifiedFinalized=client->isNotifiedFinalized() ;
           } while (!notifiedFinalized) ;
           client->releaseBuffers();
         }
         closeAllFile();
         //ym writerClientIn & writerServerIn not released here ==> to check !!
      }
      else if (serviceType_==CServicesManager::WRITER)
      {
        closeAllFile();
        writerClientIn_[0]->releaseBuffers();
        writerServerIn_[0]->releaseBuffers();
      }
      else if (serviceType_==CServicesManager::READER)
      {
        closeAllFile();
        readerClientIn_[0]->releaseBuffers();
        readerServerIn_[0]->releaseBuffers();
      }

      freeComms() ;
        
      parentServerContext_->freeComm() ;
      finalized = true;
      info(20)<<"CContext: Context <"<<getId()<<"> is finalized."<<endl;
   }
   CATCH_DUMP_ATTR

   //! Free internally allocated communicators
   void CContext::freeComms(void)
   TRY
   {
     for (std::list<MPI_Comm>::iterator it = comms.begin(); it != comms.end(); ++it)
       MPI_Comm_free(&(*it));
     comms.clear();
   }
   CATCH_DUMP_ATTR

   
   /*!
   \brief Close all the context defintion and do processing data
      After everything is well defined on client side, they will be processed and sent to server
   From the version 2.0, sever and client work no more on the same database. Moreover, client(s) will send
   all necessary information to server, from which each server can build its own database.
   Because the role of server is to write out field data on a specific netcdf file,
   the only information that it needs is the enabled files
   and the active fields (fields will be written onto active files)
   */
  void CContext::closeDefinition(void)
   TRY
   {
     CMemChecker::logMem( "CContext::closeDefinition" );

     CTimer::get("Context : close definition").resume() ;
     
     // create intercommunicator with servers. 
     // not sure it is the good place to be called here 
     createServerInterComm() ;


     // After xml is parsed, there are some more works with post processing
//     postProcessing();

    
    // Make sure the calendar was correctly created
    if (serviceType_!=CServicesManager::CLIENT) CCalendarWrapper::get(CCalendarWrapper::GetDefName())->createCalendar();
    if (!calendar)
      ERROR("CContext::postProcessing()", << "A calendar must be defined for the context \"" << getId() << "!\"")
    else if (calendar->getTimeStep() == NoneDu)
      ERROR("CContext::postProcessing()", << "A timestep must be defined for the context \"" << getId() << "!\"")
    // Calendar first update to set the current date equals to the start date
    calendar->update(0);

    // Résolution des héritages descendants (càd des héritages de groupes)
    // pour chacun des contextes.
    solveDescInheritance(true);
 
    // Check if some axis, domains or grids are eligible to for compressed indexed output.
    // Warning: This must be done after solving the inheritance and before the rest of post-processing
    // --> later ????    checkAxisDomainsGridsEligibilityForCompressedOutput();      

      // Check if some automatic time series should be generated
      // Warning: This must be done after solving the inheritance and before the rest of post-processing      

    // The timeseries should only be prepared in client
    prepareTimeseries();

    //Initialisation du vecteur 'enabledFiles' contenant la liste des fichiers à sortir.
    findEnabledFiles();

    // Solve inheritance for field to know if enabled or not.
    for (auto field : CField::getAll()) field->solveRefInheritance();

    findEnabledWriteModeFiles();
    findEnabledReadModeFiles();
    findEnabledCouplerIn();
    findEnabledCouplerOut();
    createCouplerInterCommunicator() ;

    // Find all enabled fields of each file      
    vector<CField*>&& fileOutField = findAllEnabledFieldsInFileOut(this->enabledWriteModeFiles);
    vector<CField*>&& fileInField = findAllEnabledFieldsInFileIn(this->enabledReadModeFiles);
    vector<CField*>&& couplerOutField = findAllEnabledFieldsCouplerOut(this->enabledCouplerOut);
    vector<CField*>&& couplerInField = findAllEnabledFieldsCouplerIn(this->enabledCouplerIn);
    findFieldsWithReadAccess();
    vector<CField*>& fieldWithReadAccess = fieldsWithReadAccess_ ;
    vector<CField*> fieldModelIn ; // fields potentially from model

    // define if files are on clientSied or serverSide 
    if (serviceType_==CServicesManager::CLIENT)
    {
      for (auto& file : enabledWriteModeFiles) file->setClientSide() ;
      for (auto& file : enabledReadModeFiles) file->setClientSide() ;
    }
    else
    {
      for (auto& file : enabledWriteModeFiles) file->setServerSide() ;
      for (auto& file : enabledReadModeFiles) file->setServerSide() ;
    }

    
    for (auto& field : couplerInField)
    {
      field->unsetGridCompleted() ;
    }
// find all field potentially at workflow end
    vector<CField*> endWorkflowFields ;
    endWorkflowFields.reserve(fileOutField.size()+couplerOutField.size()+fieldWithReadAccess.size()) ;
    endWorkflowFields.insert(endWorkflowFields.end(),fileOutField.begin(), fileOutField.end()) ;
    endWorkflowFields.insert(endWorkflowFields.end(),couplerOutField.begin(), couplerOutField.end()) ;
    endWorkflowFields.insert(endWorkflowFields.end(),fieldWithReadAccess.begin(), fieldWithReadAccess.end()) ;

    bool workflowGraphIsCompleted ;
    
    bool first=true ;
   
    do
    {
      workflowGraphIsCompleted=true; 
      for(auto endWorkflowField : endWorkflowFields) 
      {
        workflowGraphIsCompleted &= endWorkflowField->buildWorkflowGraph(garbageCollector) ;
      }
   
      for(auto couplerIn : enabledCouplerIn) couplerIn->assignContext() ;
      for(auto field : couplerInField) field->makeGridAliasForCoupling();
      for(auto field : couplerInField) this->sendCouplerInReady(field->getContextClient()) ;
    

      // assign context to coupler out and related fields
      for(auto couplerOut : enabledCouplerOut) couplerOut->assignContext() ;
      // for now supose that all coupling out endpoint are succesfull. The difficultie is client/server buffer evaluation
      for(auto field : couplerOutField) 
      {
        // connect to couplerOut -> to do
      }

      bool couplersReady ;
      do 
      {
        couplersReady=true ;
        for(auto field : couplerOutField)
        {
          bool ready = isCouplerInReady(field->getContextClient()) ; 
          if (ready) field->sendFieldToCouplerOut() ;
          couplersReady &= ready ;
        }
        this->scheduledEventLoop() ;

      } while (!couplersReady) ;
      
      first=false ;
      this->scheduledEventLoop() ;

    } while (!workflowGraphIsCompleted) ;


    for( auto field : couplerInField) couplerInFields_.push_back(field) ;

    // get all field coming potentially from model
    for (auto field : CField::getAll() ) if (field->getModelIn()) fieldModelIn.push_back(field) ;

    // Distribute files between secondary servers according to the data size => assign a context to a file and then to fields
    if (serviceType_==CServicesManager::CLIENT || serviceType_==CServicesManager::GATHERER ) distributeFiles(this->enabledWriteModeFiles);
    //else if (serviceType_==CServicesManager::CLIENT) for(auto file : this->enabledWriteModeFiles) file->setContextClient(client) ;

    // client side, assign context for file reading
    if (serviceType_==CServicesManager::CLIENT) for(auto file : this->enabledReadModeFiles) file->setContextClient(readerClientOut_[0]) ;
    
    // server side, assign context where to send file data read
    if (serviceType_==CServicesManager::READER) for(auto file : this->enabledReadModeFiles) file->setContextClient(readerClientIn_[0]) ;
   

    // workflow startpoint => data from server on client side
    // important : sendFieldToInputFileServer must be done prior sendFieldToFileServer because for the first case the grid remoteConnectorIn
    //             and grid remoteConnectorOut will be computed, and in the second case only the remoteConnectorOut.
    if (serviceType_==CServicesManager::CLIENT)
    {
      for(auto field : fileInField) 
      {
        field->sendFieldToInputFileServer() ;
        field->connectToServerInput(garbageCollector) ; // connect the field to server filter
        fileInFields_.push_back(field) ;
      }
    }

    // workflow endpoint => sent to IO/SERVER
    if (serviceType_==CServicesManager::CLIENT || serviceType_==CServicesManager::GATHERER)
    {
      for(auto field : fileOutField) 
      {
        field->connectToFileServer(garbageCollector) ; // connect the field to server filter
      }
      for(auto field : fileOutField) field->sendFieldToFileServer() ;
    }

    // workflow endpoint => write to file
    if (serviceType_==CServicesManager::WRITER)
    {
      for(auto field : fileOutField) 
      {
        field->connectToFileWriter(garbageCollector) ; // connect the field to server filter
      }
    }
    
    // workflow endpoint => Send data from server to client
    if (serviceType_==CServicesManager::READER || serviceType_==CServicesManager::GATHERER)
    {
      for(auto field : fileInField) 
      {
        field->connectToServerToClient(garbageCollector) ;
      }
    }

    // workflow endpoint => sent to model on client side
    if (serviceType_==CServicesManager::CLIENT)
    {
      for(auto field : fieldWithReadAccess) field->connectToModelOutput(garbageCollector) ;
    }


    // workflow startpoint => data from model
    if (serviceType_==CServicesManager::CLIENT)
    {
      for(auto field : fieldModelIn) 
      {
        field->connectToModelInput(garbageCollector) ; // connect the field to server filter
        // grid index will be computed on the fly
      }
    }
    
    // workflow startpoint => data from client on server side
    if (serviceType_==CServicesManager::WRITER || serviceType_==CServicesManager::GATHERER)
    {
      for(auto field : fieldModelIn) 
      {
        field->connectToClientInput(garbageCollector) ; // connect the field to server filter
      }
    }

    
    for(auto field : couplerInField) 
    {
      field->connectToCouplerIn(garbageCollector) ; // connect the field to server filter
    }
    
    
    for(auto field : couplerOutField) 
    {
      field->connectToCouplerOut(garbageCollector) ; // for now the same kind of filter that for file server
    }

    // workflow startpoint => data read from file on server side
    if (serviceType_==CServicesManager::READER)
    {
      for(auto field : fileInField) 
      {
        field->connectToFileReader(garbageCollector) ;
      }
    }
    
    // construct slave server list
    map<string, CContextClient*> slaves ; // need an ordered list ; 
    if (serviceType_==CServicesManager::CLIENT) 
    {
      for(auto field : fileOutField) slaves[clientsId_[field->getContextClient()]] = field->getContextClient() ; 
      for(auto field : fileInField) slaves[clientsId_[field->getContextClient()]] = field->getContextClient() ; 
    }
    else if (serviceType_==CServicesManager::GATHERER) 
      for(auto field : fileOutField) slaves[clientsId_[field->getContextClient()]] = field->getContextClient() ;
    for(auto& slave : slaves) slaveServers_.push_back(slave.second) ;   


    for(auto& slaveServer : slaveServers_) sendCloseDefinition(slaveServer) ;

    if (serviceType_==CServicesManager::WRITER)  
    {
      createFileHeader();
    }

    if (serviceType_==CServicesManager::CLIENT) startPrefetchingOfEnabledReadModeFiles();
   
    // send signal to couplerIn context that definition phasis is done

    for(auto& couplerInClient : couplerInClient_) sendCouplerInCloseDefinition(couplerInClient.second) ;

    // wait until all couplerIn signal that closeDefition is done.
    bool ok;
    do
    {
      ok = true ;
      for(auto& couplerOutClient : couplerOutClient_) ok &= isCouplerInCloseDefinition(couplerOutClient.second) ;
      this->scheduledEventLoop() ;
    } while (!ok) ;

    // Now evaluate the size of the context client buffers
    map<CContextClient*,map<int,size_t>> fieldBufferEvaluation ;
    for(auto field : fileOutField) field->evaluateBufferSize(fieldBufferEvaluation, CXios::isOptPerformance) ; // output to server
    for(auto field : couplerOutField) field->evaluateBufferSize(fieldBufferEvaluation, CXios::isOptPerformance) ; // output to coupler
    for(auto field : fileInField) field->evaluateBufferSize(fieldBufferEvaluation, CXios::isOptPerformance) ; // server to client (for io servers)
    
    // fix size for each context client
    for(auto& it : fieldBufferEvaluation) it.first->setBufferSize(it.second) ;

     CTimer::get("Context : close definition").suspend() ;
     CMemChecker::logMem( "CContext::closeDefinition END" );
  }
  CATCH_DUMP_ATTR


  vector<CField*> CContext::findAllEnabledFieldsInFileOut(const std::vector<CFile*>& activeFiles)
   TRY
   {
     vector<CField*> fields ;
     for(auto file : activeFiles)
     {
        const vector<CField*>&& fieldList=file->getEnabledFields() ;
        for(auto field : fieldList) field->setFileOut(file) ;
        fields.insert(fields.end(),fieldList.begin(),fieldList.end());
     }
     return fields ;
   }
   CATCH_DUMP_ATTR

   vector<CField*> CContext::findAllEnabledFieldsInFileIn(const std::vector<CFile*>& activeFiles)
   TRY
   {
     vector<CField*> fields ;
     for(auto file : activeFiles)
     {
        const vector<CField*>&& fieldList=file->getEnabledFields() ;
        for(auto field : fieldList) field->setFileIn(file) ;
        fields.insert(fields.end(),fieldList.begin(),fieldList.end());
     }
     return fields ;
   }
   CATCH_DUMP_ATTR

   vector<CField*> CContext::findAllEnabledFieldsCouplerOut(const std::vector<CCouplerOut*>& activeCouplerOut)
   TRY
   {
     vector<CField*> fields ;
     for (auto couplerOut :activeCouplerOut)
     {
        const vector<CField*>&& fieldList=couplerOut->getEnabledFields() ;
        for(auto field : fieldList) field->setCouplerOut(couplerOut) ;
        fields.insert(fields.end(),fieldList.begin(),fieldList.end());
     }
     return fields ;
   }
   CATCH_DUMP_ATTR

   vector<CField*> CContext::findAllEnabledFieldsCouplerIn(const std::vector<CCouplerIn*>& activeCouplerIn)
   TRY
   {
     vector<CField*> fields ;
     for (auto couplerIn :activeCouplerIn)
     {
        const vector<CField*>&& fieldList=couplerIn->getEnabledFields() ;
        for(auto field : fieldList) field->setCouplerIn(couplerIn) ;
        fields.insert(fields.end(),fieldList.begin(),fieldList.end());
     }
     return fields ;
   }
   CATCH_DUMP_ATTR

 /*!
  * Send context attribute and calendar to file server, it must be done once by context file server
  * \param[in] client : context client to send   
  */  
  void CContext::sendContextToFileServer(CContextClient* client)
  {
    if (sendToFileServer_done_.count(client)!=0) return ;
    else sendToFileServer_done_.insert(client) ;
    
    this->sendAllAttributesToServer(client); // Send all attributes of current context to server
    CCalendarWrapper::get(CCalendarWrapper::GetDefName())->sendAllAttributesToServer(client); // Send all attributes of current cale
  }

 
   void CContext::readAttributesOfEnabledFieldsInReadModeFiles()
   TRY
   {
      for (unsigned int i = 0; i < this->enabledReadModeFiles.size(); ++i)
        (void)this->enabledReadModeFiles[i]->readAttributesOfEnabledFieldsInReadMode();
   }
   CATCH_DUMP_ATTR


   void CContext::postProcessFilterGraph()
   TRY
   {
     int size = enabledFiles.size();
     for (int i = 0; i < size; ++i)
     {
        enabledFiles[i]->postProcessFilterGraph();
     }
   }
   CATCH_DUMP_ATTR

   void CContext::startPrefetchingOfEnabledReadModeFiles()
   TRY
   {
     int size = enabledReadModeFiles.size();
     for (int i = 0; i < size; ++i)
     {
        enabledReadModeFiles[i]->prefetchEnabledReadModeFields();
     }
   }
   CATCH_DUMP_ATTR

   void CContext::doPreTimestepOperationsForEnabledReadModeFiles()
   TRY
   {
     int size = enabledReadModeFiles.size();
     for (int i = 0; i < size; ++i)
     {
        enabledReadModeFiles[i]->doPreTimestepOperationsForEnabledReadModeFields();
     }
   }
   CATCH_DUMP_ATTR

   void CContext::doPostTimestepOperationsForEnabledReadModeFiles()
   TRY
   {
     int size = enabledReadModeFiles.size();
     for (int i = 0; i < size; ++i)
     {
        enabledReadModeFiles[i]->doPostTimestepOperationsForEnabledReadModeFields();
     }
   }
   CATCH_DUMP_ATTR

  void CContext::findFieldsWithReadAccess(void)
  TRY
  {
    fieldsWithReadAccess_.clear();
    const vector<CField*> allFields = CField::getAll();
    for (size_t i = 0; i < allFields.size(); ++i)
    {
      CField* field = allFields[i];
      if (!field->read_access.isEmpty() && field->read_access && (field->enabled.isEmpty() || field->enabled))
      {
        fieldsWithReadAccess_.push_back(field);
        field->setModelOut() ;
      }
    }
  }
  CATCH_DUMP_ATTR

  
   void CContext::solveAllInheritance(bool apply)
   TRY
   {
     // Résolution des héritages descendants (càd des héritages de groupes)
     // pour chacun des contextes.
      solveDescInheritance(apply);

     // Résolution des héritages par référence au niveau des fichiers.
      const vector<CFile*> allFiles=CFile::getAll();
      const vector<CCouplerIn*> allCouplerIn=CCouplerIn::getAll();
      const vector<CCouplerOut*> allCouplerOut=CCouplerOut::getAll();
      const vector<CGrid*> allGrids= CGrid::getAll();

      if (serviceType_==CServicesManager::CLIENT)
      {
        for (unsigned int i = 0; i < allFiles.size(); i++)
          allFiles[i]->solveFieldRefInheritance(apply);

        for (unsigned int i = 0; i < allCouplerIn.size(); i++)
          allCouplerIn[i]->solveFieldRefInheritance(apply);

        for (unsigned int i = 0; i < allCouplerOut.size(); i++)
          allCouplerOut[i]->solveFieldRefInheritance(apply);
      }

      unsigned int vecSize = allGrids.size();
      unsigned int i = 0;
      for (i = 0; i < vecSize; ++i)
        allGrids[i]->solveElementsRefInheritance(apply);

   }
  CATCH_DUMP_ATTR

   void CContext::findEnabledFiles(void)
   TRY
   {
      const std::vector<CFile*> allFiles = CFile::getAll();
      const CDate& initDate = calendar->getInitDate();

      for (unsigned int i = 0; i < allFiles.size(); i++)
         if (!allFiles[i]->enabled.isEmpty()) // Si l'attribut 'enabled' est défini.
         {
            if (allFiles[i]->enabled.getValue()) // Si l'attribut 'enabled' est fixé à vrai.
            {
              if (allFiles[i]->output_freq.isEmpty())
              {
                 ERROR("CContext::findEnabledFiles()",
                     << "Mandatory attribute output_freq must be defined for file \""<<allFiles[i]->getFileOutputName()
                     <<" \".")
              }
              if ((initDate + allFiles[i]->output_freq.getValue()) < (initDate + this->getCalendar()->getTimeStep()))
              {
                error(0)<<"WARNING: void CContext::findEnabledFiles()"<<endl
                    << "Output frequency in file \""<<allFiles[i]->getFileOutputName()
                    <<"\" is less than the time step. File will not be written."<<endl;
              }
              else
               enabledFiles.push_back(allFiles[i]);
            }
	    else // Si l'attribut 'enabled' est fixé à faux.
	    {
	      // disabled all fields contained in file (used in findFieldsWithReadAccess through field_ref dependencies)
	      const vector<CField*>&& fieldList=allFiles[i]->getEnabledFields() ;
	      for(auto field : fieldList) field->enabled.setValue(false);
	    }
         }
         else
         {
           if (allFiles[i]->output_freq.isEmpty())
           {
              ERROR("CContext::findEnabledFiles()",
                  << "Mandatory attribute output_freq must be defined for file \""<<allFiles[i]->getFileOutputName()
                  <<" \".")
           }
           if ( (initDate + allFiles[i]->output_freq.getValue()) < (initDate + this->getCalendar()->getTimeStep()))
           {
             error(0)<<"WARNING: void CContext::findEnabledFiles()"<<endl
                 << "Output frequency in file \""<<allFiles[i]->getFileOutputName()
                 <<"\" is less than the time step. File will not be written."<<endl;
           }
           else
             enabledFiles.push_back(allFiles[i]); // otherwise true by default
         }

      if (enabledFiles.size() == 0)
         DEBUG(<<"Aucun fichier ne va être sorti dans le contexte nommé \""
               << getId() << "\" !");

   }
   CATCH_DUMP_ATTR

   void CContext::findEnabledCouplerIn(void)
   TRY
   {
      const std::vector<CCouplerIn*> allCouplerIn = CCouplerIn::getAll();
      bool enabled ;
      for (size_t i = 0; i < allCouplerIn.size(); i++)
      {
        if (allCouplerIn[i]->enabled.isEmpty()) enabled=true ;
        else enabled=allCouplerIn[i]->enabled ;
        if (enabled) enabledCouplerIn.push_back(allCouplerIn[i]) ;
      }
   }
   CATCH_DUMP_ATTR

   void CContext::findEnabledCouplerOut(void)
   TRY
   {
      const std::vector<CCouplerOut*> allCouplerOut = CCouplerOut::getAll();
      bool enabled ;
      for (size_t i = 0; i < allCouplerOut.size(); i++)
      {
        if (allCouplerOut[i]->enabled.isEmpty()) enabled=true ;
        else enabled=allCouplerOut[i]->enabled ;
        if (enabled) enabledCouplerOut.push_back(allCouplerOut[i]) ;
      }
   }
   CATCH_DUMP_ATTR




   void CContext::distributeFiles(const vector<CFile*>& files)
   TRY
   {
     bool distFileMemory=false ;
     distFileMemory=CXios::getin<bool>("server2_dist_file_memory", distFileMemory);

     int nbPools = writerClientOut_.size();
     if (nbPools==1) distributeFileOverOne(files) ;
     else if (distFileMemory) distributeFileOverMemoryBandwith(files) ;
     else distributeFileOverBandwith(files) ;
   }
   CATCH_DUMP_ATTR

   void CContext::distributeFileOverOne(const vector<CFile*>& files)
   TRY
   {
    for(auto& file : files) file->setContextClient(writerClientOut_[0]) ;
   }
   CATCH_DUMP_ATTR

   void CContext::distributeFileOverBandwith(const vector<CFile*>& files)
   TRY
   {
     double eps=std::numeric_limits<double>::epsilon()*10 ;
     
     std::ofstream ofs(("distribute_file_"+getId()+".dat").c_str(), std::ofstream::out);
     int nbPools = writerClientOut_.size();

     // (1) Find all enabled files in write mode
     // for (int i = 0; i < this->enabledFiles.size(); ++i)
     // {
     //   if (enabledFiles[i]->mode.isEmpty() || (!enabledFiles[i]->mode.isEmpty() && enabledFiles[i]->mode.getValue() == CFile::mode_attr::write ))
     //    enabledWriteModeFiles.push_back(enabledFiles[i]);
     // }

     // (2) Estimate the data volume for each file
     int size = files.size();
     std::vector<std::pair<double, CFile*> > dataSizeMap;
     double dataPerPool = 0;
     int nfield=0 ;
     ofs<<size<<endl ;
     for (size_t i = 0; i < size; ++i)
     {
       CFile* file = files[i];
       ofs<<file->getId()<<endl ;
       StdSize dataSize=0;
       std::vector<CField*> enabledFields = file->getEnabledFields();
       size_t numEnabledFields = enabledFields.size();
       ofs<<numEnabledFields<<endl ;
       for (size_t j = 0; j < numEnabledFields; ++j)
       {
         dataSize += enabledFields[j]->getGlobalWrittenSize() ;
         ofs<<enabledFields[j]->getGrid()->getId()<<endl ;
         ofs<<enabledFields[j]->getGlobalWrittenSize()<<endl ;
       }
       double outFreqSec = (Time)(calendar->getCurrentDate()+file->output_freq)-(Time)(calendar->getCurrentDate()) ;
       double dataSizeSec= dataSize/ outFreqSec;
       ofs<<dataSizeSec<<endl ;
       nfield++ ;
// add epsilon*nField to dataSizeSec in order to  preserve reproductive ordering when sorting
       dataSizeMap.push_back(make_pair(dataSizeSec + dataSizeSec * eps * nfield , file));
       dataPerPool += dataSizeSec;
     }
     dataPerPool /= nbPools;
     std::sort(dataSizeMap.begin(), dataSizeMap.end());

     // (3) Assign contextClient to each enabled file

     std::multimap<double,int> poolDataSize ;
// multimap is not garanty to preserve stable sorting in c++98 but it seems it does for c++11

     int j;
     double dataSize ;
     for (j = 0 ; j < nbPools ; ++j) poolDataSize.insert(std::pair<double,int>(0.,j)) ;  
             
     for (int i = dataSizeMap.size()-1; i >= 0; --i)
     {
       dataSize=(*poolDataSize.begin()).first ;
       j=(*poolDataSize.begin()).second ;
       dataSizeMap[i].second->setContextClient(writerClientOut_[j]);
       dataSize+=dataSizeMap[i].first;
       poolDataSize.erase(poolDataSize.begin()) ;
       poolDataSize.insert(std::pair<double,int>(dataSize,j)) ; 
     }

     for (std::multimap<double,int>:: iterator it=poolDataSize.begin() ; it!=poolDataSize.end(); ++it) info(30)<<"Load Balancing for servers (perfect=1) : "<<it->second<<" :  ratio "<<it->first*1./dataPerPool<<endl ;
   }
   CATCH_DUMP_ATTR

   void CContext::distributeFileOverMemoryBandwith(const vector<CFile*>& filesList)
   TRY
   {
     int nbPools = writerClientOut_.size();
     double ratio=0.5 ;
     ratio=CXios::getin<double>("server2_dist_file_memory_ratio", ratio);

     int nFiles = filesList.size();
     vector<SDistFile> files(nFiles);
     vector<SDistGrid> grids;
     map<string,int> gridMap ;
     string gridId; 
     int gridIndex=0 ;

     for (size_t i = 0; i < nFiles; ++i)
     {
       StdSize dataSize=0;
       CFile* file = filesList[i];
       std::vector<CField*> enabledFields = file->getEnabledFields();
       size_t numEnabledFields = enabledFields.size();

       files[i].id_=file->getId() ;
       files[i].nbGrids_=numEnabledFields;
       files[i].assignedGrid_ = new int[files[i].nbGrids_] ;
         
       for (size_t j = 0; j < numEnabledFields; ++j)
       {
         gridId=enabledFields[j]->getGrid()->getId() ;
         if (gridMap.find(gridId)==gridMap.end())
         {
            gridMap[gridId]=gridIndex  ;
            SDistGrid newGrid; 
            grids.push_back(newGrid) ;
            gridIndex++ ;
         }
         files[i].assignedGrid_[j]=gridMap[gridId] ;
         grids[files[i].assignedGrid_[j]].size_=enabledFields[j]->getGlobalWrittenSize() ;
         dataSize += enabledFields[j]->getGlobalWrittenSize() ; // usefull
       }
       double outFreqSec = (Time)(calendar->getCurrentDate()+file->output_freq)-(Time)(calendar->getCurrentDate()) ;
       files[i].bandwith_= dataSize/ outFreqSec ;
     }

     double bandwith=0 ;
     double memory=0 ;
   
     for(int i=0; i<nFiles; i++)  bandwith+=files[i].bandwith_ ;
     for(int i=0; i<nFiles; i++)  files[i].bandwith_ = files[i].bandwith_/bandwith * ratio ;

     for(int i=0; i<grids.size(); i++)  memory+=grids[i].size_ ;
     for(int i=0; i<grids.size(); i++)  grids[i].size_ = grids[i].size_ / memory * (1.0-ratio) ;
       
     distributeFileOverServer2(nbPools, grids.size(), &grids[0], nFiles, &files[0]) ;

     vector<double> memorySize(nbPools,0.) ;
     vector< set<int> > serverGrids(nbPools) ;
     vector<double> bandwithSize(nbPools,0.) ;
       
     for (size_t i = 0; i < nFiles; ++i)
     {
       bandwithSize[files[i].assignedServer_] += files[i].bandwith_* bandwith /ratio ;
       for(int j=0 ; j<files[i].nbGrids_;j++)
       {
         if (serverGrids[files[i].assignedServer_].find(files[i].assignedGrid_[j]) == serverGrids[files[i].assignedServer_].end())
         {
           memorySize[files[i].assignedServer_]+= grids[files[i].assignedGrid_[j]].size_ * memory / (1.0-ratio);
           serverGrids[files[i].assignedServer_].insert(files[i].assignedGrid_[j]) ;
         }
       }
       filesList[i]->setContextClient(writerClientOut_[files[i].assignedServer_]) ;
       delete [] files[i].assignedGrid_ ;
     }

     for (int i = 0; i < nbPools; ++i) info(100)<<"Pool server level2 "<<i<<"   assigned file bandwith "<<bandwithSize[i]*86400.*4./1024/1024.<<" Mb / days"<<endl ;
     for (int i = 0; i < nbPools; ++i) info(100)<<"Pool server level2 "<<i<<"   assigned grid memory "<<memorySize[i]*100/1024./1024.<<" Mb"<<endl ;

   }
   CATCH_DUMP_ATTR

   /*!
      Find all files in write mode
   */
   void CContext::findEnabledWriteModeFiles(void)
   TRY
   {
     int size = this->enabledFiles.size();
     for (int i = 0; i < size; ++i)
     {
       if (enabledFiles[i]->mode.isEmpty() || 
          (!enabledFiles[i]->mode.isEmpty() && enabledFiles[i]->mode.getValue() == CFile::mode_attr::write ))
        enabledWriteModeFiles.push_back(enabledFiles[i]);
     }
   }
   CATCH_DUMP_ATTR

   /*!
      Find all files in read mode
   */
   void CContext::findEnabledReadModeFiles(void)
   TRY
   {
     int size = this->enabledFiles.size();
     for (int i = 0; i < size; ++i)
     {
       if (!enabledFiles[i]->mode.isEmpty() && enabledFiles[i]->mode.getValue() == CFile::mode_attr::read)
        enabledReadModeFiles.push_back(enabledFiles[i]);
     }
   }
   CATCH_DUMP_ATTR

   void CContext::closeAllFile(void)
   TRY
   {
     std::vector<CFile*>::const_iterator
            it = this->enabledFiles.begin(), end = this->enabledFiles.end();

     for (; it != end; it++)
     {
       info(30)<<"Closing File : "<<(*it)->getId()<<endl;
       (*it)->close();
     }
   }
   CATCH_DUMP_ATTR

   /*!
   \brief Dispatch event received from client
      Whenever a message is received in buffer of server, it will be processed depending on
   its event type. A new event type should be added in the switch list to make sure
   it processed on server side.
   \param [in] event: Received message
   */
   bool CContext::dispatchEvent(CEventServer& event)
   TRY
   {

      if (SuperClass::dispatchEvent(event)) return true;
      else
      {
        switch(event.type)
        {
           case EVENT_ID_CLOSE_DEFINITION :
             recvCloseDefinition(event);
             return true;
             break;
           case EVENT_ID_UPDATE_CALENDAR:
             recvUpdateCalendar(event);
             return true;
             break;
           case EVENT_ID_COUPLER_IN_READY:
             recvCouplerInReady(event);
             return true;
             break;
           case EVENT_ID_COUPLER_IN_CLOSE_DEFINITION:
             recvCouplerInCloseDefinition(event);
             return true;
             break;
           case EVENT_ID_COUPLER_IN_CONTEXT_FINALIZED:
             recvCouplerInContextFinalized(event);
             return true;
             break;  
           default :
             ERROR("bool CContext::dispatchEvent(CEventServer& event)",
                    <<"Unknown Event");
           return false;
         }
      }
   }
   CATCH

   //  ! Client side: Send a message to server to make it close
   void CContext::sendCloseDefinition(CContextClient* client)
   TRY
   {
      if (sendCloseDefinition_done_.count(client)!=0) return ;
      else sendCloseDefinition_done_.insert(client) ;

      CEventClient event(getType(),EVENT_ID_CLOSE_DEFINITION);
      if (client->isServerLeader())
      {
        CMessage msg;
        for(auto rank : client->getRanksServerLeader()) event.push(rank,1,msg);
        client->sendEvent(event);
      }
     else client->sendEvent(event);
   }
   CATCH_DUMP_ATTR

   //! Server side: Receive a message of client announcing a context close
   void CContext::recvCloseDefinition(CEventServer& event)
   TRY
   {
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      getCurrent()->closeDefinition();
   }
   CATCH

   //! Client side: Send a message to update calendar in each time step
   void CContext::sendUpdateCalendar(int step)
   TRY
   {
     for(auto client : slaveServers_) 
     {
       CEventClient event(getType(),EVENT_ID_UPDATE_CALENDAR);
       if (client->isServerLeader())
       {
         CMessage msg;
         msg<<step;
         for (auto& rank : client->getRanksServerLeader() ) event.push(rank,1,msg);
         client->sendEvent(event);
       }
       else client->sendEvent(event);
     }
   }
   CATCH_DUMP_ATTR

   //! Server side: Receive a message of client annoucing calendar update
   void CContext::recvUpdateCalendar(CEventServer& event)
   TRY
   {
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      getCurrent()->recvUpdateCalendar(*buffer);
   }
   CATCH

   //! Server side: Receive a message of client annoucing calendar update
   void CContext::recvUpdateCalendar(CBufferIn& buffer)
   TRY
   {
      int step;
      buffer>>step;
      updateCalendar(step);
      if (serviceType_==CServicesManager::GATHERER)
      {        
        sendUpdateCalendar(step);
      }
   }
   CATCH_DUMP_ATTR


   void CContext::createCouplerInterCommunicator(void)
   TRY
   {
      int rank=this->getIntraCommRank() ;
      map<string,list<CCouplerOut*>> listCouplerOut ;  
      map<string,list<CCouplerIn*>> listCouplerIn ;  

      for(auto couplerOut : enabledCouplerOut) listCouplerOut[couplerOut->getCouplingContextId()].push_back(couplerOut) ;
      for(auto couplerIn : enabledCouplerIn) listCouplerIn[couplerIn->getCouplingContextId()].push_back(couplerIn) ;

      CCouplerManager* couplerManager = CXios::getCouplerManager() ;
      if (rank==0)
      {
        for(auto couplerOut : listCouplerOut) couplerManager->registerCoupling(this->getContextId(),couplerOut.first) ;
        for(auto couplerIn : listCouplerIn) couplerManager->registerCoupling(couplerIn.first,this->getContextId()) ;
      }

      do
      {
        for(auto couplerOut : listCouplerOut) 
        {
          bool isNextCoupling ;
          if (rank==0) isNextCoupling = couplerManager->isNextCoupling(this->getContextId(),couplerOut.first) ;
          MPI_Bcast(&isNextCoupling,1,MPI_C_BOOL, 0, getIntraComm()) ; 
          if (isNextCoupling) 
          {
            addCouplingChanel(couplerOut.first, true) ;
            listCouplerOut.erase(couplerOut.first) ;
            break ;
          }            
        }
        for(auto couplerIn : listCouplerIn) 
        {
          bool isNextCoupling ;
          if (rank==0) isNextCoupling = couplerManager->isNextCoupling(couplerIn.first,this->getContextId());
          MPI_Bcast(&isNextCoupling,1,MPI_C_BOOL, 0, getIntraComm()) ; 
          if (isNextCoupling) 
          {
            addCouplingChanel(couplerIn.first, false) ;
            listCouplerIn.erase(couplerIn.first) ;
            break ;
          }           
        }

      } while (!listCouplerOut.empty() || !listCouplerIn.empty()) ;

   }
   CATCH_DUMP_ATTR

  
     //! Client side: Send infomation of active files (files are enabled to write out)
   void CContext::sendEnabledFiles(const std::vector<CFile*>& activeFiles)
   TRY
   {
     int size = activeFiles.size();

     // In a context, each type has a root definition, e.g: axis, domain, field.
     // Every object must be a child of one of these root definition. In this case
     // all new file objects created on server must be children of the root "file_definition"
     StdString fileDefRoot("file_definition");
     CFileGroup* cfgrpPtr = CFileGroup::get(fileDefRoot);

     for (int i = 0; i < size; ++i)
     {
       CFile* f = activeFiles[i];
       cfgrpPtr->sendCreateChild(f->getId(),f->getContextClient());
       f->sendAllAttributesToServer(f->getContextClient());
       f->sendAddAllVariables(f->getContextClient());
     }
   }
   CATCH_DUMP_ATTR

   //! Client side: Send information of active fields (ones are written onto files)
   void CContext::sendEnabledFieldsInFiles(const std::vector<CFile*>& activeFiles)
   TRY
   {
     int size = activeFiles.size();
     for (int i = 0; i < size; ++i)
     {
       activeFiles[i]->sendEnabledFields(activeFiles[i]->getContextClient());
     }
   }
   CATCH_DUMP_ATTR

  
   //! Client side: Prepare the timeseries by adding the necessary files
   void CContext::prepareTimeseries()
   TRY
   {
     const std::vector<CFile*> allFiles = CFile::getAll();
     for (size_t i = 0; i < allFiles.size(); i++)
     {
       CFile* file = allFiles[i];

       std::vector<CVariable*> fileVars, fieldVars, vars = file->getAllVariables();
       for (size_t k = 0; k < vars.size(); k++)
       {
         CVariable* var = vars[k];

         if (var->ts_target.isEmpty()
              || var->ts_target == CVariable::ts_target_attr::file || var->ts_target == CVariable::ts_target_attr::both)
           fileVars.push_back(var);

         if (!var->ts_target.isEmpty()
              && (var->ts_target == CVariable::ts_target_attr::field || var->ts_target == CVariable::ts_target_attr::both))
           fieldVars.push_back(var);
       }

       if (!file->timeseries.isEmpty() && file->timeseries != CFile::timeseries_attr::none)
       {
         StdString fileNameStr("%file_name%") ;
         StdString tsPrefix = !file->ts_prefix.isEmpty() ? file->ts_prefix : fileNameStr ;
         
         StdString fileName=file->getFileOutputName();
         size_t pos=tsPrefix.find(fileNameStr) ;
         while (pos!=std::string::npos)
         {
           tsPrefix=tsPrefix.replace(pos,fileNameStr.size(),fileName) ;
           pos=tsPrefix.find(fileNameStr) ;
         }
        
         const std::vector<CField*> allFields = file->getAllFields();
         for (size_t j = 0; j < allFields.size(); j++)
         {
           CField* field = allFields[j];

           if (!field->ts_enabled.isEmpty() && field->ts_enabled)
           {
             CFile* tsFile = CFile::create();
             tsFile->duplicateAttributes(file);

             // Add variables originating from file and targeted to timeserie file
             for (size_t k = 0; k < fileVars.size(); k++)
               tsFile->getVirtualVariableGroup()->addChild(fileVars[k]);

            
             tsFile->name = tsPrefix + "_";
             if (!field->name.isEmpty())
               tsFile->name.get() += field->name;
             else if (field->hasDirectFieldReference()) // We cannot use getBaseFieldReference() just yet
               tsFile->name.get() += field->field_ref;
             else
               tsFile->name.get() += field->getId();

             if (!field->ts_split_freq.isEmpty())
               tsFile->split_freq = field->ts_split_freq;

             CField* tsField = tsFile->addField();
             tsField->field_ref = field->getId();

             // Add variables originating from file and targeted to timeserie field
             for (size_t k = 0; k < fieldVars.size(); k++)
               tsField->getVirtualVariableGroup()->addChild(fieldVars[k]);

             vars = field->getAllVariables();
             for (size_t k = 0; k < vars.size(); k++)
             {
               CVariable* var = vars[k];

               // Add variables originating from field and targeted to timeserie field
               if (var->ts_target.isEmpty()
                    || var->ts_target == CVariable::ts_target_attr::field || var->ts_target == CVariable::ts_target_attr::both)
                 tsField->getVirtualVariableGroup()->addChild(var);

               // Add variables originating from field and targeted to timeserie file
               if (!var->ts_target.isEmpty()
                    && (var->ts_target == CVariable::ts_target_attr::file || var->ts_target == CVariable::ts_target_attr::both))
                 tsFile->getVirtualVariableGroup()->addChild(var);
             }

             tsFile->solveFieldRefInheritance(true);

             if (file->timeseries == CFile::timeseries_attr::exclusive)
               field->enabled = false;
           }
         }

         // Finally disable the original file is need be
         if (file->timeseries == CFile::timeseries_attr::only)
          file->enabled = false;
       }
     }
   }
   CATCH_DUMP_ATTR

  
   //! Client side: Send information of reference domain, axis and scalar of active fields
   void CContext::sendRefDomainsAxisScalars(const std::vector<CFile*>& activeFiles)
   TRY
   {
     std::set<pair<StdString,CContextClient*>> domainIds, axisIds, scalarIds;

     // Find all reference domain and axis of all active fields
     int numEnabledFiles = activeFiles.size();
     for (int i = 0; i < numEnabledFiles; ++i)
     {
       std::vector<CField*> enabledFields = activeFiles[i]->getEnabledFields();
       int numEnabledFields = enabledFields.size();
       for (int j = 0; j < numEnabledFields; ++j)
       {
         CContextClient* contextClient=enabledFields[j]->getContextClient() ;
         const std::vector<StdString>& prDomAxisScalarId = enabledFields[j]->getRefDomainAxisIds();
         if ("" != prDomAxisScalarId[0]) domainIds.insert(make_pair(prDomAxisScalarId[0],contextClient));
         if ("" != prDomAxisScalarId[1]) axisIds.insert(make_pair(prDomAxisScalarId[1],contextClient));
         if ("" != prDomAxisScalarId[2]) scalarIds.insert(make_pair(prDomAxisScalarId[2],contextClient));
       }
     }

     // Create all reference axis on server side
     std::set<StdString>::iterator itDom, itAxis, itScalar;
     std::set<StdString>::const_iterator itE;

     StdString scalarDefRoot("scalar_definition");
     CScalarGroup* scalarPtr = CScalarGroup::get(scalarDefRoot);
     
     for (auto itScalar = scalarIds.begin(); itScalar != scalarIds.end(); ++itScalar)
     {
       if (!itScalar->first.empty())
       {
         scalarPtr->sendCreateChild(itScalar->first,itScalar->second);
         CScalar::get(itScalar->first)->sendAllAttributesToServer(itScalar->second);
       }
     }

     StdString axiDefRoot("axis_definition");
     CAxisGroup* axisPtr = CAxisGroup::get(axiDefRoot);
     
     for (auto itAxis = axisIds.begin(); itAxis != axisIds.end(); ++itAxis)
     {
       if (!itAxis->first.empty())
       {
         axisPtr->sendCreateChild(itAxis->first, itAxis->second);
         CAxis::get(itAxis->first)->sendAllAttributesToServer(itAxis->second);
       }
     }

     // Create all reference domains on server side
     StdString domDefRoot("domain_definition");
     CDomainGroup* domPtr = CDomainGroup::get(domDefRoot);
     
     for (auto itDom = domainIds.begin(); itDom != domainIds.end(); ++itDom)
     {
       if (!itDom->first.empty()) {
          domPtr->sendCreateChild(itDom->first, itDom->second);
          CDomain::get(itDom->first)->sendAllAttributesToServer(itDom->second);
       }
     }
   }
   CATCH_DUMP_ATTR

   void CContext::triggerLateFields(void)
   TRY
   {
    for(auto& field : fileInFields_) field->triggerLateField() ;
    for(auto& field : couplerInFields_) field->triggerLateField() ;
   }
   CATCH_DUMP_ATTR

   //! Update calendar in each time step
   void CContext::updateCalendar(int step)
   TRY
   {
      int prevStep = calendar->getStep();

      if (prevStep < step)
      {
        CMemChecker::logMem( "CContext::updateCalendar_"+std::to_string(step) );
        if (serviceType_==CServicesManager::CLIENT) // For now we only use server level 1 to read data
        {
          triggerLateFields();
        }

        info(50) << "updateCalendar : before : " << calendar->getCurrentDate() << endl;
        calendar->update(step);
        info(50) << "updateCalendar : after : " << calendar->getCurrentDate() << endl;
  #ifdef XIOS_MEMTRACK_LIGHT
        info(50) << " Current memory used by XIOS : "<<  MemTrack::getCurrentMemorySize()*1.0/(1024*1024)<<" Mbyte, at timestep "<<step<<" of context "<<this->getId()<<endl ;
  #endif

        if (serviceType_==CServicesManager::CLIENT) // For now we only use server level 1 to read data
        {
          doPostTimestepOperationsForEnabledReadModeFiles();
          garbageCollector.invalidate(calendar->getCurrentDate());
        }
      }
      else if (prevStep == step)
        info(50) << "updateCalendar: already at step " << step << ", no operation done." << endl;
      else // if (prevStep > step)
        ERROR("void CContext::updateCalendar(int step)",
              << "Illegal calendar update: previous step was " << prevStep << ", new step " << step << "is in the past!")
   }
   CATCH_DUMP_ATTR

   void CContext::initReadFiles(void)
   TRY
   {
      vector<CFile*>::const_iterator it;

      for (it=enabledReadModeFiles.begin(); it != enabledReadModeFiles.end(); it++)
      {
         (*it)->initRead();
      }
   }
   CATCH_DUMP_ATTR

   //! Server side: Create header of netcdf file
   void CContext::createFileHeader(void)
   TRY
   {
      vector<CFile*>::const_iterator it;

      //for (it=enabledFiles.begin(); it != enabledFiles.end(); it++)
      for (it=enabledWriteModeFiles.begin(); it != enabledWriteModeFiles.end(); it++)
      {
         (*it)->initWrite();
      }
   }
   CATCH_DUMP_ATTR

   //! Get current context
   CContext* CContext::getCurrent(void)
   TRY
   {
     return CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId()).get();
   }
   CATCH

   /*!
   \brief Set context with an id be the current context
   \param [in] id identity of context to be set to current
   */
   void CContext::setCurrent(const string& id)
   TRY
   {
     CObjectFactory::SetCurrentContextId(id);
     CGroupFactory::SetCurrentContextId(id);
   }
   CATCH

  /*!
  \brief Create a context with specific id
  \param [in] id identity of new context
  \return pointer to the new context or already-existed one with identity id
  */
  CContext* CContext::create(const StdString& id)
  TRY
  {
    CContext::setCurrent(id);

    bool hasctxt = CContext::has(id);
    CContext* context = CObjectFactory::CreateObject<CContext>(id).get();
    getRoot();
    if (!hasctxt) CGroupFactory::AddChild(root, context->getShared());

#define DECLARE_NODE(Name_, name_) \
    C##Name_##Definition::create(C##Name_##Definition::GetDefName());
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"

    return (context);
  }
  CATCH

  
  void CContext::sendFinalizeClient(CContextClient* contextClient, const string& contextClientId)
  TRY
  {
    CEventClient event(getType(),EVENT_ID_CONTEXT_FINALIZE_CLIENT);
    if (contextClient->isServerLeader())
    {
      CMessage msg;
      msg<<contextClientId ;
      const std::list<int>& ranks = contextClient->getRanksServerLeader();
      for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
           event.push(*itRank,1,msg);
      contextClient->sendEvent(event);
    }
    else contextClient->sendEvent(event);
  }
  CATCH_DUMP_ATTR

 
  void CContext::recvFinalizeClient(CEventServer& event)
  TRY
  {
    CBufferIn* buffer=event.subEvents.begin()->buffer;
    string id;
    *buffer>>id;
    get(id)->recvFinalizeClient(*buffer);
  }
  CATCH

  void CContext::recvFinalizeClient(CBufferIn& buffer)
  TRY
  {
    countChildContextFinalized_++ ;
  }
  CATCH_DUMP_ATTR




 //! Client side: Send a message  announcing that context can receive grid definition from coupling
   void CContext::sendCouplerInReady(CContextClient* client)
   TRY
   {
      if (sendCouplerInReady_done_.count(client)!=0) return ;
      else sendCouplerInReady_done_.insert(client) ;

      CEventClient event(getType(),EVENT_ID_COUPLER_IN_READY);

      if (client->isServerLeader())
      {
        CMessage msg;
        msg<<this->getId();
        for (auto& rank : client->getRanksServerLeader()) event.push(rank,1,msg);
        client->sendEvent(event);
      }
      else client->sendEvent(event);
   }
   CATCH_DUMP_ATTR

   //! Server side: Receive a message announcing that context can send grid definition for context coupling
   void CContext::recvCouplerInReady(CEventServer& event)
   TRY
   {
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      getCurrent()->recvCouplerInReady(*buffer);
   }
   CATCH

   //! Server side: Receive a message announcing that context can send grid definition for context coupling
   void CContext::recvCouplerInReady(CBufferIn& buffer)
   TRY
   {
      string contextId ;
      buffer>>contextId;
      couplerInReady_.insert(getCouplerOutClient(contextId)) ;
   }
   CATCH_DUMP_ATTR





 //! Client side: Send a message  announcing that a coupling context have done it closeDefinition, so data can be sent now.
   void CContext::sendCouplerInCloseDefinition(CContextClient* client)
   TRY
   {
      if (sendCouplerInCloseDefinition_done_.count(client)!=0) return ;
      else sendCouplerInCloseDefinition_done_.insert(client) ;

      CEventClient event(getType(),EVENT_ID_COUPLER_IN_CLOSE_DEFINITION);

      if (client->isServerLeader())
      {
        CMessage msg;
        msg<<this->getId();
        for (auto& rank : client->getRanksServerLeader()) event.push(rank,1,msg);
        client->sendEvent(event);
      }
      else client->sendEvent(event);
   }
   CATCH_DUMP_ATTR

   //! Server side: Receive a message announcing that a coupling context have done it closeDefinition, so data can be sent now.
   void CContext::recvCouplerInCloseDefinition(CEventServer& event)
   TRY
   {
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      getCurrent()->recvCouplerInCloseDefinition(*buffer);
   }
   CATCH

   //! Server side: Receive a message announcing that a coupling context have done it closeDefinition, so data can be sent now.
   void CContext::recvCouplerInCloseDefinition(CBufferIn& buffer)
   TRY
   {
      string contextId ;
      buffer>>contextId;
      couplerInCloseDefinition_.insert(getCouplerOutClient(contextId)) ;
   }
   CATCH_DUMP_ATTR




//! Client side: Send a message  announcing that a coupling context have done it contextFinalize, so it can also close it own context.
   void CContext::sendCouplerInContextFinalized(CContextClient* client)
   TRY
   {
      if (sendCouplerInContextFinalized_done_.count(client)!=0) return ;
      else sendCouplerInContextFinalized_done_.insert(client) ;

      CEventClient event(getType(),EVENT_ID_COUPLER_IN_CONTEXT_FINALIZED);

      if (client->isServerLeader())
      {
        CMessage msg;
        msg<<this->getId();
        for (auto& rank : client->getRanksServerLeader()) event.push(rank,1,msg);
        client->sendEvent(event);
      }
      else client->sendEvent(event);
   }
   CATCH_DUMP_ATTR

   //! Server side: Receive a message announcing that a coupling context have done it contextFinalize, so it can also close it own context.
   void CContext::recvCouplerInContextFinalized(CEventServer& event)
   TRY
   {
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      getCurrent()->recvCouplerInContextFinalized(*buffer);
   }
   CATCH

   //! Server side: Receive a message announcing that a coupling context have done it contextFinalize, so it can also close it own context.
   void CContext::recvCouplerInContextFinalized(CBufferIn& buffer)
   TRY
   {
      string contextId ;
      buffer>>contextId;
      couplerInContextFinalized_.insert(getCouplerOutClient(contextId)) ;
   }
   CATCH_DUMP_ATTR




  /*!
  * \fn bool CContext::isFinalized(void)
  * Context is finalized if it received context post finalize event.
  */
  bool CContext::isFinalized(void)
  TRY
  {
    return finalized;
  }
  CATCH_DUMP_ATTR
  ///--------------------------------------------------------------
  StdString CContext::dumpClassAttributes(void)
  {
    StdString str;
    str.append("enabled files=\"");
    int size = this->enabledFiles.size();
    for (int i = 0; i < size; ++i)
    {
      str.append(enabledFiles[i]->getId());
      str.append(" ");
    }
    str.append("\"");
    return str;
  }

} // namespace xios
