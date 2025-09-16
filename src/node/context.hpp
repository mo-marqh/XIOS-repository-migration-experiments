#ifndef __XIOS_CContext__
#define __XIOS_CContext__

/// XIOS headers ///
#include "xios_spl.hpp"
//#include "node_type.hpp"
#include "calendar_wrapper.hpp"

#include "declare_group.hpp"
#include "data_output.hpp"
#include "garbage_collector.hpp"
#include "registry.hpp"
#include "mpi.hpp"
#include "services_manager.hpp"
#include "server_context.hpp"
#include "event_scheduler.hpp"
#include "file.hpp"

namespace xios
{
   class CContextClient;
   class CContextServer;


   /// ////////////////////// Déclarations ////////////////////// ///
   class CContextGroup;
   class CContextAttributes;
   class CContext;
   class CFile;
   class CCouplerIn ;
   class CCouplerOut ;
   ///--------------------------------------------------------------

   // Declare/Define CFileAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CContext)
#  include "context_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CContext)

   ///--------------------------------------------------------------
  /*!
  \class CContext
   This class corresponds to the concrete presentation of context in xml file and in play an essential role in XIOS
   Each object of this class contains all root definition of elements: files, fiels, domains, axis, etc, ... from which
   we can have access to each element.
   In fact, every thing must a be inside a particuliar context. After the xml file (iodef.xml) is parsed,
   object of the class is created and its contains all information of other elements in the xml file.
  */
   class CContext
      : public CObjectTemplate<CContext>
      , public CContextAttributes
   {
         public :
         enum EEventId
         {
           EVENT_ID_COLLECTIVE=100,
           EVENT_ID_CLOSE_DEFINITION,EVENT_ID_UPDATE_CALENDAR,
           EVENT_ID_CONTEXT_FINALIZE,
           EVENT_ID_CONTEXT_FINALIZE_CLIENT,
           EVENT_ID_COUPLER_IN_READY,
           EVENT_ID_COUPLER_IN_CLOSE_DEFINITION,
           EVENT_ID_COUPLER_IN_CONTEXT_FINALIZED,
           EVENT_ID_NO_COLLECTIVE=1000,
         };

         /// typedef ///
         typedef CObjectTemplate<CContext>   SuperClass;
         typedef CContextAttributes SuperClassAttribute;

      public :

         typedef CContextAttributes RelAttributes;
         typedef CContext           RelGroup;

         //---------------------------------------------------------

      public :

         /// Constructeurs ///
         CContext(void);
         explicit CContext(const StdString & id);
         CContext(CContext* context) : CContext() {}
         explicit CContext(CContext* context, const StdString & id) : CContext(id) {}
         CContext(const CContext & context);       // Not implemented yet.
         CContext(const CContext * const context); // Not implemented yet.

         /// Destructeur ///
         virtual ~CContext(void);

         static void releaseStaticAllocation(void) ;

         //---------------------------------------------------------

      public :

         /// Mutateurs ///
         void setCalendar(std::shared_ptr<CCalendar> newCalendar);

         /// Accesseurs ///
         std::shared_ptr<CCalendar>      getCalendar(void) const;

      public :
         // Initialize server or client
         void init(CServerContext* parentServerContext, MPI_Comm intraComm, int serviceType);
         void initClient(MPI_Comm intraComm, int serviceType);
         
         void initServer(MPI_Comm intraComm, int serviceType );
         
         bool isInitialized(void);

         StdString dumpClassAttributes(void);

         // Put sever or client into loop state
         bool eventLoop(bool enableEventsProcessing=true);
         bool scheduledEventLoop(bool enableEventsProcessing=true) ; 
         void globalEventLoop(void);
         void yield(void) ;
         void synchronize(void) ;

         // Finalize a context
         void finalize(void);

         bool isFinalized(void);
         void closeDefinition(void);

         // to be removed     
         std::vector<CField*> findAllEnabledFieldsInFiles(const std::vector<CFile*>& activeFiles);
         // Some functions to process context
         std::vector<CField*> findAllEnabledFieldsInFileOut(const std::vector<CFile*>& activeFiles);
         std::vector<CField*> findAllEnabledFieldsInFileIn(const std::vector<CFile*>& activeFiles);
         std::vector<CField*> findAllEnabledFieldsCouplerOut(const std::vector<CCouplerOut*>& activeCouplerOut);
         std::vector<CField*> findAllEnabledFieldsCouplerIn(const std::vector<CCouplerIn*>& activeCouplerIn);
         // void findAllEnabledFields(void);
         // void findAllEnabledFieldsInReadModeFiles(void);
         void readAttributesOfEnabledFieldsInReadModeFiles();
         void solveAllInheritance(bool apply=true);
         void findEnabledFiles(void);
         void findEnabledCouplerIn(void);
         void findEnabledCouplerOut(void);
         void createCouplerInterCommunicator(void) ;
         void findEnabledWriteModeFiles(void);
         void findEnabledReadModeFiles(void);
         void closeAllFile(void);
         void updateCalendar(int step);
         void createFileHeader(void);
         void initReadFiles(void);
         void prepareTimeseries(void);
         void startPrefetchingOfEnabledReadModeFiles();
         void doPostTimestepOperationsForEnabledReadModeFiles();
         void findFieldsWithReadAccess(void);
         void triggerLateFields(void) ;
          
         std::map<int, StdSize> getAttributesBufferSize(std::map<int, StdSize>& maxEventSize, CContextClient* contextClient, bool bufferForWriting = false);
         std::map<int, StdSize> getDataBufferSize(std::map<int, StdSize>& maxEventSize, CContextClient* contextClient, bool bufferForWriting = false);

         // Distribute files (in write mode) among secondary-server pools according to the estimated data flux
         void distributeFiles(const vector<CFile*>& files) ;
         void distributeFilesOnSameService(const vector<CFile*>& files, const string& poolId, const string& serviceId) ;
         void distributeFileOverOne(const vector<CFile*>& files, const string& poolId, const string& serviceId) ; //!< Distribute files over one single server (no distribution)
         void distributeFileOverBandwith(const std::vector<CFile*>& files, const string& poolId, const string& serviceId) ; //!< Distribute files overs servers to balance the I/O bandwith
         void distributeFileOverMemoryBandwith(const std::vector<CFile*>& files, const string& poolId, const string& serviceId) ; //!< Distribute files overs servers to minimize the memory consumption
         
       public:
         // Send context close definition
         void sendCloseDefinition(CContextClient* client) ;
       private:
         set<CContextClient*> sendCloseDefinition_done_ ;
       public:
         // There are something to send on closing context defintion
         void sendUpdateCalendar(int step);
         void sendEnabledFiles(const std::vector<CFile*>& activeFiles);
         void sendEnabledFieldsInFiles(const std::vector<CFile*>& activeFiles);
         void sendRefDomainsAxisScalars(const std::vector<CFile*>& activeFiles);
         //!< after be gathered to the root process of the context, merged registry is sent to the root process of the servers
         void sendFinalizeClient(CContextClient* contextClient, const string& contextClientId);
         
         public:
         void sendContextToFileServer(CContextClient* client) ;
         private:
         std::set<CContextClient*> sendToFileServer_done_ ;
         
         public: 
         std::string getContextId() {return contextId_;}

         // Client side: Receive and process messages
         static void recvUpdateCalendar(CContext* context, CEventServer& event);
         void recvUpdateCalendar(CBufferIn& buffer);
         static void recvCloseDefinition(CContext* context, CEventServer& event);
         static void recvSolveInheritanceContext(CContext* context, CEventServer& event);
         void recvSolveInheritanceContext(CBufferIn& buffer);
         static void recvFinalizeClient(CContext* context, CEventServer& event) ;
         void recvFinalizeClient(CBufferIn& buffer);
        
       public:
         void sendCouplerInReady(CContextClient* client);
       private:
         set<CContextClient*> sendCouplerInReady_done_;
       public:
         static void recvCouplerInReady(CContext* context, CEventServer& event) ;
         void recvCouplerInReady(CBufferIn& buffer) ; //!< coupler is ready to receive grid definition.
         set<CContextClient*> couplerInReady_;
         bool isCouplerInReady(CContextClient* client) { return couplerInReady_.count(client)!=0 ;}

       public:
        void sendCouplerInCloseDefinition(CContextClient* client) ;
        set<CContextClient*> sendCouplerInCloseDefinition_done_;
        static void recvCouplerInCloseDefinition(CContext* context, CEventServer& event) ;
        void recvCouplerInCloseDefinition(CBufferIn& buffer) ; //!< coupler has finished it defintion, data can be sent     
        set<CContextClient*> couplerInCloseDefinition_ ;
        bool isCouplerInCloseDefinition(CContextClient* client) { return couplerInCloseDefinition_.count(client)!=0 ;}

       public:
        void sendCouplerInContextFinalized(CContextClient* client) ;
        set<CContextClient*> sendCouplerInContextFinalized_done_;
        static void recvCouplerInContextFinalized(CContext* context, CEventServer& event) ;
        void recvCouplerInContextFinalized(CBufferIn& buffer) ; //!< coupler has finished it defintion, data can be sent     
        set<CContextClient*> couplerInContextFinalized_ ;
        bool isCouplerInContextFinalized(CContextClient* client) { return couplerInContextFinalized_.count(client)!=0 ;}

       public:  
        void freeComms(void);                  //!< Free internally allcoated communicators

         // dispatch event
         static bool dispatchEvent(CContext* context, CEventServer& event);

      public:
        // Get current context
        static CContext* getCurrent(void);

        // Get context root
        static CContextGroup* getRoot(void);
       
        // Set current context
        static void setCurrent(const string& id);

        // Create new context
        static CContext* create(const string& id = "");

        /// Accesseurs statiques ///
        static StdString GetName(void);
        static StdString GetDefName(void);
        static ENodeType GetType(void);

        static CContextGroup* GetContextGroup(void);

        // Some functions to visualize structure of current context
        static void ShowTree(CContext* context, StdOStream & out = std::clog);
        void CleanTree(void);
        static void removeContext(const std::string& contextId);
        static void removeAllContexts(void) ;
        int getServiceType(void) {return serviceType_;}

      public :
         // Parse xml node and write all info into context
         virtual void parse(xml::CXMLNode & node);

         // Visualize a context
         virtual StdString toString(void) const;


         // Solve all inheritance relation in current context
         virtual void solveDescInheritance(bool apply, const CAttributeMap * const parent = 0);

         // Verify if all root definition in a context have children
         virtual bool hasChild(void) const;

         bool isProcessingEvent(void) {return isProcessingEvent_;}
         void setProcessingEvent(void) {isProcessingEvent_=true ;}
         void unsetProcessingEvent(void) {isProcessingEvent_=false ;}
         
         void addCouplingChanel(const std::string& contextId, bool out) ;

      public :
         // Calendar of context
         std::shared_ptr<CCalendar>   calendar;

         // List of all enabled files (files on which fields are written or read)
         std::vector<CFile*> enabledFiles;
         // List of all enabled files in read mode (files on which fields are read)
         std::vector<CFile*> enabledReadModeFiles;
         // List of all enabled files in write mode
         std::vector<CFile*> enabledWriteModeFiles;

         std::vector<CCouplerIn*> enabledCouplerIn;
         std::vector<CCouplerOut*> enabledCouplerOut;


         // List of all enabled fields whose instant data is accessible from the public API
         // but which are not part of a file
         std::vector<CField*> fieldsWithReadAccess_;
         std::vector<CField*> couplerInFields_;
         std::vector<CField*> fileInFields_;


         // Context root
         static std::shared_ptr<CContextGroup> root;

         // Determine context on client or not
         bool hasClient;

         // Determine context on server or not
         bool hasServer;
      public:
        void registerFileToWrite(CFile* file) { filesToWrite_.insert(file); } // Add a file that need to be write for example to create headers
      private:  
        std::set<CFile*,FilePtrCompare> filesToWrite_ ;  

      private:
        CContextClient* onlineContextClient_=nullptr ;
        
        std::string defaultPoolWriterId_ ;
        std::string defaultPoolReaderId_ ;
        std::string defaultPoolGathererId_ ;
        std::string defaultWriterId_ ;
        std::string defaultReaderId_ ;
        std::string defaultGathererId_ ;
        bool defaultUsingServer2_ ;
        void setDefaultServices(void) ;


        std::map<std::pair<string,string>,std::vector<pair<CContextClient*,CContextServer*>>> serversMap_ ;

        std::vector<CContextClient*> writerClientOut_ ;
        std::vector<CContextServer*> writerServerOut_ ;
        std::vector<CContextClient*> writerClientIn_ ;
        std::vector<CContextServer*> writerServerIn_ ;

        std::vector<CContextClient*> readerClientOut_ ;
        std::vector<CContextServer*> readerServerOut_ ;
        std::vector<CContextClient*> readerClientIn_ ;
        std::vector<CContextServer*> readerServerIn_ ;


        std::map<std::string, CContextClient*> clients_ ;
        std::map<std::string, CContextClient*> servers_ ;
        std::map<CContextClient*, std::string> clientsId_ ;
        std::map<CContextServer*, std::string> serversId_ ;

        // list of slave servers (IO server or others)
        std::vector<CContextClient*> slaveServers_ ;

        // the map containing context client associated to it string id for coupling out ;
        std::map<std::string, CContextClient*> couplerOutClient_ ;
        // the map containing context server associated to it string id for coupling out ;
        std::map<std::string, CContextServer*> couplerOutServer_ ;
        // the map containing context client associated to it string id for coupling in ;
        std::map<std::string, CContextClient*> couplerInClient_ ;
        // the map containing context server associated to it string id for coupling in ;
        std::map<std::string, CContextServer*> couplerInServer_ ;
      public:
         void createClientInterComm(MPI_Comm interCommClient, MPI_Comm interCommServer)  ;
         void createServerInterComm(void)  ; // obsolete
         void createServerInterComm_old(void)  ;
         void createServerInterComm(const string& poolId, const string& serverId, vector<pair<string, pair<CContextClient*,CContextServer*>>>& clientServers ) ;
         void getServerInterComm(const string& poolId, const string& serviceId,  vector<pair<CContextClient*,CContextServer*>>& clientServers) ;
         vector<CContextClient*> getContextClient(const string& poolId, const string& serviceId) ;
         CContextClient* getCouplerInClient(const string& contextId) { return couplerInClient_[contextId] ;}
         CContextServer* getCouplerInServer(const string& contextId) { return couplerInServer_[contextId] ;}
         CContextClient* getCouplerOutClient(const string& contextId) { return couplerOutClient_[contextId] ;}
         CContextServer* getCouplerOutServer(const string& contextId) { return couplerOutServer_[contextId] ;}
       
       public: // must be privatize using accessors
        
        CRegistry* registryIn=nullptr ;    //!< input registry which is read from file
        CRegistry* registryOut=nullptr ;   //!< output registry which will be written into file at the finalize


        MPI_Comm intraComm_ ; //! context intra communicator
        int intraCommRank_ ; //! context intra communicator rank
        int intraCommSize_ ; //! context intra communicator size
       public: 
        MPI_Comm getIntraComm(void) {return intraComm_ ;}
        int getIntraCommRank(void) {return intraCommRank_;}
        int getIntraCommSize(void) {return intraCommSize_;}
      
      public:
        shared_ptr<CEventScheduler> getEventScheduler(void) {return eventScheduler_ ;}
      private:
         shared_ptr<CEventScheduler> eventScheduler_ ; //! The local event scheduler for context
         size_t hashId_ ; //! the local hashId for scheduler
         size_t timeLine_=0 ;
         void initEventScheduler(void) ;

         bool isPostProcessed;
         bool allProcessed;
         bool finalized;
         int countChildContextFinalized_;        //!< Counter of child contexts (for now it is the number of secondary server pools)
         CGarbageCollector garbageCollector;
         std::list<MPI_Comm> comms; //!< Communicators allocated internally

         int serviceType_;  //!< service associated to the context
         string contextId_ ; //!< context client id for the servers. For clients this is same as getId() 
         bool isProcessingEvent_ ;
    private:     
         CServerContext* parentServerContext_ ;
    public:
         CServerContext* getParentServerContext(void) { return parentServerContext_; }
    private: 
      bool lockedContext_=false;
    public: 
        void lockContext(void) {lockedContext_=true; }
        void unlockContext(void) {lockedContext_=false; }
        bool isLockedContext(void) { return lockedContext_;}
      public: // Some function maybe removed in the near future
        // virtual void toBinary  (StdOStream & os) const;
        // virtual void fromBinary(StdIStream & is);
   }; // class CContext

   ///--------------------------------------------------------------

   // Declare/Define CContextGroup and CContextDefinition
   DECLARE_GROUP_CONTEXT(CContext);

   template <>
   void CGroupTemplate<CContext, CContextGroup, CContextAttributes>::parse(xml::CXMLNode & node, bool withAttr, const std::set<StdString>& parseContextList) ;

   ///--------------------------------------------------------------

} // namespace xios

#endif // __XIOS_CContext__
