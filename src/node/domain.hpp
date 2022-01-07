#ifndef __XIOS_CDomain__
#define __XIOS_CDomain__

/// XIOS headers ///
#include "xios_spl.hpp"
#include "group_factory.hpp"

#include "declare_group.hpp"
#include "declare_ref_func.hpp"
#include "event_client.hpp"
#include "event_server.hpp"
#include "buffer_in.hpp"
#include "array_new.hpp"
#include "attribute_array.hpp"
#include "attribute_enum.hpp"
#include "transformation.hpp"
#include "transformation_enum.hpp"
#include "transformation_path.hpp"
#include "server_distribution_description.hpp"
#include "mesh.hpp"
#include "element.hpp"
#include "local_connector.hpp"
#include "scatterer_connector.hpp"
#include "gatherer_connector.hpp"
#include "distribution_type.hpp"
#include "generic_algorithm_transformation.hpp"


namespace xios
{

   /// ////////////////////// Déclarations ////////////////////// ///

   class CDomainGroup;
   class CDomainAttributes;
   class CDomain;
   class CFile;
   class CField ;

   ///--------------------------------------------------------------

   // Declare/Define CDomainAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CDomain)
#  include "domain_attribute.conf"
#  include "domain_attribute_private.conf"
   END_DECLARE_ATTRIBUTE_MAP(CDomain)

   ///--------------------------------------------------------------

   class CDomain
      : public CObjectTemplate<CDomain>
      , public CDomainAttributes
   {
     /// typedef ///
     typedef CObjectTemplate<CDomain>   SuperClass;
     typedef CDomainAttributes SuperClassAttribute;
     public:
         enum EEventId
         {
           EVENT_ID_DOMAIN_DISTRIBUTION, EVENT_ID_SEND_DISTRIBUTED_ATTRIBUTE
         } ;

      public:

         typedef CDomainAttributes RelAttributes;
         typedef CDomainGroup      RelGroup;
         
         /// Constructeurs ///
         CDomain(void);
         explicit CDomain(const StdString & id);
         CDomain(const CDomain & domain);       // Not implemented yet.
         CDomain(const CDomain * const domain); // Not implemented yet.
         static void releaseStaticAllocation(void) ; // release static allocation on heap

         static CDomain* createDomain();
         static CDomain* get(const string& id, bool noError=false) ; //<! return domain pointer using id
         static bool has(const string& id) ; //<! return domain pointer using id
         static CField*  getFieldFromId(const string& id) ;

         CMesh* mesh;
         void assignMesh(const StdString, const int);
        
         virtual void parse(xml::CXMLNode & node);

         void setContextClient(CContextClient* contextClient);

         /// Vérifications ///
         void checkAttributes(void);
         bool checkAttributes_done_ = false ;

         
      //////////////////////////
      ///// transformations ////
      //////////////////////////
      public:
        typedef CTransformation<CDomain>::TransformationMapTypes TransMapTypes;
      private:
        static bool initializeTransformationMap(std::map<StdString, ETranformationType>& m);
        static std::map<StdString, ETranformationType> transformationMapList_;
        static bool dummyTransformationMapList_;
        TransMapTypes transformationMap_;         

      public:
        CTransformation<CDomain>* addTransformation(ETranformationType transType, const StdString& id="");
        CTransformation<CDomain>* addTransformation(ETranformationType transType, CTransformation<CDomain>* transformation) ;
        void setTransformations(const TransMapTypes&);         
        void duplicateTransformation(CDomain*);
        TransMapTypes getAllTransformations();
        bool hasTransformation();
        void solveInheritanceTransformation_old(); // to remove later
        void solveInheritanceTransformation();
      private:
        bool solveInheritanceTransformation_done_= false ;
      public:
        bool activateFieldWorkflow(CGarbageCollector& gc) ;
      private:
        bool activateFieldWorkflow_done_=false ;

      private:
        shared_ptr<CGenericAlgorithmTransformation> transformationAlgorithm_ = nullptr ;
      public:
        void setTransformationAlgorithm(shared_ptr<CGenericAlgorithmTransformation> transformationAlgorithm) { transformationAlgorithm_=transformationAlgorithm ;}
        shared_ptr<CGenericAlgorithmTransformation> getTransformationAlgorithm(void) { return transformationAlgorithm_ ;}   
      private:
        CTransformationPaths transformationPaths_ ;
      public:
        CTransformationPaths getTransformationPaths(void) {return transformationPaths_;} 
        void setTransformationPaths(const CTransformationPaths& transformationPaths) { transformationPaths_=transformationPaths ;}

      ////////////////////////////

         void redistribute(int nbLocalDomain);

      public:
         const std::set<StdString> & getRelFiles(void) const;
         bool IsWritten(const StdString & filename) const;
         bool isWrittenCompressed(const StdString& filename) const;
         
         std::map<int, StdSize> getAttributesBufferSize(CContextClient* client, bool bufferForWriting = false);

         bool isEmpty(void) const;
         bool isDistributed(void) const;

        public :
         /*!
            \brief return if the domain can be written or not in a compressed way.
            ie if there are some masked or indexed point on the domain. Valid only on server side.
            \return true if domain can be writtedn in a compressed way
         */ 
         bool isCompressible(void) { if (!isCompressibleComputed_) computeIsCompressible() ; return isCompressible_ ;} 
        private :
         bool isCompressible_ ; /** specify if the domain can be written in a compressed way */ 
         bool isCompressibleComputed_=false ; /** Indicate if compressability has been computed*/
         void computeIsCompressible() ;

      public :
         bool isEqual(CDomain* domain);

         static bool dispatchEvent(CEventServer& event);
      
      private:
         /** define if the domain is completed or not ie all attributes have been received before in case 
             of grid reading from file or coupling */ 
         bool isCompleted_=true ;  
      public:     
        /*!
           \brief Check if a domain is completed
           Before make any domain processing, we must be sure that all domain informations have
           been sent, for exemple when reading a grid in a file or when grid elements are sent by an
           other context (coupling). So all direct reference of the domain (domain_ref) must be also completed
           \return true if domain and domain reference are completed
          */
         bool isCompleted(void)
         {
           if (hasDirectDomainReference()) if (!getDirectDomainReference()->isCompleted()) return false;
           else return isCompleted_ ;

           MISSING_RETURN( "bool CDomain::isCompleted() " );
           return true;
         }
         void setCompleted(void) { isCompleted_=true ; }
         void unsetCompleted(void) { isCompleted_=false ; }

      public:
         /// Mutateur ///
         void addRelFile(const StdString & filename);
         void addRelFileCompressed(const StdString& filename);            
         
         void AllgatherRectilinearLonLat(CArray<double,1>& lon, CArray<double,1>& lat,
                                         CArray<double,1>& lon_g, CArray<double,1>& lat_g);

         void fillInRectilinearBoundLonLat(CArray<double,1>& lon, CArray<double,1>& lat,
                                           CArray<double,2>& boundsLon, CArray<double,2>& boundsLat);
         
         void fillInLonLat();
         bool distributionAttributesHaveValue() const;

         size_t getGlobalWrittenSize() ;
         /// Destructeur ///
         virtual ~CDomain(void);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);

         static ENodeType GetType(void);        

      public:
         CArray<double, 1> lonvalue, latvalue;
         CArray<double, 2> bounds_lonvalue, bounds_latvalue;
         CArray<double, 1> areavalue;


         CArray<bool, 1> domainMask; // mask_1d, mask_2d -> domainMask
         CArray<bool, 1> localMask; // domainMask + indexing
         bool isCurvilinear ;
         bool hasBounds ;
         bool hasArea;
         bool hasLonLat;
         bool hasPole ;
         bool hasLatInReadFile_ ; // specify if latitude is defined on read file, so it can be read later when grid distribution will be defined
         bool hasBoundsLatInReadFile_ ; // specify if latitude boundarues are defined on read file, so it can be read later when grid distribution will be defined
         bool hasLonInReadFile_ ; // specify if longitude is defined on read file, so it can be read later when grid distribution will be defined
         bool hasBoundsLonInReadFile_ ; // specify if longitude boundaries are defined on read file, so it can be read later when grid distribution will be defined

         void computeLocalMask(void) ;
      
         /** The number of server of a context client. Avoid to re-compute indice computed in a previous computeConnectedClient */
         private: std::set<int> listNbServer_ ;
         
      private:
         void checkDomain(void);
         void checkLocalIDomain(void);
         void checkLocalJDomain(void);

         void checkMask(void);
         void checkDomainData(void);
         void checkCompression(void);

         void checkBounds(void);
         void checkArea(void);
         void checkLonLat();
         
       public:
         void sendDomainToFileServer(CContextClient* client) ;
       private:
         std::set<CContextClient*> sendDomainToFileServer_done_ ;
       public:
         void sendDomainToCouplerOut(CContextClient* client, const string& fieldId, int posInGrid) ;
       private:
         std::set<CContextClient*> sendDomainToCouplerOut_done_ ;
      
       public:
        void makeAliasForCoupling(const string& fieldId, int posInGrid) ;
        string getCouplingAlias(const string& fieldId, int posInGrid) ;
       private:

         void convertLonLatValue();
         void fillInRectilinearLonLat();
         void fillInCurvilinearLonLat();
         void fillInUnstructuredLonLat();
         void completeLonLatClient(void);  
         
         
       private:         

/** Clients that have to send a domain. There can be multiple clients in case of secondary server, otherwise only one client. */
         std::list<CContextClient*> clients;
         std::set<CContextClient*> clientsSet;

         bool isChecked ;
         std::set<StdString> relFiles, relFilesCompressed;

/** only on client side : Global index of each client sent to server: map<serverSize, map<serverRank, indexes>> 
    indSrv_[nbServers] -->  first map is related to the server distribution (ie associated with the contextClient)
    indSrv_[nbServers][server_rank] -> array of global index sent to the server of rank "server_rank"
    indSrv_[nbServers][server_rank](ind) --> global index on server of the local element "ind" sent (for lon, lat, mask, etc...) 
*/
         std::map<int, std::unordered_map<int, vector<size_t> > > indSrv_;

         std::map<int, std::vector<int> > connectedServerRank_;

         bool isRedistributed_;
         bool isUnstructed_;

       
       //////////////////////////////////////////////////////////////////////////////////////
       //  this part is related to distribution, element definition, views and connectors  //
       //////////////////////////////////////////////////////////////////////////////////////
       private:
         shared_ptr<CLocalElement> localElement_ = nullptr ;
         void initializeLocalElement(void) ;
       
       public:  
         shared_ptr<CLocalElement> getLocalElement(void) { if (localElement_==nullptr) initializeLocalElement() ; return localElement_ ; }
         shared_ptr<CLocalView> getLocalView(CElementView::type type) { return getLocalElement()->getView(type) ;}
       
       private:  
         void addFullView(void) ;
         void addWorkflowView(void) ;
         void addModelView(void) ;
        
       private:
         shared_ptr<CLocalConnector> modelToWorkflowConnector_ ;
         void computeModelToWorkflowConnector(void)  ;
       public:
         shared_ptr<CLocalConnector> getModelToWorkflowConnector(void) { if (modelToWorkflowConnector_==nullptr) computeModelToWorkflowConnector() ; return modelToWorkflowConnector_ ;}

       public:
         void computeRemoteElement(CContextClient* client, EDistributionType) ;
         void distributeToServer(CContextClient* client, std::map<int, CArray<size_t,1>>& globalIndex, shared_ptr<CScattererConnector> &scattererConnector,
                                 const string& domainId="") ;

         static void recvDomainDistribution(CEventServer& event) ;
         void receivedDomainDistribution(CEventServer& event, int phasis) ;

         void sendDistributedAttributes(CContextClient* client, shared_ptr<CScattererConnector> scaterrerConnector, const string& domainId) ;
         static void recvDistributedAttributes(CEventServer& event) ;
         void recvDistributedAttributes(CEventServer& event, const string& type) ;
         void setServerMask(CArray<bool,1>& serverMask, CContextClient* client) ;

       private:
         map<CContextClient*, shared_ptr<CDistributedElement>> remoteElement_ ;
       public: 
         shared_ptr<CDistributedElement> getRemoteElement(CContextClient* client) {return remoteElement_[client] ;}
       private:
         map<CContextClient*, shared_ptr<CScattererConnector>> clientToServerConnector_ ;
       public: 
         shared_ptr<CScattererConnector> getClientToServerConnector(CContextClient* client) { return clientToServerConnector_[client] ;}
       
       private:
         shared_ptr<CGathererConnector>  gathererConnector_ ;
       public:
         shared_ptr<CGathererConnector> getGathererConnector(void) { return gathererConnector_ ;}
        private:
         shared_ptr<CGathererConnector> serverFromClientConnector_ ;
         shared_ptr<CDistributedElement> elementFrom_ ;
       public:
         shared_ptr<CGathererConnector> getServerFromClientConnector(void) { return serverFromClientConnector_ ;}

       private:
         shared_ptr<CScattererConnector>  serverToClientConnector_ = nullptr ;
       public: 
         shared_ptr<CScattererConnector> getServerToClientConnector(void) { return serverToClientConnector_ ;} 

       private:
         map<CContextClient*,shared_ptr<CGathererConnector>>  clientFromServerConnector_  ;
       public: 
         shared_ptr<CGathererConnector> getClientFromServerConnector(CContextClient* client) { return clientFromServerConnector_[client] ;}        
         

         DECLARE_REF_FUNC(Domain,domain)

   }; // class CDomain

   ///--------------------------------------------------------------

   // Declare/Define CDomainGroup and CDomainDefinition
   DECLARE_GROUP(CDomain);

   ///--------------------------------------------------------------

} // namespace xios

#endif //__XIOS_CDomain__
