#ifndef __XIOS_CAxis__
#define __XIOS_CAxis__

/// XIOS headers ///
#include "xios_spl.hpp"
#include "group_factory.hpp"
#include "virtual_node.hpp"

#include "declare_group.hpp"
#include "declare_ref_func.hpp"
#include "declare_virtual_node.hpp"
#include "attribute_array.hpp"
#include "attribute_enum.hpp"
#include "attribute_enum_impl.hpp"
#include "server_distribution_description.hpp"
#include "transformation.hpp"
#include "transformation_enum.hpp"
#include "element.hpp"
#include "local_connector.hpp"
#include "scatterer_connector.hpp"
#include "gatherer_connector.hpp"
#include "distribution_type.hpp"


namespace xios {
   /// ////////////////////// Déclarations ////////////////////// ///

   class CAxisGroup;
   class CAxisAttributes;
   class CAxis;

   ///--------------------------------------------------------------

   // Declare/Define CAxisAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CAxis)
#  include "axis_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CAxis)

   ///--------------------------------------------------------------

   class CAxis
      : public CObjectTemplate<CAxis>
      , public CAxisAttributes
   {
               /// typedef ///
         typedef CObjectTemplate<CAxis>   SuperClass;
         typedef CAxisAttributes SuperClassAttribute;
         
      public:
         enum EEventId
         {
           EVENT_ID_DISTRIBUTION_ATTRIBUTE,           
           EVENT_ID_DISTRIBUTED_VALUE,
           EVENT_ID_NON_DISTRIBUTED_VALUE,
           EVENT_ID_NON_DISTRIBUTED_ATTRIBUTES,
           EVENT_ID_DISTRIBUTED_ATTRIBUTES,
           EVENT_ID_AXIS_DISTRIBUTION,
           EVENT_ID_SEND_DISTRIBUTED_ATTRIBUTE
         } ;

      public:
         typedef CAxisAttributes RelAttributes;
         typedef CAxisGroup      RelGroup;
         typedef CTransformation<CAxis>::TransformationMapTypes TransMapTypes;

      public:
         /// Constructeurs ///
         CAxis(void);
         explicit CAxis(const StdString & id);
         CAxis(const CAxis & axis);       // Not implemented yet.
         CAxis(const CAxis * const axis); // Not implemented yet.

         static CAxis* createAxis();

         /// Accesseurs ///
         const std::set<StdString> & getRelFiles(void) const;

         int getNumberWrittenIndexes(MPI_Comm writtenCom);
         int getTotalNumberWrittenIndexes(MPI_Comm writtenCom);
         int getOffsetWrittenIndexes(MPI_Comm writtenCom);
         CArray<int, 1>& getCompressedIndexToWriteOnServer(MPI_Comm writtenCom);

         std::map<int, StdSize> getAttributesBufferSize(CContextClient* client, const std::vector<int>& globalDim, int orderPositionInGrid,
                                                        CServerDistributionDescription::ServerDistributionType disType = CServerDistributionDescription::BAND_DISTRIBUTION);

         /// Test ///
         bool IsWritten(const StdString & filename) const;
         bool isWrittenCompressed(const StdString& filename) const;
         bool isDistributed(void) const;
        
        public:
        /*!
            \brief return if the axis can be written or not in a compressed way.
            ie if there are some masked or indexed point on the domain. Valid only on server side.
            \return true if domain can be writtedn in a compressed way
         */ 
         bool isCompressible(void) { if (!isCompressibleComputed_) computeIsCompressible() ; return isCompressible_ ;} 
        private:
         bool isCompressible_ ; /** specify if the domain can be written in a compressed way */ 
         bool isCompressibleComputed_=false ; /** Indicate if compressability has been computed*/
         void computeIsCompressible() ;
        
        public:

         /// Mutateur ///
         void addRelFile(const StdString & filename);
         void addRelFileCompressed(const StdString& filename);

         

         /// Destructeur ///
         virtual ~CAxis(void);

         virtual void parse(xml::CXMLNode & node);

         void setContextClient(CContextClient* contextClient);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);
         static ENodeType GetType(void);

         static bool dispatchEvent(CEventServer& event);         
        
         /// Vérifications ///
         void checkAttributes(void);
         bool checkAttributes_done_ = false ;
         
         void checkAttributesOnClient();
         void checkAttributesOnClientAfterTransformation(const std::vector<int>& globalDim, int orderPositionInGrid,
                                                         CServerDistributionDescription::ServerDistributionType distType = CServerDistributionDescription::BAND_DISTRIBUTION);
         void sendCheckedAttributes(const std::vector<int>& globalDim, int orderPositionInGrid,
                                    CServerDistributionDescription::ServerDistributionType disType = CServerDistributionDescription::BAND_DISTRIBUTION);

         size_t getGlobalWrittenSize(void) ;

         void computeWrittenIndex();
         void computeWrittenCompressedIndex(MPI_Comm);
         bool hasTransformation();
         void solveInheritanceTransformation();
         TransMapTypes getAllTransformations();         
         void duplicateTransformation(CAxis*);
         CTransformation<CAxis>* addTransformation(ETranformationType transType, const StdString& id="");
         bool isEqual(CAxis* axis);

      public: 
        bool hasValue;        
        bool hasBounds;
        bool hasLabel;

        CArray<int,1> localIndexToWriteOnServer;
         
         void computeConnectedClients(CContextClient* client, const std::vector<int>& globalDim, int orderPositionInGrid);
         private: std::set<CContextClient*> computeConnectedClients_done_ ; public :
         /** The number of server of a context client. Avoid to re-compute indice computed in a previous computeConnectedClient */
         private: std::set<int> listNbServer_ ; public:

      private:
         void checkData();
         void checkMask();
         void checkBounds();
         void checkLabel();
      public:
         void sendAxisToFileServer(CContextClient* client, const std::vector<int>& globalDim, int orderPositionInGrid) ;
      private:
         std::set<CContextClient*> sendAxisToFileServer_done_ ;
      
      public:
         void sendAxisToCouplerOut(CContextClient* client, const std::vector<int>& globalDim, int orderPositionInGrid, const string& fieldId, int posInGrid) ;
      private:
         std::set<CContextClient*> sendAxisToCouplerOut_done_ ;
    
      public:
         void makeAliasForCoupling(const string& fieldId, int posInGrid) ;

      private:
         void sendAttributes(CContextClient* client, const std::vector<int>& globalDim, int orderPositionInGrid,
                             CServerDistributionDescription::ServerDistributionType distType, const string& axisId="");
         void sendDistributionAttribute(CContextClient* client, const std::vector<int>& globalDim, int orderPositionInGrid,
                                        CServerDistributionDescription::ServerDistributionType distType, const string& axisId="");
         

         void sendNonDistributedAttributes(CContextClient* client, const string& axisId="");
         void sendDistributedAttributes_old(CContextClient* client, const string& axisId="");

         static void recvNonDistributedAttributes(CEventServer& event);
         static void recvDistributedAttributes_old(CEventServer& event);
         static void recvDistributionAttribute(CEventServer& event);
         void recvNonDistributedAttributes(int rank, CBufferIn& buffer);
         void recvDistributedAttributes_old(vector<int>& rank, vector<CBufferIn*> buffers);
         void recvDistributionAttribute(CBufferIn& buffer);

         void setTransformations(const TransMapTypes&);

      private:

/** Clients that have to send a axis. There can be multiple clients in case of secondary server, otherwise only one client. */
         std::list<CContextClient*> clients;
         std::set<CContextClient*> clientsSet;

      private:
         /** define if the axis is completed or not ie all attributes have been received before in case 
             of grid reading from file or coupling */ 
         bool isCompleted_=true ;  
      public:     
         /*!
           \brief Check if a axis is completed
           Before make any axis processing, we must be sure that all axis informations have
           been sent, for exemple when reading a grid in a file or when grid elements are sent by an
           other context (coupling). So all direct reference of the axis (axis_ref) must be also completed
           \return true if axis and axis reference are completed
          */
         bool isCompleted(void)
         {
           if (hasDirectAxisReference()) if (!getDirectAxisReference()->isCompleted()) return false;
           else return isCompleted_ ;
         }
         void setCompleted(void) { isCompleted_=true ; }
         void unsetCompleted(void) { isCompleted_=false ; }
      
      private:
         bool isChecked;
         bool areClientAttributesChecked_;
         bool isClientAfterTransformationChecked;
         std::set<StdString> relFiles, relFilesCompressed;
         TransMapTypes transformationMap_;         

         std::map<int, map<int,int> > nbSenders; // Mapping of number of communicating client to a server
         std::map<int, std::unordered_map<int, vector<size_t> > > indSrv_; // Global index of each client sent to server
         // std::map<int, vector<int> > indWrittenSrv_; // Global written index of each client sent to server
         std::unordered_map<size_t,size_t> globalLocalIndexMap_;
         std::map<int,int> numberWrittenIndexes_, totalNumberWrittenIndexes_, offsetWrittenIndexes_;
         std::map<int, CArray<int, 1> > compressedIndexToWriteOnServer;
         std::map<int, std::vector<int> > connectedServerRank_;
         bool computedWrittenIndex_;                  

       private:
         static bool initializeTransformationMap(std::map<StdString, ETranformationType>& m);
         static std::map<StdString, ETranformationType> transformationMapList_;
         static bool dummyTransformationMapList_;


       //////////////////////////////////////////////////////////////////////////////////////
       //  this part is related to distribution, element definition, views and connectors  //
       //////////////////////////////////////////////////////////////////////////////////////
         
        private:
         CLocalElement* localElement_ = nullptr ;
         void initializeLocalElement(void) ;
        public: 
         CLocalElement* getLocalElement(void) { if (localElement_==nullptr) initializeLocalElement() ; return localElement_ ; }
         CLocalView* getLocalView(CElementView::type type) { return getLocalElement()->getView(type) ;}
        private:
         void addFullView(void) ;
         void addWorkflowView(void) ;
         void addModelView(void) ;

        private:
         CLocalConnector* modelToWorkflowConnector_ ;
         void computeModelToWorkflowConnector(void)  ;
        public:
         CLocalConnector* getModelToWorkflowConnector(void) { if (modelToWorkflowConnector_==nullptr) computeModelToWorkflowConnector() ; return modelToWorkflowConnector_ ;}
       
       public:
         void computeRemoteElement(CContextClient* client, EDistributionType) ;
         void distributeToServer(CContextClient* client, std::map<int, CArray<size_t,1>>& globalIndex, CScattererConnector* &scattererConnector,
                                 const string& axisId="") ;

         static void recvAxisDistribution(CEventServer& event) ;
         void receivedAxisDistribution(CEventServer& event, int phasis) ;
         void setServerMask(CArray<bool,1>& serverMask, CContextClient* client ) ;
         void sendDistributedAttributes(CContextClient* client, CScattererConnector& scattererConnector, const string& axisId) ;
         static void recvDistributedAttributes(CEventServer& event) ;
         void recvDistributedAttributes(CEventServer& event, const string& type) ;
       private:
         map<CContextClient*, CDistributedElement*> remoteElement_ ;
       public: 
         CDistributedElement* getRemoteElement(CContextClient* client) {return remoteElement_[client] ;}
       private:
         map<CContextClient*, CScattererConnector*> clientToServerConnector_ ;
       public: 
         CScattererConnector* getClientToServerConnector(CContextClient* client) { return clientToServerConnector_[client] ;}
       private:
         CGathererConnector*  gathererConnector_ ;
       public:
         CGathererConnector* getGathererConnector(void) { return gathererConnector_ ;}
       private:
         CGathererConnector* serverFromClientConnector_ ;
         CDistributedElement* elementFrom_ ;
       public:
        CGathererConnector* getServerFromClientConnector(void) { return serverFromClientConnector_ ;}

       private:
         CScattererConnector*  serverToClientConnector_ = nullptr ;
       public: 
         CScattererConnector* getServerToClientConnector(void) { return serverToClientConnector_ ;} 

       private:
          map<CContextClient*,CGathererConnector*>  clientFromServerConnector_  ;
       public: 
         CGathererConnector* getClientFromServerConnector(CContextClient* client) { return clientFromServerConnector_[client] ;} 

         DECLARE_REF_FUNC(Axis,axis)
   }; // class CAxis

   ///--------------------------------------------------------------

   // Declare/Define CAxisGroup and CAxisDefinition
   DECLARE_GROUP(CAxis);
} // namespace xios

#endif // __XIOS_CAxis__
