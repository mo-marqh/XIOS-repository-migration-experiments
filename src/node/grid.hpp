#ifndef __XIOS_CGrid__
#define __XIOS_CGrid__

/// XIOS headers ///
#include "xios_spl.hpp"
#include "group_factory.hpp"

#include "declare_group.hpp"
#include "domain.hpp"
#include "axis.hpp"
#include "scalar.hpp"
#include "array_new.hpp"
#include "attribute_array.hpp"
#include "distribution_server.hpp"
#include "client_server_mapping.hpp"
#include "utils.hpp"
#include "transformation_enum.hpp"
#include "grid_local_connector.hpp"
#include "grid_elements.hpp"

namespace xios {

   /// ////////////////////// Declarations ////////////////////// ///

   class CGridGroup;
   class CGridAttributes;
   class CDomainGroup;
   class CAxisGroup;
   class CScalarGroup;
   class CGrid;
   class CDistributionClient;
   class CDistributionServer;
   class CServerDistributionDescription;
   class CClientServerMapping;
   class CGridTransformation;

   ///--------------------------------------------------------------

   // Declare/Define CGridAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CGrid)
#  include "grid_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CGrid)

   ///--------------------------------------------------------------

   class CGrid
      : public CObjectTemplate<CGrid>
      , public CGridAttributes
   {
         /// typedef ///
         typedef CObjectTemplate<CGrid>   SuperClass;
         typedef CGridAttributes SuperClassAttribute;

      public:

         typedef CGridAttributes RelAttributes;
         typedef CGridGroup      RelGroup;

         enum EEventId
         {
           EVENT_ID_INDEX, EVENT_ID_ADD_DOMAIN, EVENT_ID_ADD_AXIS, EVENT_ID_ADD_SCALAR
         };

         /// Constructeurs ///
         CGrid(void);
         explicit CGrid(const StdString& id);
         CGrid(const CGrid& grid);       // Not implemented yet.
         CGrid(const CGrid* const grid); // Not implemented yet.

         /// Traitements ///
//         void solveReference(void);

         void checkEligibilityForCompressedOutput();



         void checkMaskIndex(bool doCalculateIndex);

 //        virtual void toBinary  (StdOStream& os) const;
//         virtual void fromBinary(StdIStream& is);

         void addRelFileCompressed(const StdString& filename);

         /// Tests ///
         bool isCompressible(void) const;
         bool isWrittenCompressed(const StdString& filename) const;

      public:

         /// Accesseurs ///
         StdSize getDimension(void);

         StdSize  getDataSize(void) ;

         /**
          * Get the local data grid size, ie the size of the compressed grid (inside the workflow)
          * \return The size od the compressed grid
          */
         StdSize  getLocalDataSize(void) ;

         /// Entrees-sorties de champs
         template <int n>
         void inputField(const CArray<double,n>& field, CArray<double,1>& stored) ;
         template <int n>
         void maskField(const CArray<double,n>& field, CArray<double,1>& stored) ;
         template <int n>
         void outputField(const CArray<double,1>& stored, CArray<double,n>& field) ;  
         template <int n>
         void uncompressField(const CArray<double,n>& data, CArray<double,1>& outData) ; 

         virtual void parse(xml::CXMLNode& node);

         /// Destructeur ///
         virtual ~CGrid(void);

      public:

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);

         static ENodeType GetType(void);

         /// Instanciateurs Statiques ///
         static CGrid* createGrid(CDomain* domain);
         static CGrid* createGrid(CDomain* domain, CAxis* axis);
         static CGrid* createGrid(const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                                  const CArray<int,1>& axisDomainOrder = CArray<int,1>());
         static CGrid* createGrid(StdString id, const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                                  const std::vector<CScalar*>& scalars, const CArray<int,1>& axisDomainOrder = CArray<int,1>());
         static CGrid* createGrid(const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                                  const std::vector<CScalar*>& scalars, const CArray<int,1>& axisDomainOrder);
         static StdString generateId(const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                                     const std::vector<CScalar*>& scalars, const CArray<int,1>& axisDomainOrder = CArray<int,1>());
         static StdString generateId(const CGrid* gridSrc, const CGrid* gridDest);
         static CGrid* cloneGrid(const StdString& idNewGrid, CGrid* gridSrc);

      public:            
         void computeIndexServer(void);
         void computeIndex(void);
         void computeIndexScalarGrid();
         void computeWrittenIndex();
         void solveDomainAxisRef(bool areAttributesChecked);
         void checkElementsAttributes(void) ;

         void solveDomainRef(bool checkAtt);
         void solveAxisRef(bool checkAtt);
         void solveScalarRef(bool checkAtt);
         void solveElementsRefInheritance(bool apply = true);
        // void solveTransformations();
         void solveDomainAxisBaseRef();

         CDomain* addDomain(const std::string& id=StdString());
         CAxis* addAxis(const std::string& id=StdString());
         CScalar* addScalar(const std::string& id=StdString());

      public:
         void sendGridToFileServer(CContextClient* client) ;
      private:
         std::set<CContextClient*> sendGridToFileServer_done_ ;
     
      public:
         void sendGridToCouplerOut(CContextClient* client, const string& fieldId) ;
      private:
         std::set<CContextClient*> sendGridToCouplerOut_done_ ;

      public:
         void makeAliasForCoupling(const string& fieldId) ;

      public:
         void sendAddDomain(const std::string& id,CContextClient* contextClient);
         void sendAddAxis(const std::string& id,CContextClient* contextClient);
         void sendAddScalar(const std::string& id,CContextClient* contextClient);
         void sendAllDomains(CContextClient* contextClient);
         void sendAllAxis(CContextClient* contextClient);
         void sendAllScalars(CContextClient* contextClient);

         static void recvAddDomain(CEventServer& event);
         void recvAddDomain(CBufferIn& buffer);
         static void recvAddAxis(CEventServer& event);
         void recvAddAxis(CBufferIn& buffer);
         static void recvAddScalar(CEventServer& event);
         void recvAddScalar(CBufferIn& buffer);

         static bool dispatchEvent(CEventServer& event);
         static void recvIndex(CEventServer& event);
         void recvIndex(vector<int> ranks, vector<CBufferIn*> buffers, CContextServer* server);
       
       public:  
         void sendIndex(CContextClient* client, const string& gridId="");
       private:
          set<CContextClient*> sendIndex_done_ ;
       
       public:
         void sendIndexScalarGrid(CContextClient* client, const string& gridId="");
       private:
          set<CContextClient*> sendIndexScalarGrid_done_ ;
      
       public:
         void setContextClient(CContextClient* contextClient);

         void computeDomConServer();
         std::map<int, int> getDomConServerSide();
         std::map<int, StdSize> getAttributesBufferSize(CContextClient* client, bool bufferForWriting = false);
         std::map<int, StdSize> getDataBufferSize(CContextClient* client, const std::string& id = "", bool bufferForWriting = false);
         std::vector<StdString> getDomainList();
         std::vector<StdString> getAxisList();
         std::vector<StdString> getScalarList();
         std::vector<CDomain*> getDomains();
         std::vector<CAxis*> getAxis();
         std::vector<CScalar*> getScalars();
         CDomain* getDomain(int domainIndex);
         CAxis* getAxis(int axisIndex);
         CScalar* getScalar(int scalarIndex);
         std::vector<int> getAxisOrder();
         std::vector<int> getGlobalDimension();
         bool isScalarGrid() const;         

         bool doGridHaveDataToWrite();
         bool doGridHaveDataDistributed(CContextClient* client = 0);
         size_t getWrittenDataSize() ;
         int getNumberWrittenIndexes() const;
         int getTotalNumberWrittenIndexes() const;
         int getOffsetWrittenIndexes() const;

         CGridTransformation* getTransformations();

         void transformGrid(CGrid* transformGridSrc);
         
         void prepareTransformGrid(CGrid* transformGridSrc);
         bool prepareTransformGrid_done_ = false ;

         void makeTransformGrid(void); 
         bool makeTransformGrid_done_ = false ;
         
         std::vector<std::string> getAuxInputTransformGrid(void) ; 

         void completeGrid(CGrid* transformGridSrc = 0);
         void doAutoDistribution(CGrid* transformGridSrc);
         bool isTransformed();
         void setTransformed();
         bool isGenerated();
         void setGenerated();
         void addTransGridSource(CGrid* gridSrc);
         std::map<CGrid*, std::pair<bool,StdString> >& getTransGridSource();
         bool hasTransform();
         size_t getGlobalWrittenSize(void) ;
         
         bool isCompleted(void) ;
         void setCompleted(void) ;
         void unsetCompleted(void) ;


         bool hasMask(void) const;
         void checkMask(void);
         void createMask(void);
         void modifyMask(const CArray<int,1>& indexToModify, bool valueToModify = false);
         void modifyMaskSize(const std::vector<int>& newDimensionSize, bool newValue = false);
        
        /** get mask pointer stored in mask_1d, or mask_2d, or..., or mask_7d */
         CArray<bool,1> mask_ ;
         CArray<bool,1>& getMask(void) ;

         void computeGridGlobalDimension(const std::vector<CDomain*>& domains,
                                         const std::vector<CAxis*>& axis,
                                         const std::vector<CScalar*>& scalars,
                                         const CArray<int,1>& axisDomainOrder);

         void computeGridIndexToFileServer(CContextClient* client) ;
 
     private:
        /** Client-like distribution calculated based on the knowledge of the entire grid */
       CDistributionClient* clientDistribution_;
     public: 
       void computeClientDistribution(void) ;
     private:
       bool computeClientDistribution_done_ = false ;
     public:
       CDistributionClient* getClientDistribution(void); 

     private:   
       /** Server-like distribution calculated upon receiving indexes */
       CDistributionServer* serverDistribution_;
       void computeServerDistribution(void) ;
       bool computeServerDistribution_done_=false ;
     public: 
       CDistributionServer* getServerDistribution(void) { if (!computeServerDistribution_done_) computeServerDistribution() ; return serverDistribution_ ;}


     private:
       template<int N>
       void checkGridMask(CArray<bool,N>& gridMask,
                          const std::vector<CArray<bool,1>* >& domainMasks,
                          const std::vector<CArray<bool,1>* >& axisMasks,
                          const CArray<int,1>& axisDomainOrder,
                          bool createMask = false);

        template<int N>
        void modifyGridMask(CArray<bool,N>& gridMask, const CArray<int,1>& indexToModify, bool valueToModify);

        template<int N>
        void modifyGridMaskSize(CArray<bool,N>& gridMask, const std::vector<int>& eachDimSize, bool newValue);

        void storeField_arr(const double* const data, CArray<double, 1>& stored) ;
        void restoreField_arr(const CArray<double, 1>& stored, double* const data) ;
        void uncompressField_arr(const double* const data, CArray<double, 1>& outData) ;
        void maskField_arr(const double* const data, CArray<double, 1>& stored) ;

        void setVirtualDomainGroup(CDomainGroup* newVDomainGroup);
        void setVirtualAxisGroup(CAxisGroup* newVAxisGroup);
        void setVirtualScalarGroup(CScalarGroup* newVScalarGroup);

        void setDomainList(const std::vector<CDomain*> domains = std::vector<CDomain*>());
        void setAxisList(const std::vector<CAxis*> axis = std::vector<CAxis*>());
        void setScalarList(const std::vector<CScalar*> scalars = std::vector<CScalar*>());

        CDomainGroup* getVirtualDomainGroup() const;
        CAxisGroup* getVirtualAxisGroup() const;
        CScalarGroup* getVirtualScalarGroup() const;

        void checkAttributesAfterTransformation();
        void setTransformationAlgorithms();
        void computeIndexByElement(const std::vector<std::unordered_map<size_t,std::vector<int> > >& indexServerOnElement,
                                   const CContextClient* client,
                                   CClientServerMapping::GlobalIndexMap& globalIndexOnServer);
        int computeGridGlobalDimension(std::vector<int>& globalDim,
                                       const std::vector<CDomain*> domains,
                                       const std::vector<CAxis*> axis,
                                       const std::vector<CScalar*> scalars,
                                       const CArray<int,1>& axisDomainOrder);
        int getDistributedDimension();

        void computeConnectedClients(CContextClient* client);
        set<CContextClient*> computeConnectedClients_done_ ;

        void computeConnectedClientsScalarGrid(CContextClient* client); 
        set<CContextClient*> computeConnectedClientsScalarGrid_done_ ;

      public:
/** Array containing the local index of the grid
 *  storeIndex_client[local_workflow_grid_index] -> local_model_grid_index. 
 *  Used to store field from model into the worklow, or to return field into models.  
 *  The size of the array is the number of local index of the workflow grid */        
        CArray<int, 1> storeIndex_client_;
        void computeStoreIndex_client(void) ;
        bool computeStoreIndex_client_done_ = false ;
        CArray<int, 1>& getStoreIndex_client(void) { if (!computeStoreIndex_client_done_) computeStoreIndex_client() ; return storeIndex_client_ ;}

/** Array containing the grid mask masked defined by the mask_nd grid attribute.        
  * The corresponding masked field value provided by the model will be replaced by a NaN value
  * in the workflow.  */
        CArray<bool, 1> storeMask_client_;
        void computeStoreMask_client(void) ;
        bool computeStoreMask_client_done_ = false ;
        CArray<bool, 1>& getStoreMask_client(void) { if (!computeStoreMask_client_done_) computeStoreMask_client() ; return storeMask_client_ ;}

/** Map containing the indexes on client side that will be sent to each connected server.
  * storeIndex_toSrv[&contextClient] -> map concerning the contextClient for which the data will be sent (client side)
  * storeIndex_toSrv[&contextClient][rank] -> array of indexes that will be sent to each "rank" of the connected servers 
  * storeIndex_toSrv[&contextClient][rank](index_of_buffer_sent_to_server) -> local index of the field of the workflow 
  * grid that will be sent to server */
         std::map<CContextClient*, map<int, CArray<int, 1> > > storeIndex_toSrv_;


/** Map containing the indexes on client side that will be received from each connected server.
  * This map is used to agreggate field data received from server (for reading) into a single array, which will be an entry
  * point of the worklow on client side.
  * storeIndex_toSrv[rank] -> array of indexes that will be received from each "rank" of the connected servers 
  * storeIndex_toSrv[rank](index_of_buffer_received_from_server) -> local index of the field in the "workflow grid"
  * that has been received from server */
         std::map<int, CArray<int, 1> > storeIndex_fromSrv_; // Support, for now, reading with level-1 server


/** Maps storing the number of participating clients for data sent a specific server for a given contextClient, identified
  * by the servers communicator size. In future must be direcly identified by context.
  * nbSender_[context_server_size] -> map the number of client sender by connected rank of servers
  * nbSender_[context_server_size] [rank_server] -> the number of client participating to a send message for a server of rank "rank_server" 
  * Usefull to indicate in a message the number of participant needed by the transfer protocol */
         std::map<int, std::map<int,int> > nbSenders_;

      private:
/** Maps storing the number of participating servers for data sent a specific client for a given contextClient.
  * Symetric of nbSenders_, but for server side which want to send data to client.
  * nbReadSender_[context_client_size] -> map the number of server sender by connected rank of clients
  * nbReadSender_[context_client_size] [rank_client] -> the number of server participating to a send message for a client of rank "rank_client" 
  * Usefull to indicate in a message the number of participant needed by the transfer protocol */
         std::map<CContextClient*, std::map<int,int> > nbReadSenders_;
      public:
         std::map<int,int>& getNbReadSenders(CContextClient* client) 
         { if (nbReadSenders_.count(client)==0) computeNbReadSenders(client) ; return nbReadSenders_[client] ;}
      private:
         void computeNbReadSenders(CContextClient* client) ;
     

// Manh Ha's comment: " A client receives global index from other clients (via recvIndex)
// then does mapping these index into local index of STORE_CLIENTINDEX
// In this way, store_clientIndex can be used as an input of a source filter
// Maybe we need a flag to determine whether a client wants to write. TODO "

      private:
       /** Map storing received data on server side. This map is the equivalent to the storeIndex_client, but for data received from client
        * instead that from model. This map is used to concatenate data received from several clients into a single array on server side
        * which match the local workflow grid.
        * outLocalIndexStoreOnClient_[client_rank] -> Array of index from client of rank "client_rank"
        * outLocalIndexStoreOnClient_[client_rank](index of buffer from client) -> local index of the workflow grid
        * The map is created in CGrid::computeClientIndex and filled upon receiving data in CField::recvUpdateData().
        * Symetrically it is also used to send data from a server to several client for reading case. */
        map<int, CArray<size_t, 1>> outLocalIndexStoreOnClient_; 
      public:
         void computeOutLocalIndexStoreOnClient(void) ;
      private:
         bool computeOutLocalIndexStoreOnClient_done_ = false ;  
      public:   
         map<int, CArray<size_t, 1>>& getOutLocalIndexStoreOnClient(void) 
         { if (!computeOutLocalIndexStoreOnClient_done_) computeOutLocalIndexStoreOnClient(); return outLocalIndexStoreOnClient_ ; }

      public:
/** Indexes calculated based on server-like distribution.
 *  They are used for writing/reading data and only calculated for server level that does the writing/reading.
 *  Along with localIndexToWriteOnClient, these indexes are used to correctly place incoming data. 
 *  size of the array : numberWrittenIndexes_ : number of index written in a compressed way
 *  localIndexToWriteOnServer_(compressed_written_index) : -> local uncompressed index that will be written in the file */
         CArray<size_t,1> localIndexToWriteOnServer_;

/** Indexes calculated based on client-like distribution.
  * They are used for writing/reading data and only calculated for server level that does the writing/reading.
  * Along with localIndexToWriteOnServer, these indexes are used to correctly place incoming data. 
  * size of the array : numberWrittenIndexes_
  * localIndexToWriteOnClient_(compressed_written_index) -> local index of the workflow grid*/
         CArray<size_t,1> localIndexToWriteOnClient_;

      public: 
        bool isDataDistributed(void) ; 
      private:

/** Clients that have to send a grid. There can be multiple clients in case of secondary server, otherwise only one client. */
        std::list<CContextClient*> clients;
        std::set<CContextClient*> clientsSet;

      private:  
        /** Map storing received indexes on server side sent by clients. Key = sender rank, value = global index array. 
            Later, the global indexes received will be mapped onto local index computed with the local distribution.
            outGlobalIndexFromClient_[rank] -> array of global index send by client of rank "rank"
            outGlobalIndexFromClient_[rank](n) -> global index of datav n sent by client
        */      
        map<int, CArray<size_t, 1> > outGlobalIndexFromClient_;
      public:  
        map<int, CArray<size_t, 1> >& getOutGlobalIndexFromClient() { return outGlobalIndexFromClient_ ;}

      private:
        bool isChecked;
        bool isDomainAxisChecked;
        bool isIndexSent;

        CDomainGroup* vDomainGroup_;
        CAxisGroup* vAxisGroup_;
        CScalarGroup* vScalarGroup_;
        std::vector<std::string> axisList_, domList_, scalarList_;
        bool isAxisListSet, isDomListSet, isScalarListSet;

        CClientServerMapping* clientServerMap_;
        int numberWrittenIndexes_, totalNumberWrittenIndexes_, offsetWrittenIndexes_;

/** Map storing local ranks of connected receivers. Key = size of receiver's intracomm.
  * It is calculated in computeConnectedClients(). */
        std::map<int, std::vector<int> > connectedServerRank_;

/** Map storing the size of data to be send. Key = size of receiver's intracomm
  * It is calculated in computeConnectedClients(). */
        std::map<int, std::map<int,size_t> > connectedDataSize_;

/** Ranks of connected receivers in case of reading. It is calculated in recvIndex(). */
        std::vector<int> connectedServerRankRead_;

/** Size of data to be send in case of reading. It is calculated in recvIndex(). */
        std::map<int,size_t> connectedDataSizeRead_;
      
         //! True if and only if the data defined on the grid can be outputted in a compressed way
        bool isCompressible_;
        std::set<std::string> relFilesCompressed;

        bool isTransformed_, isGenerated_;
        bool computedWrittenIndex_;
        
        std::vector<int> axisPositionInGrid_;
        void computeAxisPositionInGrid(void) ;
        bool computeAxisPositionInGrid_done_ = false ;
        std::vector<int>& getAxisPositionInGrid(void) { if (!computeAxisPositionInGrid_done_) computeAxisPositionInGrid() ; return axisPositionInGrid_ ;}

        CGridTransformation* transformations_;
        bool hasDomainAxisBaseRef_;        
        std::map<CGrid*, std::pair<bool,StdString> > gridSrc_;
        bool hasTransform_;

/** Map storing global indexes of server-like (band-wise) distribution for sending to receivers (client side).
  * Key = size of receiver's intracomm (i.e. number of servers)
  * ~ map<int, umap<int, std::vector<size_t> >> globalIndexOnServer_
  * globalIndexOnServer_[servers_size] -> map for a distribution of size "servers_size" (number of servers)
  * globalIndexOnServer_[servers_size][server_rank] -> array of global index managed by server of rank "server_rank"
  * globalIndexOnServer_[servers_size][server_rank][n] -> global index of data to be send to the server by client based on sub element of the grid.
  * -> grid masking is not included.
  */
//        std::map<CContextClient*, CClientServerMapping::GlobalIndexMap> globalIndexOnServer_;
        std::map<int, CClientServerMapping::GlobalIndexMap> globalIndexOnServer_;


/** List order of axis and domain in a grid, if there is a domain, it will take value 2, axis 1, scalar 0 */
        std::vector<int> order_;

      private:  
        CGridLocalElements* gridLocalElements_= nullptr ;
        void computeGridLocalElements(void) ;
      public:
        CGridLocalElements* getGridLocalElements(void) { if (gridLocalElements_==nullptr) computeGridLocalElements() ; return gridLocalElements_ ;}

      private:
        CGridLocalConnector* modelToWorkflowConnector_ ;
      public:
        void computeModelToWorkflowConnector(void) ;
        CGridLocalConnector* getModelToWorkflowConnector(void) { if (modelToWorkflowConnector_==nullptr) computeModelToWorkflowConnector() ; return modelToWorkflowConnector_;}

   }; // class CGrid

   ///--------------------------------------------------------------

   template <int n>
   void CGrid::inputField(const CArray<double,n>& field, CArray<double,1>& stored)
   TRY
   {
//#ifdef __XIOS_DEBUG
      if (this->getDataSize() != field.numElements())
         ERROR("void CGrid::inputField(const  CArray<double,n>& field, CArray<double,1>& stored) const",
                << "[ Awaiting data of size = " << this->getDataSize() << ", "
                << "Received data size = "      << field.numElements() << " ] "
                << "The data array does not have the right size! "
                << "Grid = " << this->getId())
//#endif
      this->storeField_arr(field.dataFirst(), stored);
   }
   CATCH

/* obsolete
   template <int n>
   void CGrid::maskField(const CArray<double,n>& field, CArray<double,1>& stored)
   {
//#ifdef __XIOS_DEBUG
      if (this->getDataSize() != field.numElements())
         ERROR("void CGrid::inputField(const  CArray<double,n>& field, CArray<double,1>& stored) const",
                << "[ Awaiting data of size = " << this->getDataSize() << ", "
                << "Received data size = "      << field.numElements() << " ] "
                << "The data array does not have the right size! "
                << "Grid = " << this->getId())
//#endif
      this->maskField_arr(field.dataFirst(), stored);
   }
*/
   template <int n>
   void CGrid::maskField(const CArray<double,n>& field, CArray<double,1>& stored)
   {
      auto connector = getModelToWorkflowConnector() ;

      if (connector->getSrcSize() != field.numElements())
         ERROR("void CGrid::inputField(const  CArray<double,n>& field, CArray<double,1>& stored) const",
                << "[ Awaiting data of size = " << this->getDataSize() << ", "
                << "Received data size = "      << field.numElements() << " ] "
                << "The data array does not have the right size! "
                << "Grid = " << this->getId())
      const double nanValue = std::numeric_limits<double>::quiet_NaN();
      connector->transfer(field, stored, nanValue) ;
   }



   template <int n>
   void CGrid::outputField(const CArray<double,1>& stored, CArray<double,n>& field)
   TRY
   {
//#ifdef __XIOS_DEBUG
      if (this->getDataSize() != field.numElements())
         ERROR("void CGrid::outputField(const CArray<double,1>& stored, CArray<double,n>& field) const",
                << "[ Size of the data = " << this->getDataSize() << ", "
                << "Output data size = "   << field.numElements() << " ] "
                << "The ouput array does not have the right size! "
                << "Grid = " << this->getId())
//#endif
      this->restoreField_arr(stored, field.dataFirst());
   }
   CATCH

   /*!
     This function removes the effect of mask on received data on the server.
     This function only serve for the checking purpose. TODO: Something must be done to seperate mask and data_index from each other in received data
     \data data received data with masking effect on the server
     \outData data without masking effect
   */
   template <int N>
   void CGrid::uncompressField(const CArray<double,N>& data, CArray<double,1>& outData)
   TRY
   {      
     uncompressField_arr(data.dataFirst(), outData);
   }
   CATCH

   template<int N>
   void CGrid::checkGridMask(CArray<bool,N>& gridMask,
                             const std::vector<CArray<bool,1>* >& domainMasks,
                             const std::vector<CArray<bool,1>* >& axisMasks,
                             const CArray<int,1>& axisDomainOrder,
                             bool createMask)
   TRY
   {
     int idx = 0;
     int numElement = axisDomainOrder.numElements();
     int dim = domainMasks.size() * 2 + axisMasks.size();
     std::vector<CDomain*> domainP = this->getDomains();
     std::vector<CAxis*> axisP = this->getAxis();

     std::vector<int> idxLoop(dim,0), indexMap(numElement), eachDimSize(dim);
     std::vector<int> currentIndex(dim);
     int idxDomain = 0, idxAxis = 0;
    for (int i = 0; i < numElement; ++i)
    {
      indexMap[i] = idx;
      if (2 == axisDomainOrder(i)) {
          eachDimSize[indexMap[i]]   = domainP[idxDomain]->ni;
          eachDimSize[indexMap[i]+1] = domainP[idxDomain]->nj;
          idx += 2; ++idxDomain;
      }
      else if (1 == axisDomainOrder(i)) {
//        eachDimSize[indexMap[i]] = axisMasks[idxAxis]->numElements();
        eachDimSize[indexMap[i]] = axisP[idxAxis]->n;
        ++idx; ++idxAxis;
      }
      else {};
    }

    if (!gridMask.isEmpty() && !createMask)
    {
      for (int i = 0; i < dim; ++i)
      {
        if (gridMask.extent(i) != eachDimSize[i])
          ERROR("CGrid::checkMask(void)",
                << "The mask has one dimension whose size is different from the one of the local grid." << std::endl
                << "Local size of dimension " << i << " is " << eachDimSize[i] << "." << std::endl
                << "Mask size for dimension " << i << " is " << gridMask.extent(i) << "." << std::endl
                << "Grid = " << this->getId())
      }
    }
    else {
        CArrayBoolTraits<CArray<bool,N> >::resizeArray(gridMask,eachDimSize);
        gridMask = true;
    }

    int ssize = gridMask.numElements();
    idx = 0;
    while (idx < ssize)
    {
      for (int i = 0; i < dim-1; ++i)
      {
        if (idxLoop[i] == eachDimSize[i])
        {
          idxLoop[i] = 0;
          ++idxLoop[i+1];
        }
      }

      // Find out outer index
      idxDomain = idxAxis = 0;
      bool maskValue = true;
      for (int i = 0; i < numElement; ++i)
      {
        if (2 == axisDomainOrder(i))
        {
          int idxTmp = idxLoop[indexMap[i]] + idxLoop[indexMap[i]+1] * eachDimSize[indexMap[i]];
          if (idxTmp < (*domainMasks[idxDomain]).numElements())
            maskValue = maskValue && (*domainMasks[idxDomain])(idxTmp);
          else
            maskValue = false;
          ++idxDomain;
        }
        else if (1 == axisDomainOrder(i))
        {
          int idxTmp = idxLoop[indexMap[i]];
          if (idxTmp < (*axisMasks[idxAxis]).numElements())
            maskValue = maskValue && (*axisMasks[idxAxis])(idxTmp);
          else
            maskValue = false;

          ++idxAxis;
        }
      }

      int maskIndex = idxLoop[0];
      int mulDim = 1;
      for (int k = 1; k < dim; ++k)
      {
        mulDim *= eachDimSize[k-1];
        maskIndex += idxLoop[k]*mulDim;
      }
      *(gridMask.dataFirst()+maskIndex) &= maskValue;

      ++idxLoop[0];
      ++idx;
    }
   }
   CATCH_DUMP_ATTR

   template<int N>
   void CGrid::modifyGridMaskSize(CArray<bool,N>& gridMask,
                                  const std::vector<int>& eachDimSize,
                                  bool newValue)
   TRY
   {
      if (N != eachDimSize.size())
      {
        // ERROR("CGrid::modifyGridMaskSize(CArray<bool,N>& gridMask,
        //                                  const std::vector<int>& eachDimSize,
        //                                  bool newValue)",
        //       << "Dimension size of the mask is different from input dimension size." << std::endl
        //       << "Mask dimension is " << N << "." << std::endl
        //       << "Input dimension is " << eachDimSize.size() << "." << std::endl
        //       << "Grid = " << this->getId())
      }
      CArrayBoolTraits<CArray<bool,N> >::resizeArray(gridMask,eachDimSize);
      gridMask = newValue;
   }
   CATCH_DUMP_ATTR
                                 

   /*!
     Modify the current mask of grid, the local index to be modified will take value false
     \param [in/out] gridMask current mask of grid
     \param [in] indexToModify local index to modify
   */
   template<int N>
   void CGrid::modifyGridMask(CArray<bool,N>& gridMask, const CArray<int,1>& indexToModify, bool valueToModify)
   TRY
   {     
     int num = indexToModify.numElements();
     for (int idx = 0; idx < num; ++idx)
     {
       *(gridMask.dataFirst()+indexToModify(idx)) = valueToModify;
     }
   }
   CATCH_DUMP_ATTR

   ///--------------------------------------------------------------



   // Declare/Define CGridGroup and CGridDefinition
   DECLARE_GROUP(CGrid);

   ///--------------------------------------------------------------

} // namespace xios

#endif // __XIOS_CGrid__
