#ifndef __XIOS_CField__
#define __XIOS_CField__

/// XIOS headers ///
#include "xios_spl.hpp"
#include "group_factory.hpp"
#include "functor.hpp"
#include "functor_type.hpp"
#include "duration.hpp"
#include "date.hpp"
#include "declare_group.hpp"
#include "calendar_util.hpp"
#include "array_new.hpp"
#include "attribute_array.hpp"
#include "declare_ref_func.hpp"
#include "transformation_enum.hpp"
#include "variable.hpp"
#include "context_client.hpp"
#include "pass_through_filter.hpp"
#include "temporal_filter.hpp"
#include "model_to_client_source_filter.hpp"
#include "client_from_client_source_filter.hpp"
#include "client_from_server_source_filter.hpp"
#include "client_to_model_store_filter.hpp"
#include "server_to_client_store_filter.hpp"
#include "server_from_client_source_filter.hpp"
#include "file_writer_store_filter.hpp"
#include "client_to_server_store_filter.hpp"
#include "file_reader_source_filter.hpp"





namespace xios 
{

   /// ////////////////////// Déclarations ////////////////////// ///

   class CFieldGroup;
   class CFieldAttributes;
   class CField;

   class CFile;
   class CCouplerIn ;
   class CCouplerOut ;
   class CGrid;
   class CContext;
   class CGenericFilter;
   class CDomain ;
   class CAxis ;
   class CScalar ;

   class CGarbageCollector;
   class COutputPin;
   class CSourceFilter;
   class CServerToClientFilter;
   ///--------------------------------------------------------------

   // Declare/Define CFieldAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CField)
#  include "field_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CField)

   class CContextClient ;

   ///--------------------------------------------------------------
   class CField
      : public CObjectTemplate<CField>
      , public CFieldAttributes
   {
         /// friend ///
         friend class CFile;

         /// typedef ///
         typedef CObjectTemplate<CField>   SuperClass;
         typedef CFieldAttributes SuperClassAttribute;

      public :   
         enum EReadField
         {
           RF_NODATA, RF_EOF, RF_DATA
         };
         
      public:

         typedef CFieldAttributes RelAttributes;
         typedef CFieldGroup      RelGroup;

         enum EEventId
         {
           EVENT_ID_UPDATE_DATA, EVENT_ID_READ_DATA, EVENT_ID_READ_DATA_READY,
           EVENT_ID_ADD_VARIABLE, EVENT_ID_ADD_VARIABLE_GROUP, EVENT_ID_GRID_COMPLETED
         };

         /// Constructeurs ///
         CField(void);
         explicit CField(const StdString& id);
         CField(const CField& field);       // Not implemented yet.
         CField(const CField* const field); // Not implemented yet.

         /// Accesseurs ///

         CGrid* getRelGrid(void) const;
         CFile* getRelFile(void) const;
         CDomain* getAssociatedDomain(const std::string& domainId, bool noError=false) const;
         CAxis*   getAssociatedAxis(const std::string& axisId, bool noError=false) const;
         CScalar* getAssociatedScalar(const std::string& scalarId, bool noError=false) const;

         func::CFunctor::ETimeType getOperationTimeType() const;

      public:
         template <int N> void getData(CArray<double, N>& _data) const;

         std::map<int, StdSize> getGridAttributesBufferSize(CContextClient* client, bool bufferForWriting = false);
         // Grid data buffer size for each connection of contextclient
         std::map<int, StdSize> getGridDataBufferSize(CContextClient* client, bool bufferForWriting = false);
         
         // evaluation the size of the buffer for the field
         bool evaluateBufferSize(map<CContextClient*,map<int,size_t>>& evaluateBuffer, bool isOptPerformance) ;
       public:
          void makeGridAliasForCoupling(void) ;
       public:
         bool isActive(bool atCurrentTimestep = false) const;
         bool hasOutputFile;

         bool wasWritten() const;
         void setWritten();

         bool getUseCompressedOutput() const;
         void setUseCompressedOutput();

         /// Traitements ///
         void solveGridReference(void);
         void solveServerOperation(void);
         void solveCheckMaskIndex(bool doSendingIndex);
         void solveGridDomainAxisRef(bool checkAtt);
         void solveGridDomainAxisBaseRef();

         void checkGridOfEnabledFields();
         void sendGridOfEnabledFields();
         void sendGridComponentOfEnabledFields();

         void sendFieldToFileServer(void) ;
         void sendCloseDefinition(void) ;
      
      public:
         void sendFieldToCouplerOut(void) ;
      private:
         bool sendFieldToCouplerOut_done_=false;
      public:

         void sendFieldToInputFileServer(void) ;

         /// Vérifications ///
         void checkTimeAttributes(CDuration* freqOp=NULL);

         bool buildWorkflowGraph(CGarbageCollector& gc) ;
         bool buildWorkflowGraphDone_ = false ;

         size_t getGlobalWrittenSize(void) ;
         
         
         std::shared_ptr<COutputPin> getSelfReference(CGarbageCollector& gc);
         std::shared_ptr<COutputPin> getTemporalDataFilter(CGarbageCollector& gc, CDuration outFreq);
         std::shared_ptr<COutputPin> getSelfTemporalDataFilter(CGarbageCollector& gc, CDuration outFreq);

//         virtual void fromBinary(StdIStream& is);

         /// Destructeur ///
         virtual ~CField(void);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);

         static ENodeType GetType(void);

        template <int N> void setData(const CArray<double, N>& _data);
        static bool dispatchEvent(CEventServer& event);
        static bool isCollectiveEvent(CEventServer& event);
        void sendAllAttributesToServer(CContextClient* client) ; 
        
        static void recvUpdateData(CEventServer& event);
        void receiveUpdateData(CEventServer& event);  

        bool sendReadDataRequest(const CDate& tsDataRequested);
        bool sendReadDataRequestIfNeeded(void);
        static void recvReadDataRequest(CEventServer& event);
        void recvReadDataRequest(void);
        static void recvReadDataReady(CEventServer& event);
        void receiveReadDataReady(CEventServer& event);
        void recvReadDataReady(vector<int> ranks, vector<CBufferIn*> buffers); // old interface to remove
        void recvDataFromCoupler(vector<int> ranks, vector<CBufferIn*> buffers) ; // old interface to remove
        void checkForLateDataFromServer(void);
        void checkForLateDataFromCoupler(void) ;

        void checkIfMustAutoTrigger(void); // ym obsolete
        void autoTriggerIfNeeded(void); //ym obsolete
        void triggerLateField(void) ;

        void parse(xml::CXMLNode& node);

        void setVirtualVariableGroup(CVariableGroup* newVVariableGroup);
        CVariableGroup* getVirtualVariableGroup(void) const;
        vector<CVariable*> getAllVariables(void) const;
        virtual void solveDescInheritance(bool apply, const CAttributeMap* const parent = 0);

        CVariable* addVariable(const string& id = "");
        CVariableGroup* addVariableGroup(const string& id = "");        
        void sendAddVariable(const string& id, CContextClient* client);
        void sendAddVariableGroup(const string& id, CContextClient* client);
        static void recvAddVariable(CEventServer& event);
        void recvAddVariable(CBufferIn& buffer);
        static void recvAddVariableGroup(CEventServer& event);
        void recvAddVariableGroup(CBufferIn& buffer);        
        void sendAddAllVariables(CContextClient* client);

        const std::vector<StdString>& getRefDomainAxisIds();

        const string& getExpression(void);
        bool hasExpression(void) const;

        bool hasGridMask(void) const;
        
        void connectToFileServer(CGarbageCollector& gc) ;
        void connectToCouplerOut(CGarbageCollector& gc) ;
        void connectToCouplerIn(CGarbageCollector& gc) ;
        void connectToModelInput(CGarbageCollector& gc) ;
        void connectToFileWriter(CGarbageCollector& gc) ;
        void connectToClientInput(CGarbageCollector& gc) ;
        void connectToServerInput(CGarbageCollector& gc) ;
        void connectToModelOutput(CGarbageCollector& gc);
        void connectToFileReader(CGarbageCollector& gc) ;
        void connectToServerToClient(CGarbageCollector& gc) ;

        void setContextClientDataBufferSize(map<CContextClient*,map<int,size_t>>& bufferSize, 
                                        map<CContextClient*,map<int,size_t>>& maxEventSize, 
                                        bool bufferForWriting) ;
        void setContextClientAttributesBufferSize(map<CContextClient*,map<int,size_t>>& bufferSize, 
                                                 map<CContextClient*,map<int,size_t>>& maxEventSize, 
                                                 bool bufferForWriting) ;
      private:
          bool isGridCompleted_ = true ;
      public:
          bool isGridCompleted()  ; 
          void setGridCompleted(void) { isGridCompleted_= true; }
          void unsetGridCompleted(void) { isGridCompleted_ = false ;}
      
      public:     
          void sendGridCompleted(void) ;
      private:   
          static void recvGridCompleted(CEventServer& event);
          void recvGridCompleted(CBufferIn& buffer);


      private:
        std::vector<CGrid*> getGridPath(void) ;

      public:
         /// Propriétés privées ///
         CVariableGroup* vVariableGroup;

         CGrid*  grid_=nullptr;
         CGrid* getGrid(void) { return grid_; } 
      
      private:
         CGrid* sentGrid_=nullptr ;
      public:
         CGrid* getSentGrid(void) { return sentGrid_; }    

      public:
//         CFile*  file;
         
         CFile* fileIn_ = nullptr ; //<! pointer to input related file
         bool hasFileIn(void) const { return fileIn_==nullptr ? false : true ;} 
         CFile* getFileIn(void) {return fileIn_;}
         void setFileIn(CFile* fileIn) { fileIn_ = fileIn ;}
         void unsetFileIn(void) { fileIn_ = nullptr ;}

         CFile* fileOut_ = nullptr ; //<! pointer to output related file
         bool hasFileOut(void) const { return fileOut_==nullptr ? false : true ;} 
         CFile* getFileOut(void) {return fileOut_;}
         void setFileOut(CFile* fileOut) { fileOut_ = fileOut ;}
         void unsetFileOut(void) { fileOut_ = nullptr ;}

         CCouplerIn* couplerIn_ = nullptr ; //<!pointer to input related coupler
         bool hasCouplerIn(void) const { return couplerIn_==nullptr ? false : true ;}
         CCouplerIn* getCouplerIn(void) {return couplerIn_;}
         void setCouplerIn(CCouplerIn* couplerIn) { couplerIn_ = couplerIn ;}
         void unsetCouplerIn(void) { couplerIn_ = nullptr ;}

         CCouplerOut* couplerOut_ = nullptr ; //<!pointer to output related coupler
         bool hasCouplerOut(void) const { return couplerOut_==nullptr ? false : true ;}
         CCouplerOut* getCouplerOut(void) {return couplerOut_;}
         void setCouplerOut(CCouplerOut* couplerOut) { couplerOut_ = couplerOut ;}
         void unsetCouplerOut(void) { couplerOut_ = nullptr ;}

         bool modelIn_ = false ; //<! field can be received from model == true 
         bool getModelIn(void) { return modelIn_ ;}
         void setModelIn(void) { modelIn_ = true ;}
         void unsetModelIn(void) { modelIn_ = false ;}
         
         bool modelOut_ = false ; //<! field can be retrieve to model == true
         bool getModelOut(void) { return modelOut_ ;}
         void setModelOut(void) { modelOut_ = true ;}
         void unsetModelOut(void) { modelOut_ = false ;}

         
         bool written; //<! Was the field written at least once
         bool mustAutoTrigger;

         string content;

         std::vector<StdString> domAxisScalarIds_;
         bool useCompressedOutput;

         // Two variables to identify the time_counter meta data written in file, which has no time_counter
         bool hasTimeInstant;
         bool hasTimeCentered;


         DECLARE_REF_FUNC(Field,field)
        
      private:
         CContextClient* client;
      public:
         void setContextClient(CContextClient* newContextClient);
         CContextClient* getContextClient(void) {return client;}

      private:

         bool areAllReferenceSolved;
         bool isReferenceSolved;
         bool isReferenceSolvedAndTransformed;
         bool isGridChecked;

       private: 
         //! define if the field is part of the active workflow. It will be tagged to true when CField::buildWorkflowGraph is successfull 
         bool workflowEnabled_ = false ;
       public: 
         /*! workflowEnabled_ public accessor
          * \return Value of workflowEnabled_ */
         bool getWorkflowEnabled(void) { return  workflowEnabled_; }
 

      private:
     
         //! The type of operation attached to the field
         func::CFunctor::ETimeType operationTimeType;

         //! The output pin of the input filter of the field
         std::shared_ptr<CPassThroughFilter> inputFilter;

         //! The self temporal data filter
         std::shared_ptr<CTemporalFilter> selfTemporalDataFilter ;
         
         //! The output pin of the filter providing the instant data for the field
         std::shared_ptr<COutputPin> instantDataFilter;
      public:
          std::shared_ptr<COutputPin> getInstantDataFilter(void) { return instantDataFilter;}

      private:

         //! The output pin of the filters providing the result of the field's temporal operation
         std::map<CDuration, std::shared_ptr<COutputPin>, DurationFakeLessComparator> temporalDataFilters;
         
         //! The output pin of the filter providing the instant data for self references
         std::shared_ptr<COutputPin> selfReferenceFilter; // probably redondant with inputFilter

         //! The source filter for data provided by the client
//         std::shared_ptr<CSourceFilter> clientSourceFilter; // obsolete to remove
 
         //! The source filter for data provided by the model to enter the client workflow
         std::shared_ptr<CModelToClientSourceFilter> modelToClientSourceFilter_;

         //! The source filter for data provided by the model to enter the client workflow
         std::shared_ptr<CClientToModelStoreFilter> clientToModelStoreFilter_;

         //! The source filter for data provided by the client that send data to server workflow
         std::shared_ptr<CServerFromClientSourceFilter> serverFromClientSourceFilter_;

         //! The source filter for data provided by an other to enter the current client workflow (coupling mode)
         std::shared_ptr<CClientFromClientSourceFilter> clientFromClientSourceFilter_;

         //! The source filter for data provided by server to enter the current client workflow (reading mode)
         std::shared_ptr<CClientFromServerSourceFilter> clientFromServerSourceFilter_;
         
         //! The source filter for data read from file on server side 
         std::shared_ptr<CFileReaderSourceFilter> fileReaderSourceFilter_;

         //! The source filter for data provided by the server
//         std::shared_ptr<CSourceFilter> serverSourceFilter; // obsolete to remove
        
         //! The terminal filter which send data to server for writing
         std::shared_ptr<CClientToServerStoreFilter> clientToServerStoreFilter_;
        
         //! The terminal filter which writes data to file
         std::shared_ptr<CFileWriterStoreFilter> fileWriterStoreFilter_;

         //! The terminal filter which send data from server to client
         std::shared_ptr<CServerToClientStoreFilter> serverToClientStoreFilter_;


   }; // class CField

   ///--------------------------------------------------------------

   // Declare/Define CFieldGroup and CFieldDefinition
   DECLARE_GROUP(CField);

   ///-----------------------------------------------------------------

   template <>
      void CGroupTemplate<CField, CFieldGroup, CFieldAttributes>::solveRefInheritance(void);

   ///-----------------------------------------------------------------
} // namespace xios


#endif // __XIOS_CField__
