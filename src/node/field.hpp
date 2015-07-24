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


namespace xios {

   /// ////////////////////// Déclarations ////////////////////// ///

   class CFieldGroup;
   class CFieldAttributes;
   class CField;

   class CFile;
   class CGrid;
   class CContext;
   class CGenericFilter;

   class CGarbageCollector;
   class COutputPin;
   class CSourceFilter;
   class CStoreFilter;
   class CFileWriterFilter;

   ///--------------------------------------------------------------

   // Declare/Define CFieldAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CField)
#  include "field_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CField)

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

         typedef CFieldAttributes RelAttributes;
         typedef CFieldGroup      RelGroup;

         enum EEventId
         {
           EVENT_ID_UPDATE_DATA, EVENT_ID_READ_DATA, EVENT_ID_READ_DATA_READY,
           EVENT_ID_ADD_VARIABLE, EVENT_ID_ADD_VARIABLE_GROUP
         };

         /// Constructeurs ///
         CField(void);
         explicit CField(const StdString& id);
         CField(const CField& field);       // Not implemented yet.
         CField(const CField* const field); // Not implemented yet.

         /// Accesseurs ///

         CGrid* getRelGrid(void) const;
         CFile* getRelFile(void) const;

         func::CFunctor::ETimeType getOperationTimeType() const;

      public :

         StdSize getNStep(void) const;

         template <int N> void getData(CArray<double, N>& _data) const;

         boost::shared_ptr<COutputPin> getInstantDataFilter();

         /// Mutateur ///
         void setRelFile(CFile* _file);
         void incrementNStep(void);
         void resetNStep();
         void resetNStepMax();

         std::map<int, StdSize> getGridDataSize();

       public :
         bool isActive(void) const;
         bool hasOutputFile;

         /// Traitements ///
         void solveGridReference(void);
         void solveServerOperation(void);
         void solveCheckMaskIndex(bool doSendingIndex);
         void solveAllReferenceEnabledField(bool doSending2Sever);
         void solveGridDomainAxisRef(bool checkAtt);
         void solveTransformedGrid();

         void buildFilterGraph(CGarbageCollector& gc, bool enableOutput);
         boost::shared_ptr<COutputPin> getSelfReference(CGarbageCollector& gc);
         boost::shared_ptr<COutputPin> getTemporalDataFilter(CGarbageCollector& gc, CDuration outFreq);

//         virtual void fromBinary(StdIStream& is);

         /// Destructeur ///
         virtual ~CField(void);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);

         static ENodeType GetType(void);

        template <int N> void setData(const CArray<double, N>& _data);
        static bool dispatchEvent(CEventServer& event);
        void sendUpdateData(const CArray<double,1>& data);
        static void recvUpdateData(CEventServer& event);
        void recvUpdateData(vector<int>& ranks, vector<CBufferIn*>& buffers);
        void writeField(void);
        void sendReadDataRequest(void);
        bool sendReadDataRequestIfNeeded(void);
        static void recvReadDataRequest(CEventServer& event);
        void recvReadDataRequest(void);
        bool readField(void);
        static void recvReadDataReady(CEventServer& event);
        void recvReadDataReady(vector<int> ranks, vector<CBufferIn*> buffers);
        void outputField(CArray<double,3>& fieldOut);
        void outputField(CArray<double,2>& fieldOut);
        void outputField(CArray<double,1>& fieldOut);
        void inputField(CArray<double,3>& fieldOut);
        void inputField(CArray<double,2>& fieldOut);
        void inputField(CArray<double,1>& fieldOut);
        void scaleFactorAddOffset(double scaleFactor, double addOffset);
        void invertScaleFactorAddOffset(double scaleFactor, double addOffset);
        void parse(xml::CXMLNode& node);

        void setVirtualVariableGroup(CVariableGroup* newVVariableGroup);
        void setVirtualVariableGroup(void);
        CVariableGroup* getVirtualVariableGroup(void) const;
        vector<CVariable*> getAllVariables(void) const;
        virtual void solveDescInheritance(bool apply, const CAttributeMap* const parent = 0);

        CVariable* addVariable(const string& id = "");
        CVariableGroup* addVariableGroup(const string& id = "");
        void sendAddVariable(const string& id = "");
        void sendAddVariableGroup(const string& id = "");
        static void recvAddVariable(CEventServer& event);
        void recvAddVariable(CBufferIn& buffer);
        static void recvAddVariableGroup(CEventServer& event);
        void recvAddVariableGroup(CBufferIn& buffer);
        void sendAddAllVariables();


        const std::pair<StdString, StdString>& getRefDomainAxisIds();

      public :
         /// Propriétés privées ///
         CVariableGroup* vVariableGroup;

         CGrid*  grid;
         CFile*  file;

         CDuration freq_operation_srv, freq_write_srv;

         StdSize nstep, nstepMax;
         bool isEOF;
         CDate lastlast_Write_srv, last_Write_srv, last_operation_srv;
         CDate lastDataRequestedFromServer;

         map<int,boost::shared_ptr<func::CFunctor> > foperation_srv;

         map<int, CArray<double,1> > data_srv;
         string content;

         bool areAllReferenceSolved;
         std::pair<StdString,StdString> domAxisIds_;
         bool isReadDataRequestPending;

         DECLARE_REF_FUNC(Field,field)

      private:
         //! The type of operation attached to the field
         func::CFunctor::ETimeType operationTimeType;

         //! The output pin of the filter providing the instant data for the field
         boost::shared_ptr<COutputPin> instantDataFilter;
         //! The output pin of the filters providing the result of the field's temporal operation
         std::map<CDuration, boost::shared_ptr<COutputPin>, DurationFakeLessComparator> temporalDataFilters;
         //! The source filter for data provided by the client
         boost::shared_ptr<CSourceFilter> clientSourceFilter;
         //! The source filter for data provided by the server
         boost::shared_ptr<CSourceFilter> serverSourceFilter;
         //! The terminal filter which stores the instant data
         boost::shared_ptr<CStoreFilter> storeFilter;
         //! The terminal filter which writes the data to file
         boost::shared_ptr<CFileWriterFilter> fileWriterFilter;
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
