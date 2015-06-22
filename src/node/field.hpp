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
#include "expr_node.hpp"
#include "declare_ref_func.hpp"
#include "generic_filter.hpp"
#include "transformation_enum.hpp"


namespace xios {

   /// ////////////////////// Déclarations ////////////////////// ///

   class CFieldGroup;
   class CFieldAttributes;
   class CField;

   class CFile;
   class CGrid;
   class CContext ;
   class CGenericFilter;

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
         } ;

         /// Constructeurs ///
         CField(void);
         explicit CField(const StdString & id);
         CField(const CField & field);       // Not implemented yet.
         CField(const CField * const field); // Not implemented yet.

         /// Accesseurs ///

         CGrid* getRelGrid(void) const ;
         CFile* getRelFile(void) const ;

      public :

         StdSize getNStep(void) const;

         const CDuration & getFreqOperation(void) const;
         const CDuration & getFreqWrite(void) const;

         boost::shared_ptr<CDate> getLastWriteDate(void) const;
         boost::shared_ptr<CDate> getLastOperationDate(void) const;

         boost::shared_ptr<func::CFunctor> getFieldOperation(void) const;

         CArray<double, 1> getData(void) const;
         template <int N> void getData(CArray<double, N>& _data) const;


         /// Mutateur ///
         void setRelFile(CFile* _file);
         void incrementNStep(void);
         void resetNStep() ;
         void resetNStepMax();

         template <int N> bool updateData(const CArray<double, N>&   data);
         template <int N> bool updateFilteredData(CArray<double, N>&   data);
         template<int N>
         void updateDataWithoutOperation(const CArray<double, N>& _data, CArray<double,1>& updatedData);
         bool updateDataFromExpression(const CArray<double, 1>&   data);
         void setDataFromExpression(const CArray<double, 1>& _data) ;

         bool updateDataServer
               (const CDate & currDate,
                const std::deque< CArray<double, 1>* > storedClient);

         std::map<int, StdSize> getGridDataSize();

       public :
         bool isActive(void) const;
         bool active ;
         bool hasOutputFile ;
         bool hasFieldOut ;

         /// Traitements ///
         void processEnabledField(void) ;

         void solveGridReference(void);
         void solveOperation(void);
         void solveCheckMaskIndex(bool doSendingIndex);
         void solveAllReferenceEnabledField(bool doSending2Sever);
         void buildAllExpressionEnabledField();
         void solveGridDomainAxisRef(bool checkAtt);
         void solveTransformedGrid();
         CGrid* getGridRefOfBaseReference();

//         virtual void fromBinary(StdIStream & is);

         /// Destructeur ///
         virtual ~CField(void);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);

         static ENodeType GetType(void);

        template <int N> void setData(const CArray<double, N>& _data) ;
        static bool dispatchEvent(CEventServer& event) ;
        void sendUpdateData(void) ;
        static void recvUpdateData(CEventServer& event) ;
        void recvUpdateData(vector<int>& ranks, vector<CBufferIn*>& buffers) ;
        void writeField(void) ;
        void sendReadDataRequest(void);
        bool sendReadDataRequestIfNeeded(void);
        static void recvReadDataRequest(CEventServer& event);
        void recvReadDataRequest(void);
        bool readField(void);
        static void recvReadDataReady(CEventServer& event);
        void recvReadDataReady(vector<int> ranks, vector<CBufferIn*> buffers);
        void outputField(CArray<double,3>& fieldOut) ;
        void outputField(CArray<double,2>& fieldOut) ;
        void outputField(CArray<double,1>& fieldOut) ;
        void inputField(CArray<double,3>& fieldOut);
        void inputField(CArray<double,2>& fieldOut);
        void inputField(CArray<double,1>& fieldOut);
        void scaleFactorAddOffset(double scaleFactor, double addOffset) ;
        void invertScaleFactorAddOffset(double scaleFactor, double addOffset);
        void parse(xml::CXMLNode & node) ;
        CArray<double,1>* getInstantData(void)  ;

        void setVirtualVariableGroup(CVariableGroup* newVVariableGroup);
        void setVirtualVariableGroup(void);
        CVariableGroup* getVirtualVariableGroup(void) const;
        vector<CVariable*> getAllVariables(void) const;
        virtual void solveDescInheritance(bool apply, const CAttributeMap * const parent = 0);

        CVariable* addVariable(const string& id="") ;
        CVariableGroup* addVariableGroup(const string& id="") ;
        void sendAddVariable(const string& id="") ;
        void sendAddVariableGroup(const string& id="") ;
        static void recvAddVariable(CEventServer& event) ;
        void recvAddVariable(CBufferIn& buffer) ;
        static void recvAddVariableGroup(CEventServer& event) ;
        void recvAddVariableGroup(CBufferIn& buffer) ;
        void sendAddAllVariables();


        const std::pair<StdString, StdString>& getRefDomainAxisIds();

        const std::vector<CField*>& getFilterSources();
        void applyFilter(const CArray<double, 1>& dataToSend, CArray<double,1>& dataToReceive);
        void sendAndReceiveTransformedData(const std::map<int, CArray<int,1>* >& localIndexToSend,
                                           const CArray<double, 1>& dataSrc,
                                           const std::map<int, std::vector<CArray<int,1>* > >& localIndexToReceive,
                                           CArray<double,1>& dataDest);
      public :
         /// Propriétés privées ///
         CVariableGroup* vVariableGroup ;

         CGrid*  grid ;
         CFile*  file;
         CField* fieldOut ;

         CDuration freq_operation, freq_write;
         CDuration freq_operation_srv, freq_write_srv;

         StdSize nstep, nstepMax;
         bool isEOF;
         boost::shared_ptr<CDate>    last_Write, last_operation;
         boost::shared_ptr<CDate>    lastlast_Write_srv,last_Write_srv, last_operation_srv;
         CDate lastDataRequestedFromServer;

         boost::shared_ptr<func::CFunctor> foperation;
         map<int,boost::shared_ptr<func::CFunctor> > foperation_srv;

         CArray<double, 1> data;
         CArray<double, 1> instantData;
         CArray<double, 1> filteredData;
         bool hasInstantData ;
         map<int, CArray<double,1>* > data_srv ;
         bool isOnceOperation ;
         bool isFirstOperation ;
         string content ;

         list< pair<CField *,int> > fieldDependency ;
         void buildExpression(void) ;
         void addDependency(CField* field, int slotId) ;
         void resetSlots(void) ;
         vector<bool> slots ;
         CDate* slotUpdateDate ;
         CFieldNode * expression ;
         bool hasExpression ;
         bool slotsFull(void) ;
         void setSlot(int slotId);
         bool processed ;
         bool areAllReferenceSolved;
         bool areAllExpressionBuilt;
         std::pair<StdString,StdString> domAxisIds_;
         bool isReadDataRequestPending;
         std::vector<CField*> filterSources_;
         std::vector<CGenericAlgorithm*> algorithms_;
         std::vector<ETransformationType> transformations_;
         DECLARE_REF_FUNC(Field,field)

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
