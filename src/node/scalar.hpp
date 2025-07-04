#ifndef __XIOS_CScalar__
#define __XIOS_CScalar__

/// XIOS headers ///
#include "xios_spl.hpp"
#include "declare_group.hpp"
#include "declare_ref_func.hpp"
#include "group_template.hpp"
#include "array_new.hpp"
#include "attribute_enum.hpp"
#include "attribute_enum_impl.hpp"
#include "attribute_array.hpp"
#include "transformation.hpp"
#include "transformation_enum.hpp"
#include "transformation_path.hpp"
#include "element.hpp"
#include "local_connector.hpp"
#include "scatterer_connector.hpp"
#include "gatherer_connector.hpp"
#include "distribution_type.hpp"
#include "generic_algorithm_transformation.hpp"
#include "grid_transformation_factory_impl.hpp"


namespace xios
{
  /// ////////////////////// Déclarations ////////////////////// ///

  class CScalarGroup;
  class CScalarAttributes;
  class CScalar;
  class CField;
  ///--------------------------------------------------------------

  // Declare/Define CVarAttribute
  BEGIN_DECLARE_ATTRIBUTE_MAP(CScalar)
#include "scalar_attribute.conf"
  END_DECLARE_ATTRIBUTE_MAP(CScalar)

  ///--------------------------------------------------------------

  class CScalar: public CObjectTemplate<CScalar>
               , public CScalarAttributes
  {
      friend class CScalarGroup;

      /// typedef ///
      typedef CObjectTemplate<CScalar>   SuperClass;
      typedef CScalarAttributes SuperClassAttribute;

    public:
      enum EEventId
      {
        EVENT_ID_COLLECTIVE=100,
        EVENT_ID_SCALAR_DISTRIBUTION,
        EVENT_ID_SEND_DISTRIBUTED_ATTRIBUTE,
        EVENT_ID_NO_COLLECTIVE=1000,
      } ;
      static bool dispatchEvent(CEventServer& event);      
           

    public :

      typedef CScalarAttributes RelAttributes;
      typedef CScalarGroup      RelGroup;

      /// Constructeurs ///
      CScalar(void);
      explicit CScalar(const StdString & id);
      CScalar(const CScalar & var);       // Not implemented yet.
      CScalar(const CScalar * const var); // Not implemented yet.
      static void releaseStaticAllocation(void) ; // release static allocation on heap
      
      /// Destructeur ///
      virtual ~CScalar(void);

    public :
      /// Accesseurs statiques ///
      static StdString GetName(void);
      static StdString GetDefName(void);
      static ENodeType GetType(void);

    public:
      static CScalar* createScalar();
      static CScalar* get(const string& id, bool noError=false) ; //<! return scalar pointer using id
      static bool     has(const string& id) ; //<! return domain pointer using id
      static CField*  getFieldFromId(const string& id) ;

    public:
      void checkAttributes(bool recheck=false);
      bool checkAttributes_done_ = false ;
            
      void addRelFile(const StdString& filename);
      bool IsWritten(const StdString& filename) const;
      virtual void parse(xml::CXMLNode & node);
        
    public:
      void sendScalarToCouplerOut(CContextClient* client, const string& fieldId, int posInGrid) ;
    private:
      std::set<CContextClient*> sendScalarToCouplerOut_done_ ;
         
    public:
      void makeAliasForCoupling(const string& fieldId, int posInGrid) ;
      string getCouplingAlias(const string& fieldId, int posInGrid) ;

      ////////////////////////////////
      ////    TRANSFORMATIONS     ////
      //////////////////////////////// 
    public:
      typedef CTransformation<CScalar>::TransformationMapTypes TransMapTypes;
    private:
      static bool initializeTransformationMap(std::map<StdString, ETranformationType>& m);
      static std::map<StdString, ETranformationType> transformationMapList_;
      static bool dummyTransformationMapList_;      TransMapTypes transformationMap_;
      void setTransformations(const TransMapTypes&);

    public:
      bool hasTransformation();
      TransMapTypes getAllTransformations();
      void duplicateTransformation(CScalar*);
      CTransformation<CScalar>* addTransformation(ETranformationType transType, const StdString& id="");
    
       void solveInheritanceTransformation_old();
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
      ////////////////////////////////
      ////////////////////////////////

      bool isEqual(CScalar* scalar);

    public:
      bool hasValue() { return hasValue_; }
      bool hasBounds() { return hasBounds_; }
      bool hasLabel() { return hasLabel_; }
    private:
      bool hasValue_  = false ;
      bool hasBounds_ = false ;
      bool hasLabel_  = false ;
    private:  
          /** Clients that have to send a scalar. There can be multiple clients in case of secondary server, otherwise only one client. */
      std::list<CContextClient*> clients;
      std::set<CContextClient*> clientsSet;
    public:
      void setContextClient(CContextClient* contextClient) ;
        
    private:
      std::set<StdString> relFiles;

    private:
      /** define if the scalar is completed or not ie all attributes have been received before in case 
           of grid reading from file or coupling */ 
      bool isCompleted_=true ;  
    public:     
      /*!
        \brief Check if a scalar is completed
        Before make any scalar processing, we must be sure that all scalar informations have
        been sent, for exemple when reading a grid in a file or when grid elements are sent by an
        other context (coupling). So all direct reference of the scalar (scalar_ref) must be also completed
        \return true if scalar and scalar reference are completed
      */
      bool isCompleted(void)
      {
        if (hasDirectScalarReference())
	{
	  if (!getDirectScalarReference()->isCompleted()) return false;
	}
        else
	{
	  return isCompleted_ ;
	}

        MISSING_RETURN( "bool CAxis::isCompleted() " );
        return true;
      }
      void setCompleted(void) { isCompleted_=true ; }
      void unsetCompleted(void) { isCompleted_=false ; }



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
      void distributeToServer(CContextClient* client, bool inOut, std::map<int, CArray<size_t,1>>& globalIndexOut, std::map<int, CArray<size_t,1>>& globalIndexIn,
                              shared_ptr<CScattererConnector> &scattererConnector, const string& scalarId="") ;

      static void recvScalarDistribution(CEventServer& event) ;
      void receivedScalarDistribution(CEventServer& event, int phasis) ;
      void setServerMask(CArray<bool,1>& serverMask, CContextClient* client) ;
      void sendDistributedAttributes(CContextClient* client, shared_ptr<CScattererConnector> scattererConnector, const string& scalarId) ;
      static void recvDistributedAttributes(CEventServer& event) ;
      void recvDistributedAttributes(CEventServer& event, const string& type) ;

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
     shared_ptr<CDistributedElement> elementTo_ ;
    public: 
      shared_ptr<CScattererConnector> getServerToClientConnector(void) { return serverToClientConnector_ ;} 

    private:
      map<CContextClient*,shared_ptr<CGathererConnector>>  clientFromServerConnector_  ;
    public: 
      shared_ptr<CGathererConnector> getClientFromServerConnector(CContextClient* client) { return clientFromServerConnector_[client] ;} 

    private:
      DECLARE_REF_FUNC(Scalar,scalar)

  }; // class CVar
  ///--------------------------------------------------------------

  // Declare/Define CScalarGroup and CScalarDefinition
  DECLARE_GROUP(CScalar);
} // namespace xios

#endif // __XIOS_CScalar__
