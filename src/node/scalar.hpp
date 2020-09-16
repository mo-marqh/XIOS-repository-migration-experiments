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

#include "element.hpp"
#include "local_connector.hpp"
#include "scatterer_connector.hpp"
#include "gatherer_connector.hpp"
#include "distribution_type.hpp"


namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///

      class CScalarGroup;
      class CScalarAttributes;
      class CScalar;
      ///--------------------------------------------------------------

      // Declare/Define CVarAttribute
      BEGIN_DECLARE_ATTRIBUTE_MAP(CScalar)
#include "scalar_attribute.conf"
      END_DECLARE_ATTRIBUTE_MAP(CScalar)

      ///--------------------------------------------------------------

      class CScalar
         : public CObjectTemplate<CScalar>
         , public CScalarAttributes
      {
            /// typedef ///
            typedef CObjectTemplate<CScalar>   SuperClass;
            typedef CScalarAttributes SuperClassAttribute;

            friend class CScalarGroup;

         public :

            typedef CScalarAttributes RelAttributes;
            typedef CScalarGroup      RelGroup;
            typedef CTransformation<CScalar>::TransformationMapTypes TransMapTypes;

            /// Constructeurs ///
            CScalar(void);
            explicit CScalar(const StdString & id);
            CScalar(const CScalar & var);       // Not implemented yet.
            CScalar(const CScalar * const var); // Not implemented yet.

            /// Destructeur ///
            virtual ~CScalar(void);

         public :
            /// Accesseurs statiques ///
            static StdString GetName(void);
            static StdString GetDefName(void);
            static ENodeType GetType(void);

         public:
            static CScalar* createScalar();

         public:
            void checkAttributes(void);
            bool checkAttributes_done_ = false ;
            
            void addRelFile(const StdString& filename);
            bool IsWritten(const StdString& filename) const;
            void checkAttributesOnClient();
            virtual void parse(xml::CXMLNode & node);
        
         public:
            void sendScalarToFileServer(CContextClient* client) ;
         private:
            std::set<CContextClient*> sendScalarToFileServer_done_ ;

         public:
            void sendScalarToCouplerOut(CContextClient* client, const string& fieldId, int posInGrid) ;
         private:
            std::set<CContextClient*> sendScalarToCouplerOut_done_ ;
         
         public:
            void makeAliasForCoupling(const string& fieldId, int posInGrid) ;

         public:
           bool hasTransformation();
           void solveInheritanceTransformation();
           TransMapTypes getAllTransformations();
           void duplicateTransformation(CScalar*);
           CTransformation<CScalar>* addTransformation(ETranformationType transType, const StdString& id="");
           bool isEqual(CScalar* scalar);
        private:  
          /** Clients that have to send a scalar. There can be multiple clients in case of secondary server, otherwise only one client. */
         std::list<CContextClient*> clients;
         std::set<CContextClient*> clientsSet;
        public:
          void setContextClient(CContextClient* contextClient) ;
        
         private:
           std::set<StdString> relFiles;
           TransMapTypes transformationMap_;

            void setTransformations(const TransMapTypes&);

       private:
           static bool initializeTransformationMap(std::map<StdString, ETranformationType>& m);
           static std::map<StdString, ETranformationType> transformationMapList_;
           static bool dummyTransformationMapList_;

           /** define if the scalar is completed or not ie all attributes have been received before in case 
               of grid reading from file or coupling */ 
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
           if (hasDirectScalarReference()) if (!getDirectScalarReference()->isCompleted()) return false;
           else return isCompleted_ ;
         }
         void setCompleted(void) { isCompleted_=true ; }
         void unsetCompleted(void) { isCompleted_=false ; }



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
         void distributeToServer(CContextClient* client, std::map<int, CArray<size_t,1>>& globalIndex) ;
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
         CGathererConnector* serverFromClientConnector_ ;
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

      private:
            DECLARE_REF_FUNC(Scalar,scalar)

      }; // class CVar
      ///--------------------------------------------------------------

      // Declare/Define CScalarGroup and CScalarDefinition
      DECLARE_GROUP(CScalar);
} // namespace xios

#endif // __XIOS_CScalar__
