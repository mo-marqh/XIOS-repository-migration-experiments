#ifndef __XMLIO_CGrid__
#define __XMLIO_CGrid__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "group_factory.hpp"

#include "declare_group.hpp"
#include "domain.hpp"
#include "axis.hpp"
#include "array_new.hpp"
#include "attribute_array.hpp"
#include "distribution_client.hpp"
#include "distribution_server.hpp"
#include "server_distribution_description.hpp"
#include "client_server_mapping.hpp"

namespace xios {

   /// ////////////////////// Déclarations ////////////////////// ///

   class CGridGroup;
   class CGridAttributes;
   class CDomainGroup;
   class CAxisGroup;
   class CGrid;
   class CDistributionClient;
   class CDistributionServer;
   class CServerDistributionDescription;
   class CClientServerMapping;

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

      public :

         typedef CGridAttributes RelAttributes;
         typedef CGridGroup      RelGroup;

         enum EEventId
         {
           EVENT_ID_INDEX, EVENT_ID_ADD_DOMAIN, EVENT_ID_ADD_AXIS
         } ;

         /// Constructeurs ///
         CGrid(void);
         explicit CGrid(const StdString & id);
         CGrid(const CGrid & grid);       // Not implemented yet.
         CGrid(const CGrid * const grid); // Not implemented yet.

         /// Traitements ///
//         void solveReference(void);

         void solveDomainAxisRef(bool areAttributesChecked);

         void checkMaskIndex(bool doCalculateIndex);

 //        virtual void toBinary  (StdOStream & os) const;
//         virtual void fromBinary(StdIStream & is);

         /// Tests ///
         bool hasAxis(void) const;

      public :

         /// Accesseurs ///
         const std::deque< CArray<int, 1>* > & getStoreIndex(void) const;
         const std::deque< CArray<int, 1>* > & getOutIIndex(void)  const;
         const std::deque< CArray<int, 1>* > & getOutJIndex(void)  const;
         const std::deque< CArray<int, 1>* > & getOutLIndex(void)  const;

         const CAxis*   getRelAxis  (void) const;
         const CDomain* getRelDomain(void) const;

         StdSize getDimension(void) const;

//         StdSize getLocalSize(void) const;
//         StdSize getGlobalSize(void) const;
         StdSize  getDataSize(void) const;
//         std::vector<StdSize> getLocalShape(void) const;
//         std::vector<StdSize> getGlobalShape(void) const;

         /// Entrées-sorties de champs ///
         template <int n>
            void inputField(const CArray<double,n>& field, CArray<double,1>& stored) const;

         void inputFieldServer(const std::deque< CArray<double, 1>* > storedClient,
                               CArray<double, 1>&  storedServer) const;

         void outputField(int rank, const CArray<double,1>& stored,  CArray<double,3>& field)  ;
         void outputField(int rank, const CArray<double,1>& stored,  CArray<double,2>& field)  ;
         void outputField(int rank, const CArray<double,1>& stored,  CArray<double,1>& field)  ;
         void outputField(int rank, const CArray<double,1>& stored,  double* field);

         virtual void parse(xml::CXMLNode & node);

         /// Destructeur ///
         virtual ~CGrid(void);

      public :

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);

         static ENodeType GetType(void);

         /// Instanciateurs Statiques ///
         static CGrid* createGrid(CDomain* domain);
         static CGrid* createGrid(CDomain* domain, CAxis* axis);
         static CGrid* createGrid(std::vector<CDomain*> domains, std::vector<CAxis*> axis);

      public :

         /// Entrées-sorties de champs (interne) ///
         void storeField_arr(const double * const data, CArray<double,1>& stored) const;

         /// Traitements protégés ///
         void computeIndexServer(void);
         void computeIndex(void);
//         void solveDomainRef(void);  //TODO temporarily comment
//         void solveAxisRef(void);   // TODO: temporarily comment

         void solveDomainRef(bool checkAtt);
         void solveAxisRef(bool checkAtt);
         void solveDomainAxisRefInheritance(bool apply = true);

         void sendAddDomain(const std::string& id="");
         void sendAddAxis(const std::string& id="");
         void sendAllDomains();
         void sendAllAxis();

         static void recvAddDomain(CEventServer& event) ;
         void recvAddDomain(CBufferIn& buffer) ;
         static void recvAddAxis(CEventServer& event) ;
         void recvAddAxis(CBufferIn& buffer) ;

         static bool dispatchEvent(CEventServer& event) ;
         void outputFieldToServer(CArray<double,1>& fieldIn, int rank, CArray<double,1>& fieldOut) ;
         static void recvIndex(CEventServer& event) ;
         void recvIndex(int rank, CBufferIn& buffer) ;
         void sendIndex(void) ;

         void computeDomConServer();
         std::map<int, int> getDomConServerSide();
         std::map<int, StdSize> getConnectedServerDataSize();
         std::vector<StdString> getDomainList();
         std::vector<StdString> getAxisList();
         std::vector<CDomain*> getDomains();
         std::vector<CAxis*> getAxis();
         std::vector<int> getAxisOrder();

      public:

         /// Propriétés privées ///
         bool withAxis ;
         bool isChecked;
         bool isDomainAxisChecked;
         bool isIndexSent;

         CAxis*   axis ;
         CDomain* domain ;

         std::deque< CArray<int, 1>* > storeIndex ;
         std::deque< CArray<int, 1>* > out_i_index ;
         std::deque< CArray<int, 1>* > out_j_index ;
         std::deque< CArray<int, 1>* > out_l_index ;

        CArray<int, 1>  storeIndex_client ;
        CArray<int, 1>  out_i_client ;
        CArray<int, 1>  out_j_client ;
        CArray<int, 1>  out_l_client ;

         map<int, CArray<int, 1>* >  storeIndex_toSrv ;
         map<int,int> nbSenders ;
//         std::deque<ARRAY(int, 1)> out_i_toSrv ;
//         std::deque<ARRAY(int, 1)> out_j_toSrv ;
//         std::deque<ARRAY(int, 1)> out_l_toSrv ;

         map<int, CArray<int, 1>* > out_i_fromClient ;
         map<int, CArray<int, 1>* > out_j_fromClient ;
         map<int, CArray<int, 1>* > out_l_fromClient ;

         map<int, CArray<size_t, 1>* > outIndexFromClient;
         void checkMask(void) ;

         std::map<int, int> domConnectedServerSide_;
         bool isDomConServerComputed_;

      private:
        void setVirtualDomainGroup(CDomainGroup* newVDomainGroup);
        void setVirtualDomainGroup();
        void setVirtualAxisGroup(CAxisGroup* newVAxisGroup);
        void setVirtualAxisGroup();
//        void setAxisList();
        void setAxisList(const std::vector<CAxis*> axis = std::vector<CAxis*>());
//        void setDomainList();
        void setDomainList(const std::vector<CDomain*> domains = std::vector<CDomain*>());

        CDomain* addDomain(const std::string& id);
        CAxis* addAxis(const std::string& id);

        CAxisGroup* getVirtualAxisGroup() const;
        CDomainGroup* getVirtualDomainGroup() const;
      private:
        CDomainGroup* vDomainGroup_;
        CAxisGroup* vAxisGroup_;
        std::vector<std::string> axisList_, domList_;
        bool isAxisListSet, isDomListSet;
        CDistributionClient* clientDistribution_;
        CDistributionServer* serverDistribution_;
        CServerDistributionDescription* serverDistributionDescription_;
        CClientServerMapping clientServerMap_;
   }; // class CGrid

   ///--------------------------------------------------------------

   template <int n>
      void CGrid::inputField(const CArray<double,n>& field, CArray<double,1>& stored) const
   {
      if (this->getDataSize() != field.numElements())
         ERROR("void CGrid::inputField(const  CArray<double,n>& field, CArray<double,1>& stored) const",
                << "[ Awaiting size of the data = " << this->getDataSize()       << ", "
                << "Received data size = "      << field.numElements() << " ] "
                << "The array of data has not the good size !")
      this->storeField_arr(field.dataFirst(), stored) ;
   }

   ///--------------------------------------------------------------

   // Declare/Define CGridGroup and CGridDefinition
   DECLARE_GROUP(CGrid);

   ///--------------------------------------------------------------

} // namespace xios

#endif // __XMLIO_CGrid__
