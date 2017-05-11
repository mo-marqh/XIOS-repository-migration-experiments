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
#include "server_distribution_description.hpp"
#include "mesh.hpp"

namespace xios {

   /// ////////////////////// Déclarations ////////////////////// ///

   class CDomainGroup;
   class CDomainAttributes;
   class CDomain;
   class CFile;

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
         
      public :
         enum EEventId
         {
           EVENT_ID_INDEX, EVENT_ID_LON, EVENT_ID_LAT, 
           EVENT_ID_AREA, EVENT_ID_MASK,
           EVENT_ID_DATA_INDEX, EVENT_ID_SERVER_ATTRIBUT,
           EVENT_ID_INDEX_ZOOM
         } ;



      public:

         typedef CDomainAttributes RelAttributes;
         typedef CDomainGroup      RelGroup;
         typedef CTransformation<CDomain>::TransformationMapTypes TransMapTypes;

         /// Constructeurs ///
         CDomain(void);
         explicit CDomain(const StdString & id);
         CDomain(const CDomain & domain);       // Not implemented yet.
         CDomain(const CDomain * const domain); // Not implemented yet.

         static CDomain* createDomain();
         
         CMesh* mesh;
         void assignMesh(const StdString, const int);
        
         virtual void parse(xml::CXMLNode & node);

         /// Vérifications ///
         void checkAttributes(void);
         void checkAttributesOnClient();
         void checkAttributesOnClientAfterTransformation();
         void checkEligibilityForCompressedOutput(void);

         void sendCheckedAttributes();

         bool hasTransformation();
         void solveInheritanceTransformation();
         TransMapTypes getAllTransformations();
         void redistribute(int nbLocalDomain);
         void duplicateTransformation(CDomain*);
         CTransformation<CDomain>* addTransformation(ETranformationType transType, const StdString& id="");

      public:
         const std::set<StdString> & getRelFiles(void) const;
         bool IsWritten(const StdString & filename) const;
         bool isWrittenCompressed(const StdString& filename) const;

         const std::vector<int>& getIndexesToWrite(void) const;
         int getNumberWrittenIndexes() const;
         int getTotalNumberWrittenIndexes() const;
         int getOffsetWrittenIndexes() const;

         const std::vector<int>& getStartWriteIndex() const;
         const std::vector<int>& getCountWriteIndex() const;
         const std::vector<int>& getLocalWriteSize() const;
         const std::vector<int>& getGlobalWriteSize() const;

         std::map<int, StdSize> getAttributesBufferSize();
         CArray<size_t,1> localIndexToWriteOnServer;

         bool isEmpty(void) const;
         bool isDistributed(void) const;
         bool isCompressible(void) const; 
 
         CArray<double, 1> lonvalue, latvalue;
         CArray<double, 2> bounds_lonvalue, bounds_latvalue;
         CArray<double, 1> areavalue;

         vector<int> connectedServer ; // list of connected server
         vector<int> nbSenders ; // for each communication with a server, number of communicating client
         vector<int> nbDataSrv ; // size of data to send to each server
         vector< vector<int> > i_indSrv ; // for each server, i global index to send
         vector< vector<int> > j_indSrv ; // for each server, j global index to send

      public:
         /// Mutateur ///
         void addRelFile(const StdString & filename);
         void addRelFileCompressed(const StdString& filename);
         void completeLonLatClient(void);         
         void computeConnectedClients();
         void computeWrittenIndex();

         void AllgatherRectilinearLonLat(CArray<double,1>& lon, CArray<double,1>& lat,
                                         CArray<double,1>& lon_g, CArray<double,1>& lat_g);

         void fillInRectilinearBoundLonLat(CArray<double,1>& lon, CArray<double,1>& lat,
                                           CArray<double,2>& boundsLon, CArray<double,2>& boundsLat);
         void fillInRectilinearLonLat();

         static bool dispatchEvent(CEventServer& event);
         static void recvDistributionAttributes(CEventServer& event);
         static void recvIndex(CEventServer& event);
         static void recvIndexZoom(CEventServer& event);
         static void recvMask(CEventServer& event);
         static void recvZoom(CEventServer& event);
         static void recvLon(CEventServer& event);
         static void recvLat(CEventServer& event);
         static void recvArea(CEventServer& event);
         static void recvDataIndex(CEventServer& event);
         void recvDistributionAttributes(CBufferIn& buffer);                  
         void recvIndex(std::map<int, CBufferIn*>& rankBuffers);
         void recvIndexZoom(std::map<int, CBufferIn*>& rankBuffers);
         void recvMask(std::map<int, CBufferIn*>& rankBuffers);
         void recvLon(std::map<int, CBufferIn*>& rankBuffers);
         void recvLat(std::map<int, CBufferIn*>& rankBuffers);
         void recvArea(std::map<int, CBufferIn*>& rankBuffers);         
         void recvDataIndex(std::map<int, CBufferIn*>& rankBuffers);

         /// Destructeur ///
         virtual ~CDomain(void);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);

         static ENodeType GetType(void);
         const boost::unordered_map<int, vector<size_t> >& getIndexServer() const;
         CArray<bool, 1> localMask;
         bool isCurvilinear ;
         bool hasBounds ;
         bool hasArea;
         bool hasLonLat;
         bool hasPole ;

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
         void checkZoom(void);
         void computeLocalMask(void) ;

         void setTransformations(const TransMapTypes&);         

         void sendAttributes();
         void sendIndex();
         void sendDistributionAttributes();
         void sendMask();
         void sendArea();
         void sendLonLat();
         void sendIndexZoom();
         void sendDataIndex();

         void convertLonLatValue();

       private:         
         std::vector<int> start_write_index_;
         std::vector<int> count_write_index_;
         std::vector<int> local_write_size_;
         std::vector<int> global_write_size_;

         bool doZoomByIndex_;
         bool isChecked, computedWrittenIndex_;
         std::set<StdString> relFiles, relFilesCompressed;
         bool isClientChecked; // Verify whether all attributes of domain on the client side are good
         bool isClientAfterTransformationChecked;
         std::map<int, CArray<int,1> > indiSrv, indjSrv, indGlob_, indGlobZoom_;
         std::map<int,int> nbConnectedClients_, nbConnectedClientsZoom_; // Mapping of number of communicating client to a server

         boost::unordered_map<int, vector<size_t> > indSrv_; // Global index of each client sent to server
         boost::unordered_map<int, vector<size_t> > indZoomSrv_; // Global index of each client sent to server
         std::map<int, vector<int> > indWrittenSrv_; // Global written index of each client sent to server
         std::vector<int> indexesToWrite;
         std::vector<int> recvClientRanks_, recvClientZoomRanks_;
         int numberWrittenIndexes_, totalNumberWrittenIndexes_, offsetWrittenIndexes_;
         std::vector<int> connectedServerRank_, connectedServerZoomRank_;
         bool isDistributed_;
         //! True if and only if the data defined on the domain can be outputted in a compressed way
         bool isCompressible_;
         bool isRedistributed_;
         TransMapTypes transformationMap_;         
         bool isUnstructed_;
         boost::unordered_map<size_t,size_t> globalLocalIndexMap_, globalLocalIndexZoomMap_;
       
       private:
         static bool initializeTransformationMap(std::map<StdString, ETranformationType>& m);
         static std::map<StdString, ETranformationType> transformationMapList_;
         static bool _dummyTransformationMapList;

         DECLARE_REF_FUNC(Domain,domain)

   }; // class CDomain

   ///--------------------------------------------------------------

   // Declare/Define CDomainGroup and CDomainDefinition
   DECLARE_GROUP(CDomain);

   ///--------------------------------------------------------------

} // namespace xios

#endif //__XIOS_CDomain__
