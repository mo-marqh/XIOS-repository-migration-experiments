#ifndef __XIOS_CGrid__
#define __XIOS_CGrid__

/// XIOS headers ///
#include "xios_spl.hpp"
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
#include "utils.hpp"

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
     DECLARE_ATTRIBUTE(bool, scalar_grid)
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
         void computeIndexScalarGrid();

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
         void sendIndexScalarGrid();

         void computeDomConServer();
         std::map<int, int> getDomConServerSide();
         std::map<int, StdSize> getConnectedServerDataSize();
         std::vector<StdString> getDomainList();
         std::vector<StdString> getAxisList();
         std::vector<CDomain*> getDomains();
         std::vector<CAxis*> getAxis();
         std::vector<int> getAxisOrder();
         std::vector<int> getGlobalDimension();

         bool doGridHaveDataToWrite();
         bool doGridHaveDataDistributed();
         size_t getWrittenDataSize() const;

         const CDistributionServer* getDistributionServer() const;

      public:

         /// Propriétés privées ///
         bool isChecked;
         bool isDomainAxisChecked;
         bool isIndexSent;

         std::deque< CArray<int, 1>* > storeIndex ;
        CArray<int, 1>  storeIndex_client ;


         map<int, CArray<int, 1>* >  storeIndex_toSrv ;
         map<int,int> nbSenders ;

         map<int, CArray<size_t, 1>* > outIndexFromClient;
         void checkMask(void) ;

      private:
       template<int N>
       void checkGridMask(CArray<bool,N>& gridMask,
                          const std::vector<CArray<bool,2>* >& domainMasks,
                          const std::vector<CArray<bool,1>* >& axisMasks,
                          const CArray<bool,1>& axisDomainOrder);
        void setVirtualDomainGroup(CDomainGroup* newVDomainGroup);
        void setVirtualDomainGroup();
        void setVirtualAxisGroup(CAxisGroup* newVAxisGroup);
        void setVirtualAxisGroup();

        void setAxisList(const std::vector<CAxis*> axis = std::vector<CAxis*>());
        void setDomainList(const std::vector<CDomain*> domains = std::vector<CDomain*>());

        void computeGridGlobalDimension(const std::vector<CDomain*>& domains,
                                        const std::vector<CAxis*>& axis,
                                        const CArray<bool,1>& axisDomainOrder);

        CDomain* addDomain(const std::string& id);
        CAxis* addAxis(const std::string& id);

        CAxisGroup* getVirtualAxisGroup() const;
        CDomainGroup* getVirtualDomainGroup() const;
        std::vector<int> globalDim_;
      private:
        CDomainGroup* vDomainGroup_;
        CAxisGroup* vAxisGroup_;
        std::vector<std::string> axisList_, domList_;
        bool isAxisListSet, isDomListSet;

        CDistributionClient* clientDistribution_;
        CDistributionServer* serverDistribution_;
        CServerDistributionDescription* serverDistributionDescription_;
        CClientServerMapping* clientServerMap_;
        size_t writtenDataSize_;
        std::map<int,size_t> connectedDataSize_;
        std::vector<int> connectedServerRank_;
        bool isDataDistributed_;
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

   template<int N>
   void CGrid::checkGridMask(CArray<bool,N>& gridMask,
                             const std::vector<CArray<bool,2>* >& domainMasks,
                             const std::vector<CArray<bool,1>* >& axisMasks,
                             const CArray<bool,1>& axisDomainOrder)
   {
     int idx = 0;
     int numElement = axisDomainOrder.numElements();
     int dim = domainMasks.size()*2 + axisMasks.size();

     std::vector<int> idxLoop(dim,0), indexMap(numElement), eachDimSize(dim);
     std::vector<int> currentIndex(dim);
     int idxDomain = 0, idxAxis = 0;
    for (int i = 0; i < numElement; ++i)
    {
      indexMap[i] = idx;
      if (true == axisDomainOrder(i)) {
          eachDimSize[indexMap[i]]   = domainMasks[idxDomain]->extent(0);
          eachDimSize[indexMap[i]+1] = domainMasks[idxDomain]->extent(1);
          idx += 2; ++idxDomain;
      }
      else {
        eachDimSize[indexMap[i]] = axisMasks[idxAxis]->numElements();
        ++idx; ++idxAxis;
      }
    }

    if (!gridMask.isEmpty())
    {
      for (int i = 0; i < dim; ++i)
      {
        if (gridMask.extent(i) != eachDimSize[i])
           ERROR("CGrid::checkMask(void)",
                <<"The mask has one dimension whose size is different from the one of the local grid"<<endl
                <<"Local size is "<< i << " " << eachDimSize[i]<<endl
                <<"Mask dimension size is "<<gridMask.extent(i));
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
        if (axisDomainOrder(i))
        {
          maskValue = maskValue && (*domainMasks[idxDomain])(idxLoop[indexMap[i]],
                                                          idxLoop[indexMap[i]+1]);
          ++idxDomain;
        }
        else
        {
          maskValue = maskValue && (*axisMasks[idxAxis])(idxLoop[indexMap[i]]);
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
      *(gridMask.dataFirst()+maskIndex) = maskValue;

      ++idxLoop[0];
      ++idx;
    }

   }

   ///--------------------------------------------------------------

   // Declare/Define CGridGroup and CGridDefinition
   DECLARE_GROUP(CGrid);

   ///--------------------------------------------------------------

} // namespace xios

#endif // __XIOS_CGrid__
