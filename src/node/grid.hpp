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
#include "distribution_server.hpp"
#include "client_server_mapping.hpp"
#include "utils.hpp"
#include "transformation_enum.hpp"

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
           EVENT_ID_INDEX, EVENT_ID_ADD_DOMAIN, EVENT_ID_ADD_AXIS
         };

         enum EElementType
         {
           GRID_ONLY_AXIS, GRID_ONLY_DOMAIN, GRID_AXIS_DOMAIN
         };

         /// Constructeurs ///
         CGrid(void);
         explicit CGrid(const StdString& id);
         CGrid(const CGrid& grid);       // Not implemented yet.
         CGrid(const CGrid* const grid); // Not implemented yet.

         /// Traitements ///
//         void solveReference(void);

         void checkEligibilityForCompressedOutput();

         void solveDomainAxisRef(bool areAttributesChecked);

         void checkMaskIndex(bool doCalculateIndex);

 //        virtual void toBinary  (StdOStream& os) const;
//         virtual void fromBinary(StdIStream& is);

         void addRelFileCompressed(const StdString& filename);

         /// Tests ///
         bool isCompressible(void) const;
         bool isWrittenCompressed(const StdString& filename) const;

      public:

         /// Accesseurs ///
         StdSize getDimension(void) const;

         StdSize  getDataSize(void) const;

         /// Entrées-sorties de champs ///
         template <int n>
         void inputField(const CArray<double,n>& field, CArray<double,1>& stored) const;
         template <int n>
         void outputField(const CArray<double,1>& stored, CArray<double,n>& field) const;

         void outputField(int rank, const CArray<double,1>& stored, double* field);
         void inputField(int rank, const double* const field, CArray<double,1>& stored);

         void outputCompressedField(int rank, const CArray<double,1>& stored, double* field);

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
                                  const CArray<bool,1>& axisDomainOrder = CArray<bool,1>());
         static CGrid* createGrid(StdString id, const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                                  const CArray<bool,1>& axisDomainOrder = CArray<bool,1>());
         static StdString generateId(const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                                     const CArray<bool,1>& axisDomainOrder = CArray<bool,1>());

      public:

         /// Entrées-sorties de champs (interne) ///
         void storeField_arr(const double* const data, CArray<double,1>& stored) const;
         void restoreField_arr(const CArray<double,1>& stored, double* const data) const;

         /// Traitements protégés ///
         void computeIndexServer(void);
         void computeIndex(void);
         void computeIndexScalarGrid();
         void computeCompressedIndex();

         void solveDomainRef(bool checkAtt);
         void solveAxisRef(bool checkAtt);
         void solveDomainAxisRefInheritance(bool apply = true);
         void solveTransformations();

         void sendAddDomain(const std::string& id="");
         void sendAddAxis(const std::string& id="");
         void sendAllDomains();
         void sendAllAxis();

         static void recvAddDomain(CEventServer& event);
         void recvAddDomain(CBufferIn& buffer);
         static void recvAddAxis(CEventServer& event);
         void recvAddAxis(CBufferIn& buffer);

         static bool dispatchEvent(CEventServer& event);
         static void recvIndex(CEventServer& event);
         void recvIndex(vector<int> ranks, vector<CBufferIn*> buffers);
         void sendIndex(void);
         void sendIndexScalarGrid();

         void computeDomConServer();
         std::map<int, int> getDomConServerSide();
         std::map<int, StdSize> getAttributesBufferSize();
         std::map<int, StdSize> getDataBufferSize(const std::string& id = "");
         std::vector<StdString> getDomainList();
         std::vector<StdString> getAxisList();
         std::vector<CDomain*> getDomains();
         std::vector<CAxis*> getAxis();
         std::vector<int> getAxisOrder();
         std::vector<int> getGlobalDimension();
         bool isScalarGrid() const;
         std::vector<int> getAxisPositionInGrid() const;

         bool doGridHaveDataToWrite();
         bool doGridHaveDataDistributed();
         size_t getWrittenDataSize() const;
         int getNumberWrittenIndexes() const;
         int getTotalNumberWrittenIndexes() const;
         int getOffsetWrittenIndexes() const;

         const CDistributionServer* getDistributionServer() const;
         const CDistributionClient* getDistributionClient() const;
         CGridTransformation* getTransformations();

         void transformGrid(CGrid* transformGridSrc);
         void completeGrid(CGrid* transformGridSrc);
         void doAutoDistribution(CGrid* transformGridSrc);
         bool isTransformed();
         void setTransformed();

      public:

         /// Propriétés privées ///
         bool isChecked;
         bool isDomainAxisChecked;
         bool isIndexSent;

         CArray<int, 1> storeIndex_client;

         map<int, CArray<int, 1> > storeIndex_toSrv;
         map<int,int> nbSenders;

         map<int, CArray<size_t, 1> > outIndexFromClient, compressedOutIndexFromClient;
         void checkMask(void);
         void modifyMask(const CArray<int,1>& indexToModify);

         void computeGridGlobalDimension(const std::vector<CDomain*>& domains,
                                         const std::vector<CAxis*>& axis,
                                         const CArray<bool,1>& axisDomainOrder);

      private:
       template<int N>
       void checkGridMask(CArray<bool,N>& gridMask,
                          const std::vector<CArray<bool,1>* >& domainMasks,
                          const std::vector<CArray<bool,1>* >& axisMasks,
                          const CArray<bool,1>& axisDomainOrder);
        template<int N>
        void modifyGridMask(CArray<bool,N>& gridMask, const CArray<int,1>& indexToModify);

        void setVirtualDomainGroup(CDomainGroup* newVDomainGroup);
        void setVirtualDomainGroup();
        void setVirtualAxisGroup(CAxisGroup* newVAxisGroup);
        void setVirtualAxisGroup();

        void setAxisList(const std::vector<CAxis*> axis = std::vector<CAxis*>());
        void setDomainList(const std::vector<CDomain*> domains = std::vector<CDomain*>());

        CDomain* addDomain(const std::string& id);
        CAxis* addAxis(const std::string& id);

        CAxisGroup* getVirtualAxisGroup() const;
        CDomainGroup* getVirtualDomainGroup() const;

        void checkAttributesAfterTransformation();


        void setTransformationAlgorithms();

        std::vector<int> globalDim_;

      private:
        CDomainGroup* vDomainGroup_;
        CAxisGroup* vAxisGroup_;
        std::vector<std::string> axisList_, domList_;
        bool isAxisListSet, isDomListSet;

        CDistributionClient* clientDistribution_;
        CDistributionServer* serverDistribution_;
        CClientServerMapping* clientServerMap_;
        size_t writtenDataSize_;
        int numberWrittenIndexes_, totalNumberWrittenIndexes_, offsetWrittenIndexes_;
        std::map<int,size_t> connectedDataSize_;
        std::vector<int> connectedServerRank_;
        bool isDataDistributed_;
        int positionDimensionDistributed_;
         //! True if and only if the data defined on the grid can be outputted in a compressed way
        bool isCompressible_;
        std::set<std::string> relFilesCompressed;

        bool isTransformed_;
        std::vector<int> axisPositionInGrid_;
        CGridTransformation* transformations_;
   }; // class CGrid

   ///--------------------------------------------------------------

   template <int n>
   void CGrid::inputField(const CArray<double,n>& field, CArray<double,1>& stored) const
   {
      if (this->getDataSize() != field.numElements())
         ERROR("void CGrid::inputField(const  CArray<double,n>& field, CArray<double,1>& stored) const",
                << "[ Awaiting data of size = " << this->getDataSize() << ", "
                << "Received data size = "      << field.numElements() << " ] "
                << "The data array does not have the right size!")
      this->storeField_arr(field.dataFirst(), stored);
   }

   template <int n>
   void CGrid::outputField(const CArray<double,1>& stored, CArray<double,n>& field) const
   {
      if (this->getDataSize() != field.numElements())
         ERROR("void CGrid::outputField(const CArray<double,1>& stored, CArray<double,n>& field) const",
                << "[ Size of the data = " << this->getDataSize() << ", "
                << "Output data size = "   << field.numElements() << " ] "
                << "The ouput array does not have the right size!")
      this->restoreField_arr(stored, field.dataFirst());
   }

   template<int N>
   void CGrid::checkGridMask(CArray<bool,N>& gridMask,
                             const std::vector<CArray<bool,1>* >& domainMasks,
                             const std::vector<CArray<bool,1>* >& axisMasks,
                             const CArray<bool,1>& axisDomainOrder)
   {
     int idx = 0;
     int numElement = axisDomainOrder.numElements();
     int dim = domainMasks.size() * 2 + axisMasks.size();
     std::vector<CDomain*> domainP = this->getDomains();

     std::vector<int> idxLoop(dim,0), indexMap(numElement), eachDimSize(dim);
     std::vector<int> currentIndex(dim);
     int idxDomain = 0, idxAxis = 0;
    for (int i = 0; i < numElement; ++i)
    {
      indexMap[i] = idx;
      if (true == axisDomainOrder(i)) {
          eachDimSize[indexMap[i]]   = domainP[idxDomain]->ni;
          eachDimSize[indexMap[i]+1] = domainP[idxDomain]->nj;
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
                << "The mask has one dimension whose size is different from the one of the local grid." << std::endl
                << "Local size of dimension " << i << " is " << eachDimSize[i] << "." << std::endl
                << "Mask size for dimension " << i << " is " << gridMask.extent(i) << ".");
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
          maskValue = maskValue && (*domainMasks[idxDomain])(idxLoop[indexMap[i]] + idxLoop[indexMap[i]+1] * eachDimSize[indexMap[i]]);
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
      *(gridMask.dataFirst()+maskIndex) &= maskValue;

      ++idxLoop[0];
      ++idx;
    }

   }

   /*!
     Modify the current mask of grid, the local index to be modified will take value false
     \param [in/out] gridMask current mask of grid
     \param [in] indexToModify local index to modify
   */
   template<int N>
   void CGrid::modifyGridMask(CArray<bool,N>& gridMask, const CArray<int,1>& indexToModify)
   {
     bool valueToModify = false;
     int num = indexToModify.numElements();
     for (int idx = 0; idx < num; ++idx)
     {
       *(gridMask.dataFirst()+indexToModify(idx)) = valueToModify;
     }
   }
   ///--------------------------------------------------------------

   // Declare/Define CGridGroup and CGridDefinition
   DECLARE_GROUP(CGrid);

   ///--------------------------------------------------------------

} // namespace xios

#endif // __XIOS_CGrid__
