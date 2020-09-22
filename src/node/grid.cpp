
#include "grid.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "message.hpp"
#include <iostream>
#include "xios_spl.hpp"
#include "type.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "context_server.hpp"
#include "array_new.hpp"
#include "server_distribution_description.hpp"
#include "client_server_mapping_distributed.hpp"
#include "distribution_client.hpp"
#include "grid_transformation.hpp"
#include "grid_generate.hpp"
#include "server.hpp"
#include "distribution_type.hpp"
#include "grid_remote_connector.hpp"
#include "grid_elements.hpp"
#include "grid_local_view.hpp"


namespace xios {

   /// ////////////////////// Dfinitions ////////////////////// ///

   CGrid::CGrid(void)
      : CObjectTemplate<CGrid>(), CGridAttributes()
      , isChecked(false), isDomainAxisChecked(false)
      , vDomainGroup_(), domList_(), isDomListSet(false)
      , vAxisGroup_(), axisList_(), isAxisListSet(false)
      , vScalarGroup_(), scalarList_(), isScalarListSet(false)
      , clientDistribution_(0), isIndexSent(false) , serverDistribution_(0), clientServerMap_(0)
      , numberWrittenIndexes_(0), totalNumberWrittenIndexes_(0), offsetWrittenIndexes_(0)
      , connectedDataSize_(), connectedServerRank_(), connectedServerRankRead_(), connectedDataSizeRead_()
	    , isCompressible_(false)
      , transformations_(0), isTransformed_(false)
      , axisPositionInGrid_(), hasDomainAxisBaseRef_(false)
      , gridSrc_(), hasTransform_(false), isGenerated_(false), order_(), globalIndexOnServer_()
      , computedWrittenIndex_(false)
      , clients()
   {
     setVirtualDomainGroup(CDomainGroup::create(getId() + "_virtual_domain_group"));
     setVirtualAxisGroup(CAxisGroup::create(getId() + "_virtual_axis_group"));
     setVirtualScalarGroup(CScalarGroup::create(getId() + "_virtual_scalar_group"));
   }

   CGrid::CGrid(const StdString& id)
      : CObjectTemplate<CGrid>(id), CGridAttributes()
      , isChecked(false), isDomainAxisChecked(false)
      , vDomainGroup_(), domList_(), isDomListSet(false)
      , vAxisGroup_(), axisList_(), isAxisListSet(false)
      , vScalarGroup_(), scalarList_(), isScalarListSet(false)
      , clientDistribution_(0), isIndexSent(false) , serverDistribution_(0), clientServerMap_(0)
      , numberWrittenIndexes_(0), totalNumberWrittenIndexes_(0), offsetWrittenIndexes_(0)
      , connectedDataSize_(), connectedServerRank_(), connectedServerRankRead_(), connectedDataSizeRead_()
	    , isCompressible_(false)
      , transformations_(0), isTransformed_(false)
      , axisPositionInGrid_(), hasDomainAxisBaseRef_(false)
      , gridSrc_(), hasTransform_(false), isGenerated_(false), order_(), globalIndexOnServer_()
      , computedWrittenIndex_(false)
      , clients()
   {
     setVirtualDomainGroup(CDomainGroup::create(getId() + "_virtual_domain_group"));
     setVirtualAxisGroup(CAxisGroup::create(getId() + "_virtual_axis_group"));
     setVirtualScalarGroup(CScalarGroup::create(getId() + "_virtual_scalar_group"));
   }

   CGrid::~CGrid(void)
   {
    if (0 != clientDistribution_) delete clientDistribution_;
    if (0 != serverDistribution_) delete serverDistribution_;
    if (0 != clientServerMap_) delete clientServerMap_;
    if (0 != transformations_) delete transformations_;
   }

   ///---------------------------------------------------------------

   StdString CGrid::GetName(void)    { return StdString("grid"); }
   StdString CGrid::GetDefName(void) { return CGrid::GetName(); }
   ENodeType CGrid::GetType(void)    { return eGrid; }


  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////              MEMBER FUNCTION RELATED TO GRID CONSTRUCTION by ELEMNTS AND MANAGEMENT                      /////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


   CGrid* CGrid::createGrid(CDomain* domain)
   TRY
   {
     std::vector<CDomain*> vecDom(1, domain);
     std::vector<CAxis*> vecAxis;
     return createGrid(vecDom, vecAxis);
   }
   CATCH

   CGrid* CGrid::createGrid(CDomain* domain, CAxis* axis)
   TRY
  {
      std::vector<CDomain*> vecDom(1, domain);
      std::vector<CAxis*> vecAxis(1, axis);

      return createGrid(vecDom, vecAxis);
   }
   CATCH

   CGrid* CGrid::createGrid(const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                            const CArray<int,1>& axisDomainOrder)
   TRY
   {
     std::vector<CScalar*> vecScalar;
     return createGrid(generateId(domains, axis, vecScalar, axisDomainOrder), domains, axis, vecScalar, axisDomainOrder);
   }
   CATCH

   CGrid* CGrid::createGrid(const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                            const std::vector<CScalar*>& scalars, const CArray<int,1>& axisDomainOrder)
   TRY
   {
     return createGrid(generateId(domains, axis, scalars, axisDomainOrder), domains, axis, scalars, axisDomainOrder);
   }
   CATCH

   CGrid* CGrid::createGrid(StdString id, const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                            const std::vector<CScalar*>& scalars, const CArray<int,1>& axisDomainOrder)
   TRY
   {
      if (axisDomainOrder.numElements() > 0 && axisDomainOrder.numElements() != (domains.size() + axis.size() + scalars.size()))
        ERROR("CGrid* CGrid::createGrid(...)",
              << "The size of axisDomainOrder (" << axisDomainOrder.numElements()
              << ") is not coherent with the number of elements (" << domains.size() + axis.size() <<").");

      CGrid* grid = CGridGroup::get("grid_definition")->createChild(id);
      grid->setDomainList(domains);
      grid->setAxisList(axis);
      grid->setScalarList(scalars);

      // By default, domains are always the first elements of a grid
      if (0 == axisDomainOrder.numElements())
      {
        int size = domains.size() + axis.size() + scalars.size();
        int nb = 0;
        grid->axis_domain_order.resize(size);
        for (int i = 0; i < size; ++i)
        {
          if (i < domains.size())
          {
            grid->axis_domain_order(i) = 2;
            grid->order_.push_back(2) ;
          }
          else if ((scalars.size() < (size-nb)) < size)
          {
            grid->axis_domain_order(i) = 1;
            grid->order_.push_back(1) ;
          }
          else
          {
            grid->axis_domain_order(i) = 0;
            grid->order_.push_back(0) ;
          }
          ++nb;
        }
      }
      else
      {
        grid->axis_domain_order.resize(axisDomainOrder.numElements());
        grid->axis_domain_order = axisDomainOrder;
        grid->order_.clear() ;
        for(int i=0; i<axisDomainOrder.numElements();i++) grid->order_.push_back(axisDomainOrder(i)) ;

      }

 //     grid->solveElementsRefInheritance(true);

      return grid;
   }
   CATCH

   //----------------------------------------------------------------

   //! Change virtual field group to a new one
   void CGrid::setVirtualDomainGroup(CDomainGroup* newVDomainGroup)
   TRY
   {
      this->vDomainGroup_ = newVDomainGroup;
   }
   CATCH_DUMP_ATTR

   //! Change virtual variable group to new one
   void CGrid::setVirtualAxisGroup(CAxisGroup* newVAxisGroup)
   TRY
   {
      this->vAxisGroup_ = newVAxisGroup;
   }
   CATCH_DUMP_ATTR

   //! Change virtual variable group to new one
   void CGrid::setVirtualScalarGroup(CScalarGroup* newVScalarGroup)
   TRY
   {
      this->vScalarGroup_ = newVScalarGroup;
   }
   CATCH_DUMP_ATTR

   //----------------------------------------------------------------

   CDomainGroup* CGrid::getVirtualDomainGroup() const
   TRY
   {
     return this->vDomainGroup_;
   }
   CATCH

   CAxisGroup* CGrid::getVirtualAxisGroup() const
   TRY
   {
     return this->vAxisGroup_;
   }
   CATCH

   CScalarGroup* CGrid::getVirtualScalarGroup() const
   TRY
   {
     return this->vScalarGroup_;
   }
   CATCH

  ///---------------------------------------------------------------

   CDomain* CGrid::addDomain(const std::string& id)
   TRY
   {
     order_.push_back(2);
     axis_domain_order.resize(order_.size());
     for (int idx = 0; idx < order_.size(); ++idx) axis_domain_order(idx)=order_[idx];
     return vDomainGroup_->createChild(id);
   }
   CATCH_DUMP_ATTR

   CAxis* CGrid::addAxis(const std::string& id)
   TRY
   {
     order_.push_back(1);
     axis_domain_order.resize(order_.size());
     for (int idx = 0; idx < order_.size(); ++idx) axis_domain_order(idx)=order_[idx];
     return vAxisGroup_->createChild(id);
   }
   CATCH_DUMP_ATTR

   CScalar* CGrid::addScalar(const std::string& id)
   TRY
   {
     order_.push_back(0);
     axis_domain_order.resize(order_.size());
     for (int idx = 0; idx < order_.size(); ++idx) axis_domain_order(idx)=order_[idx];
     return vScalarGroup_->createChild(id);
   }
   CATCH_DUMP_ATTR




  /*!
  \brief Get the list of domain pointers
  \return list of domain pointers
  */
  std::vector<CDomain*> CGrid::getDomains()
  TRY
  {
    std::vector<CDomain*> domList;
    if (!domList_.empty())
    {
      for (int i = 0; i < domList_.size(); ++i) domList.push_back(CDomain::get(domList_[i]));
    }
    return domList;
  }
  CATCH_DUMP_ATTR

  /*!
  \brief Get the list of  axis pointers
  \return list of axis pointers
  */
  std::vector<CAxis*> CGrid::getAxis()
  TRY
  {
    std::vector<CAxis*> aList;
    if (!axisList_.empty())
      for (int i =0; i < axisList_.size(); ++i) aList.push_back(CAxis::get(axisList_[i]));

    return aList;
  }
  CATCH_DUMP_ATTR

  /*!
  \brief Get the list of  axis pointers
  \return list of axis pointers
  */
  std::vector<CScalar*> CGrid::getScalars()
  TRY
  {
    std::vector<CScalar*> sList;
    if (!scalarList_.empty())
      for (int i =0; i < scalarList_.size(); ++i) sList.push_back(CScalar::get(scalarList_[i]));

    return sList;
  }
  CATCH_DUMP_ATTR

  /*!
  \brief Get domain pointer with index
  \return domain pointer
  */
  CDomain* CGrid::getDomain(int domainIndex)
  TRY
  {
    std::vector<CDomain*> domainListP = this->getDomains();
    if (domainListP.empty())
    {
      ERROR("CGrid::getDomain(int domainIndex)",
            << "No domain associated to this grid. " << std::endl
            << "Grid id = " << this->getId());
    }

    if (domainIndex >= domainListP.size() || (domainIndex < 0))
      ERROR("CGrid::getDomain(int domainIndex)",
            << "Domain with the index doesn't exist " << std::endl
            << "Grid id = " << this->getId() << std::endl
            << "Grid has only " << domainListP.size() << " domain but domain index required is " << domainIndex << std::endl);

    return domainListP[domainIndex];
  }
  CATCH_DUMP_ATTR

  /*!
  \brief Get the axis pointer with index
  \return axis pointer
  */
  CAxis* CGrid::getAxis(int axisIndex)
  TRY
  {
    std::vector<CAxis*> axisListP = this->getAxis();
    if (axisListP.empty())
    {
      ERROR("CGrid::getDomain(int axisIndex)",
            << "No axis associated to this grid. " << std::endl
            << "Grid id = " << this->getId());
    }

    if (axisIndex >= axisListP.size() || (axisIndex < 0))
      ERROR("CGrid::getDomain(int axisIndex)",
            << "Domain with the index doesn't exist " << std::endl
            << "Grid id = " << this->getId() << std::endl
            << "Grid has only " << axisListP.size() << " axis but axis index required is " << axisIndex << std::endl);

    return axisListP[axisIndex];
  }
  CATCH_DUMP_ATTR

  /*!
  \brief Get the a scalar pointer
  \return scalar pointer
  */
  CScalar* CGrid::getScalar(int scalarIndex)
  TRY
  {
    std::vector<CScalar*> scalarListP = this->getScalars();
    if (scalarListP.empty())
    {
      ERROR("CGrid::getScalar(int scalarIndex)",
            << "No scalar associated to this grid. " << std::endl
            << "Grid id = " << this->getId());
    }

    if (scalarIndex >= scalarListP.size() || (scalarIndex < 0))
      ERROR("CGrid::getScalar(int scalarIndex)",
            << "Scalar with the index doesn't exist " << std::endl
            << "Grid id = " << this->getId() << std::endl
            << "Grid has only " << scalarListP.size() << " scalar but scalar index required is " << scalarIndex << std::endl);

    return scalarListP[scalarIndex];
  }
  CATCH_DUMP_ATTR

  /*!
  \brief Set domain(s) of a grid from a list
  \param[in] domains list of domains
  */
  void CGrid::setDomainList(const std::vector<CDomain*> domains)
  TRY
  {
    if (isDomListSet) return;
    std::vector<CDomain*> domList = this->getVirtualDomainGroup()->getAllChildren();
    if (!domains.empty() && domList.empty())
    {
      for (int i = 0; i < domains.size(); ++i)
        this->getVirtualDomainGroup()->addChild(domains[i]);
      domList = this->getVirtualDomainGroup()->getAllChildren();
    }

    if (!domList.empty())
    {
      int sizeDom = domList.size();
      domList_.resize(sizeDom);
      for (int i = 0; i < sizeDom; ++i)
      {
        domList_[i] = domList[i]->getId();
      }
      isDomListSet = true;
    }
  }
  CATCH_DUMP_ATTR

  /*!
  \brief Set axis(s) of a grid from a list
  \param[in] axis list of axis
  */
  void CGrid::setAxisList(const std::vector<CAxis*> axis)
  TRY
  {
    if (isAxisListSet) return;
    std::vector<CAxis*> aList = this->getVirtualAxisGroup()->getAllChildren();
    if (!axis.empty() && aList.empty())
    {
      for (int i = 0; i < axis.size(); ++i)
        this->getVirtualAxisGroup()->addChild(axis[i]);
      aList = this->getVirtualAxisGroup()->getAllChildren();
    }

    if (!aList.empty())
    {
      int sizeAxis = aList.size();
      axisList_.resize(sizeAxis);
      for (int i = 0; i < sizeAxis; ++i)
      {
        axisList_[i] = aList[i]->getId();
      }
      isAxisListSet = true;
    }
  }
  CATCH_DUMP_ATTR

  /*!
  \brief Set scalar(s) of a grid from a list
  \param[in] scalars list of scalars
  */
  void CGrid::setScalarList(const std::vector<CScalar*> scalars)
  TRY
  {
    if (isScalarListSet) return;
    std::vector<CScalar*> sList = this->getVirtualScalarGroup()->getAllChildren();
    if (!scalars.empty() && sList.empty())
    {
      for (int i = 0; i < scalars.size(); ++i)
        this->getVirtualScalarGroup()->addChild(scalars[i]);
      sList = this->getVirtualScalarGroup()->getAllChildren();
    }

    if (!sList.empty())
    {
      int sizeScalar = sList.size();
      scalarList_.resize(sizeScalar);
      for (int i = 0; i < sizeScalar; ++i)
      {
        scalarList_[i] = sList[i]->getId();
      }
      isScalarListSet = true;
    }
  }
  CATCH_DUMP_ATTR

  /*!
  \brief Get list of id of domains
  \return id list of domains
  */
  std::vector<StdString> CGrid::getDomainList()
  TRY
  {
    setDomainList();
    return domList_;
  }
  CATCH

  /*!
  \brief Get list of id of axis
  \return id list of axis
  */
  std::vector<StdString> CGrid::getAxisList()
  TRY
  {
    setAxisList();
    return axisList_;
  }
  CATCH

  /*!
  \brief Get list of id of scalar
  \return id list of scalar
  */
  std::vector<StdString> CGrid::getScalarList()
  TRY
  {
    setScalarList();
    return scalarList_;
  }
  CATCH


  void CGrid::computeElements(void)
  {
    const auto& domains = getDomains() ;
    const auto& axis = getAxis() ;
    const auto& scalars = getScalars() ;
    int idxDomain = 0, idxAxis=0 , idxScalar=0 ; 
 
    for(auto type : order_)
    {
      if      (type == 0) { elements_.push_back({scalars[idxScalar], TYPE_SCALAR, scalars[idxScalar], nullptr, nullptr } ) ; idxScalar++;}
      else if (type == 1) { elements_.push_back({axis[idxAxis], TYPE_AXIS, nullptr, axis[idxAxis], nullptr}) ; idxAxis++;}
      else if (type == 2) { elements_.push_back({domains[idxDomain], TYPE_DOMAIN, nullptr, nullptr, domains[idxDomain] }) ; idxDomain++;}        
    }
    elementsComputed_ = true ;
  }
  
  
 /*!
    Parse a grid, for now, it contains only domain, axis and scalar
  */
  void CGrid::parse(xml::CXMLNode& node)
  TRY
  {
    SuperClass::parse(node);

    if (node.goToChildElement())
    {
      StdString domainName("domain");
      StdString axisName("axis");
      StdString scalarName("scalar");
      do
      {
        if (node.getElementName() == domainName) {
          order_.push_back(2);
          this->getVirtualDomainGroup()->parseChild(node);
        }
        if (node.getElementName() == axisName) {
          order_.push_back(1);
          this->getVirtualAxisGroup()->parseChild(node);
        }
        if (node.getElementName() == scalarName) {
          order_.push_back(0);
          this->getVirtualScalarGroup()->parseChild(node);
        }
      } while (node.goToNextElement());
      node.goToParentElement();
    }

    if (!order_.empty())
    {
      int sizeOrd = order_.size();
      axis_domain_order.resize(sizeOrd);
      for (int i = 0; i < sizeOrd; ++i)
      {
        axis_domain_order(i) = order_[i];
      }
    }

    setDomainList();
    setAxisList();
    setScalarList();
   }
   CATCH_DUMP_ATTR


  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



   StdSize CGrid::getDimension(void)
   TRY
   {
      return getGlobalDimension().size();
   }
   CATCH_DUMP_ATTR

   //---------------------------------------------------------------

   StdSize CGrid::getDataSize(void) 
   TRY
   {
     StdSize retvalue = 1;
     if (!isScalarGrid())
     {
       std::vector<int> dataNindex = getClientDistribution()->getDataNIndex();
       for (int i = 0; i < dataNindex.size(); ++i) retvalue *= dataNindex[i];       
     }
     return retvalue;
   }
   CATCH
   
   /*!
    * Get the local data grid size, ie the size of the compressed grid (inside the workflow)
    * \return The size od the compressed grid
    */
    StdSize  CGrid::getLocalDataSize(void) { return getClientDistribution()->getLocalDataSize();}


   /*!
    * Compute the minimum buffer size required to send the attributes to the server(s).
    *
    * \return A map associating the server rank with its minimum buffer size.
    * TODO: Refactor code
    */
   std::map<int, StdSize> CGrid::getAttributesBufferSize(CContextClient* client, bool bufferForWriting)
   TRY
   {
     std::map<int, StdSize> attributesSizes = getMinimumBufferSizeForAttributes(client);

     // The grid indexes require a similar size as the actual data
     std::map<int, StdSize> dataSizes = getDataBufferSize(client, "", bufferForWriting);
     std::map<int, StdSize>::iterator it, itE = dataSizes.end();
     for (it = dataSizes.begin(); it != itE; ++it)
     {
       it->second += 2 * sizeof(bool);
       if (it->second > attributesSizes[it->first])
         attributesSizes[it->first] = it->second;
     }
     
     // Account for the axis attributes
     std::vector<CAxis*> axisList = getAxis();
     for (size_t i = 0; i < axisList.size(); ++i)
     {
       std::map<int, StdSize> axisAttBuffSize = axisList[i]->getAttributesBufferSize(client, getGlobalDimension(),getAxisPositionInGrid()[i]);
       for (it = axisAttBuffSize.begin(), itE = axisAttBuffSize.end(); it != itE; ++it)
       {
         it->second += 2 * sizeof(bool);
         if (it->second > attributesSizes[it->first])
           attributesSizes[it->first] = it->second;
       }
     }

     // Account for the domain attributes
     std::vector<CDomain*> domList = getDomains();
     for (size_t i = 0; i < domList.size(); ++i)
     {
       std::map<int, StdSize> domAttBuffSize = domList[i]->getAttributesBufferSize(client);
       for (it = domAttBuffSize.begin(), itE = domAttBuffSize.end(); it != itE; ++it)
       {
         it->second += 2 * sizeof(bool);
         if (it->second > attributesSizes[it->first])
           attributesSizes[it->first] = it->second;
       }
     }

     return attributesSizes;
  }
   CATCH_DUMP_ATTR

   /*!
    * Compute the minimum buffer size required to send the data.
    * \param client contextClient used to determine the size of connected receivers
    * \param id the id used to tag the data
    * \param bufferForWriting flag indicating if a buffer is used to send data for writing
    * \return A map associating the sender rank with its minimum buffer size.
    */
   std::map<int, StdSize> CGrid::getDataBufferSize(CContextClient* client, const std::string& id /*= ""*/, bool bufferForWriting /*= "false"*/)
   TRY
   {     
     // The record index is sometimes sent along with the data but we always
     // include it in the size calculation for the sake of simplicity
     const size_t extraSize = CEventClient::headerSize + (id.empty() ? getId() : id).size() 
                                                       + 2 * sizeof(size_t) 
                                                       + sizeof(size_t);

     std::map<int, StdSize> dataSizes;
     int receiverSize = client->serverSize;
     std::map<int,size_t>& dataSizeMap = bufferForWriting ? connectedDataSize_[receiverSize]: connectedDataSizeRead_;
     std::vector<int>& connectedServerRanks = bufferForWriting ? connectedServerRank_[receiverSize] : connectedServerRankRead_;

     std::map<int, size_t>::const_iterator itEnd = dataSizeMap.end();
     for (size_t k = 0; k < connectedServerRanks.size(); ++k)
     {
       int rank = connectedServerRanks[k];
       std::map<int, size_t>::const_iterator it = dataSizeMap.find(rank);
       size_t count = (it != itEnd) ? it->second : 0;

       dataSizes.insert(std::make_pair(rank, extraSize + CArray<double,1>::size(count)));
     }

     return dataSizes;
   }
   CATCH_DUMP_ATTR

   size_t CGrid::getGlobalWrittenSize(void)
   TRY
   {
	 std::vector<CDomain*> domainP = this->getDomains();
     std::vector<CAxis*> axisP = this->getAxis();
     
     size_t globalGridSize=1 ;
     for (std::vector<CDomain*>::iterator it=domainP.begin(); it!=domainP.end();++it) globalGridSize*=(*it)->getGlobalWrittenSize() ;
     for (std::vector<CAxis*>::iterator it=axisP.begin(); it!=axisP.end();++it) globalGridSize*=(*it)->getGlobalWrittenSize() ;
     return globalGridSize ;
   }
   CATCH_DUMP_ATTR
   

   void CGrid::computeAxisPositionInGrid(void)
   {
     axisPositionInGrid_.resize(0);
     int idx = 0;
     for (int i = 0; i < axis_domain_order.numElements(); ++i)
     {
       int elementDimension = axis_domain_order(i);
       if (1 == elementDimension)
       {
         axisPositionInGrid_.push_back(idx);
         ++idx;
       }
       else if (2 == elementDimension) idx += 2;
     }
   }

   void CGrid::checkAttributesAfterTransformation()
   TRY
   {
      setAxisList();
      std::vector<CAxis*> axisListP = this->getAxis();
      for (int i = 0; i < axisListP.size(); ++i)
        axisListP[i]->checkAttributesOnClientAfterTransformation(getGlobalDimension(), getAxisPositionInGrid()[i]);
   
      setDomainList();
      std::vector<CDomain*> domListP = this->getDomains();
      if (!domListP.empty())
      {
        for (int i = 0; i < domListP.size(); ++i)
        {
          domListP[i]->checkAttributesOnClientAfterTransformation();
        }
      }
   }
   CATCH_DUMP_ATTR

   //---------------------------------------------------------------

   /*!
    * Test whether the data defined on the grid can be outputted in a compressed way.
    *
    * \return true if and only if a mask was defined for this grid
    */
   bool CGrid::isCompressible(void) const
   TRY
   {
      return isCompressible_;
   }
   CATCH

   //---------------------------------------------------------------

   void CGrid::addRelFileCompressed(const StdString& filename)
   TRY
   {
      this->relFilesCompressed.insert(filename);
   }
   CATCH_DUMP_ATTR

   bool CGrid::isWrittenCompressed(const StdString& filename) const
   TRY
   {
      return (this->relFilesCompressed.find(filename) != this->relFilesCompressed.end());
   }
   CATCH

   //---------------------------------------------------------------
   /*
     Find all reference of grid's components and inherite attributes if necessary
   */
   void CGrid::solveDomainAxisRef(bool areAttributesChecked)
   TRY
   {
     if (this->isDomainAxisChecked) return;

     this->solveScalarRef(areAttributesChecked);
     this->solveAxisRef(areAttributesChecked);
     this->solveDomainRef(areAttributesChecked);     
     this->isDomainAxisChecked = areAttributesChecked;
   }
   CATCH_DUMP_ATTR

   /*
     Go up hierachy reference and fill in the base reference with attributes of the children
     This function should be only used after reading component's attributes from file
   */
   void CGrid::solveDomainAxisBaseRef()
   TRY
   {
     if (this->hasDomainAxisBaseRef_) return;
     // Account for the scalar attributes
     std::vector<CScalar*> scalarList = getScalars();
     for (size_t i = 0; i < scalarList.size(); ++i)
     {
       scalarList[i]->setAttributesReference();
     }

     // Account for the axis attributes
     std::vector<CAxis*> axisList = getAxis();
     for (size_t i = 0; i < axisList.size(); ++i)
     {
       axisList[i]->setAttributesReference();
     }

     // Account for the domain attributes
     std::vector<CDomain*> domList = getDomains();
     for (size_t i = 0; i < domList.size(); ++i)
     {
       domList[i]->setAttributesReference();
     }

     this->hasDomainAxisBaseRef_ = true;
   }
   CATCH_DUMP_ATTR

   void CGrid::checkEligibilityForCompressedOutput()
   TRY
   {
     // We don't check if the mask is valid here, just if a mask has been defined at this point.
     isCompressible_ = !mask_1d.isEmpty() || !mask_2d.isEmpty() || !mask_3d.isEmpty();
   }
   CATCH_DUMP_ATTR

   //ym obsolete -> to be removed later
   void CGrid::checkMaskIndex(bool doSendingIndex)
   TRY
   {
     CContext* context = CContext::getCurrent();
     if (context->getServiceType()==CServicesManager::CLIENT || context->getServiceType()==CServicesManager::GATHERER)     
     {
       if (this->isChecked && doSendingIndex && !isIndexSent) 
       { 
         if (isScalarGrid())  /*sendIndexScalarGrid()*/;
         else  /*sendIndex()*/;
         this->isIndexSent = true; 
       }
     }

     if (this->isChecked) return;
     this->checkAttributesAfterTransformation();

     // TODO: Transfer grid attributes
     //if (!context->hasClient && context->hasServer) this->createMask();
     this->computeIndex();

     if (!(this->hasTransform() && !this->isTransformed()))
      this->isChecked = true;

     if (!(this->hasTransform() && (!this->isGenerated())))
      this->isChecked = true;
   }
   CATCH_DUMP_ATTR


   bool CGrid::hasMask() const
   TRY
   {
     return (!mask_1d.isEmpty() || !mask_2d.isEmpty() || !mask_3d.isEmpty() ||
             !mask_4d.isEmpty() || !mask_5d.isEmpty() || !mask_6d.isEmpty() || !mask_7d.isEmpty());
   }
   CATCH

   
   CArray<bool,1>& CGrid::getMask(void)
   {
      
      if (mask_.isEmpty())
      {  
        if (!mask_0d.isEmpty()) mask_.reference(CArray<bool,1>(mask_0d.dataFirst(),shape(mask_0d.numElements()), neverDeleteData)) ;
        if (!mask_1d.isEmpty()) mask_.reference(CArray<bool,1>(mask_1d.dataFirst(),shape(mask_1d.numElements()), neverDeleteData)) ;
        if (!mask_2d.isEmpty()) mask_.reference(CArray<bool,1>(mask_2d.dataFirst(),shape(mask_2d.numElements()), neverDeleteData)) ;
        if (!mask_3d.isEmpty()) mask_.reference(CArray<bool,1>(mask_3d.dataFirst(),shape(mask_3d.numElements()), neverDeleteData)) ;
        if (!mask_4d.isEmpty()) mask_.reference(CArray<bool,1>(mask_4d.dataFirst(),shape(mask_4d.numElements()), neverDeleteData)) ;
        if (!mask_5d.isEmpty()) mask_.reference(CArray<bool,1>(mask_5d.dataFirst(),shape(mask_5d.numElements()), neverDeleteData)) ;
        if (!mask_6d.isEmpty()) mask_.reference(CArray<bool,1>(mask_6d.dataFirst(),shape(mask_6d.numElements()), neverDeleteData)) ;
        if (!mask_7d.isEmpty()) mask_.reference(CArray<bool,1>(mask_7d.dataFirst(),shape(mask_7d.numElements()), neverDeleteData)) ;
      }
      return mask_ ;
   }
   /*
     Create mask of grid from mask of its components
   */
   void CGrid::createMask(void)
   TRY
   {
      using namespace std;
      std::vector<CDomain*> domainP = this->getDomains();
      std::vector<CAxis*> axisP = this->getAxis();
      int dim = domainP.size() * 2 + axisP.size();

      std::vector<CArray<bool,1>* > domainMasks(domainP.size());
      for (int i = 0; i < domainMasks.size(); ++i) domainMasks[i] = &(domainP[i]->domainMask);
      std::vector<CArray<bool,1>* > axisMasks(axisP.size());
      for (int i = 0; i < axisMasks.size(); ++i) axisMasks[i] = &(axisP[i]->mask);

      switch (dim) {
        case 1:
          checkGridMask(mask_1d, domainMasks, axisMasks, axis_domain_order, true);
          break;
        case 2:
          checkGridMask(mask_2d, domainMasks, axisMasks, axis_domain_order, true);
          break;
        case 3:
          checkGridMask(mask_3d, domainMasks, axisMasks, axis_domain_order, true);
          break;
        case 4:
          checkGridMask(mask_4d, domainMasks, axisMasks, axis_domain_order, true);
          break;
        case 5:
          checkGridMask(mask_5d, domainMasks, axisMasks, axis_domain_order, true);
          break;
        case 6:
          checkGridMask(mask_6d, domainMasks, axisMasks, axis_domain_order, true);
          break;
        case 7:
          checkGridMask(mask_7d, domainMasks, axisMasks, axis_domain_order, true);
          break;
        default:
          break;
      }
   }
   CATCH_DUMP_ATTR

   /*
     Check validity of grid's mask by using the masks of its components
   */
   void CGrid::checkMask(void)
   TRY
   {
      using namespace std;
      std::vector<CDomain*> domainP = this->getDomains();
      std::vector<CAxis*> axisP = this->getAxis();
      int dim = domainP.size() * 2 + axisP.size();

      std::vector<CArray<bool,1>* > domainMasks(domainP.size());
      for (int i = 0; i < domainMasks.size(); ++i) domainMasks[i] = &(domainP[i]->domainMask);
      std::vector<CArray<bool,1>* > axisMasks(axisP.size());
      for (int i = 0; i < axisMasks.size(); ++i) axisMasks[i] = &(axisP[i]->mask);

      switch (dim) {
        case 1:
          checkGridMask(mask_1d, domainMasks, axisMasks, axis_domain_order);
          break;
        case 2:
          checkGridMask(mask_2d, domainMasks, axisMasks, axis_domain_order);
          break;
        case 3:
          checkGridMask(mask_3d, domainMasks, axisMasks, axis_domain_order);
          break;
        case 4:
          checkGridMask(mask_4d, domainMasks, axisMasks, axis_domain_order);
          break;
        case 5:
          checkGridMask(mask_5d, domainMasks, axisMasks, axis_domain_order);
          break;
        case 6:
          checkGridMask(mask_6d, domainMasks, axisMasks, axis_domain_order);
          break;
        case 7:
          checkGridMask(mask_7d, domainMasks, axisMasks, axis_domain_order);
          break;
        default:
          break;
      }
   }
   CATCH_DUMP_ATTR

   /*
     Modify value of mask in a certain index
     This function can be used to correct the mask of grid after being constructed with createMask
     \param [in] indexToModify
     \param [in] modifyValue
   */
   void CGrid::modifyMask(const CArray<int,1>& indexToModify, bool modifyValue)
   TRY
   {
      using namespace std;
      std::vector<CDomain*> domainP = this->getDomains();
      std::vector<CAxis*> axisP = this->getAxis();
      int dim = domainP.size() * 2 + axisP.size();

      switch (dim) {
        case 0:
          modifyGridMask(mask_0d, indexToModify, modifyValue);
          break;
        case 1:
          modifyGridMask(mask_1d, indexToModify, modifyValue);
          break;
        case 2:
          modifyGridMask(mask_2d, indexToModify, modifyValue);
          break;
        case 3:
          modifyGridMask(mask_3d, indexToModify, modifyValue);
          break;
        case 4:
          modifyGridMask(mask_4d, indexToModify, modifyValue);
          break;
        case 5:
          modifyGridMask(mask_5d, indexToModify, modifyValue);
          break;
        case 6:
          modifyGridMask(mask_6d, indexToModify, modifyValue);
          break;
        case 7:
          modifyGridMask(mask_7d, indexToModify, modifyValue);
          break;
        default:
          break;
      }
   }
   CATCH_DUMP_ATTR

   /*
     Change the mask size. This function is used on reconstructing mask in server side
     \param [in] newDimensionSize
     \param [in] newValue 
   */
   void CGrid::modifyMaskSize(const std::vector<int>& newDimensionSize, bool newValue)
   TRY
   {      
      std::vector<CDomain*> domainP = this->getDomains();
      std::vector<CAxis*> axisP = this->getAxis();            
      int dim = domainP.size() * 2 + axisP.size();

      switch (dim) {
        case 0:
          modifyGridMaskSize(mask_0d, newDimensionSize, newValue);
          break;
        case 1:
          modifyGridMaskSize(mask_1d, newDimensionSize, newValue);
          break;
        case 2:
          modifyGridMaskSize(mask_2d, newDimensionSize, newValue);
          break;
        case 3:
          modifyGridMaskSize(mask_3d, newDimensionSize, newValue);
          break;
        case 4:
          modifyGridMaskSize(mask_4d, newDimensionSize, newValue);
          break;
        case 5:
          modifyGridMaskSize(mask_5d, newDimensionSize, newValue);
          break;
        case 6:
          modifyGridMaskSize(mask_6d, newDimensionSize, newValue);
          break;
        case 7:
          modifyGridMaskSize(mask_7d, newDimensionSize, newValue);
          break;
        default:
          break;
      }
   }
   CATCH_DUMP_ATTR

   //---------------------------------------------------------------

   void CGrid::solveDomainRef(bool sendAtt)
   TRY
   {
      setDomainList();
      std::vector<CDomain*> domListP = this->getDomains();
      if (!domListP.empty())
      {
        for (int i = 0; i < domListP.size(); ++i)
        {
          if (sendAtt) domListP[i]->sendCheckedAttributes();
          else domListP[i]->checkAttributesOnClient();
        }
      }
   }
   CATCH_DUMP_ATTR

   //---------------------------------------------------------------

   void CGrid::solveAxisRef(bool sendAtt)
   TRY
   {
      setAxisList();
      std::vector<CAxis*> axisListP = this->getAxis();
      if (!axisListP.empty())
      {
        for (int i = 0; i < axisListP.size(); ++i)
        {
          if (sendAtt)
            axisListP[i]->sendCheckedAttributes(getGlobalDimension(),getAxisPositionInGrid()[i]);
          else
            axisListP[i]->checkAttributesOnClient();
        }
      }
   }
   CATCH_DUMP_ATTR

   //---------------------------------------------------------------

   void CGrid::solveScalarRef(bool sendAtt)
   TRY
   {
      setScalarList();
      std::vector<CScalar*> scalarListP = this->getScalars();
      if (!scalarListP.empty())
      {
        for (int i = 0; i < scalarListP.size(); ++i)
        {
          /*Nothing to do for now */
//          if (sendAtt) scalarListP[i]->sendCheckedAttributes();
//          else scalarListP[i]->checkAttributesOnClient();
        }
      }
   }
   CATCH_DUMP_ATTR

   /*!
      Compute the index to for write data into a file
   */
   void CGrid::computeWrittenIndex()
   TRY
   {      
      if (computedWrittenIndex_) return;
      computedWrittenIndex_ = true;

      if (isScalarGrid())
      {
        size_t nbWritten = 1;
        int writtenIndex = 0;

        localIndexToWriteOnClient_.resize(nbWritten);  
        localIndexToWriteOnServer_.resize(nbWritten);
        localIndexToWriteOnServer_(0) = writtenIndex;
        localIndexToWriteOnClient_(0) = writtenIndex;
        
        return;
      }

      size_t nbWritten = 0, indGlo;
      CDistributionClient::GlobalLocalDataMap& globalDataIndex = getClientDistribution()->getGlobalDataIndexOnClient();
      CDistributionClient::GlobalLocalDataMap::const_iterator itb = globalDataIndex.begin(),
                                                              ite = globalDataIndex.end(), it;    
      const CDistributionServer::GlobalLocalMap& globalLocalIndex = serverDistribution_->getGlobalLocalIndex();                                                              
      CDistributionServer::GlobalLocalMap::const_iterator itSrvb = globalLocalIndex.begin(),
                                                          itSrve = globalLocalIndex.end(), itSrv;
      for (it = itb; it != ite; ++it)
      {
        indGlo = it->first;
        if (globalLocalIndex.end() != globalLocalIndex.find(indGlo)) ++nbWritten;                
      }

      localIndexToWriteOnClient_.resize(nbWritten);  
      localIndexToWriteOnServer_.resize(nbWritten);
      
      {
        numberWrittenIndexes_ = nbWritten;
        if (isDataDistributed())
        {
          CContext* context = CContext::getCurrent();      
          MPI_Allreduce(&numberWrittenIndexes_, &totalNumberWrittenIndexes_, 1, MPI_INT, MPI_SUM, context->intraComm_);
          MPI_Scan(&numberWrittenIndexes_, &offsetWrittenIndexes_, 1, MPI_INT, MPI_SUM, context->intraComm_);
          offsetWrittenIndexes_ -= numberWrittenIndexes_;
        }
        else
          totalNumberWrittenIndexes_ = numberWrittenIndexes_;
      }

      nbWritten = 0; 
      for (it = itb; it != ite; ++it)
      {
        indGlo = it->first;
        itSrv = globalLocalIndex.find(indGlo);
        if (itSrve != itSrv)
        {
          localIndexToWriteOnServer_(nbWritten) = itSrv->second;
          localIndexToWriteOnClient_(nbWritten) = it->second;
          ++nbWritten;                
        } 
      }
   }
   CATCH_DUMP_ATTR

   

   /*!
     Compute the global index of grid to send to server as well as the connected server of the current client.
     First of all, from the local data on each element of grid, we can calculate their local index which also allows us to know
     their global index. We can have a map of global index of grid and local index that each client holds
     Then, each client holds a piece of information about the distribution of servers, which permits to compute the connected server(s)
     of the current client.
   */
   void CGrid::computeGridIndexToFileServer(CContextClient* client)
   {
     if (isScalarGrid()) computeConnectedClientsScalarGrid(client);
     else computeConnectedClients(client);
    
     // compute indices for client/server transfer for domain
     for (const auto& domainId : domList_) CDomain::get(domainId)->computeConnectedClients(client);
   
   
     // compute indices for client/server transfer for axis
     std::vector<CAxis*> axisList = this->getAxis();
     for(int i=0 ; i<axisList.size(); i++) axisList[i] -> computeConnectedClients(client, getGlobalDimension(),getAxisPositionInGrid()[i]) ;
   }
   //---------------------------------------------------------------

   
   void CGrid::computeClientDistribution(void)
   {
     if (computeClientDistribution_done_) return ;
     else computeClientDistribution_done_ = true ;

     CContext* context = CContext::getCurrent();
     int rank = context-> getIntraCommRank();
     clientDistribution_ = new CDistributionClient(rank, this);
   }

   void CGrid::computeStoreIndex_client(void)
   {
     if (computeStoreIndex_client_done_) return ;
     else computeStoreIndex_client_done_ = true ;
     if (isScalarGrid())
     {
       storeIndex_client_.resize(1);
       storeIndex_client_(0) = 0;
     }
     else
     {
       CDistributionClient* clientDistribution = getClientDistribution() ;
       const std::vector<int>& localDataIndex = clientDistribution->getLocalDataIndexOnClient() ;
       int nbStoreIndex = localDataIndex.size() ;
       storeIndex_client_.resize(nbStoreIndex);
       for (int idx = 0; idx < nbStoreIndex; ++idx) storeIndex_client_(idx) = localDataIndex[idx];
     }
   }

   void CGrid::computeStoreMask_client(void)
   {
     if (computeStoreMask_client_done_) return ;
     else computeStoreMask_client_done_ = true ;
     if (isScalarGrid())
     {
       storeMask_client_.resize(1);
       storeMask_client_(0) = true;
     }
     else
     {
       CDistributionClient* clientDistribution = getClientDistribution() ;
       const std::vector<bool>& localMaskIndex = clientDistribution->getLocalMaskIndexOnClient() ;
       int nbMaskIndex = localMaskIndex.size() ;
       storeMask_client_.resize(nbMaskIndex);
       for (int idx = 0; idx < nbMaskIndex; ++idx) storeMask_client_(idx) = localMaskIndex[idx];
     }
   }


  void CGrid::computeOutLocalIndexStoreOnClient(void)
  {
    if (computeOutLocalIndexStoreOnClient_done_) return ;
    else computeOutLocalIndexStoreOnClient_done_=true ;

    if (isScalarGrid())
    {
        auto& outGlobalIndexFromClient  = getOutGlobalIndexFromClient();
        auto itb = outGlobalIndexFromClient.begin(), ite = outGlobalIndexFromClient.end() ;
        for (auto it = itb; it != ite; ++it)
        {
          int rank = it->first;
          CArray<size_t,1>& globalIndex = outGlobalIndexFromClient[rank];
          outLocalIndexStoreOnClient_.insert(make_pair(rank, CArray<size_t,1>(globalIndex.numElements())));
          CArray<size_t,1>& localIndex = outLocalIndexStoreOnClient_[rank];
          if (1 != globalIndex.numElements())
            ERROR("void CGrid::computeClientIndexScalarGrid()",
              << "Something wrong happened. "
              << "Number of received global index on scalar grid should equal to 1" 
              << "Number of received global index " << globalIndex.numElements() << ".");

          localIndex(0) = globalIndex(0);
        }
    }
    else
    {
      CDistributionClient::GlobalLocalDataMap& globalDataIndex = getClientDistribution()->getGlobalDataIndexOnClient();
      CDistributionClient::GlobalLocalDataMap::const_iterator itGloe = globalDataIndex.end();
      auto& outGlobalIndexFromClient  = getOutGlobalIndexFromClient();
      auto itb = outGlobalIndexFromClient.begin(), ite = outGlobalIndexFromClient.end() ;
    
      for (auto it = itb; it != ite; ++it)
      {
         int rank = it->first;
         CArray<size_t,1>& globalIndex = outGlobalIndexFromClient[rank];
         outLocalIndexStoreOnClient_.insert(make_pair(rank, CArray<size_t,1>(globalIndex.numElements())));
         CArray<size_t,1>& localIndex = outLocalIndexStoreOnClient_[rank];
         size_t nbIndex = 0;

        // Keep this code for this moment but it should be removed (or moved to DEBUG) to improve performance
        for (size_t idx = 0; idx < globalIndex.numElements(); ++idx)
          if (itGloe != globalDataIndex.find(globalIndex(idx))) ++nbIndex;

        if (nbIndex != localIndex.numElements())
             ERROR("void CGrid::computeClientIndex()",
                 << "Number of local index on client is different from number of received global index"
                 << "Rank of sent client " << rank <<"."
                 << "Number of local index " << nbIndex << ". "
                 << "Number of received global index " << localIndex.numElements() << ".");

        nbIndex = 0;
        for (size_t idx = 0; idx < globalIndex.numElements(); ++idx)
          if (itGloe != globalDataIndex.find(globalIndex(idx)))
            localIndex(idx) = globalDataIndex[globalIndex(idx)];
      }
    }
  }

  bool CGrid::isDataDistributed(void) 
  { 
    return getClientDistribution()->isDataDistributed() ;
  }

   /*!
     Compute connected receivers and indexes to be sent to these receivers.
   */
   void CGrid::computeConnectedClients(CContextClient* client)
   TRY
   {
     if (computeConnectedClients_done_.count(client)!=0) return ;
     else  computeConnectedClients_done_.insert(client) ;

     CContext* context = CContext::getCurrent();
     
     set<int> listReceiverSize ;
     int receiverSize = client->serverSize;
      
     if (listReceiverSize.find(receiverSize)==listReceiverSize.end())
     {
       listReceiverSize.insert(receiverSize) ;
       if (connectedServerRank_.find(receiverSize) != connectedServerRank_.end())
       {
          // delete corresponding map in case of recompute, probably because a grid could has been modifiedd 
          // by a transformation
          connectedServerRank_.erase(receiverSize);
          connectedDataSize_.erase(receiverSize);
          globalIndexOnServer_.erase(receiverSize);
          nbSenders_.erase(receiverSize);
       }

       if (!doGridHaveDataDistributed(client))
       {
          if (client->isServerLeader())
          {
            size_t ssize = getClientDistribution()->getLocalDataIndexOnClient().size();
            const std::list<int>& ranks = client->getRanksServerLeader();
            for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
            {
              connectedServerRank_[receiverSize].push_back(*itRank);
              connectedDataSize_[receiverSize][*itRank] = ssize;
            }
          }
          return;
       }

       // Compute mapping between client and server
       std::vector<std::unordered_map<size_t,std::vector<int> > > indexServerOnElement;
       CServerDistributionDescription serverDistributionDescription(getGlobalDimension(), client->serverSize);
       std::vector<int> serverZeroIndex = serverDistributionDescription.computeServerGlobalByElement(indexServerOnElement,
                                                                                                    client->clientRank,
                                                                                                    client->clientSize,
                                                                                                    axis_domain_order,
                                                                                                    getDistributedDimension());

       // Even if servers have no index, they must received something from client
       // We only use several client to send "empty" message to these servers
       std::list<int> serverZeroIndexLeader;
       std::list<int> serverZeroIndexNotLeader;
       CContextClient::computeLeader(client->clientRank, client->clientSize, serverZeroIndex.size(), serverZeroIndexLeader, serverZeroIndexNotLeader);
       for (std::list<int>::iterator it = serverZeroIndexLeader.begin(); it != serverZeroIndexLeader.end(); ++it)
         *it = serverZeroIndex[*it];

       if (globalIndexOnServer_.find(receiverSize) == globalIndexOnServer_.end())
         computeIndexByElement(indexServerOnElement, client, globalIndexOnServer_[receiverSize]);

       const CDistributionClient::GlobalLocalDataMap& globalLocalIndexSendToServer = getClientDistribution()->getGlobalLocalDataSendToServer();
       CDistributionClient::GlobalLocalDataMap::const_iterator iteGlobalLocalIndexMap = globalLocalIndexSendToServer.end(), itGlobalLocalIndexMap;
       CClientServerMapping::GlobalIndexMap::const_iterator iteGlobalMap, itbGlobalMap, itGlobalMap;
       itbGlobalMap = globalIndexOnServer_[receiverSize].begin();
       iteGlobalMap = globalIndexOnServer_[receiverSize].end();

       for (itGlobalMap  = itbGlobalMap; itGlobalMap != iteGlobalMap; ++itGlobalMap)
       {
         int serverRank = itGlobalMap->first;
         int indexSize = itGlobalMap->second.size();
         const std::vector<size_t>& indexVec = itGlobalMap->second;
         for (int idx = 0; idx < indexSize; ++idx)
         {
            itGlobalLocalIndexMap = globalLocalIndexSendToServer.find(indexVec[idx]);
            if (iteGlobalLocalIndexMap != itGlobalLocalIndexMap)
            {
              if (connectedDataSize_[receiverSize].end() == connectedDataSize_[receiverSize].find(serverRank))
                connectedDataSize_[receiverSize][serverRank] = 1;
              else
                ++connectedDataSize_[receiverSize][serverRank];
            }
         }
       }

       // Connected servers which really have index
       for (itGlobalMap = itbGlobalMap; itGlobalMap != iteGlobalMap; ++itGlobalMap) 
         connectedServerRank_[receiverSize].push_back(itGlobalMap->first);
     
       // Connected servers which have no index at all
       for (std::list<int>::iterator it = serverZeroIndexLeader.begin(); it != serverZeroIndexLeader.end(); ++it)
         connectedServerRank_[receiverSize].push_back(*it);

       // Even if a client has no index, it must connect to at least one server and
       // send an "empty" data to this server
       if (connectedServerRank_[receiverSize].empty())
         connectedServerRank_[receiverSize].push_back(client->clientRank % client->serverSize);
 
       // Now check if all servers have data to receive. If not, master client will send empty data.
       // This ensures that all servers will participate in collective calls upon receiving even if they have no date to receive.
       std::vector<int> counts (client->clientSize);
       std::vector<int> displs (client->clientSize);
       displs[0] = 0;
       int localCount = connectedServerRank_[receiverSize].size() ;
       MPI_Gather(&localCount, 1, MPI_INT, &counts[0], 1, MPI_INT, 0, client->intraComm) ;
       for (int i = 0; i < client->clientSize-1; ++i) displs[i+1] = displs[i] + counts[i];
       std::vector<int> allConnectedServers(displs[client->clientSize-1]+counts[client->clientSize-1]);
       MPI_Gatherv(&(connectedServerRank_[receiverSize])[0], localCount, MPI_INT, &allConnectedServers[0], &counts[0], &displs[0], MPI_INT, 0, client->intraComm);

       if ((allConnectedServers.size() != receiverSize) && (client->clientRank == 0))
       {
         std::vector<bool> isSrvConnected (receiverSize, false);
         for (int i = 0; i < allConnectedServers.size(); ++i) isSrvConnected[allConnectedServers[i]] = true;
         for (int i = 0; i < receiverSize; ++i) if (!isSrvConnected[i]) connectedServerRank_[receiverSize].push_back(i);
       }

       nbSenders_[receiverSize] = CClientServerMapping::computeConnectedClients(receiverSize, client->clientSize, client->intraComm, connectedServerRank_[receiverSize]);
     }
   }
   CATCH_DUMP_ATTR

   /*!
     Compute the global index of grid to send to server as well as the connected server of the current client.
     First of all, from the local data on each element of grid, we can calculate their local index which also allows us to know
     their global index. We can have a map of global index of grid and local index that each client holds
     Then, each client holds a piece of information about the distribution of servers, which permits to compute the connected server(s)
     of the current client.
   */
   // ym obsolete : to be removed....
   void CGrid::computeIndex(void)
   TRY
   {
    // old interface
     CContext* context = CContext::getCurrent();
     if (isScalarGrid())
     {
       //computeClientIndexScalarGrid();
       if (context->getServiceType()==CServicesManager::CLIENT || context->getServiceType()==CServicesManager::GATHERER)
       {
         // ym computeConnectedClientsScalarGrid();
       }
     }
     else
     {
       //computeClientIndex();
       if (context->getServiceType()==CServicesManager::CLIENT || context->getServiceType()==CServicesManager::GATHERER)
       {
         //computeConnectedClients();
       }
     }
//ym     if (CServer::serverLevel==2)
     if (context->getServiceType()==CServicesManager::OUT_SERVER)
     {
       computeWrittenIndex() ;
       if (serverDistribution_!=0) serverDistribution_->partialClear() ;
       if (clientDistribution_!=0) clientDistribution_->partialClear() ;
       outGlobalIndexFromClient_.clear() ;
     }
   }
   CATCH_DUMP_ATTR

   /*!
      Compute the global of (client) grid to send to server with the global index of each element of grid
      Each element of grid has its own global index associated to a groups of server. We only search for the global index of each element whose
      server is the same, then calculate the global index of grid. This way can reduce so much the time for executing DHT, which only needs to run
      on each element whose size is much smaller than one of whole grid.
      \param [in] indexServerOnElement global index of each element and the rank of server associated with these index
      \param [in] client contextClient
      \param [out] globalIndexOnServer global index of grid and its corresponding rank of server.
   */
   void CGrid::computeIndexByElement(const std::vector<std::unordered_map<size_t,std::vector<int> > >& indexServerOnElement,
                                     const CContextClient* client,
                                     CClientServerMapping::GlobalIndexMap& globalIndexOnServer)
   TRY
   {
     int serverSize = client->serverSize;

     std::vector<CDomain*> domList = getDomains();
     std::vector<CAxis*> axisList = getAxis();

     // Some pre-calculations of global index on each element of current grid.
     int nbElement = axis_domain_order.numElements();
     std::vector<CArray<size_t,1> > globalIndexElement(nbElement);
     int domainIdx = 0, axisIdx = 0, scalarIdx = 0;
     std::vector<size_t> elementNGlobal(nbElement);
     elementNGlobal[0] = 1;
     size_t globalSize = 1;
     for (int idx = 0; idx < nbElement; ++idx)
     {
       elementNGlobal[idx] = globalSize;
       size_t elementSize;
       size_t elementGlobalSize = 1;
       if (2 == axis_domain_order(idx)) // This is domain
       {
         elementSize = domList[domainIdx]->i_index.numElements();
         globalIndexElement[idx].resize(elementSize);
         for (int jdx = 0; jdx < elementSize; ++jdx)
         {
           globalIndexElement[idx](jdx) = (domList[domainIdx]->i_index)(jdx) + domList[domainIdx]->ni_glo * (domList[domainIdx]->j_index)(jdx);
         }
         elementGlobalSize = domList[domainIdx]->ni_glo.getValue() * domList[domainIdx]->nj_glo.getValue();
         ++domainIdx;
       }
       else if (1 == axis_domain_order(idx))  // This is axis
       {
         elementSize = axisList[axisIdx]->index.numElements();
         globalIndexElement[idx].resize(elementSize);
         for (int jdx = 0; jdx < elementSize; ++jdx)
         {
           globalIndexElement[idx](jdx) = (axisList[axisIdx]->index)(jdx);
         }
         elementGlobalSize = axisList[axisIdx]->n_glo.getValue();
         ++axisIdx;
       }
       else  // Of course, this is scalar
       {
         globalIndexElement[idx].resize(1);
         globalIndexElement[idx](0) = 0;
         elementGlobalSize = 1;
       }
       globalSize *= elementGlobalSize;
     }

     std::vector<std::vector<bool> > elementOnServer(nbElement, std::vector<bool>(serverSize, false));
     std::vector<std::unordered_map<int,std::vector<size_t> > > globalElementIndexOnServer(nbElement);
     CArray<int,1> nbIndexOnServer(serverSize); // Number of distributed global index held by each client for each server
     // Number of temporary distributed global index held by each client for each server
     // We have this variable for the case of non-distributed element (often axis) to check the duplicate server rank
     CArray<int,1> nbIndexOnServerTmp(serverSize);
     for (int idx = 0; idx < nbElement; ++idx)
     {
       nbIndexOnServer = 0;
       const std::unordered_map<size_t,std::vector<int> >& indexServerElement = indexServerOnElement[idx];
       const CArray<size_t,1>& globalIndexElementOnClient = globalIndexElement[idx];
       CClientClientDHTInt clientClientDHT(indexServerElement, client->intraComm);
       clientClientDHT.computeIndexInfoMapping(globalIndexElementOnClient);
       const CClientClientDHTInt::Index2VectorInfoTypeMap& globalIndexElementOnServerMap = clientClientDHT.getInfoIndexMap();
       CClientClientDHTInt::Index2VectorInfoTypeMap::const_iterator itb = globalIndexElementOnServerMap.begin(),
                                                                    ite = globalIndexElementOnServerMap.end(), it;
       for (it = itb; it != ite; ++it)
       {
         const std::vector<int>& tmp = it->second;
         nbIndexOnServerTmp = 0;
         for (int i = 0; i < tmp.size(); ++i)
         {
           if (0 == nbIndexOnServerTmp(tmp[i])) ++nbIndexOnServerTmp(tmp[i]);
         }
         nbIndexOnServer += nbIndexOnServerTmp;
       }

       for (int i = 0; i < serverSize; ++i)
       {
         if (0 != nbIndexOnServer(i))
         {
           globalElementIndexOnServer[idx][i].resize(nbIndexOnServer(i));
           elementOnServer[idx][i] = true;
         }
       }

     nbIndexOnServer = 0;
     for (size_t j = 0; j < globalIndexElementOnServerMap.size(); ++j)
     {
       it = globalIndexElementOnServerMap.find(globalIndexElementOnClient(j));
       if (it != ite)
       {
         const std::vector<int>& tmp = it->second;
         nbIndexOnServerTmp = 0;
         for (int i = 0; i < tmp.size(); ++i)
         {
           if (0 == nbIndexOnServerTmp(tmp[i]))
           {
             globalElementIndexOnServer[idx][tmp[i]][nbIndexOnServer(tmp[i])] = it->first;
             ++nbIndexOnServerTmp(tmp[i]);
           }
         }
         nbIndexOnServer += nbIndexOnServerTmp;
       }
     }
   }

    // Determine server which contain global source index
    std::vector<bool> intersectedProc(serverSize, true);
    for (int idx = 0; idx < nbElement; ++idx)
    {
      std::transform(elementOnServer[idx].begin(), elementOnServer[idx].end(),
                     intersectedProc.begin(), intersectedProc.begin(),
                     std::logical_and<bool>());
    }

    std::vector<int> srcRank;
    for (int idx = 0; idx < serverSize; ++idx)
    {
      if (intersectedProc[idx]) srcRank.push_back(idx);
    }

    // Compute the global index of grid from global index of each element.
    for (int i = 0; i < srcRank.size(); ++i)
    {
      size_t ssize = 1;
      int rankSrc = srcRank[i];
      std::vector<std::vector<size_t>* > globalIndexOfElementTmp(nbElement);
      std::vector<size_t> currentIndex(nbElement,0);
      for (int idx = 0; idx < nbElement; ++idx)
      {
        ssize *= (globalElementIndexOnServer[idx][rankSrc]).size();
        globalIndexOfElementTmp[idx] = &(globalElementIndexOnServer[idx][rankSrc]);
      }
      globalIndexOnServer[rankSrc].resize(ssize);

      std::vector<int> idxLoop(nbElement,0);
      int innnerLoopSize = (globalIndexOfElementTmp[0])->size();
      size_t idx = 0;
      while (idx < ssize)
      {
        for (int ind = 0; ind < nbElement; ++ind)
        {
          if (idxLoop[ind] == (globalIndexOfElementTmp[ind])->size())
          {
            idxLoop[ind] = 0;
            ++idxLoop[ind+1];
          }

          currentIndex[ind] = (*(globalIndexOfElementTmp[ind]))[idxLoop[ind]];
        }

        for (int ind = 0; ind < innnerLoopSize; ++ind)
        {
          currentIndex[0] = (*globalIndexOfElementTmp[0])[ind];
          size_t globalSrcIndex = 0;
          for (int idxElement = 0; idxElement < nbElement; ++idxElement)
          {
            globalSrcIndex += currentIndex[idxElement] * elementNGlobal[idxElement];
          }
          globalIndexOnServer[rankSrc][idx] = globalSrcIndex;
          ++idx;
          ++idxLoop[0];
        }
      }
    }
   }
   CATCH_DUMP_ATTR
//----------------------------------------------------------------



   CGrid* CGrid::cloneGrid(const StdString& idNewGrid, CGrid* gridSrc)
   TRY
   {
     std::vector<CDomain*> domainSrcTmp = gridSrc->getDomains(), domainSrc;
     std::vector<CAxis*> axisSrcTmp = gridSrc->getAxis(), axisSrc;
     std::vector<CScalar*> scalarSrcTmp = gridSrc->getScalars(), scalarSrc;

     for (int idx = 0; idx < domainSrcTmp.size(); ++idx)
     {
       CDomain* domain = CDomain::createDomain();
       domain->duplicateAttributes(domainSrcTmp[idx]);
       domain->duplicateTransformation(domainSrcTmp[idx]);
       domain->solveRefInheritance(true);
       domain->solveInheritanceTransformation();
       domainSrc.push_back(domain);
     }

     for (int idx = 0; idx < axisSrcTmp.size(); ++idx)
     {
       CAxis* axis = CAxis::createAxis();
       axis->duplicateAttributes(axisSrcTmp[idx]);
       axis->duplicateTransformation(axisSrcTmp[idx]);
       axis->solveRefInheritance(true);
       axis->solveInheritanceTransformation();
       axisSrc.push_back(axis);
     }

     for (int idx = 0; idx < scalarSrcTmp.size(); ++idx)
     {
       CScalar* scalar = CScalar::createScalar();
       scalar->duplicateAttributes(scalarSrcTmp[idx]);
       scalar->duplicateTransformation(scalarSrcTmp[idx]);
       scalar->solveRefInheritance(true);
       scalar->solveInheritanceTransformation();
       scalarSrc.push_back(scalar);
     }

      CGrid* grid = CGrid::createGrid(idNewGrid, domainSrc, axisSrc, scalarSrc, gridSrc->axis_domain_order);

      return grid;
   }
   CATCH

   StdString CGrid::generateId(const std::vector<CDomain*>& domains, const std::vector<CAxis*>& axis,
                               const std::vector<CScalar*>& scalars, const CArray<int,1>& axisDomainOrder)
   TRY
   {
      if (axisDomainOrder.numElements() > 0 && axisDomainOrder.numElements() != (domains.size() + axis.size() + scalars.size()))
        ERROR("CGrid* CGrid::generateId(...)",
              << "The size of axisDomainOrder (" << axisDomainOrder.numElements()
              << ") is not coherent with the number of elements (" << domains.size() + axis.size() <<").");

      std::ostringstream id;

      if (domains.empty() && axis.empty() && !scalars.empty())
        id << "__scalar_";

      if (0 != (domains.size() + axis.size() + scalars.size()))
      {
        id << "__grid";

        if (0 == axisDomainOrder.numElements())
        {
          for (size_t i = 0; i < domains.size(); ++i) id << "_" << domains[i]->getId();
          for (size_t i = 0; i < axis.size(); ++i) id << "_" << axis[i]->getId();
          for (size_t i = 0; i < scalars.size(); ++i) id << "_" << scalars[i]->getId();
        }
        else
        {
          size_t iDomain = 0, iAxis = 0, iScalar = 0;
          for (size_t i = 0; i < axisDomainOrder.numElements(); ++i)
          {
            if (2 == axisDomainOrder(i))
              id << "_" << domains[iDomain++]->getId();
            else if (1 == axisDomainOrder(i))
              id << "_" << axis[iAxis++]->getId();
            else
              id << "_" << scalars[iScalar++]->getId();
          }
        }

        id << "__";
      }

      return id.str();
   }
   CATCH

   StdString CGrid::generateId(const CGrid* gridSrc, const CGrid* gridDest)
   TRY
   {
     StdString idSrc  = gridSrc->getId();
     StdString idDest = gridDest->getId();

     std::ostringstream id;
     id << idSrc << "__" << idDest;

     return id.str();
   }
   CATCH


   //----------------------------------------------------------------

   void CGrid::storeField_arr(const double* const data, CArray<double, 1>& stored) 
   TRY
   {
      auto& storeIndex_client = getStoreIndex_client() ;
      const StdSize size = storeIndex_client.numElements();

      stored.resize(size);
      for(StdSize i = 0; i < size; i++) stored(i) = data[storeIndex_client(i)];
   }
   CATCH

   void CGrid::restoreField_arr(const CArray<double, 1>& stored, double* const data) 
   TRY
   {
      auto& storeIndex_client=getStoreIndex_client() ;
      const StdSize size = storeIndex_client.numElements();

      for(StdSize i = 0; i < size; i++) data[storeIndex_client(i)] = stored(i);
   }
   CATCH

   void CGrid::maskField_arr(const double* const data, CArray<double, 1>& stored) 
   {
      auto& storeIndex_client=getStoreIndex_client() ;
      auto& storeMask_client=getStoreMask_client() ;
        
      const StdSize size = storeIndex_client.numElements();
      stored.resize(size);
      const double nanValue = std::numeric_limits<double>::quiet_NaN();

      if (storeMask_client.numElements() != 0)
        for(StdSize i = 0; i < size; i++) stored(i) = (storeMask_client(i)) ? data[storeIndex_client(i)] : nanValue;
      else
        for(StdSize i = 0; i < size; i++) stored(i) = data[storeIndex_client(i)];
   }

   void CGrid::uncompressField_arr(const double* const data, CArray<double, 1>& out) 
   TRY
   {
      const std::vector<int>& localMaskedDataIndex = getClientDistribution()->getLocalMaskedDataIndexOnClient();
      const int size = localMaskedDataIndex.size();
      for(int i = 0; i < size; ++i) out(localMaskedDataIndex[i]) = data[i];
   }
   CATCH

  
  void CGrid::computeConnectedClientsScalarGrid(CContextClient* client)
  TRY
  {
    if (computeConnectedClientsScalarGrid_done_.count(client)!=0) return ;

    CContext* context = CContext::getCurrent();
     
    set<int> listReceiverSize ;

    int receiverSize = client->serverSize;
      
    if (listReceiverSize.find(receiverSize)==listReceiverSize.end())
    {
      listReceiverSize.insert(receiverSize) ;
      if (connectedServerRank_.find(receiverSize) != connectedServerRank_.end())
      {
         // delete corresponding map in case of recompute, probably because a grid could has been modifiedd 
         // by a transformation
        connectedServerRank_.erase(receiverSize);
        connectedDataSize_.erase(receiverSize);
        globalIndexOnServer_.erase(receiverSize);
        nbSenders_.erase(receiverSize);
      }

      if (client->isServerLeader())
      {
        const std::list<int>& ranks = client->getRanksServerLeader();
        for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
        {
          int rank = *itRank;
          int nb = 1;
          connectedServerRank_[receiverSize].push_back(rank);
          connectedDataSize_[receiverSize][rank] = nb;
          nbSenders_[receiverSize][rank] = nb;
        }
      }
      else
      {
        const std::list<int>& ranks = client->getRanksServerNotLeader();
        for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
        {
          int rank = *itRank;
          int nb = 1;
          connectedServerRank_[receiverSize].push_back(rank);
          connectedDataSize_[receiverSize][rank] = nb;
          nbSenders_[receiverSize][rank] = nb;
        }
      }
    }
   
    computeConnectedClientsScalarGrid_done_.insert(client) ;
  }
  CATCH_DUMP_ATTR

  void CGrid::sendIndexScalarGrid(CContextClient* client, const string& gridId)
  TRY
  {
    if (sendIndexScalarGrid_done_.count(client)!=0) return ;
    else sendIndexScalarGrid_done_.insert(client) ;

    CContext* context = CContext::getCurrent();
    
    string serverGridId = gridId.empty() ? serverGridId=this->getId() : serverGridId=gridId ;

    int receiverSize = client->serverSize;

    CEventClient event(getType(), EVENT_ID_INDEX);
    list<CMessage> listMsg;
    list<CArray<size_t,1> > listOutIndex;

    if (client->isServerLeader())
    {
      const std::list<int>& ranks = client->getRanksServerLeader();
      for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
      {
        int rank = *itRank;
        int nb = 1;
        storeIndex_toSrv_[client].insert(std::make_pair(rank, CArray<int,1>(nb)));
        listOutIndex.push_back(CArray<size_t,1>(nb));

        CArray<int, 1>& outLocalIndexToServer = storeIndex_toSrv_[client][rank];
        CArray<size_t, 1>& outGlobalIndexOnServer = listOutIndex.back();

        for (int k = 0; k < nb; ++k)
        {
          outGlobalIndexOnServer(k) = 0;
          outLocalIndexToServer(k)  = 0;
        }

        if (context->getServiceType()==CServicesManager::CLIENT)  // -> what about for coupling probably unusefull to be check
          storeIndex_fromSrv_.insert(std::make_pair(rank, CArray<int,1>(outLocalIndexToServer)));

        listMsg.push_back(CMessage());
        listMsg.back() << serverGridId << isCompressible_ << listOutIndex.back();

        event.push(rank, 1, listMsg.back());
      }
      client->sendEvent(event);
    }
    else
    {
      const std::list<int>& ranks = client->getRanksServerNotLeader();
      for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
      {
        int rank = *itRank;
        int nb = 1;          
        CArray<int, 1> outLocalIndexToServer(nb);
        for (int k = 0; k < nb; ++k)
        {
          outLocalIndexToServer(k)  = 0;
        }

        if (context->getServiceType()==CServicesManager::CLIENT)
          storeIndex_fromSrv_.insert(std::make_pair(rank, CArray<int,1>(outLocalIndexToServer)));
      }
      client->sendEvent(event);
    }
  }
  CATCH_DUMP_ATTR

  void CGrid::sendIndex(CContextClient* client, const string& gridId)
  TRY
  {
    if (sendIndex_done_.count(client)!=0) return ;
    else sendIndex_done_.insert(client) ;
    CContext* context = CContext::getCurrent();
    string serverGridId = gridId.empty() ? this->getId() : gridId ;



    int receiverSize = client->serverSize;

    CEventClient event(getType(), EVENT_ID_INDEX);
    int rank;
    list<CMessage> listMsg;
    list<CArray<size_t,1> > listOutIndex;
    const CDistributionClient::GlobalLocalDataMap& globalLocalIndexSendToServer = getClientDistribution()->getGlobalLocalDataSendToServer();
    CDistributionClient::GlobalLocalDataMap::const_iterator itbIndex = globalLocalIndexSendToServer.begin(), itIndex,
                                                            iteIndex = globalLocalIndexSendToServer.end();
    itIndex = itbIndex;                                                              

    if (!doGridHaveDataDistributed(client))
    {
      if (client->isServerLeader())
      {
        int indexSize = globalLocalIndexSendToServer.size();
        CArray<size_t,1> outGlobalIndexOnServer(indexSize);
        CArray<int,1> outLocalIndexToServer(indexSize);
        for (int idx = 0; itIndex != iteIndex; ++itIndex, ++idx)
        {
          outGlobalIndexOnServer(idx) = itIndex->first;
          outLocalIndexToServer(idx) = itIndex->second;
        }

        const std::list<int>& ranks = client->getRanksServerLeader();
        for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
        {
          storeIndex_toSrv_[client].insert(std::make_pair(*itRank, CArray<int,1>(outLocalIndexToServer)));
          if (context->getServiceType()==CServicesManager::CLIENT)  // -> what about for coupling probably unusefull to be check
            storeIndex_fromSrv_.insert(std::make_pair(*itRank, CArray<int,1>(outLocalIndexToServer)));
          
          listOutIndex.push_back(CArray<size_t,1>(outGlobalIndexOnServer));

          listMsg.push_back(CMessage());
          listMsg.back() << serverGridId << isCompressible_ << listOutIndex.back();

          event.push(*itRank, 1, listMsg.back());
        }
        client->sendEvent(event);
      }
      else
      {
         int indexSize = globalLocalIndexSendToServer.size();
         CArray<int,1> outLocalIndexToServer(indexSize);
         for (int idx = 0; itIndex != iteIndex; ++itIndex, ++idx)
         {
           outLocalIndexToServer(idx) = itIndex->second;
         }

         const std::list<int>& ranks = client->getRanksServerNotLeader();
         for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
         {
           storeIndex_fromSrv_.insert(std::make_pair(*itRank, CArray<int,1>(outLocalIndexToServer)));
         }
         client->sendEvent(event);
       }
    }
    else
    {
      CClientServerMapping::GlobalIndexMap::const_iterator iteGlobalMap, itGlobalMap;
      itGlobalMap = globalIndexOnServer_[receiverSize].begin();
      iteGlobalMap = globalIndexOnServer_[receiverSize].end();

      std::map<int,std::vector<int> >localIndexTmp;
      std::map<int,std::vector<size_t> > globalIndexTmp;
      for (; itGlobalMap != iteGlobalMap; ++itGlobalMap)
      {
        int serverRank = itGlobalMap->first;
        int indexSize = itGlobalMap->second.size();
        const std::vector<size_t>& indexVec = itGlobalMap->second;
        for (int idx = 0; idx < indexSize; ++idx)
        {
          itIndex = globalLocalIndexSendToServer.find(indexVec[idx]);
          if (iteIndex != itIndex)
          {
            globalIndexTmp[serverRank].push_back(itIndex->first);
            localIndexTmp[serverRank].push_back(itIndex->second);
          }
        }
      }

      for (int ns = 0; ns < connectedServerRank_[receiverSize].size(); ++ns)
      {
        rank = connectedServerRank_[receiverSize][ns];
        int nb = 0;
        if (globalIndexTmp.end() != globalIndexTmp.find(rank))
          nb = globalIndexTmp[rank].size();

        storeIndex_toSrv_[client].insert(make_pair(rank, CArray<int,1>(nb)));
        listOutIndex.push_back(CArray<size_t,1>(nb));

        CArray<int, 1>& outLocalIndexToServer = storeIndex_toSrv_[client][rank];
        CArray<size_t, 1>& outGlobalIndexOnServer = listOutIndex.back();

        for (int k = 0; k < nb; ++k)
        {
          outGlobalIndexOnServer(k) = globalIndexTmp[rank].at(k);
          outLocalIndexToServer(k)  = localIndexTmp[rank].at(k);
        }

        storeIndex_fromSrv_.insert(make_pair(rank, CArray<int,1>(outLocalIndexToServer)));
        listMsg.push_back(CMessage());
        listMsg.back() << serverGridId  << isCompressible_ << listOutIndex.back();

        event.push(rank, nbSenders_[receiverSize][rank], listMsg.back());
      }
      client->sendEvent(event);
    }
  }
  CATCH_DUMP_ATTR

  void CGrid::recvIndex(CEventServer& event)
  TRY
  {
    string gridId;
    vector<int> ranks;
    vector<CBufferIn*> buffers;

    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {
      ranks.push_back(it->rank);
      CBufferIn* buffer = it->buffer;
      *buffer >> gridId;
      buffers.push_back(buffer);
    }
    get(gridId)->recvIndex(ranks, buffers, event.getContextServer());
  }
  CATCH

  void CGrid::recvIndex(vector<int> ranks, vector<CBufferIn*> buffers, CContextServer* server)
  TRY
  {
    CContextClient* client = server->getAssociatedClient();
    connectedServerRankRead_ = ranks;
    for (int n = 0; n < ranks.size(); n++)
    {
      int rank = ranks[n];
      CBufferIn& buffer = *buffers[n];
      buffer >>  isCompressible_; // probably to be removed later
      CArray<size_t,1> outIndex;
      buffer >> outIndex;
      outGlobalIndexFromClient_.insert(std::make_pair(rank, outIndex));
      connectedDataSizeRead_[rank] = outIndex.numElements();
    }
    // ym : displaced to avoid collective call at message reception
/* 
    nbReadSenders_[client] = CClientServerMappingDistributed::computeConnectedClients(client->serverSize, client->clientSize,
                                                                                     client->intraComm, ranks);
*/
  }
  CATCH_DUMP_ATTR
  
  
  /*!
   * Compute the number of connected client for a given contextClient and insert it in the nbReadSenders map.
   * /param[in] client : the given contextClient
   */
  void CGrid::computeNbReadSenders(CContextClient* client) 
  TRY
  
  { 
    nbReadSenders_[client] = CClientServerMappingDistributed::computeConnectedClients(client->serverSize, client->clientSize, client->intraComm, connectedServerRankRead_);
  }
  CATCH_DUMP_ATTR
  
  void CGrid::computeServerDistribution(void)
  TRY
  {
    if (computeServerDistribution_done_) return ;
    else computeServerDistribution_done_=true ;

    CContext* context = CContext::getCurrent();
      
    int idx = 0, numElement = axis_domain_order.numElements();
    int ssize = numElement;
    std::vector<int> indexMap(numElement);
    for (int i = 0; i < numElement; ++i)
    {
      indexMap[i] = idx;
      if (2 == axis_domain_order(i))
      {
        ++ssize;
        idx += 2;
      }
      else
        ++idx;
    }

    for (int n = 0; n < connectedServerRankRead_.size(); n++)
    {
      int rank = connectedServerRankRead_[n];
      size_t dataSize = 0;

      if (0 == serverDistribution_)
      {
        int axisId = 0, domainId = 0, scalarId = 0, globalSize = 1;
        std::vector<CDomain*> domainList = getDomains();
        std::vector<CAxis*> axisList = getAxis();
        std::vector<int> nBegin(ssize), nSize(ssize), nGlob(ssize), nBeginGlobal(ssize), nGlobElement(numElement);
        std::vector<CArray<int,1> > globalIndex(numElement);
        for (int i = 0; i < numElement; ++i)
        {
          nGlobElement[i] = globalSize;
          if (2 == axis_domain_order(i)) //domain
          {
            nBegin[indexMap[i]] = domainList[domainId]->ibegin;
            nSize[indexMap[i]]  = domainList[domainId]->ni;
            nBeginGlobal[indexMap[i]] = 0;
            nGlob[indexMap[i]] = domainList[domainId]->ni_glo;

            nBegin[indexMap[i] + 1] = domainList[domainId]->jbegin;
            nSize[indexMap[i] + 1] = domainList[domainId]->nj;
            nBeginGlobal[indexMap[i] + 1] = 0;
            nGlob[indexMap[i] + 1] = domainList[domainId]->nj_glo;

            {
              int count = 0;
              globalIndex[i].resize(nSize[indexMap[i]]*nSize[indexMap[i]+1]);
              for (int jdx = 0; jdx < nSize[indexMap[i]+1]; ++jdx)
                for (int idx = 0; idx < nSize[indexMap[i]]; ++idx)
                {
                  globalIndex[i](count) = (nBegin[indexMap[i]] + idx) + (nBegin[indexMap[i]+1] + jdx) * nGlob[indexMap[i]];
                  ++count;
                }
            }

            ++domainId;
          }
          else if (1 == axis_domain_order(i)) // axis
          {
            nBegin[indexMap[i]] = axisList[axisId]->begin;
            nSize[indexMap[i]]  = axisList[axisId]->n;
            nBeginGlobal[indexMap[i]] = 0;
            nGlob[indexMap[i]] = axisList[axisId]->n_glo;     
            globalIndex[i].resize(nSize[indexMap[i]]);
            for (int idx = 0; idx < nSize[indexMap[i]]; ++idx)
              globalIndex[i](idx) = nBegin[indexMap[i]] + idx;

            ++axisId;
          }
          else // scalar
          { 
            nBegin[indexMap[i]] = 0;
            nSize[indexMap[i]]  = 1;
            nBeginGlobal[indexMap[i]] = 0;
            nGlob[indexMap[i]] = 1;
            globalIndex[i].resize(1);
            globalIndex[i](0) = 0;
            ++scalarId;
          }
        }
        dataSize = 1;

        for (int i = 0; i < nSize.size(); ++i)
        dataSize *= nSize[i];
        serverDistribution_ = new CDistributionServer(context->intraCommRank_, 
                                                      globalIndex, axis_domain_order,
                                                      nBegin, nSize, nBeginGlobal, nGlob);
      }
    }
  }
  CATCH_DUMP_ATTR






/* old interface => transform into compute receivedIndex
  void CGrid::recvIndex(vector<int> ranks, vector<CBufferIn*> buffers, CContextServer* server)
  TRY
  {
    CContext* context = CContext::getCurrent();
    connectedServerRankRead_ = ranks;

    nbReadSenders_.clear();
    CContextClient* client = server->getAssociatedClient();   
      
    int idx = 0, numElement = axis_domain_order.numElements();
    int ssize = numElement;
    std::vector<int> indexMap(numElement);
    for (int i = 0; i < numElement; ++i)
    {
      indexMap[i] = idx;
      if (2 == axis_domain_order(i))
      {
        ++ssize;
        idx += 2;
      }
      else
        ++idx;
    }

    for (int n = 0; n < ranks.size(); n++)
    {
      int rank = ranks[n];
      CBufferIn& buffer = *buffers[n];

      buffer >>  isCompressible_;
      size_t dataSize = 0;

      if (0 == serverDistribution_)
      {
        int axisId = 0, domainId = 0, scalarId = 0, globalSize = 1;
        std::vector<CDomain*> domainList = getDomains();
        std::vector<CAxis*> axisList = getAxis();
        std::vector<int> nBegin(ssize), nSize(ssize), nGlob(ssize), nBeginGlobal(ssize), nGlobElement(numElement);
        std::vector<CArray<int,1> > globalIndex(numElement);
        for (int i = 0; i < numElement; ++i)
        {
          nGlobElement[i] = globalSize;
          if (2 == axis_domain_order(i)) //domain
          {
            nBegin[indexMap[i]] = domainList[domainId]->ibegin;
            nSize[indexMap[i]]  = domainList[domainId]->ni;
            nBeginGlobal[indexMap[i]] = 0;
            nGlob[indexMap[i]] = domainList[domainId]->ni_glo;

            nBegin[indexMap[i] + 1] = domainList[domainId]->jbegin;
            nSize[indexMap[i] + 1] = domainList[domainId]->nj;
            nBeginGlobal[indexMap[i] + 1] = 0;
            nGlob[indexMap[i] + 1] = domainList[domainId]->nj_glo;

            {
              int count = 0;
              globalIndex[i].resize(nSize[indexMap[i]]*nSize[indexMap[i]+1]);
              for (int jdx = 0; jdx < nSize[indexMap[i]+1]; ++jdx)
                for (int idx = 0; idx < nSize[indexMap[i]]; ++idx)
                {
                  globalIndex[i](count) = (nBegin[indexMap[i]] + idx) + (nBegin[indexMap[i]+1] + jdx) * nGlob[indexMap[i]];
                  ++count;
                }
            }

            ++domainId;
          }
          else if (1 == axis_domain_order(i)) // axis
          {
            nBegin[indexMap[i]] = axisList[axisId]->begin;
            nSize[indexMap[i]]  = axisList[axisId]->n;
            nBeginGlobal[indexMap[i]] = 0;
            nGlob[indexMap[i]] = axisList[axisId]->n_glo;     
            globalIndex[i].resize(nSize[indexMap[i]]);
            for (int idx = 0; idx < nSize[indexMap[i]]; ++idx)
              globalIndex[i](idx) = nBegin[indexMap[i]] + idx;

            ++axisId;
          }
          else // scalar
          { 
            nBegin[indexMap[i]] = 0;
            nSize[indexMap[i]]  = 1;
            nBeginGlobal[indexMap[i]] = 0;
            nGlob[indexMap[i]] = 1;
            globalIndex[i].resize(1);
            globalIndex[i](0) = 0;
            ++scalarId;
          }
        }
        dataSize = 1;

        for (int i = 0; i < nSize.size(); ++i)
        dataSize *= nSize[i];
        serverDistribution_ = new CDistributionServer(context->intraCommRank_, 
                                                      globalIndex, axis_domain_order,
                                                      nBegin, nSize, nBeginGlobal, nGlob);
      }

      CArray<size_t,1> outIndex;
      buffer >> outIndex;
      outGlobalIndexFromClient_.insert(std::make_pair(rank, outIndex));
      connectedDataSizeRead_[rank] = outIndex.numElements();

      if (doGridHaveDataDistributed(client))
      {}
      else
      {
        // THE PROBLEM HERE IS THAT DATA CAN BE NONDISTRIBUTED ON CLIENT AND DISTRIBUTED ON SERVER
        // BELOW IS THE TEMPORARY FIX only for a single type of element (domain, asix, scalar)
        dataSize = serverDistribution_->getGridSize();
      }
      writtenDataSize_ += dataSize;
    }


    // Compute mask of the current grid
    {
      int axisId = 0, domainId = 0, scalarId = 0, globalSize = 1;
      std::vector<CDomain*> domainList = getDomains();
      std::vector<CAxis*> axisList = getAxis();
      int dimSize = 2 * domainList.size() + axisList.size();
      std::vector<int> nBegin(dimSize), nSize(dimSize), nGlob(dimSize), nBeginGlobal(dimSize);        
      for (int i = 0; i < numElement; ++i)
      {          
        if (2 == axis_domain_order(i)) //domain
        {
          nBegin[indexMap[i]] = domainList[domainId]->ibegin;
          nSize[indexMap[i]]  = domainList[domainId]->ni;
          nBeginGlobal[indexMap[i]] = 0;              
          nGlob[indexMap[i]] = domainList[domainId]->ni_glo;

          nBegin[indexMap[i] + 1] = domainList[domainId]->jbegin;
          nSize[indexMap[i] + 1] = domainList[domainId]->nj;
          nBeginGlobal[indexMap[i] + 1] = 0;              
          nGlob[indexMap[i] + 1] = domainList[domainId]->nj_glo;
          ++domainId;
        }
        else if (1 == axis_domain_order(i)) // axis
        {
          nBegin[indexMap[i]] = axisList[axisId]->begin;
          nSize[indexMap[i]]  = axisList[axisId]->n;
          nBeginGlobal[indexMap[i]] = 0;              
          nGlob[indexMap[i]] = axisList[axisId]->n_glo;              
          ++axisId;
        }
        else // scalar
        {  
        }
      }
     
      if (nSize.empty()) // Scalar grid
      {
        nBegin.push_back(0);
        nSize.push_back(1);
        nBeginGlobal.push_back(0);              
        nGlob.push_back(1);  
      }
    }

    if (isScalarGrid()) return;

    nbReadSenders_[client] = CClientServerMappingDistributed::computeConnectedClients(client->serverSize, client->clientSize,
                                                                                      client->intraComm, ranks);

  }
  CATCH_DUMP_ATTR
*/


  /*
     Compute on the fly the global dimension of a grid with its elements
     \param[in/out] globalDim global dimension of grid
     \param[in] domains list of its domains
     \param[in] axiss list of its axis
     \param[in] scalars list of its scalars
     \param[in] axisDomainOrder the order of element in a grid (e.g: scalar then axis)
     \return The dimension of which we do distribution (often for server)
  */
  int CGrid::computeGridGlobalDimension(std::vector<int>& globalDim,
                                        const std::vector<CDomain*> domains,
                                        const std::vector<CAxis*> axis,
                                        const std::vector<CScalar*> scalars,
                                        const CArray<int,1>& axisDomainOrder)
  TRY
 {
 //   globalDim.resize(domains.size()*2+axis.size()+scalars.size());
    globalDim.resize(domains.size()*2+axis.size());
    int positionDimensionDistributed = 1;
    int idx = 0, idxDomain = 0, idxAxis = 0, idxScalar = 0;
    for (int i = 0; i < axisDomainOrder.numElements(); ++i)
    {
      if (2 == axisDomainOrder(i))
      {
        if (!(domains[idxDomain]->type.isEmpty()) && (domains[idxDomain]->type==CDomain::type_attr::unstructured))
        {
          positionDimensionDistributed = idx;
        }
        else
        {
          positionDimensionDistributed = idx +1;
        }

        globalDim[idx]   = domains[idxDomain]->ni_glo.getValue();
        globalDim[idx+1] = domains[idxDomain]->nj_glo.getValue();

        ++idxDomain;
        idx += 2;
      }
      else if (1 == axisDomainOrder(i))
      {
        globalDim[idx] = axis[idxAxis]->n_glo.getValue();
        ++idxAxis;
        ++idx;
      }
      else
      {
//        globalDim[idx] = 1;
        ++idxScalar;
//        ++idx;
      }
    }

    return positionDimensionDistributed;
  }
  CATCH_DUMP_ATTR

  // Retrieve the global dimension of grid
  std::vector<int> CGrid::getGlobalDimension()
  TRY
  {
    std::vector<int> globalDim;
    computeGridGlobalDimension(globalDim, getDomains(), getAxis(), getScalars(), axis_domain_order);

    return globalDim;
  }
  CATCH_DUMP_ATTR

  // Retrieve dimension on which we do distribution (Very often, it should be 2nd dimension)
  int CGrid::getDistributedDimension()
  TRY
  {
    std::vector<int> globalDim;
    return computeGridGlobalDimension(globalDim, getDomains(), getAxis(), getScalars(), axis_domain_order);    
  }
  CATCH_DUMP_ATTR

  bool CGrid::isScalarGrid() const
  TRY
  {
    return (axisList_.empty() && domList_.empty());
  }
  CATCH

  /*!
    Verify whether one server need to write data
    There are some cases on which one server has nodata to write. For example, when we
    just only want to zoom on a domain.
  */
  bool CGrid::doGridHaveDataToWrite()
  TRY
  {
     return (0 != getGridLocalElements()->getView(CElementView::FULL)->getSize());
  }
  CATCH_DUMP_ATTR

  /*!
    Return size of data which is written on each server
    Whatever dimension of a grid, data which are written on server must be presented as
    an one dimension array.
    \return size of data written on server
  */
  size_t CGrid::getWrittenDataSize() 
  TRY
  {
    return getGridLocalElements()->getView(CElementView::FULL)->getSize() ;
  }
  CATCH

  /*!
    Returns the number of indexes written by each server.
    \return the number of indexes written by each server
  */
  int CGrid::getNumberWrittenIndexes() const
  TRY
  {
    return numberWrittenIndexes_;
  }
  CATCH

  /*!
    Returns the total number of indexes written by the servers.
    \return the total number of indexes written by the servers
  */
  int CGrid::getTotalNumberWrittenIndexes() const
  TRY
  {
    return totalNumberWrittenIndexes_;
  }
  CATCH

  /*!
    Returns the offset of indexes written by each server.
    \return the offset of indexes written by each server
  */
  int CGrid::getOffsetWrittenIndexes() const
  TRY
  {
    return offsetWrittenIndexes_;
  }
  CATCH

  
  CDistributionClient* CGrid::getClientDistribution()
  TRY
  {
    if (!computeClientDistribution_done_) computeClientDistribution() ;
    return clientDistribution_;
  }
  CATCH_DUMP_ATTR

  bool CGrid::doGridHaveDataDistributed(CContextClient* client)
  TRY
  {
    // This function is now useless because it will return false only if server and client size are equal to 1
    // to be seriously check in future 

    if (isScalarGrid()) return false;
    else if (0 != client)
    {
      return  (isDataDistributed() ||  (1 != client->clientSize) || (1 != client->serverSize));
    }
    else
      return isDataDistributed();    
  }
  CATCH_DUMP_ATTR

   /*!
   \brief Dispatch event received from client
      Whenever a message is received in buffer of server, it will be processed depending on
   its event type. A new event type should be added in the switch list to make sure
   it processed on server side.
   \param [in] event: Received message
   */
  bool CGrid::dispatchEvent(CEventServer& event)
  TRY
  {

    if (SuperClass::dispatchEvent(event)) return true;
    else
    {
      switch(event.type)
      {
        case EVENT_ID_INDEX :
          recvIndex(event);
          return true;
          break;

         case EVENT_ID_ADD_DOMAIN :
           recvAddDomain(event);
           return true;
           break;

         case EVENT_ID_ADD_AXIS :
           recvAddAxis(event);
           return true;
           break;

         case EVENT_ID_ADD_SCALAR :
           recvAddScalar(event);
           return true;
           break;
        default :
          ERROR("bool CDomain::dispatchEvent(CEventServer& event)",
                << "Unknown Event");
          return false;
      }
    }
  }
  CATCH

 

  void CGrid::sendGridToFileServer(CContextClient* client)
  {
    if (sendGridToFileServer_done_.count(client)!=0) return ;
    else sendGridToFileServer_done_.insert(client) ;

    StdString gridDefRoot("grid_definition");
    CGridGroup* gridPtr = CGridGroup::get(gridDefRoot);
    gridPtr->sendCreateChild(this->getId(),client);
    this->sendAllAttributesToServer(client);
    //if (isScalarGrid())  sendIndexScalarGrid(client);
    //else  sendIndex(client);
    //this->sendAllDomains(client);
    //this->sendAllAxis(client);
    //this->sendAllScalars(client);

    distributeGridToFileServer(client) ;
  }


  void CGrid::distributeGridToFileServer(CContextClient* client)
  {
    CContext* context = CContext::getCurrent();
    // simple Distribution for now 
    // distribute over the fisrt element except if it is a scalar
    auto& elements = getElements() ;
    int posDistributed = 0 ;
    for(auto& element : elements)
    {
      if (element.type==TYPE_DOMAIN) break ;
      else if (element.type==TYPE_AXIS) break ;
      else if (element.type==TYPE_SCALAR) posDistributed++ ;
    }
    
    vector<CLocalView*> localViews ;
    vector<CDistributedView*> remoteViews ;

    for(int i=0 ; i<elements.size() ; i++)
    {
      if (elements[i].type==TYPE_DOMAIN) 
      { 
         CDomain* domain = (CDomain*) elements[i].ptr ;
         domain->computeRemoteElement(client, posDistributed==i ? EDistributionType::BANDS : EDistributionType::NONE) ;
         remoteViews.push_back(domain->getRemoteElement(client)->getView(CElementView::FULL)) ;
         localViews.push_back(domain->getLocalView(CElementView::FULL)) ;
      }
      else if (elements[i].type==TYPE_AXIS)
      {
        CAxis* axis = (CAxis*) elements[i].ptr ;
        axis->computeRemoteElement(client, posDistributed==i ? EDistributionType::BANDS : EDistributionType::NONE) ;
        remoteViews.push_back(axis->getRemoteElement(client)->getView(CElementView::FULL)) ;
        localViews.push_back(axis->getLocalView(CElementView::FULL)) ;
      }
      else if (elements[i].type==TYPE_SCALAR)
      {
        CScalar* scalar = (CScalar*) elements[i].ptr ;
        scalar->computeRemoteElement(client, posDistributed==i ? EDistributionType::BANDS : EDistributionType::NONE) ;
        remoteViews.push_back(scalar->getRemoteElement(client)->getView(CElementView::FULL)) ;
        localViews.push_back(scalar->getLocalView(CElementView::FULL)) ;
      }
    }
    CGridRemoteConnector gridRemoteConnector(localViews, remoteViews, context->getIntraComm(), client->getRemoteSize()) ;
    gridRemoteConnector.computeConnector() ;
    
    vector<CScattererConnector*> clientToServerConnectors ;
    vector<CGathererConnector*>  clientFromServerConnectors ;
    for(int i=0 ; i<elements.size() ; i++)
    {
      if (elements[i].type==TYPE_DOMAIN) 
      { 
         CDomain* domain = (CDomain*) elements[i].ptr ;
         sendAddDomain(domain->getId(),client) ;
         domain->distributeToServer(client, gridRemoteConnector.getDistributedGlobalIndex(i)) ;
         clientToServerConnectors.push_back(domain->getClientToServerConnector(client)) ;
         clientFromServerConnectors.push_back(domain->getClientFromServerConnector(client)) ;
      }
      else if (elements[i].type==TYPE_AXIS)
      {
        CAxis* axis = (CAxis*) elements[i].ptr ;
        sendAddAxis(axis->getId(),client) ;
        axis->distributeToServer(client, gridRemoteConnector.getDistributedGlobalIndex(i)) ;
        clientToServerConnectors.push_back(axis->getClientToServerConnector(client)) ;
        clientFromServerConnectors.push_back(axis->getClientFromServerConnector(client)) ;

      }
      else if (elements[i].type==TYPE_SCALAR)
      {
        CScalar* scalar = (CScalar*) elements[i].ptr ;
        sendAddScalar(scalar->getId(),client) ;
        scalar->distributeToServer(client, gridRemoteConnector.getDistributedGlobalIndex(i)) ;
        clientToServerConnectors.push_back(scalar->getClientToServerConnector(client)) ;
        clientFromServerConnectors.push_back(scalar->getClientFromServerConnector(client)) ;
      }
    }
    
    // compute the grid clientToServerConnector to send flux from client to servers
    clientToServerConnector_[client] = new CGridScattererConnector(clientToServerConnectors) ;
    clientFromServerConnector_[client] = new CGridGathererConnector(clientFromServerConnectors) ;


  }


  void CGrid::sendGridToCouplerOut(CContextClient* client, const string& fieldId)
  {
    if (sendGridToCouplerOut_done_.count(client)!=0) return ;
    else sendGridToCouplerOut_done_.insert(client) ;
 
    CContext* context = CContext::getCurrent();
    // simple Distribution for now 
    // distribute over the fisrt element except if it is a scalar
    auto& elements = getElements() ;
    int posDistributed = 0 ;
    for(auto& element : elements)
    {
      if (element.type==TYPE_DOMAIN) break ;
      else if (element.type==TYPE_AXIS) break ;
      else if (element.type==TYPE_SCALAR) posDistributed++ ;
    }
    
    vector<CLocalView*> localViews ;
    vector<CDistributedView*> remoteViews ;

    for(int i=0 ; i<elements.size() ; i++)
    {
      if (elements[i].type==TYPE_DOMAIN) 
      { 
         CDomain* domain = (CDomain*) elements[i].ptr ;
         domain->computeRemoteElement(client, posDistributed==i ? EDistributionType::BANDS : EDistributionType::NONE) ;
         remoteViews.push_back(domain->getRemoteElement(client)->getView(CElementView::FULL)) ;
         localViews.push_back(domain->getLocalView(CElementView::FULL)) ;
      }
      else if (elements[i].type==TYPE_AXIS)
      {
        CAxis* axis = (CAxis*) elements[i].ptr ;
        axis->computeRemoteElement(client, posDistributed==i ? EDistributionType::BANDS : EDistributionType::NONE) ;
        remoteViews.push_back(axis->getRemoteElement(client)->getView(CElementView::FULL)) ;
        localViews.push_back(axis->getLocalView(CElementView::FULL)) ;
      }
      else if (elements[i].type==TYPE_SCALAR)
      {
        CScalar* scalar = (CScalar*) elements[i].ptr ;
        scalar->computeRemoteElement(client, posDistributed==i ? EDistributionType::BANDS : EDistributionType::NONE) ;
        remoteViews.push_back(scalar->getRemoteElement(client)->getView(CElementView::FULL)) ;
        localViews.push_back(scalar->getLocalView(CElementView::FULL)) ;
      }
    }
    CGridRemoteConnector gridRemoteConnector(localViews, remoteViews, context->getIntraComm(), client->getRemoteSize()) ;
    gridRemoteConnector.computeConnector() ;
    
    vector<CScattererConnector*> clientToClientConnectors ;
    for(int i=0 ; i<elements.size() ; i++)
    {
      if (elements[i].type==TYPE_DOMAIN) 
      { 
         CDomain* domain = (CDomain*) elements[i].ptr ;
         sendAddDomain(domain->getId(),client) ;
         domain->distributeToServer(client, gridRemoteConnector.getDistributedGlobalIndex(i)) ;
         clientToClientConnectors.push_back(domain->getClientToServerConnector(client)) ;
      }
      else if (elements[i].type==TYPE_AXIS)
      {
        CAxis* axis = (CAxis*) elements[i].ptr ;
        sendAddAxis(axis->getId(),client) ;
        axis->distributeToServer(client, gridRemoteConnector.getDistributedGlobalIndex(i)) ;
        clientToClientConnectors.push_back(axis->getClientToServerConnector(client)) ;
      }
      else if (elements[i].type==TYPE_SCALAR)
      {
        CScalar* scalar = (CScalar*) elements[i].ptr ;
        sendAddScalar(scalar->getId(),client) ;
        scalar->distributeToServer(client, gridRemoteConnector.getDistributedGlobalIndex(i)) ;
        clientToClientConnectors.push_back(scalar->getClientToServerConnector(client)) ;
      }
    }
    
    // compute the grid clientToServerConnector to send flux from client to servers
    clientToClientConnector_[client] = new CGridScattererConnector(clientToClientConnectors) ;
  }

  void CGrid::makeAliasForCoupling(const string& fieldId)
  {
    string gridId="_grid_of_"+fieldId ;
    createAlias(gridId) ;
    
    const auto& domVect = getDomains() ;
    for (int pos=0; pos<domVect.size();pos++) domVect[pos]->makeAliasForCoupling(fieldId, pos);

    const auto& axisVect=getAxis() ;
    for (int pos=0; pos<axisVect.size();pos++) axisVect[pos]->makeAliasForCoupling(fieldId, pos);

    const auto& scalVect=getScalars() ;
    for (int pos=0; pos<scalVect.size();pos++) scalVect[pos]->makeAliasForCoupling(fieldId, pos);
  }

   /*!
   \brief Send a message to create a domain on server side
   \param[in] id String identity of domain that will be created on server
   */
   void CGrid::sendAddDomain(const string& id, CContextClient* contextClient)
   TRY
  {
      sendAddItem(id, (int)EVENT_ID_ADD_DOMAIN, contextClient);
   }
   CATCH_DUMP_ATTR

   /*!
   \brief Send a message to create an axis on server side
   \param[in] id String identity of axis that will be created on server
   */
   void CGrid::sendAddAxis(const string& id, CContextClient* contextClient)
   TRY
   {
      sendAddItem(id, (int)EVENT_ID_ADD_AXIS, contextClient);
   }
   CATCH_DUMP_ATTR

   /*!
   \brief Send a message to create a scalar on server side
   \param[in] id String identity of scalar that will be created on server
   */
   void CGrid::sendAddScalar(const string& id, CContextClient* contextClient)
   TRY
   {
      sendAddItem(id, (int)EVENT_ID_ADD_SCALAR, contextClient);
   }
   CATCH_DUMP_ATTR

   /*!
   \brief Receive a message annoucing the creation of a domain on server side
   \param[in] event Received event
   */
   void CGrid::recvAddDomain(CEventServer& event)
   TRY
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer >> id;
      get(id)->recvAddDomain(*buffer);
   }
   CATCH

   /*!
   \brief Receive a message annoucing the creation of a domain on server side
   \param[in] buffer Buffer containing message
   */
   void CGrid::recvAddDomain(CBufferIn& buffer)
   TRY
   {
      string id;
      buffer >> id;
      addDomain(id);
   }
   CATCH_DUMP_ATTR

   /*!
   \brief Receive a message annoucing the creation of an axis on server side
   \param[in] event Received event
   */
   void CGrid::recvAddAxis(CEventServer& event)
   TRY
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer >> id;
      get(id)->recvAddAxis(*buffer);
   }
   CATCH

   /*!
   \brief Receive a message annoucing the creation of an axis on server side
   \param[in] buffer Buffer containing message
   */
   void CGrid::recvAddAxis(CBufferIn& buffer)
   TRY
   {
      string id;
      buffer >> id;
      addAxis(id);
   }
   CATCH_DUMP_ATTR

   /*!
   \brief Receive a message annoucing the creation of an scalar on server side
   \param[in] event Received event
   */
   void CGrid::recvAddScalar(CEventServer& event)
   TRY
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer >> id;
      get(id)->recvAddScalar(*buffer);
   }
   CATCH

   /*!
   \brief Receive a message annoucing the creation of an scalar on server side
   \param[in] buffer Buffer containing message
   */
   void CGrid::recvAddScalar(CBufferIn& buffer)
   TRY
   {
      string id;
      buffer >> id;
      addScalar(id);
   }
   CATCH_DUMP_ATTR

  /*!
  \brief Check if all elements of the grid are complete
  Before make any grid processing, we must be sure that all grid information elements have
  been sent, for exemple when reading a grid in a file or when grid elements are sent by an
  other context (coupling)
  */
  bool CGrid::isCompleted(void)
  {
    setDomainList();
    for (auto domainId : domList_) if (!CDomain::get(domainId)->isCompleted()) return false ;
    setAxisList() ;
    for (auto axisId : axisList_) if (!CAxis::get(axisId)->isCompleted()) return false ;
    setScalarList() ;
    for (auto scalarId : scalarList_) if (!CScalar::get(scalarId)->isCompleted()) return false ;
    return true ;
  }

  /*!
  \brief impose that all elements of the grid are complete
  Before make any grid processing, we must be sure that all grid information elements have
  been sent, for exemple when reading a grid in a file or when grid elements are sent by an
  other context (coupling)
  */
  void CGrid::setCompleted(void)
  {
    setDomainList();
    for (auto domainId : domList_) CDomain::get(domainId)->setCompleted() ;
    setAxisList() ;
    for (auto axisId : axisList_) CAxis::get(axisId)->setCompleted() ;
    setScalarList() ;
    for (auto scalarId : scalarList_) CScalar::get(scalarId)->setCompleted() ;
  }

/*!
  \brief impose that all elements of the grid are incomplete
  Before make any grid processing, we must be sure that all grid information elements have
  been sent, for exemple when reading a grid in a file or when grid elements are sent by an
  other context (coupling)
  */
  void CGrid::unsetCompleted(void)
  {
    setDomainList();
    for (auto domainId : domList_) CDomain::get(domainId)->unsetCompleted() ;
    setAxisList() ;
    for (auto axisId : axisList_) CAxis::get(axisId)->unsetCompleted() ;
    setScalarList() ;
    for (auto scalarId : scalarList_) CScalar::get(scalarId)->unsetCompleted() ;
  }

  /*!
  \brief Solve domain and axis references
  As field, domain and axis can refer to other domains or axis. In order to inherit correctly
  all attributes from their parents, they should be processed with this function
  \param[in] apply inherit all attributes of parents (true)
  */
  void CGrid::solveElementsRefInheritance(bool apply)
  TRY
  {
    setDomainList();
    for (auto domainId : domList_)
    {
      CDomain* pDom = CDomain::get(domainId);
      pDom->solveRefInheritance(apply);
      pDom->solveInheritanceTransformation();
    }

    setAxisList();
    for (auto axisId : axisList_)
    {
      CAxis* pAxis = CAxis::get(axisId);
      pAxis->solveRefInheritance(apply);
      pAxis->solveInheritanceTransformation();
    }

    setScalarList();
    for (auto scalarId : scalarList_)
    {
      CScalar* pScalar = CScalar::get(scalarId);
      pScalar->solveRefInheritance(apply);
      pScalar->solveInheritanceTransformation();
    }
  }
  CATCH_DUMP_ATTR

 /*!
  \brief check attributes of all elements of the grid
  */
  void CGrid::checkElementsAttributes(void)
  TRY
  {
    setDomainList();
    for (auto domainId : domList_) CDomain::get(domainId)->checkAttributes();

    setAxisList();
    for (auto axisId : axisList_) CAxis::get(axisId)->checkAttributes();
    
    setScalarList();
    for (auto scalarId : scalarList_) CScalar::get(scalarId)->checkAttributes();
  }
  CATCH_DUMP_ATTR

  bool CGrid::isTransformed()
  TRY
  {
    return isTransformed_;
  }
  CATCH_DUMP_ATTR

  void CGrid::setTransformed()
  TRY
  {
    isTransformed_ = true;
  }
  CATCH_DUMP_ATTR

  CGridTransformation* CGrid::getTransformations()
  TRY
  {
    return transformations_;
  }
  CATCH_DUMP_ATTR

  void CGrid::addTransGridSource(CGrid* gridSrc)
  TRY
  {
    if (gridSrc_.end() == gridSrc_.find(gridSrc))
      gridSrc_.insert(make_pair(gridSrc,make_pair(false,"")));
  }
  CATCH_DUMP_ATTR

  std::map<CGrid*,std::pair<bool,StdString> >& CGrid::getTransGridSource()
  TRY
  {
    return gridSrc_;
  }
  CATCH_DUMP_ATTR

  /*!
     Complete all the necessary (and lacking) attributes of a grid
     This function is similar to gridTransformation but works only (till now) on generate_rectilinear_domain transformation
  */
  void CGrid::completeGrid(CGrid* transformGridSrc)
  TRY
  {
    if (nullptr != transformGridSrc)
    {
      if (axis_domain_order.numElements() != transformGridSrc->axis_domain_order.numElements())
      {
        ERROR("CGrid::completeGrid(CGrid* transformGridSrc)",
             << "Two grids have different number of elements. " << std::endl
             << "Number of element of grid destination " << this->getId() << " is " << axis_domain_order.numElements() << std::endl
             << "Number of element of grid source " << transformGridSrc->getId() << " is " << transformGridSrc->axis_domain_order.numElements());
      }
    }

    if (isGenerated()) return;
    setGenerated();

    CGridGenerate gridGenerate(this, transformGridSrc);
    gridGenerate.completeGrid();
  }
  CATCH_DUMP_ATTR

  bool CGrid::isGenerated()
  TRY
  {
    return isGenerated_;
  }
  CATCH

  void CGrid::setGenerated()
  TRY
  {
    isGenerated_ = true;
  }
  CATCH_DUMP_ATTR

  void CGrid::transformGrid(CGrid* transformGridSrc)
  TRY
  {
    if (!transformGridSrc)
      ERROR("CGrid::transformGrid(CGrid* transformGridSrc)",
            << "Impossible to transform grid '" << getId() << "', the source grid is null.");

    if (isTransformed()) return;
    setTransformed();
    if (axis_domain_order.numElements() != transformGridSrc->axis_domain_order.numElements())
    {
      ERROR("CGrid::transformGrid(CGrid* transformGridSrc)",
           << "Two grids have different number of elements. " << std::endl
           << "Number of element of grid destination " << this->getId() << " is " << axis_domain_order.numElements() << std::endl
           << "Number of element of grid source " << transformGridSrc->getId() << " is " << transformGridSrc->axis_domain_order.numElements());
    }
    else
    {
    }

    transformations_ = new CGridTransformation(this, transformGridSrc);
    transformations_->computeAll();
    if (0 < transformations_->getNbAlgo()) hasTransform_ = true;

    // Ok, now need to compute index of grid source
    transformGridSrc->checkMaskIndex(false);
  }
  CATCH_DUMP_ATTR




  void CGrid::prepareTransformGrid(CGrid* transformGridSrc)
  TRY
  {
    if (prepareTransformGrid_done_) return ;

    if (!transformGridSrc)
      ERROR("CGrid::transformGrid(CGrid* transformGridSrc)",
            << "Impossible to transform grid '" << getId() << "', the source grid is null.");

    if (isTransformed()) return;
    setTransformed();
    if (axis_domain_order.numElements() != transformGridSrc->axis_domain_order.numElements())
    {
      ERROR("CGrid::transformGrid(CGrid* transformGridSrc)",
           << "Two grids have different number of elements. " << std::endl
           << "Number of element of grid destination " << this->getId() << " is " << axis_domain_order.numElements() << std::endl
           << "Number of element of grid source " << transformGridSrc->getId() << " is " << transformGridSrc->axis_domain_order.numElements());
    }
    else
    {
    }

    transformations_ = new CGridTransformation(this, transformGridSrc);
    if (0 < transformations_->getNbAlgo()) hasTransform_ = true;

    prepareTransformGrid_done_ = true; 
  }
  CATCH_DUMP_ATTR


  void CGrid::makeTransformGrid(void)
  TRY
  {
    if (makeTransformGrid_done_) return ;
    transformations_->computeAll();

    makeTransformGrid_done_ = true ; 
  }
  CATCH_DUMP_ATTR


  vector<std::string> CGrid::getAuxInputTransformGrid(void)
  TRY
  {
    if (transformations_ != nullptr) return transformations_->getAuxInputs() ;
  }
  CATCH_DUMP_ATTR







  bool CGrid::hasTransform()
  TRY
  {
    if (hasTransform_) return hasTransform_;

    std::vector<CDomain*> domList = getDomains();
    std::vector<CAxis*> axisList = getAxis();
    std::vector<CScalar*> scalarList = getScalars();

    for (int idx = 0; idx < domList.size(); ++idx) hasTransform_ |= domList[idx]->hasTransformation();
    for (int idx = 0; idx < axisList.size(); ++idx) hasTransform_ |= axisList[idx]->hasTransformation();
    for (int idx = 0; idx < scalarList.size(); ++idx) hasTransform_ |= scalarList[idx]->hasTransformation();

    return hasTransform_;
  }
  CATCH_DUMP_ATTR



  /*!
    Send all attributes of domains from client to server
  */
  void CGrid::sendAllDomains(CContextClient* contextClient)
  TRY
  {
    std::vector<CDomain*> domList = this->getVirtualDomainGroup()->getAllChildren();
    for (auto domain : domList)
    {
      sendAddDomain(domain->getId(),contextClient);
      domain->sendDomainToFileServer(contextClient);
    }
  }
  CATCH_DUMP_ATTR

  /*!
    Send all attributes of axis from client to server
  */
  void CGrid::sendAllAxis(CContextClient* contextClient)
  TRY
  {
    std::vector<CAxis*> aList = this->getVirtualAxisGroup()->getAllChildren();
    for (int i=0; i<aList.size() ; ++i)
    {
      sendAddAxis(aList[i]->getId(),contextClient);
      aList[i]->sendAxisToFileServer(contextClient, getGlobalDimension(), getAxisPositionInGrid()[i]);
    }
  }
  CATCH_DUMP_ATTR

  /*!
    Send all attributes of scalars from client to server
  */
  void CGrid::sendAllScalars(CContextClient* contextClient)
  TRY
  {
    std::vector<CScalar*> sList = this->getVirtualScalarGroup()->getAllChildren();
    for (auto scalar : sList)
    {
      sendAddScalar(scalar->getId(),contextClient);
      scalar->sendScalarToFileServer(contextClient);
    }
  }
  CATCH_DUMP_ATTR

  void CGrid::setContextClient(CContextClient* contextClient)
  TRY
  {
    if (clientsSet.find(contextClient)==clientsSet.end())
    {
      clients.push_back(contextClient) ;
      clientsSet.insert(contextClient);
    }
    for (auto domain : getDomains()) domain->setContextClient(contextClient);
    for (auto axis : getAxis()) axis->setContextClient(contextClient);
    for (auto scalar : getScalars()) scalar->setContextClient(contextClient);
   
  }
  CATCH_DUMP_ATTR

 
  void CGrid::computeGridLocalElements()
  {
    std::vector<CDomain*> domainList = this->getDomains();
    std::vector<CAxis*> axisList = this->getAxis();
    auto domain=domainList.begin() ;
    auto axis=axisList.begin() ;
    vector<CLocalElement*> elements;
    for(auto order : order_)
    {
      if (order==2) 
      {
        elements.push_back((*domain)->getLocalElement());
        domain++ ;
      }
      else if (order==1)
      {
        elements.push_back((*axis)->getLocalElement());
        axis++ ;
      }
      else if (order==0)
      { 
      }
    }
    if (hasMask()) 
    {
      vector<bool> mask(getMask().getVector()) ;
      gridLocalElements_ = new CGridLocalElements(elements, mask) ;  
    }
    else gridLocalElements_ = new CGridLocalElements(elements) ;  
  }

  void CGrid::computeModelToWorkflowConnector(void)
  {
    modelToWorkflowConnector_ = getGridLocalElements()->getConnector(CElementView::MODEL,CElementView::WORKFLOW) ;
  }

  void CGrid::computeWorkflowToFullConnector(void)
  {
    workflowToFullConnector_ = getGridLocalElements()->getConnector(CElementView::WORKFLOW,CElementView::FULL) ;
  }

  void CGrid::computeWorkflowToModelConnector(void)
  {
    workflowToModelConnector_ = getGridLocalElements()->getConnector(CElementView::WORKFLOW,CElementView::MODEL) ;
  }

  void CGrid::computeFullToWorkflowConnector(void)
  {
    fullToWorkflowConnector_ = getGridLocalElements()->getConnector(CElementView::FULL,CElementView::WORKFLOW) ;
  }

  void CGrid::computeServerFromClientConnector(void)
  {
    vector<CGathererConnector*> connectors ;
    for(auto& element : getElements())
    {
      if (element.type==TYPE_DOMAIN) connectors.push_back(element.domain->getServerFromClientConnector()) ;
      else if (element.type==TYPE_AXIS) connectors.push_back(element.axis->getServerFromClientConnector()) ; 
      else if (element.type==TYPE_SCALAR) connectors.push_back(element.scalar->getServerFromClientConnector()) ; 
    }
    serverFromClientConnector_ = new CGridGathererConnector(connectors) ;
  }

  void CGrid::computeServerToClientConnector(void)
  {
    vector<CScattererConnector*> connectors ;
    for(auto& element : getElements())
    {
      if (element.type==TYPE_DOMAIN) connectors.push_back(element.domain->getServerToClientConnector()) ;
      else if (element.type==TYPE_AXIS) connectors.push_back(element.axis->getServerToClientConnector()) ; 
      else if (element.type==TYPE_SCALAR) connectors.push_back(element.scalar->getServerToClientConnector()) ; 
    }
    serverToClientConnector_ = new CGridScattererConnector(connectors) ;
  }

  void CGrid::computeClientFromClientConnector(void)
  {
    vector<CGathererConnector*> connectors ;
    for(auto& element : getElements())
    {
      if (element.type==TYPE_DOMAIN) connectors.push_back(element.domain->getServerFromClientConnector()) ;
      else if (element.type==TYPE_AXIS) connectors.push_back(element.axis->getServerFromClientConnector()) ; 
      else if (element.type==TYPE_SCALAR) connectors.push_back(element.scalar->getServerFromClientConnector()) ; 
    }
    clientFromClientConnector_ = new CGridGathererConnector(connectors) ;
  }

  
} // namespace xios
