
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
#include "server.hpp"
#include "distribution_type.hpp"
#include "grid_remote_connector.hpp"
#include "grid_elements.hpp"
#include "grid_local_view.hpp"
#include "grid_mask_connector.hpp"
#include "transformation_path.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "transform_filter.hpp"
#include "grid_algorithm.hpp"
#include "grid_algorithm_generic.hpp"
#include "generic_algorithm_transformation.hpp"
#include "algo_types.hpp"

#include <regex>

namespace xios
{

   /// ////////////////////// Dfinitions ////////////////////// ///

   CGrid::CGrid(void)
      : CObjectTemplate<CGrid>(), CGridAttributes()
      , isChecked(false), isDomainAxisChecked(false)
      , vDomainGroup_(), domList_(), isDomListSet(false)
      , vAxisGroup_(), axisList_(), isAxisListSet(false)
      , vScalarGroup_(), scalarList_(), isScalarListSet(false)
      , clientDistribution_(0), isIndexSent(false)
      , connectedDataSize_(), connectedServerRank_(), connectedServerRankRead_(), connectedDataSizeRead_()
	    , isCompressible_(false)
      , axisPositionInGrid_(), hasDomainAxisBaseRef_(false)
      , gridSrc_(), order_()
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
      , clientDistribution_(0), isIndexSent(false)
      , connectedDataSize_(), connectedServerRank_(), connectedServerRankRead_(), connectedDataSizeRead_()
	    , isCompressible_(false)
      , axisPositionInGrid_(), hasDomainAxisBaseRef_(false)
      , gridSrc_(), order_()
      , clients()
   {
     setVirtualDomainGroup(CDomainGroup::create(getId() + "_virtual_domain_group"));
     setVirtualAxisGroup(CAxisGroup::create(getId() + "_virtual_axis_group"));
     setVirtualScalarGroup(CScalarGroup::create(getId() + "_virtual_scalar_group"));
   }

   CGrid::~CGrid(void)
   {
    if (0 != clientDistribution_) delete clientDistribution_;
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
      grid->computeElements() ;
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
     CDomain* domain = vDomainGroup_->createChild(id);
     isDomListSet=false ;
     computeElements();
     return domain ;
   }
   CATCH_DUMP_ATTR

   CAxis* CGrid::addAxis(const std::string& id)
   TRY
   {
     order_.push_back(1);
     axis_domain_order.resize(order_.size());
     for (int idx = 0; idx < order_.size(); ++idx) axis_domain_order(idx)=order_[idx];
     CAxis* axis=vAxisGroup_->createChild(id);
     isAxisListSet=false ;
     computeElements(); 
     return axis ;
   }
   CATCH_DUMP_ATTR

   CScalar* CGrid::addScalar(const std::string& id)
   TRY
   {
     order_.push_back(0);
     axis_domain_order.resize(order_.size());
     for (int idx = 0; idx < order_.size(); ++idx) axis_domain_order(idx)=order_[idx];
     CScalar* scalar =  vScalarGroup_->createChild(id);
     isScalarListSet=false ;
     computeElements();
     return scalar;
   }
   CATCH_DUMP_ATTR




  /*!
  \brief Get the list of domain pointers
  \return list of domain pointers
  */
  std::vector<CDomain*> CGrid::getDomains()
  TRY
  {
    setDomainList();
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
    setAxisList();
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
    setScalarList() ;
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

  CDomain* CGrid::getAssociatedDomain(const string& domainId)
  {
    const regex r("\\[[0-9]*\\]");
    smatch m;
    string id=domainId ;
    int pos=-1 ;
    if (regex_search(domainId, m, r))
    {
        if (m.size()!=1) ERROR("CGrid::getAssociatedDomain(const string& domainId)", <<" domainId = "<<domainId<< "  -> bad format id, separator [] append more than one time");
        id=m.prefix() ;
        pos = stoi(m.str(0).substr(1,m.str(0).size()-2)) ;
    }
    std::vector<CDomain*> domainList = this->getDomains();
    if (domainList.empty()) ERROR("CGrid::getAssociatedDomain(const string& domainId)", <<"no domain is compsing the grid");
    if (id.empty())
    {
      if (pos==-1)
      {
        if (domainList.size()==1) return domainList[0] ;
        else ERROR("CGrid::getAssociatedDomain(const string& domainId)", <<"the grid contain more than 1 domain, use [#n] to specify which one must be retrieved");
      }
      else
      {
        if (domainList.size()>pos) return domainList[pos] ;
        else ERROR("CGrid::getAssociatedDomain(const string& domainId)", <<"the position of the requested domain [ pos = "<<pos
                   <<" ] is greater than the number of domain composing the grid  [ numDomain = "<<domainList.size()<<" ]");
      }
    }
    else
    {
      if (pos==-1) 
      {
        int nbDomain=0 ;
        for(int i=0; i<domainList.size();i++) if (domainList[i]->getTemplateId()==id) nbDomain++ ;
        if (nbDomain>1) ERROR("CGrid::getAssociatedDomain(const string& domainId)", <<"no domain with the id = "<<id
                              <<" is composing the grid") ;
        if (nbDomain==0) ERROR("CGrid::getAssociatedDomain(const string& domainId)", <<"the grid contain more than 1 domain with the id = "
                               <<id<<" , use [#n] to specify which one must be retrieved") ;
        for(int i=0; i<domainList.size();i++) if (domainList[i]->getTemplateId()==id) return domainList[i]  ;
      }
      else
      {
        int currentPos=0 ;
        for(int i=0; i<domainList.size();i++) 
        {
          if (domainList[i]->getTemplateId()==id && pos==currentPos) return domainList[i] ;
          currentPos++ ;
        }
        ERROR("CGrid::getAssociatedDomain(const string& domainId)",<<"Cannot find domain with [ id = "<< id <<" ] at [ pos = "<<pos<<" ] in the grid");
      }  
    }
  } 

  CAxis* CGrid::getAssociatedAxis(const string& axisId)
  {
    const regex r("\\[[0-9]*\\]");
    smatch m;
    string id=axisId ;
    int pos=-1 ;
    if (regex_search(axisId, m, r))
    {
        if (m.size()!=1) ERROR("CGrid::getAssociatedAxis(const string& axisId)", <<" axisId = "<<axisId<< "  -> bad format id, separator [] append more than one time");
        id=m.prefix() ;
        pos = stoi(m.str(0).substr(1,m.str(0).size()-2)) ;
    }
    std::vector<CAxis*> axisList = this->getAxis();
    if (axisList.empty()) ERROR("CGrid::getAssociatedAxis(const string& AxisId)", <<"no axis is composing the grid");
    if (id.empty())
    {
      if (pos==-1)
      {
        if (axisList.size()==1) return axisList[0] ;
        else ERROR("CGrid::getAssociatedAxis(const string& axisId)", <<"the grid contain more than 1 axis, use [#n] to specify which one must be retrieved");
      }
      else
      {
        if (axisList.size()>pos) return axisList[pos] ;
        else ERROR("CGrid::getAssociatedAxis(const string& axisId)", <<"the position of the requested axis [ pos = "<<pos
                   <<" ] is greater than the number of axis composing the grid  [ numAxis = "<<axisList.size()<<" ]");
      }
    }
    else
    {
      if (pos==-1) 
      {
        int nbAxis=0 ;
        for(int i=0; i<axisList.size();i++) if (axisList[i]->getTemplateId()==id) nbAxis++ ;
        if (nbAxis>1) ERROR("CGrid::getAssociatedAxis(const string& axisId)", <<"no axis with the id = "<<id
                              <<" is composing the grid") ;
        if (nbAxis==0) ERROR("CGrid::getAssociatedAxis(const string& axisId)", <<"the grid contain more than 1 axis with the id = "
                               <<id<<" , use [#n] to specify which one must be retrieved") ;
        for(int i=0; i<axisList.size();i++) if (axisList[i]->getTemplateId()==id) return axisList[i]  ;
      }
      else
      {
        int currentPos=0 ;
        for(int i=0; i<axisList.size();i++) 
        {
          if (axisList[i]->getTemplateId()==id && pos==currentPos) return axisList[i] ;
          currentPos++ ;
        }
        ERROR("CGrid::getAssociatedAxis(const string& axisId)",<<"Cannot find axis with [ id = "<< id <<" ] at [ pos = "<<pos<<" ] in the grid");
      }  
    }
  } 

  CScalar* CGrid::getAssociatedScalar(const string& scalarId)
  {
    const regex r("\\[[0-9]*\\]");
    smatch m;
    string id=scalarId ;
    int pos=-1 ;
    if (regex_search(scalarId, m, r))
    {
        if (m.size()!=1) ERROR("CGrid::getAssociatedScalar(const string& scalarId)", <<" scalarId = "<<scalarId<< "  -> bad format id, separator [] append more than one time");
        id=m.prefix() ;
        pos = stoi(m.str(0).substr(1,m.str(0).size()-2)) ;
    }
    std::vector<CScalar*> scalarList = this->getScalars();
    if (scalarList.empty()) ERROR("CGrid::getAssociatedScalar(const string& scalarId)", <<"no scalar is composing the grid");
    if (id.empty())
    {
      if (pos==-1)
      {
        if (scalarList.size()==1) return scalarList[0] ;
        else ERROR("CGrid::getAssociatedScalar(const string& scalarId)", <<"the grid contain more than 1 scalar, use [#n] to specify which one must be retrieved");
      }
      else
      {
        if (scalarList.size()>pos) return scalarList[pos] ;
        else ERROR("CGrid::getAssociatedScalar(const string& scalarId)", <<"the position of the requested scalar [ pos = "<<pos
                   <<" ] is greater than the number of scalar composing the grid  [ numScalar = "<<scalarList.size()<<" ]");
      }
    }
    else
    {
      if (pos==-1) 
      {
        int nbScalar=0 ;
        for(int i=0; i<scalarList.size();i++) if (scalarList[i]->getTemplateId()==id) nbScalar++ ;
        if (nbScalar>1) ERROR("CGrid::getAssociatedScalar(const string& scalarId)", <<"no scalar with the id = "<<id
                              <<" is composing the grid") ;
        if (nbScalar==0) ERROR("CGrid::getAssociatedScalar(const string& scalarId)", <<"the grid contain more than 1 scalar with the id = "
                               <<id<<" , use [#n] to specify which one must be retrieved") ;
        for(int i=0; i<scalarList.size();i++) if (scalarList[i]->getTemplateId()==id) return scalarList[i]  ;
      }
      else
      {
        int currentPos=0 ;
        for(int i=0; i<scalarList.size();i++) 
        {
          if (scalarList[i]->getTemplateId()==id && pos==currentPos) return scalarList[i] ;
          currentPos++ ;
        }
        ERROR("CGrid::getAssociatedScalar(const string& scalarId)",<<"Cannot find scalar with [ id = "<< id <<" ] at [ pos = "<<pos<<" ] in the grid");
      }  
    }
  } 


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
 
    elements_.clear() ;
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
    computeElements() ;
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
     
     if (this->isChecked) return;
     this->checkElementsAttributes();
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

  
   //---------------------------------------------------------------

   void CGrid::solveDomainRef(bool sendAtt)
   TRY
   {
      setDomainList();
      std::vector<CDomain*> domListP = this->getDomains();
      if (!domListP.empty())
        for (int i = 0; i < domListP.size(); ++i) domListP[i]->checkAttributes();
   }
   CATCH_DUMP_ATTR

   //---------------------------------------------------------------

   void CGrid::solveAxisRef(bool sendAtt)
   TRY
   {
      setAxisList();
      std::vector<CAxis*> axisListP = this->getAxis();
      if (!axisListP.empty())
        for (int i = 0; i < axisListP.size(); ++i)  axisListP[i]->checkAttributes();
   }
   CATCH_DUMP_ATTR

   //---------------------------------------------------------------

   void CGrid::solveScalarRef(bool sendAtt)
   TRY
   {
      setScalarList();
      std::vector<CScalar*> scalarListP = this->getScalars();
      if (!scalarListP.empty())
        for (int i = 0; i < scalarListP.size(); ++i) scalarListP[i]->checkAttributes() ;
   }
   CATCH_DUMP_ATTR


    //---------------------------------------------------------------
   CDistributionClient* CGrid::getClientDistribution()
   TRY
   {
     if (!computeClientDistribution_done_) computeClientDistribution() ;
     return clientDistribution_;
   }
   CATCH_DUMP_ATTR
   
   void CGrid::computeClientDistribution(void)
   {
     if (computeClientDistribution_done_) return ;
     else computeClientDistribution_done_ = true ;

     CContext* context = CContext::getCurrent();
     int rank = context-> getIntraCommRank();
     clientDistribution_ = new CDistributionClient(rank, this);
   }


  bool CGrid::isDataDistributed(void) 
  { 
    return getClientDistribution()->isDataDistributed() ;
  }

  

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

         case EVENT_ID_SEND_MASK :
           recvMask(event);
           return true;
           break;
        default :
          ERROR("bool CGrid::dispatchEvent(CEventServer& event)",
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
    distributeGridToServer(client) ;
  }


  void CGrid::sendGridToCouplerOut(CContextClient* client, const string& fieldId)
  {
    if (sendGridToCouplerOut_done_.count(client)!=0) return ;
    else sendGridToCouplerOut_done_.insert(client) ;
    this->sendAllAttributesToServer(client, getCouplingAlias(fieldId));
    distributeGridToServer(client,fieldId) ;
  }


  void CGrid::distributeGridToServer(CContextClient* client, const string& fieldId)
  {
    CContext* context = CContext::getCurrent();
    bool isCoupling = !fieldId.empty() ;
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
    
    vector<CScattererConnector*> scattererConnectors ;
    CScattererConnector* scattererConnector;
    for(int i=0 ; i<elements.size() ; i++)
    {
      if (elements[i].type==TYPE_DOMAIN) 
      { 
        CDomain* domain = (CDomain*) elements[i].ptr ;
        if (isCoupling) domain->distributeToServer(client, gridRemoteConnector.getDistributedGlobalIndex(i), scattererConnector,  domain->getCouplingAlias(fieldId,i)) ;
        else 
        {
          sendAddDomain(domain->getId(),client) ;
          domain->distributeToServer(client, gridRemoteConnector.getDistributedGlobalIndex(i), scattererConnector) ;
        }
        scattererConnectors.push_back(scattererConnector) ;
      }
      else if (elements[i].type==TYPE_AXIS)
      {
        CAxis* axis = (CAxis*) elements[i].ptr ;
        if (isCoupling) axis->distributeToServer(client, gridRemoteConnector.getDistributedGlobalIndex(i), scattererConnector,  axis->getCouplingAlias(fieldId,i)) ;
        else 
        {
          sendAddAxis(axis->getId(),client) ;
          axis->distributeToServer(client, gridRemoteConnector.getDistributedGlobalIndex(i), scattererConnector) ;
        }
        scattererConnectors.push_back(scattererConnector) ;
      }
      else if (elements[i].type==TYPE_SCALAR)
      {
        CScalar* scalar = (CScalar*) elements[i].ptr ;
        if (isCoupling) scalar->distributeToServer(client, gridRemoteConnector.getDistributedGlobalIndex(i), scattererConnector,  scalar->getCouplingAlias(fieldId,i)) ;
        else 
        {
          sendAddScalar(scalar->getId(),client) ;
          scalar->distributeToServer(client, gridRemoteConnector.getDistributedGlobalIndex(i), scattererConnector) ;
        }
        scattererConnectors.push_back(scattererConnector) ;
      }
    }

    CGridScattererConnector gridScattererConnector(scattererConnectors) ;
    CGridLocalConnector* workflowToFull = getGridLocalElements()->getConnector(CElementView::WORKFLOW, CElementView::FULL) ;
    CArray<bool,1> maskIn(workflowToFull->getSrcSize()) ;
    CArray<bool,1> maskOut(workflowToFull->getDstSize()) ;
    maskIn = true ;
    workflowToFull->transfer(maskIn,maskOut,false) ;

    CEventClient event(getType(), EVENT_ID_SEND_MASK);
    CMessage message ;
    if (isCoupling) message<<getCouplingAlias(fieldId) ;
    else message<<getId() ; 
    gridScattererConnector.transfer(maskOut, client, event, message) ;
    for(auto& it : scattererConnectors) delete it ;

    vector<CScattererConnector*> clientToServerConnectors ;
    vector<CGathererConnector*>  clientFromServerConnectors ;
    for(auto& element : elements)
    {
      if (element.type==TYPE_DOMAIN) 
      { 
         clientToServerConnectors.push_back(element.domain->getClientToServerConnector(client)) ;
         clientFromServerConnectors.push_back(element.domain->getClientFromServerConnector(client)) ;
      }
      else if (element.type==TYPE_AXIS)
      {
        clientToServerConnectors.push_back(element.axis->getClientToServerConnector(client)) ;
        clientFromServerConnectors.push_back(element.axis->getClientFromServerConnector(client)) ;

      }
      else if (element.type==TYPE_SCALAR)
      {
        clientToServerConnectors.push_back(element.scalar->getClientToServerConnector(client)) ;
        clientFromServerConnectors.push_back(element.scalar->getClientFromServerConnector(client)) ;
      }
    }
    
    // compute the grid clientToServerConnector to send flux from client to servers
    clientToServerConnector_[client] = new CGridScattererConnector(clientToServerConnectors) ;
    clientFromServerConnector_[client] = new CGridGathererConnector(clientFromServerConnectors) ;

  }


  void CGrid::recvMask(CEventServer& event)
  {
    string gridId;
    for (auto& subEvent : event.subEvents) (*subEvent.buffer) >> gridId  ;
    get(gridId)->receiveMask(event);
  }
  
  void CGrid::receiveMask(CEventServer& event)
  {
    vector<CGathererConnector*> gathererConnectors ;
    vector<CLocalView*> fullViews ;

    for(auto& element : getElements())
    {
      if (element.type==TYPE_DOMAIN) 
      {
        gathererConnectors.push_back(element.domain->getGathererConnector());
        fullViews.push_back(element.domain->getLocalElement()->getView(CElementView::FULL));
        
      }
      else if (element.type==TYPE_AXIS)
      {
       gathererConnectors.push_back(element.axis->getGathererConnector());
       fullViews.push_back(element.axis->getLocalElement()->getView(CElementView::FULL));
      }
      else if (element.type==TYPE_SCALAR) 
      {
        gathererConnectors.push_back(element.scalar->getGathererConnector());
        fullViews.push_back(element.scalar->getLocalElement()->getView(CElementView::FULL));
      }
    }
    CGridGathererConnector gridGathererConnector(gathererConnectors) ;
    CGridMaskConnector gridMaskConnector(fullViews) ;

    CArray<bool,1> maskOut ;
    gridGathererConnector.transfer_or(event,maskOut) ;
    gridMaskConnector.computeConnector(maskOut) ;

    CContextClient* client = event.getContextServer()->getAssociatedClient() ;
    int i=0 ;
    for(auto& element : getElements())
    {
      if (element.type==TYPE_DOMAIN) element.domain->setServerMask(gridMaskConnector.getElementMask(i),client);
      else if (element.type==TYPE_AXIS) element.axis->setServerMask(gridMaskConnector.getElementMask(i),client);
      else if (element.type==TYPE_SCALAR) element.scalar->setServerMask(gridMaskConnector.getElementMask(i),client);
      i++ ;
    }
  }

  string CGrid::getCouplingAlias(const string& fieldId)
  {
    return "_grid_of_"+fieldId;
  }

  void CGrid::makeAliasForCoupling(const string& fieldId)
  {
    string gridId=getCouplingAlias(fieldId) ;
    createAlias(gridId) ;

    auto& elements = getElements() ;
    for(int i=0 ; i<elements.size() ; i++)
    {
      if (elements[i].type==TYPE_DOMAIN) elements[i].domain->makeAliasForCoupling(fieldId, i);
      else if (elements[i].type==TYPE_AXIS) elements[i].axis->makeAliasForCoupling(fieldId, i);
      else if (elements[i].type==TYPE_SCALAR) elements[i].scalar->makeAliasForCoupling(fieldId, i);
    }
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



//**********************************************************
//**************   New transformation method  **************
//**********************************************************

  std::pair<std::shared_ptr<CFilter>, std::shared_ptr<CFilter> > 
  CGrid::buildTransformationGraph(CGarbageCollector& gc, bool isSource, CGrid* gridSrc, double detectMissingValues, double defaultValue, CGrid*& newGrid, bool graphEnabled, CField* field)
  TRY
  {
    static bool transformationGoing = false;
    registerAlgorithmTransformation() ; // needed to enable self-registration of the transformations
                                        // big mystery why it doesn't work witout that...
                                        // problem with the linker ?? 

    std::shared_ptr<CFilter> inputFilter = std::shared_ptr<CPassThroughFilter>(new CPassThroughFilter(gc));
    std::shared_ptr<CFilter> outputFilter = inputFilter ;


    string newId ;
    if (gridSrc!=nullptr) newId = gridSrc->getId() + " --> " + this->getId()  ;
    else newId = " --> " + this->getId()  ;
    bool isNewGrid ;
    if (CGrid::has(newId))
    {
      cout<<"Retrive existing grid : "<<newId<<endl ;
      newGrid = CGrid::get(newId);
      isNewGrid = false ;
    }
    else  
    {
      cout<<"Create new grid : "<<newId<<endl ;
      newGrid = CGrid::create(newId) ;
      isNewGrid = true ;
    }

    bool hadTransform=false ;
    bool hasTransform=false ;
    bool hasRemainTransform=false ;
    CGenericAlgorithmTransformation* algo ;
    int pos ;

    for(int i=0 ; i<elements_.size(); i++)
    {
      CTransformationPaths transformationPath ;
      auto dstElement = elements_[i] ;

      if (dstElement.type==TYPE_DOMAIN)      transformationPath = dstElement.domain->getTransformationPaths() ;
      else if (dstElement.type==TYPE_AXIS)   transformationPath = dstElement.axis->getTransformationPaths() ;
      else if (dstElement.type==TYPE_SCALAR) transformationPath = dstElement.scalar->getTransformationPaths() ;

      SElement srcElement  ;
      if (gridSrc==nullptr) srcElement = this->elements_[i] ;
      else srcElement = gridSrc->elements_[i] ;

      if (gridSrc==nullptr) transformationPath.mergePaths() ;
      else
      {  
        if (srcElement.type==TYPE_DOMAIN)      transformationPath.mergePaths(srcElement.domain->getTransformationPaths()) ;
        else if (srcElement.type==TYPE_AXIS)   transformationPath.mergePaths(srcElement.axis->getTransformationPaths()) ;
        else if (srcElement.type==TYPE_SCALAR) transformationPath.mergePaths(srcElement.scalar->getTransformationPaths()) ;
      }

      hasTransform=transformationPath.hasTransform()  ;
      
      if (hasTransform && !hadTransform)
      {
        pos=i ;
        EElement dstElementType=transformationPath.getNextElementType() ;
        string dstElementId=transformationPath.getNextElementId() ;
        string srcElementId=transformationPath.getNextElementSrcId() ;
        auto transType = transformationPath.getNextTransformationType() ;
        auto transId = transformationPath.getNextTransformationId() ;

        CGrid* tmpGridSrc=CGrid::create(); // source grid 
        if (srcElement.type==TYPE_DOMAIN)      tmpGridSrc->addDomain(srcElement.domain->getId()) ;
        else if (srcElement.type==TYPE_AXIS)   tmpGridSrc->addAxis(srcElement.axis->getId()) ;
        else if (srcElement.type==TYPE_SCALAR) tmpGridSrc->addScalar(srcElement.scalar->getId()) ;
        // WARNING -> suppress checkElement attribute ? What append ?
        // tmpGridSrc->checkElementsAttributes() ;
        CGrid* tmpGridDst=CGrid::create(); // destination Grid
        map<int,int> posInGrid={{0,0}} ;
               
        cout<<"--> New transform from "<<srcElementId<<" to "<<dstElementId<<endl ;
        if (dstElementType==EElement::DOMAIN)
        {
          CDomain* dstDomain ;
          CDomain* lastDstDomain ;
          bool isGenerate=false ;
   
          do 
          {

            if (CDomain::has(dstElementId)) 
            {
              dstDomain = CDomain::get(dstElementId) ;
              cout<<"Retrive existing domain : "<<dstElementId<<endl ;
            }
            else
            {
              dstDomain = CDomain::create() ;
              dstDomain->createAlias(dstElementId) ;
              cout<<"Create new domain : "<<dstDomain->getId()<<" with alias : "<<dstElementId<<endl ;

              if (isGenerate) 
              {
                dstDomain->duplicateAttributes(lastDstDomain) ;
                dstDomain->setTemplateId(lastDstDomain) ;
              }
              else if (srcElementId=="" && srcElement.type==TYPE_DOMAIN)  
              {
                dstDomain->duplicateAttributes(srcElement.domain) ; // make a copy
                dstDomain->setTemplateId(srcElement.domain) ;
              }
              else 
              {
                dstDomain->duplicateAttributes(dstElement.domain) ; // make a copy
                dstDomain->setTemplateId(dstElement.domain) ;
              }
              CTransformation<CDomain>* transformation = CTransformation<CDomain>::createTransformation(transType,"") ;
              auto srcTransform = CTransformation<CDomain>::getTransformation(transType, transId) ;
              transformation->inheritFrom(srcTransform) ;
              CGrid* tmpGridDst=CGrid::create(); // destination Grid
              tmpGridDst->addDomain(dstDomain->getId()) ;

              algo = transformation -> createAlgorithm(false, tmpGridDst, tmpGridSrc, 0, 
                                                       posInGrid,posInGrid,posInGrid,
                                                       posInGrid,posInGrid,posInGrid );


              dstDomain->setTransformationAlgorithm(algo) ;
              dstDomain->setTransformationPaths(transformationPath) ;
            }
            algo=dstDomain->getTransformationAlgorithm() ;
            isGenerate = algo->isGenerateTransformation() ;
            transformationPath.removeNextTransform() ;
            dstElementId=transformationPath.getNextElementId() ;
            srcElementId=transformationPath.getNextElementSrcId() ;
            transType = transformationPath.getNextTransformationType() ;
            transId = transformationPath.getNextTransformationId() ;
            lastDstDomain=dstDomain ;
            dstDomain->setTransformationPaths(transformationPath) ;
          } while(transformationPath.hasTransform() && isGenerate) ;

          if (isNewGrid) newGrid->addDomain(dstDomain->getId()) ;
          algo = dstDomain->getTransformationAlgorithm() ;
        }
        else if (dstElementType==EElement::AXIS)
        {
          CAxis* dstAxis ;
          CAxis* lastDstAxis ;
          bool isGenerate=false ;

          do 
          {
            if (CAxis::has(dstElementId)) 
            {
              dstAxis = CAxis::get(dstElementId) ;
              cout<<"Retrive existing axis : "<<dstElementId<<endl ;
            }
            else
            {
              dstAxis = CAxis::create() ;
              dstAxis->createAlias(dstElementId) ;
              cout<<"Create new axis : "<<dstAxis->getId()<<" with alias : "<<dstElementId<<endl ;
              
              if (isGenerate) 
              {
                dstAxis->duplicateAttributes(lastDstAxis) ;
                dstAxis->setTemplateId(lastDstAxis) ;
              }
              else if (srcElementId=="" && srcElement.type==TYPE_AXIS)  
              { 
                dstAxis->duplicateAttributes(srcElement.axis) ; // make a copy
                dstAxis->setTemplateId(srcElement.axis) ;
              }
              else 
              {
                dstAxis->duplicateAttributes(dstElement.axis) ; // make a copy$
                dstAxis->setTemplateId(dstElement.axis) ;
              }
              CTransformation<CAxis>* transformation = CTransformation<CAxis>::createTransformation(transType,"") ;
              auto srcTransform = CTransformation<CAxis>::getTransformation(transType, transId) ;
              transformation->inheritFrom(srcTransform) ;
              tmpGridDst->addAxis(dstAxis->getId()) ;

              algo = transformation -> createAlgorithm(false, tmpGridDst, tmpGridSrc, 0, 
                                                      posInGrid,posInGrid,posInGrid,
                                                      posInGrid,posInGrid,posInGrid );

              dstAxis->setTransformationAlgorithm(algo) ;
              dstAxis->setTransformationPaths(transformationPath) ;
            }
           
            algo=dstAxis->getTransformationAlgorithm() ;
            isGenerate = algo->isGenerateTransformation() ;
            transformationPath.removeNextTransform() ;
            dstElementId=transformationPath.getNextElementId() ;
            srcElementId=transformationPath.getNextElementSrcId() ;
            transType = transformationPath.getNextTransformationType() ;
            transId = transformationPath.getNextTransformationId() ;
            lastDstAxis=dstAxis ;
            dstAxis->setTransformationPaths(transformationPath) ;
          } while(transformationPath.hasTransform() && isGenerate) ;
           
          if (isNewGrid) newGrid->addAxis(dstAxis->getId()) ;
          algo = dstAxis->getTransformationAlgorithm() ;
        }
        else if (dstElementType==EElement::SCALAR)
        {
          CScalar* dstScalar ;
          CScalar* lastDstScalar ;
          bool isGenerate=false ;

          do 
          {
            if (CScalar::has(dstElementId)) 
            {
              dstScalar = CScalar::get(dstElementId) ;
              cout<<"Retrive existing scalar : "<<dstElementId<<endl ;
            }
            else
            {
              dstScalar = CScalar::create() ;
              dstScalar->createAlias(dstElementId) ;
              cout<<"Create new scalar : "<<dstScalar->getId()<<" with alias : "<<dstElementId<<endl ;
              
              if (isGenerate) 
              {
                dstScalar->duplicateAttributes(lastDstScalar) ;
                dstScalar->setTemplateId(lastDstScalar) ;
              }
              else if (srcElementId=="" && srcElement.type==TYPE_SCALAR)
              {
                dstScalar->duplicateAttributes(srcElement.scalar) ; // make a copy
                dstScalar->setTemplateId(srcElement.scalar) ;
              }
              else 
              {
                dstScalar->duplicateAttributes(dstElement.scalar) ; // make a copy
                dstScalar->setTemplateId(dstElement.scalar) ;
              }
              CTransformation<CScalar>* transformation = CTransformation<CScalar>::createTransformation(transType,"") ;
              auto srcTransform = CTransformation<CScalar>::getTransformation(transType, transId) ;
              transformation->inheritFrom(srcTransform) ;
              tmpGridDst->addScalar(dstScalar->getId()) ;

              algo = transformation -> createAlgorithm(false, tmpGridDst, tmpGridSrc, 0, 
                                                       posInGrid,posInGrid,posInGrid,
                                                       posInGrid,posInGrid,posInGrid );
              
              dstScalar->setTransformationAlgorithm(algo) ;
              dstScalar->setTransformationPaths(transformationPath) ;
            }
            algo=dstScalar->getTransformationAlgorithm() ;
            isGenerate = algo->isGenerateTransformation() ;
            transformationPath.removeNextTransform() ;
            dstElementId=transformationPath.getNextElementId() ;
            srcElementId=transformationPath.getNextElementSrcId() ;
            transType = transformationPath.getNextTransformationType() ;
            transId = transformationPath.getNextTransformationId() ;
            lastDstScalar=dstScalar ;
            dstScalar->setTransformationPaths(transformationPath) ;
          } while(transformationPath.hasTransform() && isGenerate) ;

          if (isNewGrid) newGrid->addScalar(dstScalar->getId()) ;
          algo = dstScalar->getTransformationAlgorithm() ;          
        }
        // here create a new spatial filter with algo
        
        hadTransform=true ;
        hasTransform=false ; 
      }
      else
      {
        string srcElementId=transformationPath.getNextElementSrcId() ;

        if (srcElement.type==TYPE_DOMAIN)      
        {
          CDomain* domain ;
          if (srcElementId=="") srcElementId=srcElement.domain->getId() ; 
          if (!CDomain::has(srcElementId)) 
          {
            domain=srcElement.domain ;
            domain->createAlias(srcElementId) ;
          }
          else domain = CDomain::get(srcElementId) ;
          domain->checkAttributes() ;
         
          if (isNewGrid) newGrid->addDomain(srcElementId) ;
        }
        else if (srcElement.type==TYPE_AXIS)
        {   
          CAxis* axis ;
          if (srcElementId=="") srcElementId=srcElement.axis->getId() ; 
          if (!CAxis::has(srcElementId)) 
          {
            axis=srcElement.axis ;
            axis->createAlias(srcElementId) ;
          }
          else axis = CAxis::get(srcElementId) ;
          axis->checkAttributes() ;
         
          if (isNewGrid) newGrid->addAxis(srcElementId) ;
        }
        else if (srcElement.type==TYPE_SCALAR)
        {
          CScalar* scalar ;
          if (srcElementId=="") srcElementId=srcElement.scalar->getId() ; 
          if (!CScalar::has(srcElementId)) 
          {
            scalar=srcElement.scalar ;
            scalar->createAlias(srcElementId) ;
          }
          else scalar = CScalar::get(srcElementId) ;
          scalar->checkAttributes() ;
         
          if (isNewGrid) newGrid->addScalar(srcElementId) ;
        }
      }
      
      if (transformationPath.hasTransform() && hadTransform) hasRemainTransform=true ;
    }  
    

    if (hadTransform)
    {
      
      if (!isSource)
      {
        CGridAlgorithm* gridAlgorithm  ;
        if (isNewGrid)
        { 
          gridAlgorithm = algo->createGridAlgorithm(gridSrc, newGrid, pos) ;
          newGrid->setGridAlgorithm(gridAlgorithm);
        }
        else gridAlgorithm = newGrid->getGridAlgorithm() ;

        shared_ptr<CTransformFilter> transformFilter = shared_ptr<CTransformFilter>(gridAlgorithm->createTransformFilter(gc, detectMissingValues, defaultValue)) ;
        outputFilter->connectOutput(transformFilter,0) ;
        if(graphEnabled)
        {
          transformFilter->graphEnabled=true;
          transformFilter->graphPackage = new CGraphPackage;
          transformFilter->graphPackage->inFields.push_back(field);
          transformFilter->graphPackage->show = !transformationGoing;
        }
        
        vector<string> auxFieldId = algo->getAuxFieldId() ; // better to do that at transformation not algo ??
        int i=1; 
        for (auto& it : auxFieldId)
        {
          CField* auxField = CField::get(it) ;
          auxField->buildWorkflowGraph(gc) ;
          auxField->getInstantDataFilter()->connectOutput(transformFilter,i) ;
          i++ ;
        }
        outputFilter = transformFilter ;
      }

      if (hasRemainTransform)
      {
        transformationGoing = true;
        gridSrc=newGrid ;
        CField *field_bis = field;
        pair<shared_ptr<CFilter>, shared_ptr<CFilter> > filters = this->buildTransformationGraph(gc, isSource, gridSrc, detectMissingValues, defaultValue, newGrid, graphEnabled, field_bis) ;
        outputFilter->connectOutput(filters.first,0) ;
        outputFilter=filters.second ;
      }
      transformationGoing = false;
    }
     
    return {inputFilter,outputFilter} ;
  }
  CATCH_DUMP_ATTR


//****************************************************************
//****************************************************************

//----------------------------------------------------------------

  CGrid* CGrid::duplicateSentGrid(void)
  {
    CGrid* newGrid ;
    string sentGridId="sent__"+getId() ;
    if (has(sentGridId)) newGrid = get(sentGridId) ;
    else
    {
      newGrid = CGrid::create(sentGridId) ;
      for(auto element : elements_)
      {
        if (element.type==TYPE_DOMAIN)      
        {
          CDomain* domain = CDomain::create();
          domain->duplicateAttributes(element.domain) ;
          domain->setTemplateId(element.domain) ;
          domain->name = element.domain->getId() ;
          newGrid->addDomain(domain->getId()) ;
        }
        else if (element.type==TYPE_AXIS)      
        {
          CAxis* axis = CAxis::create();
          axis->duplicateAttributes(element.axis) ;
          axis->setTemplateId(element.axis) ;
          axis->name = element.axis->getId() ;
          newGrid->addAxis(axis->getId()) ;
        }
        else if (element.type==TYPE_SCALAR)      
        {
          CScalar* scalar = CScalar::create();
          scalar->duplicateAttributes(element.scalar) ;
          scalar->setTemplateId(element.scalar) ;
          scalar->name = element.scalar->getId() ;
          newGrid->addScalar(scalar->getId()) ;
        }
      }
      newGrid->checkElementsAttributes() ;
    }
    return newGrid ;
  }


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
    std::vector<CScalar*> scalarList = this->getScalars();
    auto domain=domainList.begin() ;
    auto axis=axisList.begin() ;
    auto scalar=scalarList.begin() ;
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
        elements.push_back((*scalar)->getLocalElement());
        scalar++ ;
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
    modelToWorkflowConnector_ = getGridLocalElements()->getConnector(CElementView::MODEL,CElementView::WORKFLOW,true) ;
  }

  void CGrid::computeWorkflowToFullConnector(void)
  {
    workflowToFullConnector_ = getGridLocalElements()->getConnector(CElementView::WORKFLOW,CElementView::FULL) ;
  }

  void CGrid::computeWorkflowToModelConnector(void)
  {
    workflowToModelConnector_ = getGridLocalElements()->getConnector(CElementView::WORKFLOW,CElementView::MODEL,true) ;
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
