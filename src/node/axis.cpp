#include "axis.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "message.hpp"
#include "type.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "context_server.hpp"
#include "xios_spl.hpp"
#include "server_distribution_description.hpp"
#include "client_server_mapping_distributed.hpp"
#include "distribution_client.hpp"

#include <algorithm>
#include <regex>

namespace xios {

   /// ////////////////////// Definitions ////////////////////// ///

   CAxis::CAxis(void)
      : CObjectTemplate<CAxis>()
      , CAxisAttributes(), isChecked(false), relFiles()
      , hasBounds(false), isCompressible_(false)
      , transformationMap_(), hasValue(false), hasLabel(false)
      , clients()
   {
   }

   CAxis::CAxis(const StdString & id)
      : CObjectTemplate<CAxis>(id)
      , CAxisAttributes(), isChecked(false), relFiles()
      , hasBounds(false), isCompressible_(false)
      , transformationMap_(), hasValue(false), hasLabel(false)
      , clients()
   {
   }

   CAxis::~CAxis(void)
   { /* Ne rien faire de plus */ }

   std::map<StdString, ETranformationType> CAxis::transformationMapList_ = std::map<StdString, ETranformationType>();
   bool CAxis::dummyTransformationMapList_ = CAxis::initializeTransformationMap(CAxis::transformationMapList_);
   bool CAxis::initializeTransformationMap(std::map<StdString, ETranformationType>& m)
   TRY
   {
     m["zoom_axis"] = TRANS_EXTRACT_AXIS;
     m["interpolate_axis"] = TRANS_INTERPOLATE_AXIS;
     m["extract_axis"] = TRANS_EXTRACT_AXIS;
     m["inverse_axis"] = TRANS_INVERSE_AXIS;
     m["reduce_domain"] = TRANS_REDUCE_DOMAIN_TO_AXIS;
     m["reduce_axis"] = TRANS_REDUCE_AXIS_TO_AXIS;
     m["extract_domain"] = TRANS_EXTRACT_DOMAIN_TO_AXIS;
     m["temporal_splitting"] = TRANS_TEMPORAL_SPLITTING;
     m["duplicate_scalar"] = TRANS_DUPLICATE_SCALAR_TO_AXIS;
     return true;
   }
   CATCH
   
   void CAxis::releaseStaticAllocation(void)
   {
     transformationMapList_.clear() ;
     CTransformation<CAxis>::unregisterAllTransformations() ;
     CGridTransformationFactory<CAxis>::unregisterAllTransformations() ;
   }

   ///---------------------------------------------------------------

   const std::set<StdString> & CAxis::getRelFiles(void) const
   TRY
   {
      return (this->relFiles);
   }
   CATCH

   bool CAxis::IsWritten(const StdString & filename) const
   TRY
   {
      return (this->relFiles.find(filename) != this->relFiles.end());
   }
   CATCH

   bool CAxis::isWrittenCompressed(const StdString& filename) const
   TRY
   {
      return (this->relFilesCompressed.find(filename) != this->relFilesCompressed.end());
   }
   CATCH

   bool CAxis::isDistributed(void) const
   TRY
   {
      bool distributed = (!this->begin.isEmpty() && !this->n.isEmpty() && (this->begin + this->n < this->n_glo)) ||
             (!this->n.isEmpty() && (this->n != this->n_glo));
      // A condition to make sure that if there is only one client, axis
      // should be considered to be distributed. This should be a temporary solution     
      distributed |= (1 == CContext::getCurrent()->intraCommSize_);
      return distributed;
   }
   CATCH

   /*!
    * Compute if the axis can be ouput in a compressed way.
    * In this case the workflow view on server side must be the same
    * than the full view for all context rank. The result is stored on
    * internal isCompressible_ attribute.
    */
   void CAxis::computeIsCompressible(void)
   TRY
   {
     // mesh is compressible contains some masked or indexed value, ie if full view is different of workflow view.
     // But now assume that the size of the 2 view must be equal for everybody. True on server side
     int isSameView = getLocalView(CElementView::FULL)->getSize() ==  getLocalView(CElementView::WORKFLOW)->getSize();
     MPI_Allreduce(MPI_IN_PLACE, &isSameView, 1, MPI_INT, MPI_LAND, CContext::getCurrent()->getIntraComm()) ;
     if (isSameView) isCompressible_ = false ;
     else isCompressible_ = true ;
     isCompressibleComputed_=true ;
   }
   CATCH

   void CAxis::addRelFile(const StdString & filename)
   TRY
   {
      this->relFiles.insert(filename);
   }
   CATCH_DUMP_ATTR

   void CAxis::addRelFileCompressed(const StdString& filename)
   TRY
   {
      this->relFilesCompressed.insert(filename);
   }
   CATCH_DUMP_ATTR

    //----------------------------------------------------------------

   /*!
    * Compute the minimum buffer size required to send the attributes to the server(s).
    *
    * \return A map associating the server rank with its minimum buffer size.
    */
   std::map<int, StdSize> CAxis::getAttributesBufferSize(CContextClient* client, const std::vector<int>& globalDim, int orderPositionInGrid,
                                                         CServerDistributionDescription::ServerDistributionType distType)
   TRY
   {

     std::map<int, StdSize> attributesSizes = getMinimumBufferSizeForAttributes(client);

//     bool isNonDistributed = (n_glo == n);
     bool isDistributed = (orderPositionInGrid == CServerDistributionDescription::defaultDistributedDimension(globalDim.size(), distType))
    		                 || (index.numElements() != n_glo);

     if (client->isServerLeader())
     {
       // size estimation for sendServerAttribut
       size_t size = 6 * sizeof(size_t);
       // size estimation for sendNonDistributedValue
       if (!isDistributed)
       {
//         size = std::max(size, CArray<double,1>::size(n_glo) + (isCompressible_ ? CArray<int,1>::size(n_glo) : 0));
         size += CArray<int,1>::size(n_glo);
         size += CArray<int,1>::size(n_glo);
         size += CArray<bool,1>::size(n_glo);
         size += CArray<double,1>::size(n_glo);
         if (hasBounds)
           size += CArray<double,2>::size(2*n_glo);
         if (hasLabel)
          size += CArray<StdString,1>::size(n_glo);
       }
       size += CEventClient::headerSize + getId().size() + sizeof(size_t);

       const std::list<int>& ranks = client->getRanksServerLeader();
       for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
       {
         if (size > attributesSizes[*itRank])
           attributesSizes[*itRank] = size;
       }
       const std::list<int>& ranksNonLeaders = client->getRanksServerNotLeader();
       for (std::list<int>::const_iterator itRank = ranksNonLeaders.begin(), itRankEnd = ranksNonLeaders.end(); itRank != itRankEnd; ++itRank)
       {
         if (size > attributesSizes[*itRank])
           attributesSizes[*itRank] = size;
       }

     }

     if (isDistributed)
     {
       // size estimation for sendDistributedValue
       std::unordered_map<int, vector<size_t> >::const_iterator it, ite = indSrv_[client->serverSize].end();
       for (it = indSrv_[client->serverSize].begin(); it != ite; ++it)
       {
         size_t size = 6 * sizeof(size_t);
         size += CArray<int,1>::size(it->second.size());
         size += CArray<int,1>::size(it->second.size());
         size += CArray<bool,1>::size(it->second.size());
         size += CArray<double,1>::size(it->second.size());
         if (hasBounds)
           size += CArray<double,2>::size(2 * it->second.size());
         if (hasLabel)
           size += CArray<StdString,1>::size(it->second.size());

         size += CEventClient::headerSize + getId().size() + sizeof(size_t);
         if (size > attributesSizes[it->first])
           attributesSizes[it->first] = size;
       }
     }
     return attributesSizes;
   }
   CATCH_DUMP_ATTR

   //----------------------------------------------------------------

   StdString CAxis::GetName(void)   { return (StdString("axis")); }
   StdString CAxis::GetDefName(void){ return (CAxis::GetName()); }
   ENodeType CAxis::GetType(void)   { return (eAxis); }

   //----------------------------------------------------------------

   CAxis* CAxis::createAxis()
   TRY
   {
     CAxis* axis = CAxisGroup::get("axis_definition")->createChild();
     return axis;
   }
   CATCH

   CAxis* CAxis::get(const string& id, bool noError)
   {
     const regex r("::");
     smatch m;
     if (regex_search(id, m, r))
     {
        if (m.size()!=1) ERROR("CAxis* CAxis::get(string& id)", <<" id = "<<id<< "  -> bad format id, separator :: append more than one time");
        string fieldId=m.prefix() ;
        if (fieldId.empty()) ERROR("CAxis* CAxis::get(string& id)", <<" id = "<<id<< "  -> bad format id, field name is empty");
        string suffix=m.suffix() ;
        if (!CField::has(fieldId)) 
          if (noError)  return nullptr ;
          else ERROR("CAxis* CAxis::get(string& id, bool noError)", <<" id = "<<id<< "  -> field Id : < "<<fieldId<<" > doesn't exist");
        CField* field=CField::get(fieldId) ;
        return field->getAssociatedAxis(suffix, noError) ;
     }
     {
       if (noError) if(!CObjectFactory::HasObject<CAxis>(id)) return nullptr ;
       return CObjectFactory::GetObject<CAxis>(id).get();
     }
   }
   
   bool CAxis::has(const string& id)
   {
     if (CAxis::get(id,true)==nullptr) return false ;
     else return true ;
   }
   
   CField* CAxis::getFieldFromId(const string& id)
   {
     const regex r("::");
     smatch m;
     if (regex_search(id, m, r))
     {
        if (m.size()!=1) ERROR("CField* CAxis::getFieldFromId(const string& id)", <<" id = "<<id<< "  -> bad format id, separator :: append more than one time");
        string fieldId=m.prefix() ;
        if (fieldId.empty()) ERROR("CField* CAxis::getFieldFromId(const string& id)", <<" id = "<<id<< "  -> bad format id, field name is empty");
        string suffix=m.suffix() ;
        CField* field=CField::get(fieldId) ;
        return field ;
     }
     else return nullptr;
   }

   /*!
     Check common attributes of an axis.
     This check should be done in the very beginning of work flow
   */
   
   void CAxis::checkAttributes(void)
   {
      if (checkAttributes_done_) return ;
      checkGeometricAttributes(true) ;
      initializeLocalElement() ;
      addFullView() ;
      addWorkflowView() ;
      addModelView() ;

      checkAttributes_done_ = true ;
   }
   
   void CAxis::resetGeometricAttributes(void)
   {
     n_glo.reset();
     index.reset();
     n.reset();
     begin.reset();
     mask.reset();
     data_index.reset();
     data_n.reset();
     data_begin.reset();
     value.reset();
     bounds.reset();
     label.reset() ;
   }

   void CAxis::setGeometricAttributes(const CAxis& axisSrc)
   {
     resetGeometricAttributes() ;
     n_glo=axisSrc.n_glo;
     if (!axisSrc.index.isEmpty())
     {
       index.resize(axisSrc.index.shape()) ;
       index=axisSrc.index;
     }

     n=axisSrc.n;
     begin=axisSrc.begin;
     if (!axisSrc.mask.isEmpty())
     {
       mask.resize(axisSrc.mask.shape()) ;
       mask=axisSrc.mask;
     }
     if (!axisSrc.data_index.isEmpty())
     {
       data_index.resize(axisSrc.data_index.shape()) ;
       data_index=axisSrc.data_index;
     }
     data_n=axisSrc.data_n;
     data_begin=axisSrc.data_begin;
     if (!axisSrc.value.isEmpty())
     {
       value.resize(axisSrc.value.shape()) ;
       value=axisSrc.value;
     }
     
     if (!axisSrc.bounds.isEmpty())
     {
       bounds.resize(axisSrc.bounds.shape()) ;
       bounds=axisSrc.bounds;
     }
     if (!axisSrc.label.isEmpty())
     {
       label.resize(axisSrc.label.shape()) ;
       label=axisSrc.label;
     }

   }
   
   bool CAxis::checkGeometricAttributes(bool generateError)
   TRY
   {
     CContext* context=CContext::getCurrent();

     if (this->n_glo.isEmpty())
        if (generateError) ERROR("CAxis::checkAttributes(void)",
                                << "[ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] "
                                << "The axis is wrongly defined, attribute 'n_glo' must be specified")
        else return false ; 
      StdSize size = this->n_glo.getValue();

      if (!this->index.isEmpty())
      {
        if (n.isEmpty()) n = index.numElements();

        // It's not so correct but if begin is not the first value of index 
        // then data on the local axis has user-defined distribution. In this case, begin has no meaning.
        if (begin.isEmpty()) begin = index(0);         
      }
      else 
      {
        if (!this->begin.isEmpty())
        {
          if (begin < 0 || begin > size - 1)
             if (generateError) ERROR("CAxis::checkAttributes(void)",
                                      << "[ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] "
                                      << "The axis is wrongly defined, attribute 'begin' (" 
                                      << begin.getValue() << ") must be non-negative and smaller than size-1 (" << size - 1 << ").")
              else return false ; 
        }
        else this->begin.setValue(0);

        if (!this->n.isEmpty())
        {
          if (n < 0 || n > size)
            if (generateError) ERROR("CAxis::checkAttributes(void)",
                                      << "[ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] "
                                      << "The axis is wrongly defined, attribute 'n' (" << n.getValue() << ") must be non-negative and smaller than size (" 
                                      << size << ").")
            else return false ; 
        }
        else this->n.setValue(size);

        {
          index.resize(n);
          for (int i = 0; i < n; ++i) index(i) = i+begin;
        }
      }

      if (!this->value.isEmpty())
      {
        StdSize true_size = value.numElements();
        if (this->n.getValue() != true_size)
          if (generateError) ERROR("CAxis::checkAttributes(void)",
                                   << "[ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] "
                                   << "The axis is wrongly defined, attribute 'value' has a different size (" << true_size
                                   << ") than the one defined by the \'size\' attribute (" << n.getValue() << ").")
          else return false ; 
        this->hasValue = true;
      }

      if (!this->checkBounds(generateError)) return false;
      if (!this->checkMask(generateError))   return false;
      if (!this->checkData(generateError))   return false;
      if (!this->checkLabel(generateError))  return false;
      
      return true ;
   }
   CATCH_DUMP_ATTR


   /*!
      Check the validity of data, fill in values if any, and apply mask.
   */
   bool CAxis::checkData(bool generateError)
   TRY
   {
      if (data_begin.isEmpty()) data_begin.setValue(0);

      if (data_n.isEmpty())
      {
        data_n.setValue(n);
      }
      else if (data_n.getValue() < 0)
      {
        if (generateError) ERROR("CAxis::checkData(void)",
                              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                              << "The data size should be strictly positive ('data_n' = " << data_n.getValue() << ").")
        else return false ;
      }

      if (data_index.isEmpty())
      {
        data_index.resize(data_n);
        for (int i = 0; i < data_n; ++i)
        {
          if ((i+data_begin) >= 0 && (i+data_begin<n))
          {
            if (mask(i+data_begin))
              data_index(i) = i+data_begin;
            else
              data_index(i) = -1;
          }
          else
            data_index(i) = -1;
        }
      }
      else
      {
        if (data_index.numElements() != data_n)
        {
          if (generateError) ERROR("CAxis::checkData(void)",
                               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                               << "The size of data_index = "<< data_index.numElements() << "is not equal to the data size data_n = " 
                               << data_n.getValue() << ").")
          else return false ;
        }
        for (int i = 0; i < data_n; ++i)
        {
           if (data_index(i) >= 0 && data_index(i)<n)
             if (!mask(data_index(i))) data_index(i) = -1;
        }
      }
      return true ;
   }
   CATCH_DUMP_ATTR

/*!
   Check validity of mask info and fill in values if any.
  */
   bool CAxis::checkMask(bool generateError)
   TRY
   {
      if (!mask.isEmpty())
      {
        if (mask.extent(0) != n)
        {
          if (generateError) ERROR("CAxis::checkMask(void)",
                                  << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                                  << "The mask does not have the same size as the local domain." << std::endl
                                  << "Local size is " << n.getValue() << "." << std::endl
                                  << "Mask size is " << mask.extent(0) << ".")
          else return false ;
        }
      }
      else
      {
        mask.resize(n);
        mask = true;
      }
      return true ;
   }
   CATCH_DUMP_ATTR

   /*!
     Check validity of bounds info and fill in values if any.
   */
   bool CAxis::checkBounds(bool generateError)
   TRY
   {
     if (!bounds.isEmpty())
     {
       if (bounds.extent(0) != 2 || bounds.extent(1) != n)
         if (generateError) ERROR("CAxis::checkAttributes(void)",
                               << "The bounds array of the axis [ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] must be of dimension 2 x axis size." << std::endl
                               << "Axis size is " << n.getValue() << "." << std::endl
                               << "Bounds size is "<< bounds.extent(0) << " x " << bounds.extent(1) << ".")
         else return false ;
       hasBounds = true;
     }
     else hasBounds = false;
     return true ;
   }
   CATCH_DUMP_ATTR

  bool CAxis::checkLabel(bool generateError)
  TRY
  {
    if (!label.isEmpty())
    {
      if (label.extent(0) != n)
        if (generateError) ERROR("CAxis::checkLabel(void)",
                              << "The label array of the axis [ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] must be of dimension of axis size." << std::endl
                              << "Axis size is " << n.getValue() << "." << std::endl
                              << "label size is "<< label.extent(0)<<  " .")
        else return false ;
      hasLabel = true;
    }
    else hasLabel = false;
    return true ;
  }
  CATCH_DUMP_ATTR


  size_t CAxis::getGlobalWrittenSize(void)
  {
    return n_glo ;
  }

   

 
  /*!
    Dispatch event from the lower communication layer then process event according to its type
  */
  bool CAxis::dispatchEvent(CEventServer& event)
  TRY
  {
     if (SuperClass::dispatchEvent(event)) return true;
     else
     {
       switch(event.type)
       {
         case EVENT_ID_AXIS_DISTRIBUTION:
           recvAxisDistribution(event);
           return true;
           break;
         case EVENT_ID_SEND_DISTRIBUTED_ATTRIBUTE:
           recvDistributedAttributes(event);
           return true;
           break;
          default :
            ERROR("bool CAxis::dispatchEvent(CEventServer& event)",
                   << "Unknown Event");
          return false;
        }
     }
  }
  CATCH

   /* to remove later when reimplementing coupling */
   void CAxis::sendAxisToCouplerOut(CContextClient* client, const std::vector<int>& globalDim, int orderPositionInGrid, const string& fieldId, int posInGrid)
   {
     if (sendAxisToCouplerOut_done_.count(client)!=0) return ;
     else sendAxisToCouplerOut_done_.insert(client) ;
     
     string axisId="_axis["+std::to_string(posInGrid)+"]_of_"+fieldId ;

    }

  string CAxis::getCouplingAlias(const string& fieldId, int posInGrid)
  {
    return "_axis["+std::to_string(posInGrid)+"]_of_"+fieldId ;
  }

  void CAxis::makeAliasForCoupling(const string& fieldId, int posInGrid)
  {
    const string axisId = getCouplingAlias(fieldId,posInGrid)  ;
    this->createAlias(axisId) ;
  }

  
  /*!
    Compare two axis objects. 
    They are equal if only if they have identical attributes as well as their values.
    Moreover, they must have the same transformations.
  \param [in] axis Compared axis
  \return result of the comparison
  */
  bool CAxis::isEqual(CAxis* obj)
  TRY
  {
    vector<StdString> excludedAttr;
    excludedAttr.push_back("axis_ref");

    bool objEqual = SuperClass::isEqual(obj, excludedAttr);    
    if (!objEqual) return objEqual;

    TransMapTypes thisTrans = this->getAllTransformations();
    TransMapTypes objTrans  = obj->getAllTransformations();

    TransMapTypes::const_iterator it, itb, ite;
    std::vector<ETranformationType> thisTransType, objTransType;
    for (it = thisTrans.begin(); it != thisTrans.end(); ++it)
      thisTransType.push_back(it->first);
    for (it = objTrans.begin(); it != objTrans.end(); ++it)
      objTransType.push_back(it->first);

    if (thisTransType.size() != objTransType.size()) return false;
    for (int idx = 0; idx < thisTransType.size(); ++idx)
      objEqual &= (thisTransType[idx] == objTransType[idx]);

    return objEqual;
  }
  CATCH_DUMP_ATTR

  /*
    Add transformation into axis. This function only servers for Fortran interface
    \param [in] transType transformation type
    \param [in] id identifier of the transformation object
  */
  CTransformation<CAxis>* CAxis::addTransformation(ETranformationType transType, const StdString& id)
  TRY
  {
    transformationMap_.push_back(std::make_pair(transType, CTransformation<CAxis>::createTransformation(transType,id)));
    return transformationMap_.back().second;
  }
  CATCH_DUMP_ATTR

  /*
    Check whether an axis has (spatial) transformation
  */
  bool CAxis::hasTransformation()
  TRY
  {
    return (!transformationMap_.empty());
  }
  CATCH_DUMP_ATTR

  /*
    Set transformation
    \param [in] axisTrans transformation to set
  */
  void CAxis::setTransformations(const TransMapTypes& axisTrans)
  TRY
  {
    transformationMap_ = axisTrans;
  }
  CATCH_DUMP_ATTR

  /*
    Return all transformation held by the axis
    \return transformation the axis has
  */
  CAxis::TransMapTypes CAxis::getAllTransformations(void)
  TRY
  {
    return transformationMap_;
  }
  CATCH_DUMP_ATTR

  /*
    Duplicate transformation of another axis
    \param [in] src axis whose transformations are copied
  */
  void CAxis::duplicateTransformation(CAxis* src)
  TRY
  {
    if (src->hasTransformation())
    {
      this->setTransformations(src->getAllTransformations());
    }
  }
  CATCH_DUMP_ATTR

  /*!
   * Go through the hierarchy to find the axis from which the transformations must be inherited
   */
  void CAxis::solveInheritanceTransformation_old()
  TRY
  {
    if (hasTransformation() || !hasDirectAxisReference())
      return;

    CAxis* axis = this;
    std::vector<CAxis*> refAxis;
    while (!axis->hasTransformation() && axis->hasDirectAxisReference())
    {
      refAxis.push_back(axis);
      axis = axis->getDirectAxisReference();
    }

    if (axis->hasTransformation())
      for (size_t i = 0; i < refAxis.size(); ++i)
        refAxis[i]->setTransformations(axis->getAllTransformations());
  }
  CATCH_DUMP_ATTR

  void CAxis::solveInheritanceTransformation()
  TRY
  {
    if (solveInheritanceTransformation_done_) return;
    else solveInheritanceTransformation_done_=true ;

    CAxis* axis = this;
    std::list<CAxis*> refAxis;
    bool out=false ;
    vector<StdString> excludedAttr;
    excludedAttr.push_back("axis_ref");
    
    refAxis.push_front(axis) ;
    while (axis->hasDirectAxisReference() && !out)
    {
      CAxis* lastAxis=axis ;
      axis = axis->getDirectAxisReference();
      axis->solveRefInheritance() ;
      if (!axis->SuperClass::isEqual(lastAxis,excludedAttr)) out=true ;
      refAxis.push_front(axis) ;
    }

    CTransformationPaths::TPath path ;
    auto& pathList = std::get<2>(path) ;
    std::get<0>(path) = EElement::AXIS ;
    std::get<1>(path) = refAxis.front()->getId() ;
    for (auto& axis : refAxis)
    {
      CAxis::TransMapTypes transformations = axis->getAllTransformations();
      for(auto& transformation : transformations) pathList.push_back({transformation.second->getTransformationType(), 
                                                                      transformation.second->getId()}) ;
    }
    transformationPaths_.addPath(path) ;

  }
  CATCH_DUMP_ATTR

  bool CAxis::activateFieldWorkflow(CGarbageCollector& gc)
  TRY
  {
    if (!axis_ref.isEmpty())
    {
      CField* field=getFieldFromId(axis_ref) ;
      if (field!=nullptr)
      {
        bool ret = field->buildWorkflowGraph(gc) ;
        if (!ret) return false ; // cannot build workflow graph at this state
      }
      else 
      {
        CAxis* axis = get(axis_ref) ;
        bool ret = axis->activateFieldWorkflow(gc) ;
        if (!ret) return false ; // cannot build workflow graph at this state
        axis_ref=axis->getId() ; // replace domain_ref by solved reference
      }
    }
    activateFieldWorkflow_done_=true ;
    return true ;
  }
  CATCH_DUMP_ATTR


  void CAxis::setContextClient(CContextClient* contextClient)
  TRY
  {
    if (clientsSet.find(contextClient)==clientsSet.end())
    {
      clients.push_back(contextClient) ;
      clientsSet.insert(contextClient);
    }
  }
  CATCH_DUMP_ATTR

  void CAxis::parse(xml::CXMLNode & node)
  TRY
  {
    SuperClass::parse(node);

    if (node.goToChildElement())
    {
      StdString nodeElementName;
      do
      {
        StdString nodeId("");
        if (node.getAttributes().end() != node.getAttributes().find("id"))
        { nodeId = node.getAttributes()["id"]; }

        nodeElementName = node.getElementName();
        std::map<StdString, ETranformationType>::const_iterator ite = transformationMapList_.end(), it;
        it = transformationMapList_.find(nodeElementName);
        if (ite != it)
        {
          if (it->first == "zoom_axis")
          {
            info(0) << "WARNING : " << it->first << " is deprecated, replaced by extract_axis." << endl;
          }
          transformationMap_.push_back(std::make_pair(it->second, CTransformation<CAxis>::createTransformation(it->second,
                                                                                                               nodeId,
                                                                                                               &node)));
        }
        else
        {
          ERROR("void CAxis::parse(xml::CXMLNode & node)",
                << "The transformation " << nodeElementName << " has not been supported yet.");
        }
      } while (node.goToNextElement()) ;
      node.goToParentElement();
    }
  }
  CATCH_DUMP_ATTR


   //////////////////////////////////////////////////////////////////////////////////////
   //  this part is related to distribution, element definition, views and connectors  //
   //////////////////////////////////////////////////////////////////////////////////////

   void CAxis::initializeLocalElement(void)
   {
      // after checkAttribute index of size n
      int rank = CContext::getCurrent()->getIntraCommRank() ;
      
      CArray<size_t,1> ind(n) ;
      for (int i=0;i<n;i++) ind(i)=index(i) ;

      localElement_ = make_shared<CLocalElement>(rank, n_glo, ind) ;
   }

   void CAxis::addFullView(void)
   {
      CArray<int,1> index(n) ;
      for(int i=0; i<n ; i++) index(i)=i ;
      localElement_ -> addView(CElementView::FULL, index) ;
   }

   void CAxis::addWorkflowView(void)
   {
     // mask + data are included into data_index
     int nk=data_index.numElements() ;
     int nMask=0 ;
     for(int k=0;k<nk;k++) if (data_index(k)>=0 && data_index(k)<n) nMask++ ;
     
     CArray<int,1> index(nMask) ;
     nMask=0 ;
     for(int k=0;k<nk;k++) 
       if (data_index(k)>=0 && data_index(k)<n) 
       {
         index(nMask) = data_index(k) ;
         nMask++ ;
       }
     localElement_ -> addView(CElementView::WORKFLOW, index) ;
   }

   void CAxis::addModelView(void)
   {
     // information for model view is stored in data_index
     localElement_->addView(CElementView::MODEL, data_index) ;
   }

   void CAxis::computeModelToWorkflowConnector(void)
   { 
     shared_ptr<CLocalView> srcView=getLocalView(CElementView::MODEL) ;
     shared_ptr<CLocalView> dstView=getLocalView(CElementView::WORKFLOW) ;
     modelToWorkflowConnector_ = make_shared<CLocalConnector>(srcView, dstView); 
     modelToWorkflowConnector_->computeConnector() ;
   }


   void CAxis::computeRemoteElement(CContextClient* client, EDistributionType type)
  {
    CContext* context = CContext::getCurrent();
    map<int, CArray<size_t,1>> globalIndex ;

    if (type==EDistributionType::BANDS) // Bands distribution to send to file server
    {
      int nbServer = client->serverSize;
      int nbClient = client->clientSize ;
      int rankClient = client->clientRank ;
      int size = nbServer / nbClient ;
      int start ;
      if (nbServer%nbClient > rankClient)
      {
       start = (size+1) * rankClient ;
       size++ ;
      }
      else start = size*rankClient + nbServer%nbClient ;
     
      for(int i=0; i<size; i++)
      { 
        int rank=start+i ; 
        size_t indSize = n_glo/nbServer ;
        size_t indStart ;
        if (n_glo % nbServer > rank)
        {
          indStart = (indSize+1) * rank ;
          indSize++ ;
        }
        else indStart = indSize*rank + n_glo%nbServer ;
       
        auto& globalInd =  globalIndex[rank] ;
        globalInd.resize(indSize) ;
        for(size_t n = 0 ; n<indSize; n++) globalInd(n)=indStart+n ;
      }
    }
    else if (type==EDistributionType::NONE) // domain is not distributed ie all servers get the same local domain
    {
      int nbServer = client->serverSize;
      size_t nglo=n_glo ;
      CArray<size_t,1> indGlo(nglo) ;
      for(size_t i=0;i<nglo;i++) indGlo(i) = i ;
      for (auto& rankServer : client->getRanksServerLeader()) globalIndex[rankServer].reference(indGlo.copy()); 
    }
    remoteElement_[client] = make_shared<CDistributedElement>(n_glo, globalIndex) ;
    remoteElement_[client]->addFullView() ;
  }
 
  void CAxis::distributeToServer(CContextClient* client, std::map<int, CArray<size_t,1>>& globalIndexOut, std::map<int, CArray<size_t,1>>& globalIndexIn, 
                                 shared_ptr<CScattererConnector> &scattererConnector, const string& axisId)
  {
    string serverAxisId = axisId.empty() ? this->getId() : axisId ;
    CContext* context = CContext::getCurrent();

    this->sendAllAttributesToServer(client, serverAxisId)  ;

    auto scatteredElement = make_shared<CDistributedElement>(n_glo,globalIndexOut) ;
    scatteredElement->addFullView() ;
    scattererConnector = make_shared<CScattererConnector>(localElement_->getView(CElementView::FULL), scatteredElement->getView(CElementView::FULL), 
                                                          context->getIntraComm(), client->getRemoteSize()) ;
    scattererConnector->computeConnector() ;
    
    // phase 0
    // send remote element to construct the full view on server, ie without hole 
    CEventClient event0(getType(), EVENT_ID_AXIS_DISTRIBUTION);
    CMessage message0 ;
    message0<<serverAxisId<<0 ; 
    remoteElement_[client]->sendToServer(client,event0,message0) ; 
    
    // phase 1
    // send the full view of element to construct the connector which connect distributed data coming from client to the full local view
    CEventClient event1(getType(), EVENT_ID_AXIS_DISTRIBUTION);
    CMessage message1 ;
    message1<<serverAxisId<<1<<localElement_->getView(CElementView::FULL)->getGlobalSize() ; 
    scattererConnector->transfer(localElement_->getView(CElementView::FULL)->getGlobalIndex(),client,event1,message1) ;

    sendDistributedAttributes(client, scattererConnector, axisId) ;
  
    // phase 2 send the mask : data index + mask2D
    {
      CArray<bool,1> maskIn(localElement_->getView(CElementView::WORKFLOW)->getSize());
      CArray<bool,1> maskOut ;
      auto workflowToFull = make_shared<CLocalConnector>(localElement_->getView(CElementView::WORKFLOW), localElement_->getView(CElementView::FULL)) ;
      workflowToFull->computeConnector() ;
      maskIn=true ;
      workflowToFull->transfer(maskIn,maskOut,false) ;

      //  prepare grid scatterer connector to send data from client to server
      map<int,CArray<size_t,1>> workflowGlobalIndex ;
      map<int,CArray<bool,1>> maskOut2 ; 
      scattererConnector->transfer(maskOut, maskOut2) ;
      scatteredElement->addView(CElementView::WORKFLOW, maskOut2) ;
      scatteredElement->getView(CElementView::WORKFLOW)->getGlobalIndexView(workflowGlobalIndex) ;
      // create new workflow view for scattered element
      auto clientToServerElement = make_shared<CDistributedElement>(scatteredElement->getGlobalSize(), workflowGlobalIndex) ;
      clientToServerElement->addFullView() ;
      CEventClient event2(getType(), EVENT_ID_AXIS_DISTRIBUTION);
      CMessage message2 ;
      message2<<serverAxisId<<2 ; 
      clientToServerElement->sendToServer(client, event2, message2) ; 
      clientToServerConnector_[client] = make_shared<CScattererConnector>(localElement_->getView(CElementView::WORKFLOW), clientToServerElement->getView(CElementView::FULL), 
                                                                        context->getIntraComm(), client->getRemoteSize()) ;
      clientToServerConnector_[client]->computeConnector() ;
    }

    ////////////
    // phase 3 : compute connector to receive from server
    ////////////
    {
      auto scatteredElement = make_shared<CDistributedElement>(n_glo, globalIndexIn) ;
      scatteredElement->addFullView() ;
      auto scattererConnector = make_shared<CScattererConnector>(localElement_->getView(CElementView::FULL), scatteredElement->getView(CElementView::FULL), 
                                                                 context->getIntraComm(), client->getRemoteSize()) ;
      scattererConnector->computeConnector() ;
      CArray<bool,1> maskIn(localElement_->getView(CElementView::WORKFLOW)->getSize());
      CArray<bool,1> maskOut ;
      auto workflowToFull = make_shared<CLocalConnector>(localElement_->getView(CElementView::WORKFLOW), localElement_->getView(CElementView::FULL)) ;
      workflowToFull->computeConnector() ;
      maskIn=true ;
      workflowToFull->transfer(maskIn,maskOut,false) ;

      map<int,CArray<size_t,1>> workflowGlobalIndex ;
      map<int,CArray<bool,1>> maskOut2 ; 
      scattererConnector->transfer(maskOut, maskOut2, false) ;
      scatteredElement->addView(CElementView::WORKFLOW, maskOut2) ;
      scatteredElement->getView(CElementView::WORKFLOW)->getGlobalIndexView(workflowGlobalIndex) ;
      auto clientToServerElement = make_shared<CDistributedElement>(scatteredElement->getGlobalSize(), workflowGlobalIndex) ;
      clientToServerElement->addFullView() ;
      CEventClient event3(getType(), EVENT_ID_AXIS_DISTRIBUTION);
      CMessage message3 ;
      message3<<serverAxisId<<3 ; 
      clientToServerElement->sendToServer(client, event3, message3) ; 

      clientFromServerConnector_[client] = make_shared<CGathererConnector>(clientToServerElement->getView(CElementView::FULL), localElement_->getView(CElementView::WORKFLOW));
      clientFromServerConnector_[client]->computeConnector() ;      
    }

//    clientFromServerConnector_[client] = make_shared<CGathererConnector>(clientToServerElement->getView(CElementView::FULL), localElement_->getView(CElementView::WORKFLOW));
//    clientFromServerConnector_[client]->computeConnector() ;


  }

  void CAxis::recvAxisDistribution(CEventServer& event)
  TRY
  {
    string axisId;
    int phasis ;
    for (auto& subEvent : event.subEvents) (*subEvent.buffer) >> axisId >> phasis ;
    get(axisId)->receivedAxisDistribution(event, phasis);
  }
  CATCH


  void CAxis::receivedAxisDistribution(CEventServer& event, int phasis)
  TRY
  {
    CContext* context = CContext::getCurrent();
    if (phasis==0) // receive the remote element to construct the full view
    {
      localElement_ = make_shared<CLocalElement>(context->getIntraCommRank(),event) ;
      localElement_->addFullView() ;
      // construct the local dimension and indexes
      auto& globalIndex=localElement_->getGlobalIndex() ;
      int nk=globalIndex.numElements() ;
      int minK=n_glo,maxK=-1 ;
      int nGlo=n_glo ;
      int indGlo ;
      for(int k=0;k<nk;k++)
      {
        indGlo=globalIndex(k) ;
        if (indGlo<minK) minK=indGlo ;
        if (indGlo>maxK) maxK=indGlo ;
      }  
      if (maxK>=minK) { begin=minK ; n=maxK-minK+1 ; }
      else {begin=0; n=0 ;}

    }
    else if (phasis==1) // receive the sent view from client to construct the full distributed full view on server
    {
      CContext* context = CContext::getCurrent();
      shared_ptr<CDistributedElement> elementFrom = make_shared<CDistributedElement>(event) ;
      elementFrom->addFullView() ;
      gathererConnector_ = make_shared<CGathererConnector>(elementFrom->getView(CElementView::FULL), localElement_->getView(CElementView::FULL)) ;
      gathererConnector_->computeConnector() ; 
    }
    else if (phasis==2)
    {
//      delete gathererConnector_ ;
      elementFrom_ = make_shared<CDistributedElement>(event) ;
      elementFrom_->addFullView() ;
//      gathererConnector_ =  make_shared<CGathererConnector>(elementFrom_->getView(CElementView::FULL), localElement_->getView(CElementView::FULL)) ;
//      gathererConnector_ -> computeConnector() ;
    }
    else if (phasis==3)
    {
      elementTo_ = make_shared<CDistributedElement>(event) ;
      elementTo_->addFullView() ;
    }
  }
  CATCH

  void CAxis::setServerMask(CArray<bool,1>& serverMask, CContextClient* client)
  TRY
  {
    CContext* context = CContext::getCurrent();
    localElement_->addView(CElementView::WORKFLOW, serverMask) ;
    mask.reference(serverMask.copy()) ;
 
    serverFromClientConnector_ = make_shared<CGathererConnector>(elementFrom_->getView(CElementView::FULL), localElement_->getView(CElementView::WORKFLOW)) ;
    serverFromClientConnector_->computeConnector() ;
      
    serverToClientConnector_ = make_shared<CScattererConnector>(localElement_->getView(CElementView::WORKFLOW), elementTo_->getView(CElementView::FULL),
                                                                context->getIntraComm(), client->getRemoteSize()) ;
    serverToClientConnector_->computeConnector() ;
  }
  CATCH_DUMP_ATTR

  void CAxis::sendDistributedAttributes(CContextClient* client, shared_ptr<CScattererConnector> scattererConnector, const string& axisId)
  {
    string serverAxisId = axisId.empty() ? this->getId() : axisId ;
    CContext* context = CContext::getCurrent();

    if (hasValue)
    {
      { // send level value
        CEventClient event(getType(), EVENT_ID_SEND_DISTRIBUTED_ATTRIBUTE);
        CMessage message ;
        message<<serverAxisId<<string("value") ; 
        scattererConnector->transfer(value, client, event,message) ;
      }
    }

    if (hasBounds)
    {
      { // send bounds level value
        CEventClient event(getType(), EVENT_ID_SEND_DISTRIBUTED_ATTRIBUTE);
        CMessage message ;
        message<<serverAxisId<<string("bounds") ; 
        scattererConnector->transfer(2, bounds, client, event,message) ;
      }
    }

    if (hasLabel)
    {
      { // send label
        // need to transform array of string (no fixed size for string) into array of array of char
        // to use connector to transfer
        // the strings must have fixed size which the maximum lenght over the string label.  
        int maxSize=0 ;
        for(int i=0; i<label.numElements();i++) 
          if (maxSize < label(i).size()) maxSize=label(i).size() ;
        MPI_Allreduce(MPI_IN_PLACE, &maxSize,1,MPI_INT,MPI_MAX, context->getIntraComm()) ;
        maxSize=maxSize+1 ;
        CArray<char,2> charArray(maxSize,label.numElements()) ;
        for(int j=0; j<label.numElements();j++) 
        {
          const char* str = label(j).c_str() ;
          int strSize=label(j).size()+1 ;
          for(int i=0; i<strSize; i++) charArray(i,j) = str[i] ;
        }
        CEventClient event(getType(), EVENT_ID_SEND_DISTRIBUTED_ATTRIBUTE);
        CMessage message ;
        message<<serverAxisId<<string("label")<<maxSize ;
        scattererConnector->transfer(maxSize, charArray, client, event,message) ;
      }
    }
  }

  void CAxis::recvDistributedAttributes(CEventServer& event)
  TRY
  {
    string axisId;
    string type ;
    for (auto& subEvent : event.subEvents) (*subEvent.buffer) >> axisId >> type ;
    get(axisId)->recvDistributedAttributes(event, type);
  }
  CATCH

  void CAxis::recvDistributedAttributes(CEventServer& event, const string& type)
  TRY
  {
    if (type=="value") 
    {
      gathererConnector_->transfer(event, value, 0.); 
    }
    else if (type=="bounds")
    {
      CArray<double,1> value ;
      gathererConnector_->transfer(event, 2, value, 0.); 
      bounds.resize(2,n) ;
      if (bounds.numElements() > 0 ) bounds=CArray<double,2>(value.dataFirst(),shape(2,n),neverDeleteData) ; 
    }
    else if (type=="label")
    {
      int maxSize ;
      for (auto& subEvent : event.subEvents) (*subEvent.buffer) >> maxSize ;
      CArray<char,1> value ;
      gathererConnector_->transfer(event, maxSize, value, '\0'); 
      CArray<char,2> charArray(maxSize,n) ;
      label.resize(n) ;
      if (n>0)
      {
        charArray=CArray<char,2>(value.dataFirst(),shape(maxSize,n),neverDeleteData) ;
        for(int j=0;j<n;j++)
        {
          int strSize ;
          for(int i=0;i<maxSize;i++) 
            if (charArray(i,j)=='\0') { strSize=i ; break; }
          string str(strSize,'\0') ;
          for(int i=0;i<strSize;i++) str[i]=charArray(i,j) ; 
          label(j)=str ;
        }
      } 
    }
  }
  CATCH

  DEFINE_REF_FUNC(Axis,axis)

   ///---------------------------------------------------------------

} // namespace xios
