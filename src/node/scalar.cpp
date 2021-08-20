#include "scalar.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "object_factory.hpp"
#include "xios_spl.hpp"
#include "type.hpp"
#include "context.hpp"

#include <algorithm>
#include <regex>

namespace xios 
{

  /// ////////////////////// Définitions ////////////////////// ///

  CScalar::CScalar(void)
     : CObjectTemplate<CScalar>()
     , CScalarAttributes()
     , relFiles()
  { /* Ne rien faire de plus */ }

  CScalar::CScalar(const StdString & id)
     : CObjectTemplate<CScalar>(id)
     , CScalarAttributes()
     , relFiles()
  { /* Ne rien faire de plus */ }

  CScalar::~CScalar(void)
  { /* Ne rien faire de plus */ }

  std::map<StdString, ETranformationType> CScalar::transformationMapList_ = std::map<StdString, ETranformationType>();
  bool CScalar::dummyTransformationMapList_ = CScalar::initializeTransformationMap(CScalar::transformationMapList_);
  bool CScalar::initializeTransformationMap(std::map<StdString, ETranformationType>& m)
  {
    m["reduce_axis"]   = TRANS_REDUCE_AXIS_TO_SCALAR;
    m["extract_axis"]  = TRANS_EXTRACT_AXIS_TO_SCALAR;
    m["reduce_domain"] = TRANS_REDUCE_DOMAIN_TO_SCALAR;
    m["reduce_scalar"] = TRANS_REDUCE_SCALAR_TO_SCALAR;
    return true;
  }

  StdString CScalar::GetName(void)   { return (StdString("scalar")); }
  StdString CScalar::GetDefName(void){ return (CScalar::GetName()); }
  ENodeType CScalar::GetType(void)   { return (eScalar); }

  CScalar* CScalar::createScalar()
  {
    CScalar* scalar = CScalarGroup::get("scalar_definition")->createChild();
    return scalar;
  }

  CScalar* CScalar::get(const string& id)
  {
    const regex r("::");
    smatch m;
    if (regex_search(id, m, r))
    {
      if (m.size()!=1) ERROR("CScalar* CScalar::get(string& id)", <<" id = "<<id<< "  -> bad format id, separator :: append more than one time");
      string fieldId=m.prefix() ;
      if (fieldId.empty()) ERROR("CScalar* CScalar::get(string& id)", <<" id = "<<id<< "  -> bad format id, field name is empty");
      string suffix=m.suffix() ;
      CField* field=CField::get(fieldId) ;
      return field->getAssociatedScalar(suffix) ;
    }
    else return CObjectFactory::GetObject<CScalar>(id).get();
  }

  bool CScalar::IsWritten(const StdString & filename) const
  {
    return (this->relFiles.find(filename) != this->relFiles.end());
  }

  void CScalar::addRelFile(const StdString& filename)
  {
      this->relFiles.insert(filename);
  }

  void CScalar::checkAttributes(void)
  {
    if (checkAttributes_done_) return ;
    checkAttributes_done_ = true ; 

    if (mask.isEmpty()) mask=true ;

    initializeLocalElement() ;
    addFullView() ;
    addWorkflowView() ;
    addModelView() ;
  }

  /*!
    Compare two scalar objects. 
    They are equal if only if they have identical attributes as well as their values.
    Moreover, they must have the same transformations.
  \param [in] scalar Compared scalar
  \return result of the comparison
  */
  bool CScalar::isEqual(CScalar* obj)
  {
    vector<StdString> excludedAttr;
    excludedAttr.push_back("scalar_ref");
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

  CTransformation<CScalar>* CScalar::addTransformation(ETranformationType transType, const StdString& id)
  {
    transformationMap_.push_back(std::make_pair(transType, CTransformation<CScalar>::createTransformation(transType,id)));
    return transformationMap_.back().second;
  }

  bool CScalar::hasTransformation()
  {
    return (!transformationMap_.empty());
  }

  void CScalar::setTransformations(const TransMapTypes& scalarTrans)
  {
    transformationMap_ = scalarTrans;
  }

  CScalar::TransMapTypes CScalar::getAllTransformations(void)
  {
    return transformationMap_;
  }

  void CScalar::duplicateTransformation(CScalar* src)
  {
    if (src->hasTransformation())
    {
      this->setTransformations(src->getAllTransformations());
    }
  }

  /*!
   * Go through the hierarchy to find the scalar from which the transformations must be inherited
   */
  void CScalar::solveInheritanceTransformation_old()
  {
    if (hasTransformation() || !hasDirectScalarReference())
      return;

    CScalar* scalar = this;
    std::vector<CScalar*> refScalar;
    while (!scalar->hasTransformation() && scalar->hasDirectScalarReference())
    {
      refScalar.push_back(scalar);
      scalar = scalar->getDirectScalarReference();
    }

    if (scalar->hasTransformation())
      for (size_t i = 0; i < refScalar.size(); ++i)
        refScalar[i]->setTransformations(scalar->getAllTransformations());
  }
 
  void CScalar::solveInheritanceTransformation()
  TRY
  {
    if (solveInheritanceTransformation_done_) return;
    else solveInheritanceTransformation_done_=true ;

    CScalar* scalar = this;
    CScalar* Lastscalar ;
    std::list<CScalar*> refScalars;
    bool out=false ;
    vector<StdString> excludedAttr;
    excludedAttr.push_back("scalar_ref");
    
    refScalars.push_front(scalar) ;
    while (scalar->hasDirectScalarReference() && !out)
    {
      CScalar* lastScalar=scalar ;
      scalar = scalar->getDirectScalarReference();
      scalar->solveRefInheritance() ;
      if (!scalar->SuperClass::isEqual(lastScalar,excludedAttr)) out=true ;
      refScalars.push_front(scalar) ;
    }

    CTransformationPaths::TPath path ;
    auto& pathList = std::get<2>(path) ;
    std::get<0>(path) = EElement::SCALAR ;
    std::get<1>(path) = refScalars.front()->getId() ;
    for (auto& scalar : refScalars)
    {
      CScalar::TransMapTypes transformations = scalar->getAllTransformations();
      for(auto& transformation : transformations) pathList.push_back({transformation.second->getTransformationType(), 
                                                                      transformation.second->getId()}) ;
    }
    transformationPaths_.addPath(path) ;

  }
  CATCH_DUMP_ATTR

  /* obsolete, to remove after reimplementing coupling */
  void CScalar::sendScalarToCouplerOut(CContextClient* client, const string& fieldId, int posInGrid)
  {
    if (sendScalarToCouplerOut_done_.count(client)!=0) return ;
    else sendScalarToCouplerOut_done_.insert(client) ;

    string scalarId = getCouplingAlias(fieldId, posInGrid) ;

    this->sendAllAttributesToServer(client, scalarId);
  }  

  string CScalar::getCouplingAlias(const string& fieldId, int posInGrid)
  {
    return "_scalar["+std::to_string(posInGrid)+"]_of_"+fieldId ;
  }

  void CScalar::makeAliasForCoupling(const string& fieldId, int posInGrid)
  {
    const string scalarId = getCouplingAlias(fieldId, posInGrid) ;
    this->createAlias(scalarId) ;
  }

  void CScalar::setContextClient(CContextClient* contextClient)
  TRY
  {
    if (clientsSet.find(contextClient)==clientsSet.end())
    {
      clients.push_back(contextClient) ;
      clientsSet.insert(contextClient);
    }
  }
  CATCH_DUMP_ATTR
  /*!
    Parse children nodes of a scalar in xml file.
    \param node child node to process
  */
  void CScalar::parse(xml::CXMLNode & node)
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
          transformationMap_.push_back(std::make_pair(it->second, CTransformation<CScalar>::createTransformation(it->second,
                                                                                                                 nodeId,
                                                                                                                 &node)));
        }
        else
        {
          ERROR("void CScalar::parse(xml::CXMLNode & node)",
                << "The transformation " << nodeElementName << " has not been supported yet.");
        }
      } while (node.goToNextElement()) ;
      node.goToParentElement();
    }
  }

   //////////////////////////////////////////////////////////////////////////////////////
   //  this part is related to distribution, element definition, views and connectors  //
   //////////////////////////////////////////////////////////////////////////////////////

   void CScalar::initializeLocalElement(void)
   {
      // after checkAttribute index of size n
      int rank = CContext::getCurrent()->getIntraCommRank() ;
      
      CArray<size_t,1> ind(1) ;
      ind(0)=0 ;
      localElement_ = new CLocalElement(rank, 1, ind) ;
   }

   void CScalar::addFullView(void)
   {
      CArray<int,1> index(1) ;
      for(int i=0; i<1 ; i++) index(0)=0 ;
      localElement_ -> addView(CElementView::FULL, index) ;
   }

   void CScalar::addWorkflowView(void)
   {
      CArray<int,1> index ;
      if (mask) 
      {
        index.resize(1) ;
        index(0)=0 ;
      }
      else index.resize(0) ;
      localElement_ -> addView(CElementView::WORKFLOW, index) ;
   }

   void CScalar::addModelView(void)
   {
     CArray<int,1> index(1) ;
     for(int i=0; i<1 ; i++) index(0)=0 ;
     localElement_->addView(CElementView::MODEL, index) ;
   }

   void CScalar::computeModelToWorkflowConnector(void)
   { 
     CLocalView* srcView=getLocalView(CElementView::MODEL) ;
     CLocalView* dstView=getLocalView(CElementView::WORKFLOW) ;
     modelToWorkflowConnector_ = new CLocalConnector(srcView, dstView); 
     modelToWorkflowConnector_->computeConnector() ;
   }


  void CScalar::computeRemoteElement(CContextClient* client, EDistributionType type)
  {
    CContext* context = CContext::getCurrent();
    map<int, CArray<size_t,1>> globalIndex ;

    int nbServer = client->serverSize;
    size_t nglo=1 ;
    CArray<size_t,1> indGlo(nglo) ;
    for(size_t i=0;i<nglo;i++) indGlo(i) = i ;
    for (auto& rankServer : client->getRanksServerLeader()) globalIndex[rankServer].reference(indGlo.copy()) ; 

    remoteElement_[client] = new CDistributedElement(nglo, globalIndex) ;
    remoteElement_[client]->addFullView() ;
  }
 
  void CScalar::distributeToServer(CContextClient* client, std::map<int, CArray<size_t,1>>& globalIndex, 
                                   CScattererConnector* &scattererConnector, const string& scalarId)
  {
    string serverScalarId = scalarId.empty() ? this->getId() : scalarId ;
    CContext* context = CContext::getCurrent();

    this->sendAllAttributesToServer(client, serverScalarId)  ;

    CDistributedElement scatteredElement(1,globalIndex) ;
    scatteredElement.addFullView() ;
    scattererConnector = new CScattererConnector(localElement_->getView(CElementView::FULL), scatteredElement.getView(CElementView::FULL), 
                                                 context->getIntraComm(), client->getRemoteSize()) ;
    scattererConnector->computeConnector() ;
    
    // phase 0
    // send remote element to construct the full view on server, ie without hole 
    CEventClient event0(getType(), EVENT_ID_SCALAR_DISTRIBUTION);
    CMessage message0 ;
    message0<<serverScalarId<<0 ; 
    remoteElement_[client]->sendToServer(client,event0,message0) ; 
    
    // phase 1
    // send the full view of element to construct the connector which connect distributed data coming from client to the full local view
    CEventClient event1(getType(), EVENT_ID_SCALAR_DISTRIBUTION);
    CMessage message1 ;
    message1<<serverScalarId<<1<<localElement_->getView(CElementView::FULL)->getGlobalSize() ; 
    scattererConnector->transfer(localElement_->getView(CElementView::FULL)->getGlobalIndex(),client,event1,message1) ;

    sendDistributedAttributes(client, *scattererConnector, scalarId) ;
  
    // phase 2 send the mask : data index + mask2D
    CArray<bool,1> maskIn(localElement_->getView(CElementView::WORKFLOW)->getSize());
    CArray<bool,1> maskOut ;
    CLocalConnector workflowToFull(localElement_->getView(CElementView::WORKFLOW), localElement_->getView(CElementView::FULL)) ;
    workflowToFull.computeConnector() ;
    maskIn=true ;
    workflowToFull.transfer(maskIn,maskOut,false) ;

    // phase 3 : prepare grid scatterer connector to send data from client to server
    map<int,CArray<size_t,1>> workflowGlobalIndex ;
    map<int,CArray<bool,1>> maskOut2 ; 
    scattererConnector->transfer(maskOut, maskOut2) ;
    scatteredElement.addView(CElementView::WORKFLOW, maskOut2) ;
    scatteredElement.getView(CElementView::WORKFLOW)->getGlobalIndexView(workflowGlobalIndex) ;
    // create new workflow view for scattered element
    CDistributedElement clientToServerElement(scatteredElement.getGlobalSize(), workflowGlobalIndex) ;
    clientToServerElement.addFullView() ;
    CEventClient event2(getType(), EVENT_ID_SCALAR_DISTRIBUTION);
    CMessage message2 ;
    message2<<serverScalarId<<2 ; 
    clientToServerElement.sendToServer(client, event2, message2) ; 
    clientToServerConnector_[client] = new CScattererConnector(localElement_->getView(CElementView::WORKFLOW), clientToServerElement.getView(CElementView::FULL),
                                                               context->getIntraComm(), client->getRemoteSize()) ;
    clientToServerConnector_[client]->computeConnector() ;

    clientFromServerConnector_[client] = new CGathererConnector(clientToServerElement.getView(CElementView::FULL), localElement_->getView(CElementView::WORKFLOW));
    clientFromServerConnector_[client]->computeConnector() ;

  }
  
  void CScalar::recvScalarDistribution(CEventServer& event)
  TRY
  {
    string scalarId;
    int phasis ;
    for (auto& subEvent : event.subEvents) (*subEvent.buffer) >> scalarId >> phasis ;
    get(scalarId)->receivedScalarDistribution(event, phasis);
  }
  CATCH
  
  void CScalar::receivedScalarDistribution(CEventServer& event, int phasis)
  TRY
  {
    CContext* context = CContext::getCurrent();
    if (phasis==0) // receive the remote element to construct the full view
    {
      localElement_ = new  CLocalElement(context->getIntraCommRank(),event) ;
      localElement_->addFullView() ;
      // construct the local dimension and indexes
      auto& globalIndex=localElement_->getGlobalIndex() ;
      int nk=globalIndex.numElements() ;
      // no distribution for scalar => nk ==1 or maybe 0 ?
    }
    else if (phasis==1) // receive the sent view from client to construct the full distributed full view on server
    {
      CContext* context = CContext::getCurrent();
      CDistributedElement* elementFrom = new  CDistributedElement(event) ;
      elementFrom->addFullView() ;
      gathererConnector_ = new CGathererConnector(elementFrom->getView(CElementView::FULL), localElement_->getView(CElementView::FULL)) ;
      gathererConnector_->computeConnector() ; 
    }
    else if (phasis==2)
    {
//      delete gathererConnector_ ;
      elementFrom_ = new  CDistributedElement(event) ;
      elementFrom_->addFullView() ;
//      gathererConnector_ =  new CGathererConnector(elementFrom_->getView(CElementView::FULL), localElement_->getView(CElementView::FULL)) ;
//      gathererConnector_ -> computeConnector() ;
    }
  }
  CATCH

  void CScalar::setServerMask(CArray<bool,1>& serverMask, CContextClient* client)
  TRY
  {
    CContext* context = CContext::getCurrent();
    localElement_->addView(CElementView::WORKFLOW, serverMask) ;
    mask = serverMask(0) ;
 
    serverFromClientConnector_ = new CGathererConnector(elementFrom_->getView(CElementView::FULL), localElement_->getView(CElementView::WORKFLOW)) ;
    serverFromClientConnector_->computeConnector() ;
      
    serverToClientConnector_ = new CScattererConnector(localElement_->getView(CElementView::WORKFLOW), elementFrom_->getView(CElementView::FULL),
                                                         context->getIntraComm(), client->getRemoteSize()) ;
    serverToClientConnector_->computeConnector() ;
  }
  CATCH_DUMP_ATTR

  void CScalar::sendDistributedAttributes(CContextClient* client, CScattererConnector& scattererConnector, const string& scalarId)
  {
    string serverScalarId = scalarId.empty() ? this->getId() : scalarId ;
    CContext* context = CContext::getCurrent();

    // nothing for now
  }

  void CScalar::recvDistributedAttributes(CEventServer& event)
  TRY
  {
    string scalarId;
    string type ;
    for (auto& subEvent : event.subEvents) (*subEvent.buffer) >> scalarId >> type ;
    get(scalarId)->recvDistributedAttributes(event, type);
  }
  CATCH

  void CScalar::recvDistributedAttributes(CEventServer& event, const string& type)
  TRY
  {
    // nothing for now
  }
  CATCH  

  bool CScalar::dispatchEvent(CEventServer& event)
  TRY
  {
     if (SuperClass::dispatchEvent(event)) return true;
     else
     {
       switch(event.type)
       {
          case EVENT_ID_SCALAR_DISTRIBUTION:
            recvScalarDistribution(event);
            return true;
            break;
          case EVENT_ID_SEND_DISTRIBUTED_ATTRIBUTE:
            recvDistributedAttributes(event);
            return true;
            break;
          default :
            ERROR("bool CScalar::dispatchEvent(CEventServer& event)",
                   << "Unknown Event");
          return false;
        }
     }
  }
  CATCH


  // Definition of some macros
  DEFINE_REF_FUNC(Scalar,scalar)

} // namespace xios
