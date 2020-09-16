#include "scalar.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "object_factory.hpp"
#include "xios_spl.hpp"
#include "type.hpp"
#include "context.hpp"

namespace xios {

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
   }

   StdString CScalar::GetName(void)   { return (StdString("scalar")); }
   StdString CScalar::GetDefName(void){ return (CScalar::GetName()); }
   ENodeType CScalar::GetType(void)   { return (eScalar); }

   CScalar* CScalar::createScalar()
   {
     CScalar* scalar = CScalarGroup::get("scalar_definition")->createChild();
     return scalar;
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
   }

  void CScalar::checkAttributesOnClient()
  {

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
  void CScalar::solveInheritanceTransformation()
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
 
  void CScalar::sendScalarToFileServer(CContextClient* client)
  {
    if (sendScalarToFileServer_done_.count(client)!=0) return ;
    else sendScalarToFileServer_done_.insert(client) ;
    StdString scalarDefRoot("scalar_definition");
    CScalarGroup* scalarPtr = CScalarGroup::get(scalarDefRoot);
    this->sendAllAttributesToServer(client);
  }

  void CScalar::sendScalarToCouplerOut(CContextClient* client, const string& fieldId, int posInGrid)
  {
    if (sendScalarToCouplerOut_done_.count(client)!=0) return ;
    else sendScalarToCouplerOut_done_.insert(client) ;

    string scalarId="_scalar["+std::to_string(posInGrid)+"]_of_"+fieldId ;
   
    if (!scalar_ref.isEmpty())
    {
      auto scalar_ref_tmp=scalar_ref.getValue() ;
      scalar_ref.reset() ; // remove the reference, find an other way to do that more cleanly
      this->sendAllAttributesToServer(client, scalarId)  ;
      scalar_ref = scalar_ref_tmp ;
    }
    else this->sendAllAttributesToServer(client, scalarId)  ;


    this->sendAllAttributesToServer(client, scalarId);
  }  

  void CScalar::makeAliasForCoupling(const string& fieldId, int posInGrid)
  {
    const string scalarId = "_scalar["+std::to_string(posInGrid)+"]_of_"+fieldId ;
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
     CArray<int,1> index(1) ;
     for(int i=0; i<1 ; i++) index(0)=0 ;
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
    CArray<size_t,1> indGlo ;
    for(size_t i=0;i<nglo;i++) indGlo(i) = i ;
    for (auto& rankServer : client->getRanksServerLeader()) globalIndex[rankServer] = indGlo ; 

    remoteElement_[client] = new CDistributedElement(nglo, globalIndex) ;
    remoteElement_[client]->addFullView() ;
  }
 
  void CScalar::distributeToServer(CContextClient* client, std::map<int, CArray<size_t,1>>& globalIndex)
  {
    CContext* context = CContext::getCurrent();
    CDistributedElement scatteredElement(1,globalIndex) ;
    clientToServerConnector_[client] = new CScattererConnector(localElement_->getView(CElementView::FULL), scatteredElement.getView(CElementView::FULL), 
                                                               context->getIntraComm()) ;
    clientToServerConnector_[client] ->computeConnector() ;

// need to be completed    

  }


  // Definition of some macros
  DEFINE_REF_FUNC(Scalar,scalar)

} // namespace xios
