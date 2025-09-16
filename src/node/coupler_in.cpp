#include "coupler_in.hpp"
#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "object_factory.hpp"
#include "context.hpp"
#include "xios_spl.hpp"

namespace xios
{

  CCouplerIn::CCouplerIn(CContext* context) : CObjectTemplate<CCouplerIn>(context), CCouplerInAttributes(),
                                 virtualFieldGroup(), enabledFields() 
  {
     setVirtualFieldGroup(CFieldGroup::create(context_, getId() + "_virtual_field_group"));
  }

   CCouplerIn::CCouplerIn(CContext* context, const StdString & id) : CObjectTemplate<CCouplerIn>(context, id), CCouplerInAttributes(),
                                                  virtualFieldGroup(), enabledFields()
    {
      setVirtualFieldGroup(CFieldGroup::create(context_, getId() + "_virtual_field_group"));
    }

   CCouplerIn::~CCouplerIn(void)
   { /* Ne rien faire de plus */ }

   ///---------------------------------------------------------------
  //! Get name of coupler_in
   StdString CCouplerIn::GetName(void)   { return (StdString("coupler_in")); }
   StdString CCouplerIn::GetDefName(void){ return (CCouplerIn::GetName()); }
   ENodeType CCouplerIn::GetType(void)   { return (eCouplerIn); }

   std::vector<CField*> CCouplerIn::getEnabledFields(void)
   TRY
   {
      if (enabledFields.empty())
      {
        this->enabledFields = this->getAllFields();
        std::vector<CField*> newEnabledFields;
        bool enabled ;
        for ( auto it = this->enabledFields.begin(); it != this->enabledFields.end(); it++ )
        {
           if ((*it)->enabled.isEmpty()) enabled=true ; 
           else enabled=(*it)->enabled ;
           if (enabled) newEnabledFields.push_back(*it);
        }
        enabledFields = newEnabledFields;
      }
      return (this->enabledFields);
   }
   CATCH_DUMP_ATTR

   const string& CCouplerIn::getCouplingContextId(void)
   {
     if (couplingContextId_.empty())
     {
       vector<string> vectStr=splitRegex(context,"::") ;
       string poolId=vectStr[0] ;
       string serviceId=poolId ;
       string contextId=vectStr[1] ;
       int contextLeader ;
       int type = CServicesManager::CLIENT ;
       couplingContextId_=CXios::getContextsManager()->getServerContextName(poolId, serviceId, 0, type, contextId) ;
     }
     return couplingContextId_ ;
   }

   void CCouplerIn::createInterCommunicator(void)
   TRY
   {
     if (context.isEmpty())
     {
        ERROR("void CCouplerOut::createInterCommunicator(void)",
               "The attribute <context> must be defined to specify the target coupling context");
     }
     CContext* contextPtr = context_;
     contextPtr->addCouplingChanel(getCouplingContextId(), false) ;
   }
   CATCH_DUMP_ATTR

  
   /*!
   \brief Parse xml file and write information into coupler_in object
   \param [in] node xmld node corresponding in xml coupler_in
   */
   void CCouplerIn::parse(xml::CXMLNode & node)
   TRY
   {
      SuperClass::parse(node);

      if (node.goToChildElement())
      {
        do
        {
           if (node.getElementName()=="field" || node.getElementName()=="field_group") this->getVirtualFieldGroup()->parseChild(node);
        } while (node.goToNextElement());
        node.goToParentElement();
      }
   }
   CATCH_DUMP_ATTR

   /*!
   \brief Get virtual field group
      In each CCouplerIn, there always exists a field group which is the ancestor of all
   fields in the CCouplerIn. This is considered be virtual because it is created automatically during
   CCouplerIn initialization and it normally doesn't appear on xml file
   \return Pointer to field group
   */
   CFieldGroup* CCouplerIn::getVirtualFieldGroup(void) const
   TRY
   {
      return (this->virtualFieldGroup);
   }
   CATCH

   void CCouplerIn::solveDescInheritance(bool apply, const CAttributeMap * const parent)
   TRY
   {
      SuperClassAttribute::setAttributes(parent,apply);
      this->getVirtualFieldGroup()->solveDescInheritance(apply, NULL);
   }
   CATCH_DUMP_ATTR

   void CCouplerIn::solveFieldRefInheritance(bool apply)
   TRY
   {
      // Rsolution des hritages par rfrence de chacun des champs contenus dans le fichier.
      std::vector<CField*> allF = this->getAllFields();
      for (unsigned int i = 0; i < allF.size(); i++)
         allF[i]->solveRefInheritance(apply);
   }
   CATCH_DUMP_ATTR
   /*!
   \brief Get virtual variable group
      In each CCouplerIn, there always exists a variable group which is the ancestor of all
   variable in the CCouplerIn. This is considered be virtual because it is created automatically during
   CCouplerIn initialization and it normally doesn't appear on xml file
   \return Pointer to variable group
   */

   //! Get all fields of a file
   std::vector<CField*> CCouplerIn::getAllFields(void) const
   TRY
   {
      return (this->virtualFieldGroup->getAllChildren());
   }
   CATCH

  /*!
   * assign the context associated to the coupler to the enabled fields
   */
   void  CCouplerIn::assignContext(void)
   {
     client_ = context_->getCouplerInClient(getCouplingContextId());
     for (auto& field : getEnabledFields()) field->setContextClient(client_) ; 
   }

   //----------------------------------------------------------------
   //! Change virtual field group to a new one
   void CCouplerIn::setVirtualFieldGroup(CFieldGroup* newVirtualFieldGroup)
   TRY
   {
      this->virtualFieldGroup = newVirtualFieldGroup;
   }
   CATCH_DUMP_ATTR
   
   ///--------------------------------------------------------------
   /*!
   */
   StdString CCouplerIn::dumpClassAttributes(void)
   {
     StdString str;
     CContext* context = context_;
     str.append("context=\"");
     str.append(context->getId());
     str.append("\"");
     str.append(" enabled fields=\"");
     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {
       str.append(this->enabledFields[i]->getId());
       str.append(" ");
     }
     str.append("\"");
     return str;
   }
}