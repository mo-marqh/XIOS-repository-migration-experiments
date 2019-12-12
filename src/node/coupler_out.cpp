#include "coupler_out.hpp"
#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "object_factory.hpp"
#include "context.hpp"
#include "xios_spl.hpp"
#include "string_tools.hpp"
#include "contexts_manager.hpp"
#include "services_manager.hpp"


namespace xios
{

  CCouplerOut::CCouplerOut(void) : CObjectTemplate<CCouplerOut>(), CCouplerOutAttributes(),
                                 virtualFieldGroup(), enabledFields() 
  {
     setVirtualFieldGroup(CFieldGroup::create(getId() + "_virtual_field_group"));
  }

  CCouplerOut::CCouplerOut(const StdString & id) : CObjectTemplate<CCouplerOut>(id), CCouplerOutAttributes(),
                                                    virtualFieldGroup(), enabledFields()
  {
    setVirtualFieldGroup(CFieldGroup::create(getId() + "_virtual_field_group"));
  }

  CCouplerOut::~CCouplerOut(void)
  { /* Ne rien faire de plus */ }

   ///---------------------------------------------------------------
  //! Get name of file
  StdString CCouplerOut::GetName(void)   { return (StdString("coupler_out")); }
  StdString CCouplerOut::GetDefName(void){ return (CCouplerOut::GetName()); }
  ENodeType CCouplerOut::GetType(void)   { return (eCouplerIn); }

   /*!
   \brief Parse xml file and write information into coupler_in object
   \param [in] node xmld node corresponding in xml coupler_in
   */
   void CCouplerOut::parse(xml::CXMLNode & node)
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

   std::vector<CField*> CCouplerOut::getEnabledFields(void)
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

      /*!
   \brief Get virtual field group
      In each CCouplerIn, there always exists a field group which is the ancestor of all
   fields in the CCouplerIn. This is considered be virtual because it is created automatically during
   CCouplerIn initialization and it normally doesn't appear on xml file
   \return Pointer to field group
   */
   CFieldGroup* CCouplerOut::getVirtualFieldGroup(void) const
   TRY
   {
      return (this->virtualFieldGroup);
   }
   CATCH


   /*!
   \brief Get virtual variable group
      In each CCouplerIn, there always exists a variable group which is the ancestor of all
   variable in the CCouplerIn. This is considered be virtual because it is created automatically during
   CCouplerIn initialization and it normally doesn't appear on xml file
   \return Pointer to variable group
   */

   //! Get all fields of a file
   std::vector<CField*> CCouplerOut::getAllFields(void) const
   TRY
   {
      return (this->virtualFieldGroup->getAllChildren());
   }
   CATCH

   void CCouplerOut::solveDescInheritance(bool apply, const CAttributeMap * const parent)
   TRY
   {
      SuperClassAttribute::setAttributes(parent,apply);
      this->getVirtualFieldGroup()->solveDescInheritance(apply, NULL);
   }
   CATCH_DUMP_ATTR

   void CCouplerOut::solveFieldRefInheritance(bool apply)
   TRY
   {
      // Rsolution des hritages par rfrence de chacun des champs contenus dans le fichier.
      std::vector<CField*> allF = this->getAllFields();
      for (unsigned int i = 0; i < allF.size(); i++)
         allF[i]->solveRefInheritance(apply);
   }
   CATCH_DUMP_ATTR

   void CCouplerOut::createInterCommunicator(void)
   TRY
   {
     if (context.isEmpty())
     {
        ERROR("void CCouplerOut::createInterCommunicator(void)",
               "The attribute <context> must be defined to specify the target coupling context");
     }
     CContext* contextPtr = CContext::getCurrent();
     contextPtr->addCouplingChanel(context, true) ;
   }
   CATCH_DUMP_ATTR

  
   void CCouplerOut::solveOnlyRefOfEnabledFields(void)
   TRY
   {
     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFields[i]->solveOnlyReferenceEnabledField();
     }
   }
   CATCH_DUMP_ATTR

   void CCouplerOut::generateNewTransformationGridDest(void)
   TRY
   {
     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFields[i]->generateNewTransformationGridDest();
     }
   }
   CATCH_DUMP_ATTR

   void CCouplerOut::solveAllRefOfEnabledFieldsAndTransform(void)
   TRY
   {
     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {       
      this->enabledFields[i]->solveAllEnabledFieldsAndTransform();
     }
   }
   CATCH_DUMP_ATTR

   /*!
    * Constructs the filter graph for each active field.
    *
    * \param gc the garbage collector to use when building the filter graph
    */
   void CCouplerOut::buildFilterGraphOfEnabledFields(CGarbageCollector& gc)
   TRY
   {
     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFields[i]->buildFilterGraph(gc, true);
     }
   }
   CATCH_DUMP_ATTR

   void CCouplerOut::checkGridOfEnabledFields(void)
   TRY
   { 
     int size = this->enabledFields.size();
     for (int i = 0; i < size; ++i)
     {
       this->enabledFields[i]->checkGridOfEnabledFields();
     }
   }
   CATCH_DUMP_ATTR

   //----------------------------------------------------------------
   //! Change virtual field group to a new one
   void CCouplerOut::setVirtualFieldGroup(CFieldGroup* newVirtualFieldGroup)
   TRY
   {
      this->virtualFieldGroup = newVirtualFieldGroup;
   }
   CATCH_DUMP_ATTR

   ///--------------------------------------------------------------
   /*!
   */
   StdString CCouplerOut::dumpClassAttributes(void)
   {
     StdString str;
     CContext* context = CContext::getCurrent();
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