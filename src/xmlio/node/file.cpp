#include "file.hpp"

#include "attribute_template_impl.hpp"
#include "object_template_impl.hpp"
#include "group_template_impl.hpp"

#include "object_factory.hpp"
#include "object_factory_impl.hpp"

namespace xmlioserver {
namespace tree {
   
   /// ////////////////////// Définitions ////////////////////// ///

   CFile::CFile(void)
      : CObjectTemplate<CFile>(), CFileAttributes()
      , vFieldGroup(), data_out(), enabledFields()
   { /* Ne rien faire de plus */ }

   CFile::CFile(const StdString & id)
      : CObjectTemplate<CFile>(id), CFileAttributes()
      , vFieldGroup(), data_out(), enabledFields()
   { /* Ne rien faire de plus */ }

   CFile::~CFile(void)
   { /* Ne rien faire de plus */ }

   ///---------------------------------------------------------------

   StdString CFile::GetName(void)   { return (StdString("file")); }
   StdString CFile::GetDefName(void){ return (CFile::GetName()); }
   ENodeType CFile::GetType(void)   { return (eFile); }

   //----------------------------------------------------------------

   boost::shared_ptr<io::CDataOutput> CFile::getDataOutput(void) const
   {
      return (data_out);
   }

   boost::shared_ptr<CFieldGroup> CFile::getVirtualFieldGroup(void) const
   {
      return (this->vFieldGroup);
   }

   std::vector<boost::shared_ptr<CField> > CFile::getAllFields(void) const
   {
      return (this->vFieldGroup->getAllChildren());
   }

   //----------------------------------------------------------------

   std::vector<boost::shared_ptr<CField> > CFile::getEnabledFields
      (int default_outputlevel, int default_level, bool default_enabled)
   {
      if (!this->enabledFields.empty())
         return (this->enabledFields);

      const int _outputlevel =
         (!output_level.isEmpty()) ? output_level.getValue() : default_outputlevel;
      std::vector<boost::shared_ptr<CField> >::iterator it;
      this->enabledFields = this->getAllFields();

      for ( it = this->enabledFields.begin() ; it != this->enabledFields.end(); it++ )
      {
         if (!(*it)->enabled.isEmpty()) // Si l'attribut 'enabled' est défini ...
         {
            if (! (*it)->enabled.getValue())
            { it--; this->enabledFields.erase(it+1); continue; }
         }
         else // Si l'attribut 'enabled' n'est pas défini ...
         {
            if (!default_enabled)
            { it--; this->enabledFields.erase(it+1); continue; }
         }

         if (!(*it)->level.isEmpty()) // Si l'attribut 'level' est défini ...
         {
            if ((*it)->level.getValue() > _outputlevel)
            { it--; this->enabledFields.erase(it+1); continue; }
         }
         else // Si l'attribut 'level' n'est pas défini ...
         {
            if (default_level > _outputlevel)
            { it--; this->enabledFields.erase(it+1); continue; }
         }

         // Le champ est finalement actif, on ajoute la référence au champ de base.
         (*it)->setRelFile(CObjectFactory::GetObject(this));
         (*it)->baseRefObject->refObject.push_back(*it);
      }

      return (this->enabledFields);
   }

   //----------------------------------------------------------------

   void CFile::setVirtualFieldGroup(boost::shared_ptr<CFieldGroup> newVFieldGroup)
   { 
      this->vFieldGroup = newVFieldGroup; 
   }

   //----------------------------------------------------------------

   void CFile::setVirtualFieldGroup(const StdString & newVFieldGroupId)
   {
      this->setVirtualFieldGroup
         (CObjectFactory::CreateObject<CFieldGroup>(newVFieldGroupId));
   }

   //----------------------------------------------------------------

   void CFile::initializeDataOutput(boost::shared_ptr<io::CDataOutput> dout)
   {
      this->data_out = dout;
      this->data_out->writeFile(CObjectFactory::GetObject<CFile>(this));
      
      std::vector<boost::shared_ptr<CField> >::iterator it, end = this->enabledFields.end();

      for (it = this->enabledFields.begin() ;it != end; it++)
      {
         boost::shared_ptr<CField> field = *it;
         this->data_out->writeFieldGrid(field);
      }
         
      for (it = this->enabledFields.begin() ;it != end; it++)
      {
         boost::shared_ptr<CField> field = *it;
         this->data_out->writeField(field);
      }
         
      this->data_out->definition_end();
   }

   //----------------------------------------------------------------

   void CFile::parse(xml::CXMLNode & node)
   {
      SuperClass::parse(node);
      if (node.goToChildElement() & this->hasId())
      { // Si la définition du fichier intégre des champs et si le fichier est identifié.
         node.goToParentElement();
         this->setVirtualFieldGroup(this->getId());
         this->getVirtualFieldGroup()->parse(node, false);
      }
   }
   //----------------------------------------------------------------

   StdString CFile::toString(void) const
   {
      StdOStringStream oss;

      oss << "<" << CFile::GetName() << " ";
      if (this->hasId())
         oss << " id=\"" << this->getId() << "\" ";
      oss << SuperClassAttribute::toString() << ">" << std::endl;
      if (this->getVirtualFieldGroup().get() != NULL)
         oss << *this->getVirtualFieldGroup() << std::endl;
      oss << "</" << CFile::GetName() << " >";
      return (oss.str());
   }

   //----------------------------------------------------------------
   
   void CFile::solveDescInheritance(const CAttributeMap * const parent)
   {
      SuperClassAttribute::setAttributes(parent);
      this->getVirtualFieldGroup()->solveDescInheritance(NULL);
   }

   //----------------------------------------------------------------

   void CFile::solveFieldRefInheritance(void)
   {
      // Résolution des héritages par référence de chacun des champs contenus dans le fichier.
      std::vector<boost::shared_ptr<CField> > allF = this->getAllFields();
      for (unsigned int i = 0; i < allF.size(); i++)
         allF[i]->solveRefInheritance();
   }

   //----------------------------------------------------------------

   void CFile::solveEFGridRef(void)
   {
      for (unsigned int i = 0; i < this->enabledFields.size(); i++)
         this->enabledFields[i]->solveGridReference();
   }

   //----------------------------------------------------------------

   void CFile::solveEFOperation(void)
   {
      for (unsigned int i = 0; i < this->enabledFields.size(); i++)
         this->enabledFields[i]->solveOperation();
   }
   
   //---------------------------------------------------------------
   
   void CFile::toBinary  (StdOStream & os) const
   {
      ENodeType genum = CFileGroup::GetType();
      bool hasVFG = (this->getVirtualFieldGroup().get() != NULL);
      SuperClass::toBinary(os);
      
      os.write (reinterpret_cast<const char*>(&genum) , sizeof(ENodeType));
      os.write (reinterpret_cast<const char*>(&hasVFG) , sizeof(bool));
      
      if (hasVFG)this->getVirtualFieldGroup()->toBinary(os);
         
   }
   
   //----------------------------------------------------------------
   
   void CFile::fromBinary(StdIStream & is)
   {
      ENodeType renum = Unknown;
      bool hasVFG = false;
      SuperClass::fromBinary(is);
      
      is.read (reinterpret_cast<char*>(&renum), sizeof(ENodeType));
      is.read (reinterpret_cast<char*>(&hasVFG), sizeof(bool));
      
      if (renum != CFileGroup::GetType())
         ERROR("CFile::fromBinary(StdIStream & is)",
               << "[ renum = " << renum << "] Bad type !");
      
      this->setVirtualFieldGroup(this->getId());
      if (hasVFG)this->getVirtualFieldGroup()->fromBinary(is);
      
   }

   ///---------------------------------------------------------------

} // namespace tree
} // namespace xmlioserver
