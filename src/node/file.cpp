#include "file.hpp"

#include "attribute_template_impl.hpp"
#include "object_template_impl.hpp"
#include "group_template_impl.hpp"

#include "object_factory.hpp"
#include "object_factory_impl.hpp"
#include "data_output.hpp"
#include "context.hpp"
#include "context_server.hpp"
#include "nc4_data_output.hpp"


namespace xmlioserver {
namespace tree {
   
   /// ////////////////////// Définitions ////////////////////// ///

   CFile::CFile(void)
      : CObjectTemplate<CFile>(), CFileAttributes()
      , vFieldGroup(), data_out(), enabledFields()
   { setVirtualFieldGroup() ;}

   CFile::CFile(const StdString & id)
      : CObjectTemplate<CFile>(id), CFileAttributes()
      , vFieldGroup(), data_out(), enabledFields()
   { setVirtualFieldGroup() ;}

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

      std::vector<boost::shared_ptr<CField> > newEnabledFields;
      
      for ( it = this->enabledFields.begin() ; it != this->enabledFields.end(); it++ )
      {
         if (!(*it)->enabled.isEmpty()) // Si l'attribut 'enabled' est défini ...
         {
            if (! (*it)->enabled.getValue()) continue;
//            { it--; this->enabledFields.erase(it+1); continue; }
         }
         else // Si l'attribut 'enabled' n'est pas défini ...
         {
            if (!default_enabled) continue ;
//            { it--; this->enabledFields.erase(it+1); continue; }
         }

         if (!(*it)->level.isEmpty()) // Si l'attribut 'level' est défini ...
         {
            if ((*it)->level.getValue() > _outputlevel) continue ;
//            { it--; this->enabledFields.erase(it+1); continue; }
         }
         else // Si l'attribut 'level' n'est pas défini ...
         {
            if (default_level > _outputlevel) continue ;
//            { it--; this->enabledFields.erase(it+1); continue; }
         }
 
//         CField* field_tmp=(*it).get() ;
//         shared_ptr<CField> sptfield=*it ;
//         field_tmp->refObject.push_back(sptfield) ;
         newEnabledFields.push_back(*it) ;
         // Le champ est finalement actif, on y ajoute sa propre reference.
         (*it)->refObject.push_back(*it);
         // Le champ est finalement actif, on y ajoute la référence au champ de base.
         (*it)->setRelFile(CObjectFactory::GetObject(this));
         (*it)->baseRefObject->refObject.push_back(*it);
         // A faire, ajouter les references intermediaires...
      }
      enabledFields=newEnabledFields ;

      return (this->enabledFields);
   }

   //----------------------------------------------------------------

   void CFile::setVirtualFieldGroup(boost::shared_ptr<CFieldGroup> newVFieldGroup)
   { 
      this->vFieldGroup = newVFieldGroup; 
   }

   //----------------------------------------------------------------

   void CFile::setVirtualFieldGroup(void)
   {
      this->setVirtualFieldGroup
         (CObjectFactory::CreateObject<CFieldGroup>());
   }

   //----------------------------------------------------------------

   void CFile::createHeader(void)
   {
    
      std::vector<boost::shared_ptr<CField> >::iterator it, end = this->enabledFields.end();

      AllDomainEmpty=true ;
      for (it = this->enabledFields.begin() ;it != end; it++)
      {
         boost::shared_ptr<CField> field = *it;
         AllDomainEmpty&=field->grid->domain->isEmpty() ;
      }
      
      if (!AllDomainEmpty ||  type.getValue()=="one_file")
      {
         StdString filename = (!name.isEmpty()) ?   name.getValue() : getId();
         StdOStringStream oss;
//         if (! output_dir.isEmpty()) oss << output_dir.getValue();
         oss << filename;
         if (!name_suffix.isEmpty()) oss << name_suffix.getValue();

         bool multifile=true ;
         if (!type.isEmpty())
         {
           if (type.getValue()=="one_file") multifile=false ;
           else if (type.getValue()=="multi_file") multifile=true ;
           else ERROR("void Context::createDataOutput(void)",
                      "incorrect file <type> attribut : must be <multi_file> or <one_file>, "
                      <<"having : <"<<type.getValue()<<">") ;
         } 
         
         shared_ptr<CContext> context=CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId()) ;
         CContextServer* server=context->server ;

         if (multifile) 
         {
            if (server->intraCommSize > 1) oss << "_" << server->intraCommRank;
         }
         oss << ".nc";

         data_out=shared_ptr<io::CDataOutput>(new io::CNc4DataOutput(oss.str(), false,server->intraComm,multifile));

         data_out->writeFile(CObjectFactory::GetObject<CFile>(this));
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
   }

   void CFile::close(void)
   {
     if (!AllDomainEmpty ||  type.getValue()=="one_file")
       this->data_out->closeFile();
   }
   //----------------------------------------------------------------

   void CFile::parse(xml::CXMLNode & node)
   {
      SuperClass::parse(node);
      if (node.goToChildElement() & this->hasId())
      { // Si la définition du fichier intégre des champs et si le fichier est identifié.
         node.goToParentElement();
//         this->setVirtualFieldGroup(this->getId());
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
      
//      this->setVirtualFieldGroup(this->getId());
      if (hasVFG)this->getVirtualFieldGroup()->fromBinary(is);
      
   }
   
   shared_ptr<CField> CFile::addField(const string& id)
   {
     return vFieldGroup->createChild(id) ;
   }

   shared_ptr<CFieldGroup> CFile::addFieldGroup(const string& id)
   {
     return vFieldGroup->createChildGroup(id) ;
   }
   
  
   void CFile::sendAddField(const string& id)
   {
    shared_ptr<CContext> context=CContext::current() ;
    
    if (! context->hasServer )
    {
       CContextClient* client=context->client ;

       CEventClient event(this->getType(),EVENT_ID_ADD_FIELD) ;   
       if (client->isServerLeader())
       {
         CMessage msg ;
         msg<<this->getId() ;
         msg<<id ;
         event.push(client->getServerLeader(),1,msg) ;
         client->sendEvent(event) ;
       }
       else client->sendEvent(event) ;
    }
      
   }
   
   void CFile::sendAddFieldGroup(const string& id)
   {
    shared_ptr<CContext> context=CContext::current() ;
    if (! context->hasServer )
    {
       CContextClient* client=context->client ;

       CEventClient event(this->getType(),EVENT_ID_ADD_FIELD_GROUP) ;   
       if (client->isServerLeader())
       {
         CMessage msg ;
         msg<<this->getId() ;
         msg<<id ;
         event.push(client->getServerLeader(),1,msg) ;
         client->sendEvent(event) ;
       }
       else client->sendEvent(event) ;
    }
      
   }
   
   void CFile::recvAddField(CEventServer& event)
   {
      
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id ;
      get(id)->recvAddField(*buffer) ;
   }
   
   
   void CFile::recvAddField(CBufferIn& buffer)
   {
      string id ;
      buffer>>id ;
      addField(id) ;
   }

   void CFile::recvAddFieldGroup(CEventServer& event)
   {
      
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id ;
      get(id)->recvAddFieldGroup(*buffer) ;
   }
   
   
   void CFile::recvAddFieldGroup(CBufferIn& buffer)
   {
      string id ;
      buffer>>id ;
      addFieldGroup(id) ;
   }
   

   bool CFile::dispatchEvent(CEventServer& event)
   {
      if (SuperClass::dispatchEvent(event)) return true ;
      else
      {
        switch(event.type)
        {
           case EVENT_ID_ADD_FIELD :
             recvAddField(event) ;
             return true ;
             break ;
         
           case EVENT_ID_ADD_FIELD_GROUP :
             recvAddFieldGroup(event) ;
             return true ;
             break ;       
         
           default :
              ERROR("bool CFile::dispatchEvent(CEventServer& event)", <<"Unknown Event") ;
           return false ;
        }
      }
   }
   
   
   
   
   ///---------------------------------------------------------------

} // namespace tree
} // namespace xmlioserver
