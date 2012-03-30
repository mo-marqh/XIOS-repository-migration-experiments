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
#include "calendar_util.hpp"
#include "date.hpp"


namespace xios {
   
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

   boost::shared_ptr<CDataOutput> CFile::getDataOutput(void) const
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
   bool CFile::isSyncTime(void)
   {
     shared_ptr<CContext> context=CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId()) ;
     CDate& currentDate=context->calendar->getCurrentDate() ;
     if (! sync_freq.isEmpty())
     {
       if (*lastSync+syncFreq < currentDate)
       {
         *lastSync=currentDate ;
         return true ;
        }
      }
      return false ;
    }
    
   void CFile::initFile(void)
   {
      shared_ptr<CContext> context=CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId()) ;
      CDate& currentDate=context->calendar->getCurrentDate() ;
      
      if (! sync_freq.isEmpty()) syncFreq = CDuration::FromString(sync_freq.getValue());
      if (! split_freq.isEmpty()) splitFreq = CDuration::FromString(split_freq.getValue());
      if (! output_freq.isEmpty()) outputFreq = CDuration::FromString(output_freq.getValue());
      lastSync=new CDate(currentDate) ;
      lastSplit=new CDate(currentDate) ;
      isOpen=false ;
    }
    
    void CFile::checkFile(void)
    {
      if (!isOpen) createHeader() ;
      checkSync() ;
      checkSplit() ;
    }
      
     
   bool CFile::checkSync(void)
   {
     shared_ptr<CContext> context=CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId()) ;
     CDate& currentDate=context->calendar->getCurrentDate() ;
     if (! sync_freq.isEmpty())
     {
       if (*lastSync+syncFreq < currentDate)
       {
         *lastSync=currentDate ;
         data_out->syncFile() ;
         return true ;
        }
      }
      return false ;
    }
    
    
    bool CFile::checkSplit(void)
    {
      shared_ptr<CContext> context=CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId()) ;
      CDate& currentDate=context->calendar->getCurrentDate() ;
      if (! split_freq.isEmpty())
      {
        if (*lastSplit+splitFreq < currentDate)
        {
          *lastSplit=currentDate-outputFreq ;
        
          std::vector<boost::shared_ptr<CField> >::iterator it, end = this->enabledFields.end();
          for (it = this->enabledFields.begin() ;it != end; it++)  (*it)->resetNStep() ;
          createHeader() ;
          return true ;
        }
      }
      return false ;
    }
    
   void CFile::createHeader(void)
   {
      shared_ptr<CContext> context=CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId()) ;
      CDate& currentDate=context->calendar->getCurrentDate() ;
      
      std::vector<boost::shared_ptr<CField> >::iterator it, end = this->enabledFields.end();

      AllDomainEmpty=true ;
      set<CDomain*> setDomain ;
      for (it = this->enabledFields.begin() ;it != end; it++)
      {
         boost::shared_ptr<CField> field = *it;
         AllDomainEmpty&=field->grid->domain->isEmpty() ;
         setDomain.insert(field->grid->domain.get()) ;
      }
      nbDomain=setDomain.size() ;

      if (!AllDomainEmpty ||  type.getValue()=="one_file")
      {
         StdString filename = (!name.isEmpty()) ?   name.getValue() : getId();
         StdOStringStream oss;
//         if (! output_dir.isEmpty()) oss << output_dir.getValue();
         oss << filename;
         if (!name_suffix.isEmpty()) oss << name_suffix.getValue();
//         if (!split_freq.isEmpty()) oss<<"-["<<currentDate.toString()<<"]" ;
         if (!split_freq.isEmpty()) oss<<"_"<<lastSplit->getStryyyymmdd()<<"-"<< (*lastSplit+(splitFreq-1*Second)).getStryyyymmdd();
         bool multifile=true ;
         if (!type.isEmpty())
         {
           if (type.getValue()=="one_file") multifile=false ;
           else if (type.getValue()=="multi_file") multifile=true ;
           else ERROR("void Context::createDataOutput(void)",
                      "incorrect file <type> attribut : must be <multi_file> or <one_file>, "
                      <<"having : <"<<type.getValue()<<">") ;
         } 
         
         CContextServer* server=context->server ;

         if (multifile) 
         {
            if (server->intraCommSize > 1) oss << "_" << server->intraCommRank;
         }
         oss << ".nc";

         if (isOpen) data_out->closeFile() ;
         bool isCollective=true ;
         if (!par_access.isEmpty())
         {
           if (par_access.getValue()=="independent") isCollective=false ;
           else if (par_access.getValue()=="collective") isCollective=true ;
           else 
           {
             ERROR("void Context::createDataOutput(void)",
                        "incorrect file <par_access> attribut : must be <collective> or <indepedent>, "
                        <<"having : <"<<type.getValue()<<">") ;
           }
         }
         data_out=shared_ptr<CDataOutput>(new CNc4DataOutput(oss.str(), false,server->intraComm,multifile, isCollective));
         isOpen=true ;

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
     delete lastSync ;
     delete lastSplit ;
     if (!AllDomainEmpty ||  type.getValue()=="one_file")
       if (isOpen) 
       {
         this->data_out->closeFile();
       }
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

} // namespace xios
