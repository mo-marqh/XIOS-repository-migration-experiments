#include "tree_manager.hpp"

#include "attribute_template_impl.hpp"
#include "object_template_impl.hpp"
#include "group_template_impl.hpp"

namespace xmlioserver
{
   namespace tree
   {
      /// ////////////////////// Définitions ////////////////////// ///

      void CTreeManager::SetCurrentContextId(const StdString & id)
      {
         CObjectFactory::SetCurrentContextId(id);
         CGroupFactory::SetCurrentContextId(id);
      }

      boost::shared_ptr<CContext> CTreeManager::CreateContext(const StdString & id)
      {
         boost::shared_ptr<CContextGroup> group_context = CContext::GetContextGroup();
         CTreeManager::SetCurrentContextId(id);
         bool hasctxt = CObjectFactory::HasObject<CContext>(id);
         
         boost::shared_ptr<tree::CContext> context =
               CObjectFactory::CreateObject<tree::CContext>(id);
         if (!hasctxt) CGroupFactory::AddChild(group_context, context);

#define DECLARE_NODE(Name_, name_) \
   CObjectFactory::CreateObject<C##Name_##Definition>(C##Name_##Definition::GetDefName());
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"

         return (context);
   }

      //--------------------------------------------------------------
      void CTreeManager::PrintTreeToFile(const StdString & path)
      {
         StdOFStream ofs(path.c_str());
         CTreeManager::PrintTreeToStream(ofs);
         ofs.close();
      }

      void CTreeManager::PrintTreeToString(StdString & content)
      {
         StdOStringStream ostrstream;
         tree::CContext::ShowTree(ostrstream);
         content.assign(CIndentedXml::Indented(ostrstream.str()));
      }

      void CTreeManager::PrintTreeToStream(StdOStream & out)
      {
         StdOStringStream ostrstream;
         tree::CContext::ShowTree(ostrstream);
         out << CIndentedXml::Indented(ostrstream.str());
      }

      //--------------------------------------------------------------

      void CTreeManager::ParseFile(const StdString & filename)
      { 
         xml::CXMLParser::ParseFile(filename); 
      }

      void CTreeManager::ParseString(const StdString & xmlContent)
      { 
         xml::CXMLParser::ParseString(xmlContent); 
      }

      void CTreeManager::ParseStream(StdIStream & stream)
      { 
         xml::CXMLParser::ParseStream(stream); 
      }
      
      //--------------------------------------------------------------
      
      void CTreeManager::ToBinary(StdOStream & os)
      {
         StdString currentContextId =
            CObjectFactory::GetCurrentContextId();
         std::vector<boost::shared_ptr<CContext> > def_vector =
            CContext::GetContextGroup()->getChildList();
            
         const StdSize size = def_vector.size();         
         const ENodeType cenum = CContext::GetType();
         const ENodeType genum = CContextGroup::GetType();
         
         os.write (reinterpret_cast<const char*>(&genum) , sizeof(ENodeType)); 
         os.write (reinterpret_cast<const char*>(&size) , sizeof(StdSize));
         
         for (StdSize i = 0; i < size; i++)
         {
            boost::shared_ptr<CContext> context = def_vector[i];         
            CTreeManager::SetCurrentContextId(context->getId());                
            const bool hid = context->hasId(); // Toujours vrai
            
            os.write (reinterpret_cast<const char*>(&cenum), sizeof(ENodeType));      
            os.write (reinterpret_cast<const char*>(&hid), sizeof(bool));
            
            if (hid)
            {
               const StdString & id = context->getId();
               const StdSize size   = id.size();
                  
               os.write (reinterpret_cast<const char*>(&size), sizeof(StdSize));
               os.write (id.data(), size * sizeof(char));         
            } 
            
            context->toBinary(os);
         }
         CTreeManager::SetCurrentContextId(currentContextId);          
      }
      
      void CTreeManager::FromBinary(StdIStream & is)
      {
         StdSize ctxtnb = 0;
         ENodeType renum = Unknown;
         
         boost::shared_ptr<CContextGroup> group_context =
                           CContext::GetContextGroup();
                           
         is.read (reinterpret_cast<char*>(&renum), sizeof(StdSize));   
         is.read (reinterpret_cast<char*>(&ctxtnb), sizeof(ENodeType));

         if (renum != CContextGroup::GetType())
            ERROR("CTreeManager::FromBinary(StdIStream & is)",
                  << "[ renum = " << renum << "] Bad type !");

         for (StdSize i = 0; i < ctxtnb; i++)
         {
            bool hid = false;
            is.read (reinterpret_cast<char*>(&renum), sizeof(tree::ENodeType));
            is.read (reinterpret_cast<char*>(&hid), sizeof(bool));
            
            if (renum != CContext::GetType())
               ERROR("CTreeManager::FromBinary(StdIStream & is)",
                     << "[ renum = " << renum << "] Bad type !");
                           
            if (hid)
            {
               StdSize size  = 0;
               is.read (reinterpret_cast<char*>(&size), sizeof(StdSize));
               StdString id(size, ' ');
               is.read (const_cast<char *>(id.data()), size * sizeof(char));
               
               CTreeManager::SetCurrentContextId(id);               
               bool hasctxt = CObjectFactory::HasObject<CContext>(id);
               
               boost::shared_ptr<CContext> context =
                  CObjectFactory::CreateObject<CContext>(id);
                  
               if (!hasctxt)
                  CGroupFactory::AddChild(group_context, context);
               context->fromBinary(is);
            }
         }
      }
      
      void CTreeManager::FromBinary(StdString & str)
      {
         StdIStringStream istrs(str);
         CTreeManager::FromBinary(istrs);
      }
            

      //--------------------------------------------------------------
      
      void CTreeManager::DomainsToBinary  (StdOStream & os)
      {
         StdString currentContextId =
            CObjectFactory::GetCurrentContextId();
         std::vector<boost::shared_ptr<CContext> > def_vector =
            CContext::GetContextGroup()->getChildList();
            
         const StdSize size = def_vector.size();         
         const ENodeType cenum = CContext::GetType();
         const ENodeType genum = CContextGroup::GetType();
         const ENodeType denum = CDomain::GetType();
         const ENodeType ienum = CGrid::GetType();
         
         os.write (reinterpret_cast<const char*>(&genum) , sizeof(ENodeType)); 
         os.write (reinterpret_cast<const char*>(&size) , sizeof(StdSize));
         
         for (StdSize i = 0; i < size; i++)
         {
            boost::shared_ptr<CContext> context = def_vector[i];         
            CTreeManager::SetCurrentContextId(context->getId());                
            const bool hid = context->hasId(); // Toujours vrai
            
            os.write (reinterpret_cast<const char*>(&cenum), sizeof(ENodeType));      
            os.write (reinterpret_cast<const char*>(&hid), sizeof(bool));
            
            if (hid)
            {
               const StdString & id = context->getId();
               const StdSize size   = id.size();
                  
               os.write (reinterpret_cast<const char*>(&size), sizeof(StdSize));
               os.write (id.data(), size * sizeof(char));         
            } 
            
            std::vector<boost::shared_ptr<CDomain> > & alldomain = 
               CDomain::GetAllVectobject(context->getId());
            std::vector<boost::shared_ptr<CGrid> > & allgrid = 
               CGrid::GetAllVectobject(context->getId());
               
            const StdSize alldomain_size = alldomain.size(); 
            const StdSize allgrid_size   = allgrid.size();
            
            os.write (reinterpret_cast<const char*>(&alldomain_size), sizeof(StdSize));
            os.write (reinterpret_cast<const char*>(&allgrid_size), sizeof(StdSize));
            
            // Écriture successive des informations binaires de domaine.
            for (StdSize j = 0; j < alldomain_size; j++)
            {
               boost::shared_ptr<CDomain> domain = alldomain[j];
               bool hid = domain->hasId();
               
               os.write (reinterpret_cast<const char*>(&denum), sizeof(ENodeType)); 
               os.write (reinterpret_cast<const char*>(&hid), sizeof(bool));
               
               if (hid)
               {
                  const StdString & id = domain->getId();
                  const StdSize idsize   = id.size();
                     
                  os.write (reinterpret_cast<const char*>(&idsize), sizeof(StdSize));
                  os.write (id.data(), idsize * sizeof(char));         
               }         
               domain->toBinary(os);               
            }
            
            // Écriture successive des informations binaires de grille.
            for (StdSize j = 0; j < allgrid_size; j++)
            {
               boost::shared_ptr<CGrid> grid = allgrid[j];
               bool hid = grid->hasId();
               
               os.write (reinterpret_cast<const char*>(&ienum), sizeof(ENodeType)); 
               os.write (reinterpret_cast<const char*>(&hid), sizeof(bool));
               
               if (hid)
               {
                  const StdString & id = grid->getId();
                  const StdSize idsize   = id.size();
                     
                  os.write (reinterpret_cast<const char*>(&idsize), sizeof(StdSize));
                  os.write (id.data(), idsize * sizeof(char));         
               }         
               grid->toBinary(os);               
            }
         }
         CTreeManager::SetCurrentContextId(currentContextId); 
         
      }
      
      void CTreeManager::DomainsFromBinary(StdIStream & is)
      {
         StdSize ctxtnb = 0;
         ENodeType renum = Unknown;
         
         boost::shared_ptr<CContextGroup> group_context =
                           CContext::GetContextGroup();
         std::vector<boost::shared_ptr<CContext> > def_vector =
            CContext::GetContextGroup()->getChildList();
            
         const StdSize size = def_vector.size();  
                  
         is.read (reinterpret_cast<char*>(&renum), sizeof(StdSize));   
         is.read (reinterpret_cast<char*>(&ctxtnb), sizeof(ENodeType));

         if (renum != CContextGroup::GetType())
            ERROR("CTreeManager::DomainsFromBinary(StdIStream & is)",
                  << "[ renum = " << renum << "] Bad type !");
                  
         if (size != ctxtnb)
            ERROR("CTreeManager::DomainsFromBinary(StdIStream & is)",
                  << "[ size = " << size << "] Bad context group size !");
                  
         for (StdSize i = 0; i < ctxtnb; i++)
         {
            boost::shared_ptr<CContext> context = def_vector[i];
            bool hid = false;
            is.read (reinterpret_cast<char*>(&renum), sizeof(ENodeType));
            is.read (reinterpret_cast<char*>(&hid), sizeof(bool));
            
            if (renum != CContext::GetType())
               ERROR("CTreeManager::DomainsFromBinary(StdIStream & is)",
                     << "[ renum = " << renum << "] Bad type !");
                           
            if (hid)
            {
               StdSize size  = 0;
               is.read (reinterpret_cast<char*>(&size), sizeof(StdSize));
               StdString id(size, ' ');
               is.read (const_cast<char *>(id.data()), size * sizeof(char));
               
               CTreeManager::SetCurrentContextId(id);
            }
            
            std::vector<boost::shared_ptr<CDomain> > & alldomain = 
               CDomain::GetAllVectobject(context->getId());
            std::vector<boost::shared_ptr<CGrid> > & allgrid = 
               CGrid::GetAllVectobject(context->getId());
               
            const StdSize allgrid_size = allgrid.size();
            const StdSize alldomain_size = alldomain.size(); 
            
            StdSize read_alldomain_size = 0; 
            StdSize read_allgrid_size = 0; 
            
            is.read (reinterpret_cast<char*>(&read_alldomain_size), sizeof(StdSize));
            is.read (reinterpret_cast<char*>(&read_allgrid_size), sizeof(StdSize));
            
            if (alldomain_size != read_alldomain_size)
               ERROR("CTreeManager::DomainsFromBinary(StdIStream & is)",
                     << "[ read_alldomain_size = " << read_alldomain_size 
                     << "] Bad domain group size !");
                     
            if (alldomain_size != read_alldomain_size)
               ERROR("CTreeManager::DomainsFromBinary(StdIStream & is)",
                     << "[ read_allgrid_size = " << read_allgrid_size 
                     << "] Bad grid group size !");
                     
            // Lecture successive des informations binaires de domaine.
            for (StdSize j = 0; j < alldomain_size; j++)
            {
               boost::shared_ptr<CDomain> domain = alldomain[j];
               bool hid = domain->hasId();
               ENodeType rrenum = Unknown;
               
               is.read (reinterpret_cast<char*>(&rrenum), sizeof(ENodeType));
               is.read (reinterpret_cast<char*>(&hid), sizeof(bool));
               
               if (rrenum != CDomain::GetType())
                  ERROR("CTreeManager::DomainsFromBinary(StdIStream & is)",
                        << "[ rrenum = " << rrenum << "] Bad type !");

               if (hid)
               {
                  StdSize size  = 0;
                  is.read (reinterpret_cast<char*>(&size), sizeof(StdSize));
                  StdString id(size, ' ');
                  is.read (const_cast<char *>(id.data()), size * sizeof(char));
                  
                  if (domain->getId().compare(id) != 0)
                     ERROR("CTreeManager::DomainsFromBinary(StdIStream & is)",
                           << "[ id = " << id << "] Bad id !"); 
               }
               domain->fromBinary(is);
            }
            
            // Lecture successive des informations binaires de grille.
            for (StdSize j = 0; j < allgrid_size; j++)
            {
               boost::shared_ptr<CGrid> grid = allgrid[j];
               bool hid = grid->hasId();
               ENodeType rrenum = Unknown;
               
               is.read (reinterpret_cast<char*>(&rrenum), sizeof(ENodeType));
               is.read (reinterpret_cast<char*>(&hid), sizeof(bool));
               
               if (rrenum != CGrid::GetType())
                  ERROR("CTreeManager::DomainsFromBinary(StdIStream & is)",
                        << "[ rrenum = " << rrenum << "] Bad type !");

               if (hid)
               {
                  StdSize size  = 0;
                  is.read (reinterpret_cast<char*>(&size), sizeof(StdSize));
                  StdString id(size, ' ');
                  is.read (const_cast<char *>(id.data()), size * sizeof(char));
                  
                  if (grid->getId().compare(id) != 0)
                     ERROR("CTreeManager::DomainsFromBinary(StdIStream & is)",
                           << "[ id = " << id << "] Bad id !"); 
               }
               grid->fromBinary(is);
            }
            
         }
      }
      
      void CTreeManager::DomainsFromBinary(StdString & str)
      {
         StdIStringStream istrs(str);
         CTreeManager::DomainsFromBinary(istrs);
      }

      ///-------------------------------------------------------------

   } // namespace tree
} // namespace xmlioserver
