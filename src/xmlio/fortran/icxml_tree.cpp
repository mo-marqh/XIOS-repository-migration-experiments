/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, XMLIOServer, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>

#include "xmlioserver.hpp"

#include "attribute_template_impl.hpp"
#include "object_template_impl.hpp"
#include "group_template_impl.hpp"

#include "icutil.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   // ----------------------- Redéfinition de types ----------------------------

   typedef xmlioserver::tree::CContext * XContextPtr;

   typedef xmlioserver::tree::CGrid      * XGridPtr;
   typedef xmlioserver::tree::CGridGroup * XGridGroupPtr;

   typedef xmlioserver::tree::CFile      * XFilePtr;
   typedef xmlioserver::tree::CFileGroup * XFileGroupPtr;

   typedef xmlioserver::tree::CField      * XFieldPtr;
   typedef xmlioserver::tree::CFieldGroup * XFieldGroupPtr;

   typedef xmlioserver::tree::CDomain      * XDomainPtr;
   typedef xmlioserver::tree::CDomainGroup * XDomainGroupPtr;

   typedef xmlioserver::tree::CAxis      * XAxisPtr;
   typedef xmlioserver::tree::CAxisGroup * XAxisGroupPtr;
   
   // ----------------------- Ajout d'enfant à un parent -----------------------
   
   void xios_xml_tree_add_field
      (XFieldGroupPtr  parent_, XFieldPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str; 
      boost::shared_ptr<xmlioserver::tree::CFieldGroup> fieldgroup =
         xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CFieldGroup>(parent_);

      if (cstr2string(child_id, child_id_size, child_id_str))
         *child_ = xmlioserver::CGroupFactory::CreateChild(fieldgroup, child_id_str).get();
      else
         *child_ = xmlioserver::CGroupFactory::CreateChild(fieldgroup).get();
   }
   
   void xios_xml_tree_add_grid
      (XGridGroupPtr   parent_, XGridPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str; 
      boost::shared_ptr<xmlioserver::tree::CGridGroup> gridgroup =
         xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CGridGroup>(parent_);

      if (cstr2string(child_id, child_id_size, child_id_str))
         *child_ = xmlioserver::CGroupFactory::CreateChild(gridgroup, child_id_str).get();
      else
         *child_ = xmlioserver::CGroupFactory::CreateChild(gridgroup).get();
   }
   
   void xios_xml_tree_add_file
      (XFileGroupPtr parent_, XFilePtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str; 
      boost::shared_ptr<xmlioserver::tree::CFileGroup> filegroup =
         xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CFileGroup>(parent_);

      if (cstr2string(child_id, child_id_size, child_id_str))
         *child_ = xmlioserver::CGroupFactory::CreateChild(filegroup, child_id_str).get();
      else
         *child_ = xmlioserver::CGroupFactory::CreateChild(filegroup).get();
   }
   
   void xios_xml_tree_add_axis
      (XAxisGroupPtr parent_, XAxisPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str; 
      boost::shared_ptr<xmlioserver::tree::CAxisGroup> axisgroup =
         xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CAxisGroup>(parent_);

      if (cstr2string(child_id, child_id_size, child_id_str))
         *child_ = xmlioserver::CGroupFactory::CreateChild(axisgroup, child_id_str).get();
      else
         *child_ = xmlioserver::CGroupFactory::CreateChild(axisgroup).get();
   }
   
   void xios_xml_tree_add_domain
      (XDomainGroupPtr parent_, XDomainPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str; 
      boost::shared_ptr<xmlioserver::tree::CDomainGroup> domaingroup =
         xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CDomainGroup>(parent_);

      if (cstr2string(child_id, child_id_size, child_id_str))
         *child_ = xmlioserver::CGroupFactory::CreateChild(domaingroup, child_id_str).get();
      else
         *child_ = xmlioserver::CGroupFactory::CreateChild(domaingroup).get();
   }
   
   void xios_xml_tree_add_fieldtofile
      (XFilePtr parent_, XFieldPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str; 
      boost::shared_ptr<xmlioserver::tree::CFile> file =
         xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CFile>(parent_);

      if (file->getVirtualFieldGroup().get() == 0 )
          file->setVirtualFieldGroup(file->getId());

      if (cstr2string(child_id, child_id_size, child_id_str))
         *child_ = xmlioserver::CGroupFactory::CreateChild(file->getVirtualFieldGroup(), child_id_str).get();
      else
         *child_ = xmlioserver::CGroupFactory::CreateChild(file->getVirtualFieldGroup()).get();
   }

   // ----------------------- Ajout de groupe à un parent ----------------------

   void xios_xml_tree_add_fieldgroup
      (XFieldGroupPtr  parent_, XFieldGroupPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str;
      boost::shared_ptr<xmlioserver::tree::CFieldGroup> fieldgroup =
         xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CFieldGroup>(parent_);

      if (cstr2string(child_id, child_id_size, child_id_str))
         *child_ = xmlioserver::CGroupFactory::CreateGroup(fieldgroup, child_id_str).get();
      else
         *child_ = xmlioserver::CGroupFactory::CreateGroup(fieldgroup).get();
   }

   void xios_xml_tree_add_gridgroup
      (XGridGroupPtr   parent_, XGridGroupPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str;
      boost::shared_ptr<xmlioserver::tree::CGridGroup> gridgroup =
         xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CGridGroup>(parent_);

      if (cstr2string(child_id, child_id_size, child_id_str))
         *child_ = xmlioserver::CGroupFactory::CreateGroup(gridgroup, child_id_str).get();
      else
         *child_ = xmlioserver::CGroupFactory::CreateGroup(gridgroup).get();
   }

   void xios_xml_tree_add_filegroup
      (XFileGroupPtr parent_, XFileGroupPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str;
      boost::shared_ptr<xmlioserver::tree::CFileGroup> filegroup =
         xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CFileGroup>(parent_);

      if (cstr2string(child_id, child_id_size, child_id_str))
         *child_ = xmlioserver::CGroupFactory::CreateGroup(filegroup, child_id_str).get();
      else
         *child_ = xmlioserver::CGroupFactory::CreateGroup(filegroup).get();
   }

   void xios_xml_tree_add_axisgroup
      (XAxisGroupPtr parent_, XAxisGroupPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str;
      boost::shared_ptr<xmlioserver::tree::CAxisGroup> axisgroup =
         xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CAxisGroup>(parent_);

      if (cstr2string(child_id, child_id_size, child_id_str))
         *child_ = xmlioserver::CGroupFactory::CreateGroup(axisgroup, child_id_str).get();
      else
         *child_ = xmlioserver::CGroupFactory::CreateGroup(axisgroup).get();
   }

   void xios_xml_tree_add_domaingroup
      (XDomainGroupPtr parent_, XDomainGroupPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str;
      boost::shared_ptr<xmlioserver::tree::CDomainGroup> domaingroup =
         xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CDomainGroup>(parent_);

      if (cstr2string(child_id, child_id_size, child_id_str))
         *child_ = xmlioserver::CGroupFactory::CreateGroup(domaingroup, child_id_str).get();
      else
         *child_ = xmlioserver::CGroupFactory::CreateGroup(domaingroup).get();
   }

   void xios_xml_tree_add_fieldgrouptofile
      (XFilePtr parent_, XFieldGroupPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str; 
      boost::shared_ptr<xmlioserver::tree::CFile> file =
         xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CFile>(parent_);

      if (file->getVirtualFieldGroup().get() == 0 )
          file->setVirtualFieldGroup(file->getId());
      
      if (cstr2string(child_id, child_id_size, child_id_str))
         *child_ = xmlioserver::CGroupFactory::CreateGroup(file->getVirtualFieldGroup(), child_id_str).get();
      else
         *child_ = xmlioserver::CGroupFactory::CreateGroup(file->getVirtualFieldGroup()).get();
   }
   
   
   // ----------------------- Affichage de l'arborescence ----------------------
   
   void xios_xml_tree_show   (const char * filename, int filename_size) 
   {
      std::string filename_str;
      try
      {
         if (cstr2string(filename, filename_size, filename_str))
            xmlioserver::CTreeManager::PrintTreeToFile(filename_str);
         else
            xmlioserver::CTreeManager::PrintTreeToStream(std::clog);
      }
      catch (xmlioserver::CException & exc)
      {
         std::cerr << exc.getMessage() << std::endl;
         exit (EXIT_FAILURE);
      }
   }
     
   
   // ----------------------- Parsing de document xml --------------------------
   
   void xios_xml_parse_file  (const char * filename  , int filename_size)
   {
      std::string filename_str; 
      if (!cstr2string(filename, filename_size, filename_str)) return;

      try
      {
         xmlioserver::CTreeManager::ParseFile(filename_str);
      }
      catch (xmlioserver::CException & exc)
      {
         std::cerr << exc.getMessage() << std::endl;
         exit (EXIT_FAILURE);
      }
   }
   
   void xios_xml_parse_string(const char * xmlcontent, int xmlcontent_size)
   {
      std::string xmlcontent_str; 
      if (!cstr2string(xmlcontent, xmlcontent_size, xmlcontent_str)) return;

      try
      {
         xmlioserver::CTreeManager::ParseString(xmlcontent_str);
      }
      catch (xmlioserver::CException & exc)
      {
         std::cerr << exc.getMessage() << std::endl;
         exit (EXIT_FAILURE);
      }
   }
   


} // extern "C"
