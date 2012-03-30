/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, xios, Avril 2010 - Octobre 2011         *
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

   typedef xios::tree::CContext * XContextPtr;

   typedef xios::tree::CGrid      * XGridPtr;
   typedef xios::tree::CGridGroup * XGridGroupPtr;

   typedef xios::tree::CFile      * XFilePtr;
   typedef xios::tree::CFileGroup * XFileGroupPtr;

   typedef xios::tree::CField      * XFieldPtr;
   typedef xios::tree::CFieldGroup * XFieldGroupPtr;

   typedef xios::tree::CDomain      * XDomainPtr;
   typedef xios::tree::CDomainGroup * XDomainGroupPtr;

   typedef xios::tree::CAxis      * XAxisPtr;
   typedef xios::tree::CAxisGroup * XAxisGroupPtr;
   
   // ----------------------- Ajout d'enfant à un parent -----------------------
   
   void cxios_xml_tree_add_field
      (XFieldGroupPtr  parent_, XFieldPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str; 

      if (cstr2string(child_id, child_id_size, child_id_str))
      {
         *child_ = parent_->createChild(child_id_str).get() ;
         parent_->sendCreateChild(child_id_str) ;
      }
      else
      {
         *child_ = parent_->createChild().get() ;
         parent_->sendCreateChild() ;
      }
  }
   
   void cxios_xml_tree_add_grid
      (XGridGroupPtr   parent_, XGridPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str; 
      if (cstr2string(child_id, child_id_size, child_id_str))
      {
         *child_ = parent_->createChild(child_id_str).get() ;
         parent_->sendCreateChild(child_id_str) ;
      }
      else
      {
         *child_ = parent_->createChild().get() ;
         parent_->sendCreateChild() ;
      }
   }
   
   void cxios_xml_tree_add_file
      (XFileGroupPtr parent_, XFilePtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str; 
      if (cstr2string(child_id, child_id_size, child_id_str))
      {
         *child_ = parent_->createChild(child_id_str).get() ;
         parent_->sendCreateChild(child_id_str) ;
      }
      else
      {
         *child_ = parent_->createChild().get() ;
         parent_->sendCreateChild() ;
      }
   }
   
   void cxios_xml_tree_add_axis
      (XAxisGroupPtr parent_, XAxisPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str; 
      if (cstr2string(child_id, child_id_size, child_id_str))
      {
         *child_ = parent_->createChild(child_id_str).get() ;
         parent_->sendCreateChild(child_id_str) ;
      }
      else
      {
         *child_ = parent_->createChild().get() ;
         parent_->sendCreateChild() ;
      }
   }
   
   void cxios_xml_tree_add_domain
      (XDomainGroupPtr parent_, XDomainPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str; 
      if (cstr2string(child_id, child_id_size, child_id_str))
      {
         *child_ = parent_->createChild(child_id_str).get() ;
         parent_->sendCreateChild(child_id_str) ;
      }
      else
      {
         *child_ = parent_->createChild().get() ;
         parent_->sendCreateChild() ;
      }
   }
   
   void cxios_xml_tree_add_fieldtofile
      (XFilePtr parent_, XFieldPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str; 
 
      if (cstr2string(child_id, child_id_size, child_id_str))
      {
         *child_ = parent_->addField(child_id_str).get();
         parent_->sendAddField(child_id_str) ;
      }
      else
      {
         *child_ = parent_->addField().get();
         parent_->sendAddField() ;
      }
   }

   // ----------------------- Ajout de groupe à un parent ----------------------

   void cxios_xml_tree_add_fieldgroup
      (XFieldGroupPtr  parent_, XFieldGroupPtr * child_, const char * child_id, int child_id_size)
   {
     std::string child_id_str; 

      if (cstr2string(child_id, child_id_size, child_id_str))
      {
         *child_ = parent_->createChildGroup(child_id_str).get() ;
         parent_->sendCreateChildGroup(child_id_str) ;
      }
      else
      {
         *child_ = parent_->createChildGroup().get() ;
         parent_->sendCreateChildGroup(child_id_str) ;
      }
   }

   void cxios_xml_tree_add_gridgroup
      (XGridGroupPtr   parent_, XGridGroupPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str;
 
      if (cstr2string(child_id, child_id_size, child_id_str))
      {
         *child_ = parent_->createChildGroup(child_id_str).get() ;
         parent_->sendCreateChildGroup(child_id_str) ;
      }
      else
      {
         *child_ = parent_->createChildGroup().get() ;
         parent_->sendCreateChildGroup(child_id_str) ;
      }
   }

   void cxios_xml_tree_add_filegroup
      (XFileGroupPtr parent_, XFileGroupPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str;
      if (cstr2string(child_id, child_id_size, child_id_str))
      {
         *child_ = parent_->createChildGroup(child_id_str).get() ;
         parent_->sendCreateChildGroup(child_id_str) ;
      }
      else
      {
         *child_ = parent_->createChildGroup().get() ;
         parent_->sendCreateChildGroup(child_id_str) ;
      }
   }

   void cxios_xml_tree_add_axisgroup
      (XAxisGroupPtr parent_, XAxisGroupPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str;
      if (cstr2string(child_id, child_id_size, child_id_str))
      {
         *child_ = parent_->createChildGroup(child_id_str).get() ;
         parent_->sendCreateChildGroup(child_id_str) ;
      }
      else
      {
         *child_ = parent_->createChildGroup().get() ;
         parent_->sendCreateChildGroup(child_id_str) ;
      }
   }

   void cxios_xml_tree_add_domaingroup
      (XDomainGroupPtr parent_, XDomainGroupPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str;
      if (cstr2string(child_id, child_id_size, child_id_str))
      {
         *child_ = parent_->createChildGroup(child_id_str).get() ;
         parent_->sendCreateChildGroup(child_id_str) ;
      }
      else
      {
         *child_ = parent_->createChildGroup().get() ;
         parent_->sendCreateChildGroup(child_id_str) ;
      }
   }

   void cxios_xml_tree_add_fieldgrouptofile
      (XFilePtr parent_, XFieldGroupPtr * child_, const char * child_id, int child_id_size)
   {
      std::string child_id_str; 
 
      if (cstr2string(child_id, child_id_size, child_id_str))
      {
         *child_ = parent_->addFieldGroup(child_id_str).get();
         parent_->sendAddFieldGroup(child_id_str) ;
      }
      else
      {
         *child_ = parent_->addFieldGroup().get();
         parent_->sendAddFieldGroup() ;
      }
   }
   
   
   // ----------------------- Affichage de l'arborescence ----------------------
   
   void cxios_xml_tree_show   (const char * filename, int filename_size) 
   {
      std::string filename_str;
      try
      {
         if (cstr2string(filename, filename_size, filename_str))
            xios::CTreeManager::PrintTreeToFile(filename_str);
         else
            xios::CTreeManager::PrintTreeToStream(std::clog);
      }
      catch (xios::CException & exc)
      {
         std::cerr << exc.getMessage() << std::endl;
         exit (EXIT_FAILURE);
      }
   }
     
   
   // ----------------------- Parsing de document xml --------------------------
   
   void cxios_xml_parse_file  (const char * filename  , int filename_size)
   {
      std::string filename_str; 
      if (!cstr2string(filename, filename_size, filename_str)) return;

      try
      {
         xios::CTreeManager::ParseFile(filename_str);
      }
      catch (xios::CException & exc)
      {
         std::cerr << exc.getMessage() << std::endl;
         exit (EXIT_FAILURE);
      }
   }
   
   void cxios_xml_parse_string(const char * xmlcontent, int xmlcontent_size)
   {
      std::string xmlcontent_str; 
      if (!cstr2string(xmlcontent, xmlcontent_size, xmlcontent_str)) return;

      try
      {
         xios::CTreeManager::ParseString(xmlcontent_str);
      }
      catch (xios::CException & exc)
      {
         std::cerr << exc.getMessage() << std::endl;
         exit (EXIT_FAILURE);
      }
   }
   


} // extern "C"
