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
   
   typedef xmlioserver::tree::CFile      * XFilePtr;
   typedef xmlioserver::tree::CFileGroup * XFileGroupPtr;

   // ------------------------- Attributs des axes -----------------------------
   
   void cxios_set_file_name(XFilePtr file_hdl, const char * name, int name_size)
   {
      std::string name_str; 
      if (!cstr2string(name, name_size, name_str)) return;

      file_hdl->name.setValue(name_str);
   }
   
   void cxios_set_file_description(XFilePtr file_hdl, const char * description, int description_size)
   {
      std::string description_str; 
      if (!cstr2string(description, description_size, description_str)) return;

      file_hdl->description.setValue(description_str);
   } 
   
   void cxios_set_file_name_suffix(XFilePtr file_hdl, const char * name_suffix, int name_suffix_size)
   {
      std::string name_suffix_str; 
      if (!cstr2string(name_suffix, name_suffix_size, name_suffix_str)) return;

      file_hdl->name_suffix.setValue(name_suffix_str);
   } 
   
   void cxios_set_file_output_freq(XFilePtr file_hdl, const char * output_freq, int output_freq_size)
   {
      std::string output_freq_str; 
      if (!cstr2string(output_freq, output_freq_size, output_freq_str)) return;

      file_hdl->output_freq.setValue(output_freq_str);
   }
   
   void cxios_set_file_output_level(XFilePtr file_hdl, int output_level)
   {
      file_hdl->output_level.setValue(output_level);
   }
   
   void cxios_set_file_enabled(XFilePtr file_hdl, bool enabled)
   {
      file_hdl->enabled.setValue(enabled);
   }
   
   // -------------------- Attributs des groupes d'axes ------------------------
   
   void cxios_set_filegroup_name(XFileGroupPtr filegroup_hdl, const char * name, int name_size)
   {
      std::string name_str; 
      if (!cstr2string(name, name_size, name_str)) return;
      if (!cstr2string(name, name_size, name_str)) return;

      filegroup_hdl->name.setValue(name_str);
   }
   
   void cxios_set_filegroup_description(XFileGroupPtr filegroup_hdl, const char * description, int description_size)
   {
      std::string description_str; 
      if (!cstr2string(description, description_size, description_str)) return;

      filegroup_hdl->description.setValue(description_str);
   } 
   
   void cxios_set_filegroup_name_suffix(XFileGroupPtr filegroup_hdl, const char * name_suffix, int name_suffix_size)
   {
      std::string name_suffix_str; 
      if (!cstr2string(name_suffix, name_suffix_size, name_suffix_str)) return;

      filegroup_hdl->name_suffix.setValue(name_suffix_str);
   } 
   
   void cxios_set_filegroup_output_freq(XFileGroupPtr filegroup_hdl, const char * output_freq, int output_freq_size)
   {
      std::string output_freq_str; 
      if (!cstr2string(output_freq, output_freq_size, output_freq_str)) return;

      filegroup_hdl->output_freq.setValue(output_freq_str);
   }
   
   void cxios_set_filegroup_output_level(XFileGroupPtr filegroup_hdl, int output_level)
   {
      filegroup_hdl->output_level.setValue(output_level);
   }
   
   void cxios_set_filegroup_enabled(XFileGroupPtr filegroup_hdl, bool enabled)
   {
      filegroup_hdl->enabled.setValue(enabled);
   }
   
   // ------------------------ Création des handle -----------------------------
   
   void cxios_file_handle_create (XFilePtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CFile>(id).get();
   }
   
   void cxios_filegroup_handle_create (XFileGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CFileGroup>(id).get();
   }

   // -------------------- Vérification des identifiants -----------------------

   void cxios_file_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::HasObject<xmlioserver::tree::CFile>(id);
   }

   void cxios_filegroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::HasObject<xmlioserver::tree::CFileGroup>(id);
   }
} // extern "C"
