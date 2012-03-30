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
   
   typedef xios::tree::CFile      * XFilePtr;
   typedef xios::tree::CFileGroup * XFileGroupPtr;

   // ------------------------ Création des handle -----------------------------
   
   void cxios_file_handle_create (XFilePtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::GetObject<xios::tree::CFile>(id).get();
   }
   
   void cxios_filegroup_handle_create (XFileGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::GetObject<xios::tree::CFileGroup>(id).get();
   }

   // -------------------- Vérification des identifiants -----------------------

   void cxios_file_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::HasObject<xios::tree::CFile>(id);
   }

   void cxios_filegroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::HasObject<xios::tree::CFileGroup>(id);
   }
} // extern "C"
