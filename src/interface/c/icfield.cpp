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
   
   typedef xios::tree::CField      * XFieldPtr;
   typedef xios::tree::CFieldGroup * XFieldGroupPtr;
   
// --------------------------------------------------------------------------   
// ------------------------ Création des handle -----------------------------
// --------------------------------------------------------------------------   
   
   void cxios_field_handle_create (XFieldPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::GetObject<xios::tree::CField>(id).get();
   }
   
   void cxios_fieldgroup_handle_create (XFieldGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::GetObject<xios::tree::CFieldGroup>(id).get();
   }


   // -------------------- Vérification des identifiants -----------------------

   void cxios_field_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::HasObject<xios::tree::CField>(id);
   }

   void cxios_fieldgroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::HasObject<xios::tree::CFieldGroup>(id);
   }

// -----------------------------------------------------------------------------------------------------   
// ------------------------- other function----------  --------------------------- ---------------------
// -----------------------------------------------------------------------------------------------------   

  void cxios_field_is_active (XFieldPtr field_hdl, bool* ret)
  {
    *ret = field_hdl->isActive();
  }
   
} // extern "C"
