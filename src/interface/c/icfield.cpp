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
   
   typedef xios::CField      * XFieldPtr;
   typedef xios::CFieldGroup * XFieldGroupPtr;
   
// --------------------------------------------------------------------------   
// ------------------------ Création des handle -----------------------------
// --------------------------------------------------------------------------   
   
   void cxios_field_handle_create (XFieldPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = CField::get(id).get();
   }
   
   void cxios_fieldgroup_handle_create (XFieldGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = CFieldGroup::get(id).get();
   }


   // -------------------- Vérification des identifiants -----------------------

   void cxios_field_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = CField::has(id);
   }

   void cxios_fieldgroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = CFieldGroup::has(id);
   }

// -----------------------------------------------------------------------------------------------------   
// ------------------------- other function----------  --------------------------- ---------------------
// -----------------------------------------------------------------------------------------------------   

  void cxios_field_is_active (XFieldPtr field_hdl, bool* ret)
  {
    *ret = field_hdl->isActive();
  }
   
} // extern "C"
