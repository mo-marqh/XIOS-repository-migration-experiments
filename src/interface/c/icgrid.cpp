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
   
   typedef xios::tree::CGrid      * XGridPtr;
   typedef xios::tree::CGridGroup * XGridGroupPtr;

   // ------------------------ Création des handle -----------------------------
  
   void cxios_grid_handle_create (XGridPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::GetObject<xios::tree::CGrid>(id).get();
   }
   
   void cxios_gridgroup_handle_create (XGridGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::GetObject<xios::tree::CGridGroup>(id).get();
   }

   // -------------------- Vérification des identifiants -----------------------

   void cxios_grid_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::HasObject<xios::tree::CGrid>(id);
   }

   void cxios_gridgroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::HasObject<xios::tree::CGridGroup>(id);
   }
} // extern "C"
