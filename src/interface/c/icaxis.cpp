/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, xios, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>

#include "xmlioserver.hpp"

#include "object_template_impl.hpp"
#include "group_template_impl.hpp"
#include "attribute_template_impl.hpp"

#include "icutil.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   // ----------------------- Redéfinition de types ----------------------------
   
   typedef xios::CAxis      * XAxisPtr;
   typedef xios::CAxisGroup * XAxisGroupPtr;

   // ------------------------ Création des handle -----------------------------
   
   void cxios_axis_handle_create (XAxisPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::GetObject<xios::CAxis>(id).get();
   }
   
   void cxios_axisgroup_handle_create (XAxisGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::GetObject<xios::CAxisGroup>(id).get();
   }

   // -------------------- Vérification des identifiants -----------------------

   void cxios_axis_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::HasObject<xios::CAxis>(id);
   }

   void cxios_axisgroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::HasObject<xios::CAxisGroup>(id);
   }
   
} // extern "C"
