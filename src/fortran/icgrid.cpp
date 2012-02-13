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
   
   typedef xmlioserver::tree::CGrid      * XGridPtr;
   typedef xmlioserver::tree::CGridGroup * XGridGroupPtr;

   // ------------------------- Attributs des axes -----------------------------
   
   void cxios_set_grid_name(XGridPtr grid_hdl, const char * name,  int name_size)
   {
      std::string name_str; 
      if (!cstr2string(name, name_size, name_str)) return;

      grid_hdl->name.setValue(name_str);
      grid_hdl->sendAttributToServer(name_str) ;
   }
   
   void cxios_set_grid_description(XGridPtr grid_hdl, const char * description,  int description_size)
   {
      std::string description_str; 
      if (!cstr2string(description, description_size, description_str)) return;

      grid_hdl->description.setValue(description_str);
      grid_hdl->sendAttributToServer(description_str) ;
   }
   
   void cxios_set_grid_domain_ref(XGridPtr grid_hdl, const char * domain_ref,  int domain_ref_size)
   {
      std::string domain_ref_str; 
      if (!cstr2string(domain_ref, domain_ref_size, domain_ref_str)) return;

      grid_hdl->domain_ref.setValue(domain_ref_str);
      grid_hdl->sendAttributToServer(domain_ref_str) ;
   }
   
   void cxios_set_grid_axis_ref(XGridPtr grid_hdl, const char * axis_ref,  int axis_ref_size)
   {
      std::string axis_ref_str; 
      if (!cstr2string(axis_ref, axis_ref_size, axis_ref_str)) return;

     grid_hdl->axis_ref.setValue(axis_ref_str);
     grid_hdl->sendAttributToServer(axis_ref_str) ;
   }
   
   // -------------------- Attributs des groupes de grilles --------------------
   
   void cxios_set_gridgroup_name(XGridGroupPtr gridgroup_hdl, const char * name,  int name_size)
   {
      std::string name_str; 
      if (!cstr2string(name, name_size, name_str)) return;

      gridgroup_hdl->name.setValue(name_str);
      gridgroup_hdl->sendAttributToServer(gridgroup_hdl->name) ;
   }
   
   void cxios_set_gridgroup_description(XGridGroupPtr gridgroup_hdl, const char * description,  int description_size)
   {
      std::string description_str; 
      if (!cstr2string(description, description_size, description_str)) return;

      gridgroup_hdl->description.setValue(description_str);
      gridgroup_hdl->sendAttributToServer(gridgroup_hdl->description) ;
   }
   
   void cxios_set_gridgroup_domain_ref(XGridGroupPtr gridgroup_hdl, const char * domain_ref,  int domain_ref_size)
   {
      std::string domain_ref_str; 
      if (!cstr2string(domain_ref, domain_ref_size, domain_ref_str)) return;

      gridgroup_hdl->domain_ref.setValue(domain_ref_str);
      gridgroup_hdl->sendAttributToServer(gridgroup_hdl->domain_ref) ;
   }
   
   void cxios_set_gridgroup_axis_ref(XGridGroupPtr gridgroup_hdl, const char * axis_ref,  int axis_ref_size)
   {
      std::string axis_ref_str; 
      if (!cstr2string(axis_ref, axis_ref_size, axis_ref_str)) return;

      gridgroup_hdl->axis_ref.setValue(axis_ref_str);
      gridgroup_hdl->sendAttributToServer(gridgroup_hdl->axis_ref) ;
   }
   
   // ------------------------ Création des handle -----------------------------
  
   void cxios_grid_handle_create (XGridPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CGrid>(id).get();
   }
   
   void cxios_gridgroup_handle_create (XGridGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CGridGroup>(id).get();
   }

   // -------------------- Vérification des identifiants -----------------------

   void cxios_grid_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::HasObject<xmlioserver::tree::CGrid>(id);
   }

   void cxios_gridgroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::HasObject<xmlioserver::tree::CGridGroup>(id);
   }
} // extern "C"
