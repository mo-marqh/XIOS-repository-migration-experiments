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
   
   typedef xmlioserver::tree::CField      * XFieldPtr;
   typedef xmlioserver::tree::CFieldGroup * XFieldGroupPtr;

   // ------------------------- Attributs des champs ---------------------------
   
   void cxios_set_field_name(XFieldPtr field_hdl, const char * name, int name_size)
   {
      std::string name_str; 
      if (!cstr2string(name, name_size, name_str)) return;

      field_hdl->name.setValue(name_str);
   }
   
   void cxios_set_field_standard_name(XFieldPtr field_hdl, const char * standard_name, int standard_name_size)
   {
      std::string standard_name_str; 
      if (!cstr2string(standard_name, standard_name_size, standard_name_str)) return;

      field_hdl->standard_name.setValue(standard_name_str);
   }
   
   void cxios_set_field_long_name(XFieldPtr field_hdl, const char * long_name, int long_name_size)
   {
      std::string long_name_str; 
      if (!cstr2string(long_name, long_name_size, long_name_str)) return;

      field_hdl->long_name.setValue(long_name_str);
   }
   
   void cxios_set_field_unit(XFieldPtr field_hdl, const char * unit, int unit_size)
   {
      std::string unit_str; 
      if (!cstr2string(unit, unit_size, unit_str)) return;

      field_hdl->unit.setValue(unit_str);
   }
   
   void cxios_set_field_operation(XFieldPtr field_hdl, const char * operation, int operation_size) 
   {
      std::string operation_str; 
      if (!cstr2string(operation, operation_size, operation_str)) return;

      field_hdl->operation.setValue(operation_str);
   }
   
   void cxios_set_field_freq_op(XFieldPtr field_hdl, const char * freq_op, int freq_op_size)
   {
      std::string freq_op_str; 
      if (!cstr2string(freq_op, freq_op_size, freq_op_str)) return;

      field_hdl->freq_op.setValue(freq_op_str);
   }
   
   void cxios_set_field_level(XFieldPtr field_hdl, int level)
   {
      field_hdl->level.setValue(level);
   }
   
   void cxios_set_field_prec(XFieldPtr field_hdl, int prec)
   {
      field_hdl->prec.setValue(prec);
   }
   
   void cxios_set_field_enabled(XFieldPtr field_hdl, bool enabled)
   {
      field_hdl->enabled.setValue(enabled);
   }
   
   void cxios_set_field_domain_ref(XFieldPtr field_hdl,  const char * domain_ref, int domain_ref_size)
   {
      std::string domain_ref_str; 
      if (!cstr2string(domain_ref, domain_ref_size, domain_ref_str)) return;

      field_hdl->domain_ref.setValue(domain_ref_str);
   }
   
   void cxios_set_field_axis_ref(XFieldPtr field_hdl,  const char * axis_ref,  int axis_ref_size)
   {
      std::string axis_ref_str; 
      if (!cstr2string(axis_ref, axis_ref_size, axis_ref_str)) return;

      field_hdl->axis_ref.setValue(axis_ref_str);
   }
   
   void cxios_set_field_grid_ref(XFieldPtr field_hdl,  const char * grid_ref, int grid_ref_size)
   {
      std::string grid_ref_str; 
      if (!cstr2string(grid_ref, grid_ref_size, grid_ref_str)) return;

      field_hdl->grid_ref.setValue(grid_ref_str);
   }
   
   void cxios_set_field_field_ref(XFieldPtr field_hdl,  const char * field_ref,  int field_ref_size)
   {
      std::string field_ref_str; 
      if (!cstr2string(field_ref, field_ref_size, field_ref_str)) return;

      field_hdl->field_ref.setValue(field_ref_str);
   }
   
   void cxios_set_field_default_value(XFieldPtr field_hdl, double default_value)
   {
      field_hdl->default_value.setValue(default_value);
   }  
   
   // -------------------- Attributs des groupes de champs ---------------------
  
   void cxios_set_fieldgroup_name(XFieldGroupPtr fieldgroup_hdl, const char * name, int name_size)
   {
      std::string name_str; 
      if (!cstr2string(name, name_size, name_str)) return;

      fieldgroup_hdl->name.setValue(name_str);
   }
   
   void cxios_set_fieldgroup_standard_name(XFieldGroupPtr fieldgroup_hdl, const char * standard_name, int standard_name_size)
   {
      std::string standard_name_str; 
      if (!cstr2string(standard_name, standard_name_size, standard_name_str)) return;

      fieldgroup_hdl->standard_name.setValue(standard_name_str);
   }
   
   void cxios_set_fieldgroup_long_name(XFieldGroupPtr fieldgroup_hdl, const char * long_name, int long_name_size)
   {
      std::string long_name_str; 
      if (!cstr2string(long_name, long_name_size, long_name_str)) return;

      fieldgroup_hdl->long_name.setValue(long_name_str);
   }
   
   void cxios_set_fieldgroup_unit(XFieldGroupPtr fieldgroup_hdl, const char * unit, int unit_size)
   {
      std::string unit_str; 
      if (!cstr2string(unit, unit_size, unit_str)) return;

      fieldgroup_hdl->unit.setValue(unit_str);
   }
   
   void cxios_set_fieldgroup_operation(XFieldGroupPtr fieldgroup_hdl, const char * operation, int operation_size) 
   {
      std::string operation_str; 
      if (!cstr2string(operation, operation_size, operation_str)) return;

      fieldgroup_hdl->operation.setValue(operation_str);
   }
   
   void cxios_set_fieldgroup_freq_op(XFieldGroupPtr fieldgroup_hdl, const char * freq_op, int freq_op_size)
   {
      std::string freq_op_str; 
      if (!cstr2string(freq_op, freq_op_size, freq_op_str)) return;

      fieldgroup_hdl->freq_op.setValue(freq_op_str);
   }
   
   void cxios_set_fieldgroup_level(XFieldGroupPtr fieldgroup_hdl, int level)
   {
      fieldgroup_hdl->level.setValue(level);
   }
   
   void cxios_set_fieldgroup_prec(XFieldGroupPtr fieldgroup_hdl, int prec)
   {
      fieldgroup_hdl->prec.setValue(prec);
   }
   
   void cxios_set_fieldgroup_enabled(XFieldGroupPtr fieldgroup_hdl, bool enabled)
   {
      fieldgroup_hdl->enabled.setValue(enabled);
   }
   
   void cxios_set_fieldgroup_domain_ref(XFieldGroupPtr fieldgroup_hdl,  const char * domain_ref, int domain_ref_size)
   {
      std::string domain_ref_str; 
      if (!cstr2string(domain_ref, domain_ref_size, domain_ref_str)) return;

      fieldgroup_hdl->domain_ref.setValue(domain_ref_str);
   }
   
   void cxios_set_fieldgroup_axis_ref(XFieldGroupPtr fieldgroup_hdl,  const char * axis_ref,  int axis_ref_size)
   {
      std::string axis_ref_str; 
      if (!cstr2string(axis_ref, axis_ref_size, axis_ref_str)) return;

      fieldgroup_hdl->axis_ref.setValue(axis_ref_str);
   }
   
   void cxios_set_fieldgroup_grid_ref(XFieldGroupPtr fieldgroup_hdl,  const char * grid_ref, int grid_ref_size)
   {
      std::string grid_ref_str; 
      if (!cstr2string(grid_ref, grid_ref_size, grid_ref_str)) return;

      fieldgroup_hdl->grid_ref.setValue(grid_ref_str);
   }
   
   void cxios_set_fieldgroup_field_ref(XFieldGroupPtr fieldgroup_hdl,  const char * field_ref,  int field_ref_size)
   {
      std::string field_ref_str; 
      if (!cstr2string(field_ref, field_ref_size, field_ref_str)) return;

      fieldgroup_hdl->field_ref.setValue(field_ref_str);
   }
   
   void cxios_set_fieldgroup_default_value(XFieldGroupPtr fieldgroup_hdl, double default_value)
   {
      fieldgroup_hdl->default_value.setValue(default_value);
   }  
   
   // ------------------------ Création des handle -----------------------------
   
   void cxios_field_handle_create (XFieldPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CField>(id).get();
   }
   
   void cxios_fieldgroup_handle_create (XFieldGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CFieldGroup>(id).get();
   }


   // -------------------- Vérification des identifiants -----------------------

   void cxios_field_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::HasObject<xmlioserver::tree::CField>(id);
   }

   void cxios_fieldgroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::HasObject<xmlioserver::tree::CFieldGroup>(id);
   }
   
} // extern "C"
