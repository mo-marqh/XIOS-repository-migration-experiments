/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, XMLIOServer, Avril 2010 - Octobre 2011         *
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
   
   typedef xmlioserver::tree::CAxis      * XAxisPtr;
   typedef xmlioserver::tree::CAxisGroup * XAxisGroupPtr;

   // ------------------------- Attributs des axes -----------------------------
   void cxios_set_axis_name 
      (XAxisPtr axis_hdl, const char * name , int name_size)
   {
      std::string name_str; 
      if (!cstr2string(name, name_size, name_str)) return;

      axis_hdl->name.setValue(name_str);
      axis_hdl->sendAttributToServer(axis_hdl->name) ;
   }

   void cxios_set_axis_standard_name
      (XAxisPtr axis_hdl, const char * standard_name , int standard_name_size)
   {
      std::string standard_name_str; 
      if (!cstr2string(standard_name, standard_name_size, standard_name_str)) return;

      axis_hdl->standard_name.setValue(standard_name_str); 
      axis_hdl->sendAttributToServer(axis_hdl->standard_name); 
  }
    
   void cxios_set_axis_long_name 
      (XAxisPtr axis_hdl, const char * long_name , int long_name_size)
   {
      std::string long_name_str; 
      if (!cstr2string(long_name, long_name_size, long_name_str)) return;

      axis_hdl->long_name.setValue(long_name_str);
      axis_hdl->sendAttributToServer(axis_hdl->long_name) ;
   }

   void cxios_set_axis_unit 
      (XAxisPtr axis_hdl, const char * unit , int unit_size)
   {
      std::string unit_str; 
      if (!cstr2string(unit, unit_size, unit_str)) return;

      axis_hdl->unit.setValue(unit_str);
      axis_hdl->sendAttributToServer(axis_hdl->unit) ;
   }
    
   void cxios_set_axis_size(XAxisPtr axis_hdl, int size)
   {
      axis_hdl->size.setValue(size);
      axis_hdl->sendAttributToServer(axis_hdl->size) ;
   }

   void cxios_set_axis_zvalue 
      (XAxisPtr axis_hdl, const double * zvalue , int zvalue_extent1)
   {
      ARRAY(double, 1) zvalue_val(new CArray<double, 1>(boost::extents [zvalue_extent1]));
      std::copy(zvalue, &(zvalue[zvalue_val->num_elements()]), zvalue_val->data());

      axis_hdl->zvalue.setValue(zvalue_val);
      axis_hdl->sendAttributToServer(axis_hdl->zvalue) ;

   }
   
   // -------------------- Attributs des groupes d'axes -------------------------
   
   void cxios_set_axisgroup_name 
      (XAxisGroupPtr axisgroup_hdl, const char * name , int name_size)
   {
      std::string name_str;
      if (!cstr2string(name, name_size, name_str)) return;

      axisgroup_hdl->name.setValue(name_str);
      axisgroup_hdl->sendAttributToServer(axisgroup_hdl->name) ;
   }

   void cxios_set_axisgroup_standard_name
      (XAxisGroupPtr axisgroup_hdl, const char * standard_name , int standard_name_size)
   {
      std::string standard_name_str;
      if (!cstr2string(standard_name, standard_name_size, standard_name_str)) return;

      axisgroup_hdl->standard_name.setValue(standard_name_str);
      axisgroup_hdl->sendAttributToServer(axisgroup_hdl->standard_name) ;
   }
    
   void cxios_set_axisgroup_long_name 
      (XAxisGroupPtr axisgroup_hdl, const char * long_name , int long_name_size)
   {
      std::string long_name_str;
      if (!cstr2string(long_name, long_name_size, long_name_str)) return;

      axisgroup_hdl->long_name.setValue(long_name_str);
      axisgroup_hdl->sendAttributToServer(axisgroup_hdl->long_name) ;
   }

   void cxios_set_axisgroup_unit 
      (XAxisGroupPtr axisgroup_hdl, const char * unit , int unit_size)
   {
      std::string unit_str;
      if (!cstr2string(unit, unit_size, unit_str)) return;

      axisgroup_hdl->unit.setValue(unit_str);
      axisgroup_hdl->sendAttributToServer(axisgroup_hdl->unit) ;
   }
    
   void cxios_set_axisgroup_size(XAxisGroupPtr axisgroup_hdl, int size)
   {
      axisgroup_hdl->size.setValue(size);
      axisgroup_hdl->sendAttributToServer(axisgroup_hdl->size) ;
   }

   void cxios_set_axisgroup_zvalue 
      (XAxisGroupPtr axisgroup_hdl, const double * zvalue , int zvalue_extent1)
   {
      ARRAY(double, 1) zvalue_val(new CArray<double, 1>(boost::extents [zvalue_extent1]));
      std::copy(zvalue, &(zvalue[zvalue_val->num_elements()]), zvalue_val->data());

      axisgroup_hdl->zvalue.setValue(zvalue_val);
      axisgroup_hdl->sendAttributToServer(axisgroup_hdl->zvalue) ;
   }
   
   // ------------------------ Création des handle -----------------------------
   
   void cxios_axis_handle_create (XAxisPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CAxis>(id).get();
   }
   
   void cxios_axisgroup_handle_create (XAxisGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CAxisGroup>(id).get();
   }

   // -------------------- Vérification des identifiants -----------------------

   void cxios_axis_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::HasObject<xmlioserver::tree::CAxis>(id);
   }

   void cxios_axisgroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::HasObject<xmlioserver::tree::CAxisGroup>(id);
   }
   
} // extern "C"
