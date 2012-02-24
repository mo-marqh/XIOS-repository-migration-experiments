/* ************************************************************************** *
 *               Interface auto generated - do not modify                   *
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
  typedef xmlioserver::tree::CField*  field_Ptr;
  
  void cxios_set_field_axis_ref(field_Ptr field_hdl, const char * axis_ref, int axis_ref_size)
  {
    std::string axis_ref_str;
    if(!cstr2string(axis_ref, axis_ref_size, axis_ref_str)) return;
    field_hdl->axis_ref.setValue(axis_ref_str);
    field_hdl->sendAttributToServer(field_hdl->axis_ref);
  }
  
  void cxios_get_field_axis_ref(field_Ptr field_hdl, char * axis_ref, int axis_ref_size)
  {
    if(!string_copy(field_hdl->axis_ref.getValue(),axis_ref , axis_ref_size))
      ERROR("void cxios_get_field_axis_ref(field_Ptr field_hdl, char * axis_ref, int axis_ref_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_field_default_value(field_Ptr field_hdl, double default_value)
  {
    field_hdl->default_value.setValue(default_value);
    field_hdl->sendAttributToServer(field_hdl->default_value);
  }
  
  void cxios_get_field_default_value(field_Ptr field_hdl, double* default_value)
  {
    *default_value = field_hdl->default_value.getValue();
  }
  
  
  void cxios_set_field_domain_ref(field_Ptr field_hdl, const char * domain_ref, int domain_ref_size)
  {
    std::string domain_ref_str;
    if(!cstr2string(domain_ref, domain_ref_size, domain_ref_str)) return;
    field_hdl->domain_ref.setValue(domain_ref_str);
    field_hdl->sendAttributToServer(field_hdl->domain_ref);
  }
  
  void cxios_get_field_domain_ref(field_Ptr field_hdl, char * domain_ref, int domain_ref_size)
  {
    if(!string_copy(field_hdl->domain_ref.getValue(),domain_ref , domain_ref_size))
      ERROR("void cxios_get_field_domain_ref(field_Ptr field_hdl, char * domain_ref, int domain_ref_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_field_enabled(field_Ptr field_hdl, bool enabled)
  {
    field_hdl->enabled.setValue(enabled);
    field_hdl->sendAttributToServer(field_hdl->enabled);
  }
  
  void cxios_get_field_enabled(field_Ptr field_hdl, bool* enabled)
  {
    *enabled = field_hdl->enabled.getValue();
  }
  
  
  void cxios_set_field_field_ref(field_Ptr field_hdl, const char * field_ref, int field_ref_size)
  {
    std::string field_ref_str;
    if(!cstr2string(field_ref, field_ref_size, field_ref_str)) return;
    field_hdl->field_ref.setValue(field_ref_str);
    field_hdl->sendAttributToServer(field_hdl->field_ref);
  }
  
  void cxios_get_field_field_ref(field_Ptr field_hdl, char * field_ref, int field_ref_size)
  {
    if(!string_copy(field_hdl->field_ref.getValue(),field_ref , field_ref_size))
      ERROR("void cxios_get_field_field_ref(field_Ptr field_hdl, char * field_ref, int field_ref_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_field_freq_offset(field_Ptr field_hdl, const char * freq_offset, int freq_offset_size)
  {
    std::string freq_offset_str;
    if(!cstr2string(freq_offset, freq_offset_size, freq_offset_str)) return;
    field_hdl->freq_offset.setValue(freq_offset_str);
    field_hdl->sendAttributToServer(field_hdl->freq_offset);
  }
  
  void cxios_get_field_freq_offset(field_Ptr field_hdl, char * freq_offset, int freq_offset_size)
  {
    if(!string_copy(field_hdl->freq_offset.getValue(),freq_offset , freq_offset_size))
      ERROR("void cxios_get_field_freq_offset(field_Ptr field_hdl, char * freq_offset, int freq_offset_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_field_freq_op(field_Ptr field_hdl, const char * freq_op, int freq_op_size)
  {
    std::string freq_op_str;
    if(!cstr2string(freq_op, freq_op_size, freq_op_str)) return;
    field_hdl->freq_op.setValue(freq_op_str);
    field_hdl->sendAttributToServer(field_hdl->freq_op);
  }
  
  void cxios_get_field_freq_op(field_Ptr field_hdl, char * freq_op, int freq_op_size)
  {
    if(!string_copy(field_hdl->freq_op.getValue(),freq_op , freq_op_size))
      ERROR("void cxios_get_field_freq_op(field_Ptr field_hdl, char * freq_op, int freq_op_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_field_grid_ref(field_Ptr field_hdl, const char * grid_ref, int grid_ref_size)
  {
    std::string grid_ref_str;
    if(!cstr2string(grid_ref, grid_ref_size, grid_ref_str)) return;
    field_hdl->grid_ref.setValue(grid_ref_str);
    field_hdl->sendAttributToServer(field_hdl->grid_ref);
  }
  
  void cxios_get_field_grid_ref(field_Ptr field_hdl, char * grid_ref, int grid_ref_size)
  {
    if(!string_copy(field_hdl->grid_ref.getValue(),grid_ref , grid_ref_size))
      ERROR("void cxios_get_field_grid_ref(field_Ptr field_hdl, char * grid_ref, int grid_ref_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_field_level(field_Ptr field_hdl, int level)
  {
    field_hdl->level.setValue(level);
    field_hdl->sendAttributToServer(field_hdl->level);
  }
  
  void cxios_get_field_level(field_Ptr field_hdl, int* level)
  {
    *level = field_hdl->level.getValue();
  }
  
  
  void cxios_set_field_long_name(field_Ptr field_hdl, const char * long_name, int long_name_size)
  {
    std::string long_name_str;
    if(!cstr2string(long_name, long_name_size, long_name_str)) return;
    field_hdl->long_name.setValue(long_name_str);
    field_hdl->sendAttributToServer(field_hdl->long_name);
  }
  
  void cxios_get_field_long_name(field_Ptr field_hdl, char * long_name, int long_name_size)
  {
    if(!string_copy(field_hdl->long_name.getValue(),long_name , long_name_size))
      ERROR("void cxios_get_field_long_name(field_Ptr field_hdl, char * long_name, int long_name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_field_name(field_Ptr field_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if(!cstr2string(name, name_size, name_str)) return;
    field_hdl->name.setValue(name_str);
    field_hdl->sendAttributToServer(field_hdl->name);
  }
  
  void cxios_get_field_name(field_Ptr field_hdl, char * name, int name_size)
  {
    if(!string_copy(field_hdl->name.getValue(),name , name_size))
      ERROR("void cxios_get_field_name(field_Ptr field_hdl, char * name, int name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_field_operation(field_Ptr field_hdl, const char * operation, int operation_size)
  {
    std::string operation_str;
    if(!cstr2string(operation, operation_size, operation_str)) return;
    field_hdl->operation.setValue(operation_str);
    field_hdl->sendAttributToServer(field_hdl->operation);
  }
  
  void cxios_get_field_operation(field_Ptr field_hdl, char * operation, int operation_size)
  {
    if(!string_copy(field_hdl->operation.getValue(),operation , operation_size))
      ERROR("void cxios_get_field_operation(field_Ptr field_hdl, char * operation, int operation_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_field_prec(field_Ptr field_hdl, int prec)
  {
    field_hdl->prec.setValue(prec);
    field_hdl->sendAttributToServer(field_hdl->prec);
  }
  
  void cxios_get_field_prec(field_Ptr field_hdl, int* prec)
  {
    *prec = field_hdl->prec.getValue();
  }
  
  
  void cxios_set_field_standard_name(field_Ptr field_hdl, const char * standard_name, int standard_name_size)
  {
    std::string standard_name_str;
    if(!cstr2string(standard_name, standard_name_size, standard_name_str)) return;
    field_hdl->standard_name.setValue(standard_name_str);
    field_hdl->sendAttributToServer(field_hdl->standard_name);
  }
  
  void cxios_get_field_standard_name(field_Ptr field_hdl, char * standard_name, int standard_name_size)
  {
    if(!string_copy(field_hdl->standard_name.getValue(),standard_name , standard_name_size))
      ERROR("void cxios_get_field_standard_name(field_Ptr field_hdl, char * standard_name, int standard_name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_field_unit(field_Ptr field_hdl, const char * unit, int unit_size)
  {
    std::string unit_str;
    if(!cstr2string(unit, unit_size, unit_str)) return;
    field_hdl->unit.setValue(unit_str);
    field_hdl->sendAttributToServer(field_hdl->unit);
  }
  
  void cxios_get_field_unit(field_Ptr field_hdl, char * unit, int unit_size)
  {
    if(!string_copy(field_hdl->unit.getValue(),unit , unit_size))
      ERROR("void cxios_get_field_unit(field_Ptr field_hdl, char * unit, int unit_size)", <<"Input string is to short");
  }
  
  
  
}
