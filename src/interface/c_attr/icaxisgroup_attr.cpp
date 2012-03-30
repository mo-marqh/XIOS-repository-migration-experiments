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
  typedef xios::CAxisGroup*  axisgroup_Ptr;
  
  void cxios_set_axisgroup_group_ref(axisgroup_Ptr axisgroup_hdl, const char * group_ref, int group_ref_size)
  {
    std::string group_ref_str;
    if(!cstr2string(group_ref, group_ref_size, group_ref_str)) return;
    axisgroup_hdl->group_ref.setValue(group_ref_str);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->group_ref);
  }
  
  void cxios_get_axisgroup_group_ref(axisgroup_Ptr axisgroup_hdl, char * group_ref, int group_ref_size)
  {
    if(!string_copy(axisgroup_hdl->group_ref.getValue(),group_ref , group_ref_size))
      ERROR("void cxios_get_axisgroup_group_ref(axisgroup_Ptr axisgroup_hdl, char * group_ref, int group_ref_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_axisgroup_long_name(axisgroup_Ptr axisgroup_hdl, const char * long_name, int long_name_size)
  {
    std::string long_name_str;
    if(!cstr2string(long_name, long_name_size, long_name_str)) return;
    axisgroup_hdl->long_name.setValue(long_name_str);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->long_name);
  }
  
  void cxios_get_axisgroup_long_name(axisgroup_Ptr axisgroup_hdl, char * long_name, int long_name_size)
  {
    if(!string_copy(axisgroup_hdl->long_name.getValue(),long_name , long_name_size))
      ERROR("void cxios_get_axisgroup_long_name(axisgroup_Ptr axisgroup_hdl, char * long_name, int long_name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_axisgroup_name(axisgroup_Ptr axisgroup_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if(!cstr2string(name, name_size, name_str)) return;
    axisgroup_hdl->name.setValue(name_str);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->name);
  }
  
  void cxios_get_axisgroup_name(axisgroup_Ptr axisgroup_hdl, char * name, int name_size)
  {
    if(!string_copy(axisgroup_hdl->name.getValue(),name , name_size))
      ERROR("void cxios_get_axisgroup_name(axisgroup_Ptr axisgroup_hdl, char * name, int name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_axisgroup_size(axisgroup_Ptr axisgroup_hdl, int size)
  {
    axisgroup_hdl->size.setValue(size);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->size);
  }
  
  void cxios_get_axisgroup_size(axisgroup_Ptr axisgroup_hdl, int* size)
  {
    *size = axisgroup_hdl->size.getValue();
  }
  
  
  void cxios_set_axisgroup_standard_name(axisgroup_Ptr axisgroup_hdl, const char * standard_name, int standard_name_size)
  {
    std::string standard_name_str;
    if(!cstr2string(standard_name, standard_name_size, standard_name_str)) return;
    axisgroup_hdl->standard_name.setValue(standard_name_str);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->standard_name);
  }
  
  void cxios_get_axisgroup_standard_name(axisgroup_Ptr axisgroup_hdl, char * standard_name, int standard_name_size)
  {
    if(!string_copy(axisgroup_hdl->standard_name.getValue(),standard_name , standard_name_size))
      ERROR("void cxios_get_axisgroup_standard_name(axisgroup_Ptr axisgroup_hdl, char * standard_name, int standard_name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_axisgroup_unit(axisgroup_Ptr axisgroup_hdl, const char * unit, int unit_size)
  {
    std::string unit_str;
    if(!cstr2string(unit, unit_size, unit_str)) return;
    axisgroup_hdl->unit.setValue(unit_str);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->unit);
  }
  
  void cxios_get_axisgroup_unit(axisgroup_Ptr axisgroup_hdl, char * unit, int unit_size)
  {
    if(!string_copy(axisgroup_hdl->unit.getValue(),unit , unit_size))
      ERROR("void cxios_get_axisgroup_unit(axisgroup_Ptr axisgroup_hdl, char * unit, int unit_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_axisgroup_value(axisgroup_Ptr axisgroup_hdl, double* value, int extent1)
  {
    ARRAY(double,1) array_tmp(new CArray<double,1>(boost::extents[extent1]));
    std::copy(value, &(value[array_tmp->num_elements()]), array_tmp->data());
    axisgroup_hdl->value.setValue(array_tmp);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->value);
  }
  
  void cxios_get_axisgroup_value(axisgroup_Ptr axisgroup_hdl, double* value, int extent1)
  {
    if (!array_copy(axisgroup_hdl->value.getValue(), value, extent1))
     ERROR("void cxios_set_axisgroup_value(axisgroup_Ptr axisgroup_hdl, double* value, int extent1)",<<"Output array size is not conform to array size attribute") ;
  }
  
  
  
}
