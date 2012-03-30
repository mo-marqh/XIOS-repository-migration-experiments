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
  typedef xios::CAxis*  axis_Ptr;
  
  void cxios_set_axis_long_name(axis_Ptr axis_hdl, const char * long_name, int long_name_size)
  {
    std::string long_name_str;
    if(!cstr2string(long_name, long_name_size, long_name_str)) return;
    axis_hdl->long_name.setValue(long_name_str);
    axis_hdl->sendAttributToServer(axis_hdl->long_name);
  }
  
  void cxios_get_axis_long_name(axis_Ptr axis_hdl, char * long_name, int long_name_size)
  {
    if(!string_copy(axis_hdl->long_name.getValue(),long_name , long_name_size))
      ERROR("void cxios_get_axis_long_name(axis_Ptr axis_hdl, char * long_name, int long_name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_axis_name(axis_Ptr axis_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if(!cstr2string(name, name_size, name_str)) return;
    axis_hdl->name.setValue(name_str);
    axis_hdl->sendAttributToServer(axis_hdl->name);
  }
  
  void cxios_get_axis_name(axis_Ptr axis_hdl, char * name, int name_size)
  {
    if(!string_copy(axis_hdl->name.getValue(),name , name_size))
      ERROR("void cxios_get_axis_name(axis_Ptr axis_hdl, char * name, int name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_axis_size(axis_Ptr axis_hdl, int size)
  {
    axis_hdl->size.setValue(size);
    axis_hdl->sendAttributToServer(axis_hdl->size);
  }
  
  void cxios_get_axis_size(axis_Ptr axis_hdl, int* size)
  {
    *size = axis_hdl->size.getValue();
  }
  
  
  void cxios_set_axis_standard_name(axis_Ptr axis_hdl, const char * standard_name, int standard_name_size)
  {
    std::string standard_name_str;
    if(!cstr2string(standard_name, standard_name_size, standard_name_str)) return;
    axis_hdl->standard_name.setValue(standard_name_str);
    axis_hdl->sendAttributToServer(axis_hdl->standard_name);
  }
  
  void cxios_get_axis_standard_name(axis_Ptr axis_hdl, char * standard_name, int standard_name_size)
  {
    if(!string_copy(axis_hdl->standard_name.getValue(),standard_name , standard_name_size))
      ERROR("void cxios_get_axis_standard_name(axis_Ptr axis_hdl, char * standard_name, int standard_name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_axis_unit(axis_Ptr axis_hdl, const char * unit, int unit_size)
  {
    std::string unit_str;
    if(!cstr2string(unit, unit_size, unit_str)) return;
    axis_hdl->unit.setValue(unit_str);
    axis_hdl->sendAttributToServer(axis_hdl->unit);
  }
  
  void cxios_get_axis_unit(axis_Ptr axis_hdl, char * unit, int unit_size)
  {
    if(!string_copy(axis_hdl->unit.getValue(),unit , unit_size))
      ERROR("void cxios_get_axis_unit(axis_Ptr axis_hdl, char * unit, int unit_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_axis_value(axis_Ptr axis_hdl, double* value, int extent1)
  {
    ARRAY(double,1) array_tmp(new CArray<double,1>(boost::extents[extent1]));
    std::copy(value, &(value[array_tmp->num_elements()]), array_tmp->data());
    axis_hdl->value.setValue(array_tmp);
    axis_hdl->sendAttributToServer(axis_hdl->value);
  }
  
  void cxios_get_axis_value(axis_Ptr axis_hdl, double* value, int extent1)
  {
    if (!array_copy(axis_hdl->value.getValue(), value, extent1))
     ERROR("void cxios_set_axis_value(axis_Ptr axis_hdl, double* value, int extent1)",<<"Output array size is not conform to array size attribute") ;
  }
  
  
  
}
