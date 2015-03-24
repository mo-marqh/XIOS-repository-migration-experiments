/* ************************************************************************** *
 *               Interface auto generated - do not modify                     *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>
#include "xmlioserver.hpp"
#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "icutil.hpp"
#include "icdate.hpp"
#include "timer.hpp"
#include "node_type.hpp"

extern "C"
{
  typedef xios::CAxisGroup* axisgroup_Ptr;

  void cxios_set_axisgroup_axis_ref(axisgroup_Ptr axisgroup_hdl, const char * axis_ref, int axis_ref_size)
  {
    std::string axis_ref_str;
    if (!cstr2string(axis_ref, axis_ref_size, axis_ref_str)) return;
    CTimer::get("XIOS").resume();
    axisgroup_hdl->axis_ref.setValue(axis_ref_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_axis_ref(axisgroup_Ptr axisgroup_hdl, char * axis_ref, int axis_ref_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(axisgroup_hdl->axis_ref.getInheritedValue(), axis_ref, axis_ref_size))
      ERROR("void cxios_get_axisgroup_axis_ref(axisgroup_Ptr axisgroup_hdl, char * axis_ref, int axis_ref_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_axis_ref(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->axis_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_data_begin(axisgroup_Ptr axisgroup_hdl, int data_begin)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->data_begin.setValue(data_begin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_data_begin(axisgroup_Ptr axisgroup_hdl, int* data_begin)
  {
    CTimer::get("XIOS").resume();
    *data_begin = axisgroup_hdl->data_begin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_data_begin(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->data_begin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_data_index(axisgroup_Ptr axisgroup_hdl, int* data_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_index, shape(extent1), neverDeleteData);
    axisgroup_hdl->data_index.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_data_index(axisgroup_Ptr axisgroup_hdl, int* data_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_index, shape(extent1), neverDeleteData);
    tmp=axisgroup_hdl->data_index.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_data_index(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->data_index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_data_n(axisgroup_Ptr axisgroup_hdl, int data_n)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->data_n.setValue(data_n);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_data_n(axisgroup_Ptr axisgroup_hdl, int* data_n)
  {
    CTimer::get("XIOS").resume();
    *data_n = axisgroup_hdl->data_n.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_data_n(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->data_n.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_group_ref(axisgroup_Ptr axisgroup_hdl, const char * group_ref, int group_ref_size)
  {
    std::string group_ref_str;
    if (!cstr2string(group_ref, group_ref_size, group_ref_str)) return;
    CTimer::get("XIOS").resume();
    axisgroup_hdl->group_ref.setValue(group_ref_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_group_ref(axisgroup_Ptr axisgroup_hdl, char * group_ref, int group_ref_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(axisgroup_hdl->group_ref.getInheritedValue(), group_ref, group_ref_size))
      ERROR("void cxios_get_axisgroup_group_ref(axisgroup_Ptr axisgroup_hdl, char * group_ref, int group_ref_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_group_ref(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->group_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_ibegin(axisgroup_Ptr axisgroup_hdl, int ibegin)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->ibegin.setValue(ibegin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_ibegin(axisgroup_Ptr axisgroup_hdl, int* ibegin)
  {
    CTimer::get("XIOS").resume();
    *ibegin = axisgroup_hdl->ibegin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_ibegin(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->ibegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_long_name(axisgroup_Ptr axisgroup_hdl, const char * long_name, int long_name_size)
  {
    std::string long_name_str;
    if (!cstr2string(long_name, long_name_size, long_name_str)) return;
    CTimer::get("XIOS").resume();
    axisgroup_hdl->long_name.setValue(long_name_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_long_name(axisgroup_Ptr axisgroup_hdl, char * long_name, int long_name_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(axisgroup_hdl->long_name.getInheritedValue(), long_name, long_name_size))
      ERROR("void cxios_get_axisgroup_long_name(axisgroup_Ptr axisgroup_hdl, char * long_name, int long_name_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_long_name(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->long_name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_mask(axisgroup_Ptr axisgroup_hdl, bool* mask, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,1> tmp(mask, shape(extent1), neverDeleteData);
    axisgroup_hdl->mask.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_mask(axisgroup_Ptr axisgroup_hdl, bool* mask, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,1> tmp(mask, shape(extent1), neverDeleteData);
    tmp=axisgroup_hdl->mask.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_mask(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->mask.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_name(axisgroup_Ptr axisgroup_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if (!cstr2string(name, name_size, name_str)) return;
    CTimer::get("XIOS").resume();
    axisgroup_hdl->name.setValue(name_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_name(axisgroup_Ptr axisgroup_hdl, char * name, int name_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(axisgroup_hdl->name.getInheritedValue(), name, name_size))
      ERROR("void cxios_get_axisgroup_name(axisgroup_Ptr axisgroup_hdl, char * name, int name_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_name(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_ni(axisgroup_Ptr axisgroup_hdl, int ni)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->ni.setValue(ni);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_ni(axisgroup_Ptr axisgroup_hdl, int* ni)
  {
    CTimer::get("XIOS").resume();
    *ni = axisgroup_hdl->ni.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_ni(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->ni.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_positive(axisgroup_Ptr axisgroup_hdl, const char * positive, int positive_size)
  {
    std::string positive_str;
    if (!cstr2string(positive, positive_size, positive_str)) return;
    CTimer::get("XIOS").resume();
    axisgroup_hdl->positive.fromString(positive_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_positive(axisgroup_Ptr axisgroup_hdl, char * positive, int positive_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(axisgroup_hdl->positive.getInheritedStringValue(), positive, positive_size))
      ERROR("void cxios_get_axisgroup_positive(axisgroup_Ptr axisgroup_hdl, char * positive, int positive_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_positive(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->positive.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_size(axisgroup_Ptr axisgroup_hdl, int size)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->size.setValue(size);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_size(axisgroup_Ptr axisgroup_hdl, int* size)
  {
    CTimer::get("XIOS").resume();
    *size = axisgroup_hdl->size.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_size(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->size.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_standard_name(axisgroup_Ptr axisgroup_hdl, const char * standard_name, int standard_name_size)
  {
    std::string standard_name_str;
    if (!cstr2string(standard_name, standard_name_size, standard_name_str)) return;
    CTimer::get("XIOS").resume();
    axisgroup_hdl->standard_name.setValue(standard_name_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_standard_name(axisgroup_Ptr axisgroup_hdl, char * standard_name, int standard_name_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(axisgroup_hdl->standard_name.getInheritedValue(), standard_name, standard_name_size))
      ERROR("void cxios_get_axisgroup_standard_name(axisgroup_Ptr axisgroup_hdl, char * standard_name, int standard_name_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_standard_name(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->standard_name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_unit(axisgroup_Ptr axisgroup_hdl, const char * unit, int unit_size)
  {
    std::string unit_str;
    if (!cstr2string(unit, unit_size, unit_str)) return;
    CTimer::get("XIOS").resume();
    axisgroup_hdl->unit.setValue(unit_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_unit(axisgroup_Ptr axisgroup_hdl, char * unit, int unit_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(axisgroup_hdl->unit.getInheritedValue(), unit, unit_size))
      ERROR("void cxios_get_axisgroup_unit(axisgroup_Ptr axisgroup_hdl, char * unit, int unit_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_unit(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->unit.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_value(axisgroup_Ptr axisgroup_hdl, double* value, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(value, shape(extent1), neverDeleteData);
    axisgroup_hdl->value.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_value(axisgroup_Ptr axisgroup_hdl, double* value, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(value, shape(extent1), neverDeleteData);
    tmp=axisgroup_hdl->value.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_value(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->value.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_zoom_begin(axisgroup_Ptr axisgroup_hdl, int zoom_begin)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->zoom_begin.setValue(zoom_begin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_zoom_begin(axisgroup_Ptr axisgroup_hdl, int* zoom_begin)
  {
    CTimer::get("XIOS").resume();
    *zoom_begin = axisgroup_hdl->zoom_begin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_zoom_begin(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->zoom_begin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_zoom_end(axisgroup_Ptr axisgroup_hdl, int zoom_end)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->zoom_end.setValue(zoom_end);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_zoom_end(axisgroup_Ptr axisgroup_hdl, int* zoom_end)
  {
    CTimer::get("XIOS").resume();
    *zoom_end = axisgroup_hdl->zoom_end.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_zoom_end(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->zoom_end.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_axisgroup_zoom_size(axisgroup_Ptr axisgroup_hdl, int zoom_size)
  {
    CTimer::get("XIOS").resume();
    axisgroup_hdl->zoom_size.setValue(zoom_size);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_axisgroup_zoom_size(axisgroup_Ptr axisgroup_hdl, int* zoom_size)
  {
    CTimer::get("XIOS").resume();
    *zoom_size = axisgroup_hdl->zoom_size.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_axisgroup_zoom_size(axisgroup_Ptr axisgroup_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = axisgroup_hdl->zoom_size.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }
}
