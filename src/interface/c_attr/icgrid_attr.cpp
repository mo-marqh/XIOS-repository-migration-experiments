/* ************************************************************************** *
 *               Interface auto generated - do not modify                     *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>
#include "xios.hpp"
#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "icutil.hpp"
#include "icdate.hpp"
#include "timer.hpp"
#include "node_type.hpp"

extern "C"
{
  typedef xios::CGrid* grid_Ptr;

  void cxios_set_grid_axis_domain_order(grid_Ptr grid_hdl, bool* axis_domain_order, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,1> tmp(axis_domain_order, shape(extent[0]), neverDeleteData);
    grid_hdl->axis_domain_order.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_grid_axis_domain_order(grid_Ptr grid_hdl, bool* axis_domain_order, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,1> tmp(axis_domain_order, shape(extent[0]), neverDeleteData);
    tmp=grid_hdl->axis_domain_order.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_grid_axis_domain_order(grid_Ptr grid_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = grid_hdl->axis_domain_order.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_grid_description(grid_Ptr grid_hdl, const char * description, int description_size)
  {
    std::string description_str;
    if (!cstr2string(description, description_size, description_str)) return;
    CTimer::get("XIOS").resume();
    grid_hdl->description.setValue(description_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_grid_description(grid_Ptr grid_hdl, char * description, int description_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(grid_hdl->description.getInheritedValue(), description, description_size))
      ERROR("void cxios_get_grid_description(grid_Ptr grid_hdl, char * description, int description_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_grid_description(grid_Ptr grid_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = grid_hdl->description.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_grid_mask1(grid_Ptr grid_hdl, bool* mask1, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,1> tmp(mask1, shape(extent[0]), neverDeleteData);
    grid_hdl->mask1.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_grid_mask1(grid_Ptr grid_hdl, bool* mask1, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,1> tmp(mask1, shape(extent[0]), neverDeleteData);
    tmp=grid_hdl->mask1.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_grid_mask1(grid_Ptr grid_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = grid_hdl->mask1.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_grid_mask2(grid_Ptr grid_hdl, bool* mask2, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,2> tmp(mask2, shape(extent[0], extent[1]), neverDeleteData);
    grid_hdl->mask2.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_grid_mask2(grid_Ptr grid_hdl, bool* mask2, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,2> tmp(mask2, shape(extent[0], extent[1]), neverDeleteData);
    tmp=grid_hdl->mask2.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_grid_mask2(grid_Ptr grid_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = grid_hdl->mask2.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_grid_mask3(grid_Ptr grid_hdl, bool* mask3, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,3> tmp(mask3, shape(extent[0], extent[1], extent[2]), neverDeleteData);
    grid_hdl->mask3.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_grid_mask3(grid_Ptr grid_hdl, bool* mask3, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,3> tmp(mask3, shape(extent[0], extent[1], extent[2]), neverDeleteData);
    tmp=grid_hdl->mask3.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_grid_mask3(grid_Ptr grid_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = grid_hdl->mask3.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_grid_name(grid_Ptr grid_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if (!cstr2string(name, name_size, name_str)) return;
    CTimer::get("XIOS").resume();
    grid_hdl->name.setValue(name_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_grid_name(grid_Ptr grid_hdl, char * name, int name_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(grid_hdl->name.getInheritedValue(), name, name_size))
      ERROR("void cxios_get_grid_name(grid_Ptr grid_hdl, char * name, int name_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_grid_name(grid_Ptr grid_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = grid_hdl->name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }
}
