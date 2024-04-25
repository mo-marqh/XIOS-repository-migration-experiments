/* ************************************************************************** *
 *               Interface auto generated - do not modify                     *
 * ************************************************************************** */

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
  typedef xios::CRedistributeDomain* redistribute_domain_Ptr;

  void cxios_set_redistribute_domain_index(redistribute_domain_Ptr redistribute_domain_hdl, int* index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(index, shape(extent[0]), neverDeleteData);
    redistribute_domain_hdl->index.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_redistribute_domain_index(redistribute_domain_Ptr redistribute_domain_hdl, int* index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(index, shape(extent[0]), neverDeleteData);
    tmp=redistribute_domain_hdl->index.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_redistribute_domain_index(redistribute_domain_Ptr redistribute_domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = redistribute_domain_hdl->index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_redistribute_domain_mask(redistribute_domain_Ptr redistribute_domain_hdl, bool* mask, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,1> tmp(mask, shape(extent[0]), neverDeleteData);
    redistribute_domain_hdl->mask.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_redistribute_domain_mask(redistribute_domain_Ptr redistribute_domain_hdl, bool* mask, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,1> tmp(mask, shape(extent[0]), neverDeleteData);
    tmp=redistribute_domain_hdl->mask.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_redistribute_domain_mask(redistribute_domain_Ptr redistribute_domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = redistribute_domain_hdl->mask.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_redistribute_domain_type(redistribute_domain_Ptr redistribute_domain_hdl, const char * type, int type_size)
  {
    std::string type_str;
    if (!cstr2string(type, type_size, type_str)) return;
    CTimer::get("XIOS").resume();
    redistribute_domain_hdl->type.fromString(type_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_redistribute_domain_type(redistribute_domain_Ptr redistribute_domain_hdl, char * type, int type_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(redistribute_domain_hdl->type.getInheritedStringValue(), type, type_size))
      ERROR("void cxios_get_redistribute_domain_type(redistribute_domain_Ptr redistribute_domain_hdl, char * type, int type_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_redistribute_domain_type(redistribute_domain_Ptr redistribute_domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = redistribute_domain_hdl->type.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }
}
