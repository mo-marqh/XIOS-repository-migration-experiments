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
  typedef xios::CInterpolateDomain* interpolate_domain_Ptr;

  void cxios_set_interpolate_domain_file(interpolate_domain_Ptr interpolate_domain_hdl, const char * file, int file_size)
  {
    std::string file_str;
    if (!cstr2string(file, file_size, file_str)) return;
    CTimer::get("XIOS").resume();
    interpolate_domain_hdl->file.setValue(file_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_interpolate_domain_file(interpolate_domain_Ptr interpolate_domain_hdl, char * file, int file_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(interpolate_domain_hdl->file.getInheritedValue(), file, file_size))
      ERROR("void cxios_get_interpolate_domain_file(interpolate_domain_Ptr interpolate_domain_hdl, char * file, int file_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_interpolate_domain_file(interpolate_domain_Ptr interpolate_domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = interpolate_domain_hdl->file.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_interpolate_domain_mode(interpolate_domain_Ptr interpolate_domain_hdl, const char * mode, int mode_size)
  {
    std::string mode_str;
    if (!cstr2string(mode, mode_size, mode_str)) return;
    CTimer::get("XIOS").resume();
    interpolate_domain_hdl->mode.fromString(mode_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_interpolate_domain_mode(interpolate_domain_Ptr interpolate_domain_hdl, char * mode, int mode_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(interpolate_domain_hdl->mode.getInheritedStringValue(), mode, mode_size))
      ERROR("void cxios_get_interpolate_domain_mode(interpolate_domain_Ptr interpolate_domain_hdl, char * mode, int mode_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_interpolate_domain_mode(interpolate_domain_Ptr interpolate_domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = interpolate_domain_hdl->mode.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_interpolate_domain_order(interpolate_domain_Ptr interpolate_domain_hdl, int order)
  {
    CTimer::get("XIOS").resume();
    interpolate_domain_hdl->order.setValue(order);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_interpolate_domain_order(interpolate_domain_Ptr interpolate_domain_hdl, int* order)
  {
    CTimer::get("XIOS").resume();
    *order = interpolate_domain_hdl->order.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_interpolate_domain_order(interpolate_domain_Ptr interpolate_domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = interpolate_domain_hdl->order.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_interpolate_domain_renormalize(interpolate_domain_Ptr interpolate_domain_hdl, bool renormalize)
  {
    CTimer::get("XIOS").resume();
    interpolate_domain_hdl->renormalize.setValue(renormalize);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_interpolate_domain_renormalize(interpolate_domain_Ptr interpolate_domain_hdl, bool* renormalize)
  {
    CTimer::get("XIOS").resume();
    *renormalize = interpolate_domain_hdl->renormalize.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_interpolate_domain_renormalize(interpolate_domain_Ptr interpolate_domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = interpolate_domain_hdl->renormalize.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }
}
