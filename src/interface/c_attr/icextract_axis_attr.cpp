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
  typedef xios::CExtractAxis* extract_axis_Ptr;

  void cxios_set_extract_axis_begin(extract_axis_Ptr extract_axis_hdl, int begin)
  {
    CTimer::get("XIOS").resume();
    extract_axis_hdl->begin.setValue(begin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_extract_axis_begin(extract_axis_Ptr extract_axis_hdl, int* begin)
  {
    CTimer::get("XIOS").resume();
    *begin = extract_axis_hdl->begin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_extract_axis_begin(extract_axis_Ptr extract_axis_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = extract_axis_hdl->begin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_extract_axis_index(extract_axis_Ptr extract_axis_hdl, int* index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(index, shape(extent[0]), neverDeleteData);
    extract_axis_hdl->index.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_extract_axis_index(extract_axis_Ptr extract_axis_hdl, int* index, int* extent)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(index, shape(extent[0]), neverDeleteData);
    tmp=extract_axis_hdl->index.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_extract_axis_index(extract_axis_Ptr extract_axis_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = extract_axis_hdl->index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_extract_axis_n(extract_axis_Ptr extract_axis_hdl, int n)
  {
    CTimer::get("XIOS").resume();
    extract_axis_hdl->n.setValue(n);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_extract_axis_n(extract_axis_Ptr extract_axis_hdl, int* n)
  {
    CTimer::get("XIOS").resume();
    *n = extract_axis_hdl->n.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_extract_axis_n(extract_axis_Ptr extract_axis_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = extract_axis_hdl->n.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }
}
