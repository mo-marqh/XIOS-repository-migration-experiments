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
  typedef xios::CZoomAxis* zoom_axis_Ptr;

  void cxios_set_zoom_axis_begin(zoom_axis_Ptr zoom_axis_hdl, int begin)
  {
    CTimer::get("XIOS").resume();
    zoom_axis_hdl->begin.setValue(begin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_zoom_axis_begin(zoom_axis_Ptr zoom_axis_hdl, int* begin)
  {
    CTimer::get("XIOS").resume();
    *begin = zoom_axis_hdl->begin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_zoom_axis_begin(zoom_axis_Ptr zoom_axis_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = zoom_axis_hdl->begin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_zoom_axis_n(zoom_axis_Ptr zoom_axis_hdl, int n)
  {
    CTimer::get("XIOS").resume();
    zoom_axis_hdl->n.setValue(n);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_zoom_axis_n(zoom_axis_Ptr zoom_axis_hdl, int* n)
  {
    CTimer::get("XIOS").resume();
    *n = zoom_axis_hdl->n.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_zoom_axis_n(zoom_axis_Ptr zoom_axis_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = zoom_axis_hdl->n.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }
}
