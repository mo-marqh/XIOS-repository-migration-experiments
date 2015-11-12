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

  void cxios_set_zoom_axis_zoom_begin(zoom_axis_Ptr zoom_axis_hdl, int zoom_begin)
  {
    CTimer::get("XIOS").resume();
    zoom_axis_hdl->zoom_begin.setValue(zoom_begin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_zoom_axis_zoom_begin(zoom_axis_Ptr zoom_axis_hdl, int* zoom_begin)
  {
    CTimer::get("XIOS").resume();
    *zoom_begin = zoom_axis_hdl->zoom_begin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_zoom_axis_zoom_begin(zoom_axis_Ptr zoom_axis_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = zoom_axis_hdl->zoom_begin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_zoom_axis_zoom_end(zoom_axis_Ptr zoom_axis_hdl, int zoom_end)
  {
    CTimer::get("XIOS").resume();
    zoom_axis_hdl->zoom_end.setValue(zoom_end);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_zoom_axis_zoom_end(zoom_axis_Ptr zoom_axis_hdl, int* zoom_end)
  {
    CTimer::get("XIOS").resume();
    *zoom_end = zoom_axis_hdl->zoom_end.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_zoom_axis_zoom_end(zoom_axis_Ptr zoom_axis_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = zoom_axis_hdl->zoom_end.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_zoom_axis_zoom_size(zoom_axis_Ptr zoom_axis_hdl, int zoom_size)
  {
    CTimer::get("XIOS").resume();
    zoom_axis_hdl->zoom_size.setValue(zoom_size);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_zoom_axis_zoom_size(zoom_axis_Ptr zoom_axis_hdl, int* zoom_size)
  {
    CTimer::get("XIOS").resume();
    *zoom_size = zoom_axis_hdl->zoom_size.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_zoom_axis_zoom_size(zoom_axis_Ptr zoom_axis_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = zoom_axis_hdl->zoom_size.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }
}
