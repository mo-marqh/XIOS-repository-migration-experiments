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
  typedef xios::CZoomDomain* zoom_domain_Ptr;

  void cxios_set_zoom_domain_zoom_ibegin(zoom_domain_Ptr zoom_domain_hdl, int zoom_ibegin)
  {
    CTimer::get("XIOS").resume();
    zoom_domain_hdl->zoom_ibegin.setValue(zoom_ibegin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_zoom_domain_zoom_ibegin(zoom_domain_Ptr zoom_domain_hdl, int* zoom_ibegin)
  {
    CTimer::get("XIOS").resume();
    *zoom_ibegin = zoom_domain_hdl->zoom_ibegin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_zoom_domain_zoom_ibegin(zoom_domain_Ptr zoom_domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = zoom_domain_hdl->zoom_ibegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_zoom_domain_zoom_jbegin(zoom_domain_Ptr zoom_domain_hdl, int zoom_jbegin)
  {
    CTimer::get("XIOS").resume();
    zoom_domain_hdl->zoom_jbegin.setValue(zoom_jbegin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_zoom_domain_zoom_jbegin(zoom_domain_Ptr zoom_domain_hdl, int* zoom_jbegin)
  {
    CTimer::get("XIOS").resume();
    *zoom_jbegin = zoom_domain_hdl->zoom_jbegin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_zoom_domain_zoom_jbegin(zoom_domain_Ptr zoom_domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = zoom_domain_hdl->zoom_jbegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_zoom_domain_zoom_ni(zoom_domain_Ptr zoom_domain_hdl, int zoom_ni)
  {
    CTimer::get("XIOS").resume();
    zoom_domain_hdl->zoom_ni.setValue(zoom_ni);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_zoom_domain_zoom_ni(zoom_domain_Ptr zoom_domain_hdl, int* zoom_ni)
  {
    CTimer::get("XIOS").resume();
    *zoom_ni = zoom_domain_hdl->zoom_ni.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_zoom_domain_zoom_ni(zoom_domain_Ptr zoom_domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = zoom_domain_hdl->zoom_ni.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_zoom_domain_zoom_nj(zoom_domain_Ptr zoom_domain_hdl, int zoom_nj)
  {
    CTimer::get("XIOS").resume();
    zoom_domain_hdl->zoom_nj.setValue(zoom_nj);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_zoom_domain_zoom_nj(zoom_domain_Ptr zoom_domain_hdl, int* zoom_nj)
  {
    CTimer::get("XIOS").resume();
    *zoom_nj = zoom_domain_hdl->zoom_nj.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_zoom_domain_zoom_nj(zoom_domain_Ptr zoom_domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = zoom_domain_hdl->zoom_nj.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }
}
