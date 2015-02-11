/* ************************************************************************** *
 *               Interface auto generated - do not modify                   *
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
  typedef xios::CGridGroup*  gridgroup_Ptr;

  void cxios_set_gridgroup_axisDomOrder(gridgroup_Ptr gridgroup_hdl, bool* axisDomainOrder, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,1> tmp(axisDomainOrder,shape(extent1),neverDeleteData) ;
    gridgroup_hdl->axisDomainOrder.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_gridgroup_axisDomOrder(gridgroup_Ptr gridgroup_hdl, bool* axisDomainOrder, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,1> tmp(axisDomainOrder,shape(extent1),neverDeleteData) ;
    tmp=gridgroup_hdl->axisDomainOrder.getInheritedValue() ;
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_gridgroup_axisDomOrder(gridgroup_Ptr gridgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return gridgroup_hdl->axisDomainOrder.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }



  void cxios_set_gridgroup_description(gridgroup_Ptr gridgroup_hdl, const char * description, int description_size)
  {
    std::string description_str;
    if(!cstr2string(description, description_size, description_str)) return;
     CTimer::get("XIOS").resume();
    gridgroup_hdl->description.setValue(description_str);
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_gridgroup_description(gridgroup_Ptr gridgroup_hdl, char * description, int description_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(gridgroup_hdl->description.getInheritedValue(),description , description_size))
      ERROR("void cxios_get_gridgroup_description(gridgroup_Ptr gridgroup_hdl, char * description, int description_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_gridgroup_description(gridgroup_Ptr gridgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return gridgroup_hdl->description.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }



  void cxios_set_gridgroup_group_ref(gridgroup_Ptr gridgroup_hdl, const char * group_ref, int group_ref_size)
  {
    std::string group_ref_str;
    if(!cstr2string(group_ref, group_ref_size, group_ref_str)) return;
     CTimer::get("XIOS").resume();
    gridgroup_hdl->group_ref.setValue(group_ref_str);
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_gridgroup_group_ref(gridgroup_Ptr gridgroup_hdl, char * group_ref, int group_ref_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(gridgroup_hdl->group_ref.getInheritedValue(),group_ref , group_ref_size))
      ERROR("void cxios_get_gridgroup_group_ref(gridgroup_Ptr gridgroup_hdl, char * group_ref, int group_ref_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_gridgroup_group_ref(gridgroup_Ptr gridgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return gridgroup_hdl->group_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }



  void cxios_set_gridgroup_mask(gridgroup_Ptr gridgroup_hdl, bool* mask, int extent1, int extent2, int extent3)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,3> tmp(mask,shape(extent1,extent2,extent3),neverDeleteData) ;
    gridgroup_hdl->mask.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_gridgroup_mask(gridgroup_Ptr gridgroup_hdl, bool* mask, int extent1, int extent2, int extent3)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,3> tmp(mask,shape(extent1,extent2,extent3),neverDeleteData) ;
    tmp=gridgroup_hdl->mask.getInheritedValue() ;
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_gridgroup_mask(gridgroup_Ptr gridgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return gridgroup_hdl->mask.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }



  void cxios_set_gridgroup_name(gridgroup_Ptr gridgroup_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if(!cstr2string(name, name_size, name_str)) return;
     CTimer::get("XIOS").resume();
    gridgroup_hdl->name.setValue(name_str);
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_gridgroup_name(gridgroup_Ptr gridgroup_hdl, char * name, int name_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(gridgroup_hdl->name.getInheritedValue(),name , name_size))
      ERROR("void cxios_get_gridgroup_name(gridgroup_Ptr gridgroup_hdl, char * name, int name_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_gridgroup_name(gridgroup_Ptr gridgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return gridgroup_hdl->name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }




}
