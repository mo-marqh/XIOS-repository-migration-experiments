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
  typedef xios::tree::CGridGroup*  gridgroup_Ptr;
  
  void cxios_set_gridgroup_axis_ref(gridgroup_Ptr gridgroup_hdl, const char * axis_ref, int axis_ref_size)
  {
    std::string axis_ref_str;
    if(!cstr2string(axis_ref, axis_ref_size, axis_ref_str)) return;
    gridgroup_hdl->axis_ref.setValue(axis_ref_str);
    gridgroup_hdl->sendAttributToServer(gridgroup_hdl->axis_ref);
  }
  
  void cxios_get_gridgroup_axis_ref(gridgroup_Ptr gridgroup_hdl, char * axis_ref, int axis_ref_size)
  {
    if(!string_copy(gridgroup_hdl->axis_ref.getValue(),axis_ref , axis_ref_size))
      ERROR("void cxios_get_gridgroup_axis_ref(gridgroup_Ptr gridgroup_hdl, char * axis_ref, int axis_ref_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_gridgroup_description(gridgroup_Ptr gridgroup_hdl, const char * description, int description_size)
  {
    std::string description_str;
    if(!cstr2string(description, description_size, description_str)) return;
    gridgroup_hdl->description.setValue(description_str);
    gridgroup_hdl->sendAttributToServer(gridgroup_hdl->description);
  }
  
  void cxios_get_gridgroup_description(gridgroup_Ptr gridgroup_hdl, char * description, int description_size)
  {
    if(!string_copy(gridgroup_hdl->description.getValue(),description , description_size))
      ERROR("void cxios_get_gridgroup_description(gridgroup_Ptr gridgroup_hdl, char * description, int description_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_gridgroup_domain_ref(gridgroup_Ptr gridgroup_hdl, const char * domain_ref, int domain_ref_size)
  {
    std::string domain_ref_str;
    if(!cstr2string(domain_ref, domain_ref_size, domain_ref_str)) return;
    gridgroup_hdl->domain_ref.setValue(domain_ref_str);
    gridgroup_hdl->sendAttributToServer(gridgroup_hdl->domain_ref);
  }
  
  void cxios_get_gridgroup_domain_ref(gridgroup_Ptr gridgroup_hdl, char * domain_ref, int domain_ref_size)
  {
    if(!string_copy(gridgroup_hdl->domain_ref.getValue(),domain_ref , domain_ref_size))
      ERROR("void cxios_get_gridgroup_domain_ref(gridgroup_Ptr gridgroup_hdl, char * domain_ref, int domain_ref_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_gridgroup_group_ref(gridgroup_Ptr gridgroup_hdl, const char * group_ref, int group_ref_size)
  {
    std::string group_ref_str;
    if(!cstr2string(group_ref, group_ref_size, group_ref_str)) return;
    gridgroup_hdl->group_ref.setValue(group_ref_str);
    gridgroup_hdl->sendAttributToServer(gridgroup_hdl->group_ref);
  }
  
  void cxios_get_gridgroup_group_ref(gridgroup_Ptr gridgroup_hdl, char * group_ref, int group_ref_size)
  {
    if(!string_copy(gridgroup_hdl->group_ref.getValue(),group_ref , group_ref_size))
      ERROR("void cxios_get_gridgroup_group_ref(gridgroup_Ptr gridgroup_hdl, char * group_ref, int group_ref_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_gridgroup_name(gridgroup_Ptr gridgroup_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if(!cstr2string(name, name_size, name_str)) return;
    gridgroup_hdl->name.setValue(name_str);
    gridgroup_hdl->sendAttributToServer(gridgroup_hdl->name);
  }
  
  void cxios_get_gridgroup_name(gridgroup_Ptr gridgroup_hdl, char * name, int name_size)
  {
    if(!string_copy(gridgroup_hdl->name.getValue(),name , name_size))
      ERROR("void cxios_get_gridgroup_name(gridgroup_Ptr gridgroup_hdl, char * name, int name_size)", <<"Input string is to short");
  }
  
  
  
}
