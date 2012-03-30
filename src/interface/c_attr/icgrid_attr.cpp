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
  typedef xios::CGrid*  grid_Ptr;
  
  void cxios_set_grid_axis_ref(grid_Ptr grid_hdl, const char * axis_ref, int axis_ref_size)
  {
    std::string axis_ref_str;
    if(!cstr2string(axis_ref, axis_ref_size, axis_ref_str)) return;
    grid_hdl->axis_ref.setValue(axis_ref_str);
    grid_hdl->sendAttributToServer(grid_hdl->axis_ref);
  }
  
  void cxios_get_grid_axis_ref(grid_Ptr grid_hdl, char * axis_ref, int axis_ref_size)
  {
    if(!string_copy(grid_hdl->axis_ref.getValue(),axis_ref , axis_ref_size))
      ERROR("void cxios_get_grid_axis_ref(grid_Ptr grid_hdl, char * axis_ref, int axis_ref_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_grid_description(grid_Ptr grid_hdl, const char * description, int description_size)
  {
    std::string description_str;
    if(!cstr2string(description, description_size, description_str)) return;
    grid_hdl->description.setValue(description_str);
    grid_hdl->sendAttributToServer(grid_hdl->description);
  }
  
  void cxios_get_grid_description(grid_Ptr grid_hdl, char * description, int description_size)
  {
    if(!string_copy(grid_hdl->description.getValue(),description , description_size))
      ERROR("void cxios_get_grid_description(grid_Ptr grid_hdl, char * description, int description_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_grid_domain_ref(grid_Ptr grid_hdl, const char * domain_ref, int domain_ref_size)
  {
    std::string domain_ref_str;
    if(!cstr2string(domain_ref, domain_ref_size, domain_ref_str)) return;
    grid_hdl->domain_ref.setValue(domain_ref_str);
    grid_hdl->sendAttributToServer(grid_hdl->domain_ref);
  }
  
  void cxios_get_grid_domain_ref(grid_Ptr grid_hdl, char * domain_ref, int domain_ref_size)
  {
    if(!string_copy(grid_hdl->domain_ref.getValue(),domain_ref , domain_ref_size))
      ERROR("void cxios_get_grid_domain_ref(grid_Ptr grid_hdl, char * domain_ref, int domain_ref_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_grid_name(grid_Ptr grid_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if(!cstr2string(name, name_size, name_str)) return;
    grid_hdl->name.setValue(name_str);
    grid_hdl->sendAttributToServer(grid_hdl->name);
  }
  
  void cxios_get_grid_name(grid_Ptr grid_hdl, char * name, int name_size)
  {
    if(!string_copy(grid_hdl->name.getValue(),name , name_size))
      ERROR("void cxios_get_grid_name(grid_Ptr grid_hdl, char * name, int name_size)", <<"Input string is to short");
  }
  
  
  
}
