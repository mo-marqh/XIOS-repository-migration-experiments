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
  typedef xios::tree::CContext*  context_Ptr;
  
  void cxios_set_context_calendar_type(context_Ptr context_hdl, const char * calendar_type, int calendar_type_size)
  {
    std::string calendar_type_str;
    if(!cstr2string(calendar_type, calendar_type_size, calendar_type_str)) return;
    context_hdl->calendar_type.setValue(calendar_type_str);
    context_hdl->sendAttributToServer(context_hdl->calendar_type);
  }
  
  void cxios_get_context_calendar_type(context_Ptr context_hdl, char * calendar_type, int calendar_type_size)
  {
    if(!string_copy(context_hdl->calendar_type.getValue(),calendar_type , calendar_type_size))
      ERROR("void cxios_get_context_calendar_type(context_Ptr context_hdl, char * calendar_type, int calendar_type_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_context_output_dir(context_Ptr context_hdl, const char * output_dir, int output_dir_size)
  {
    std::string output_dir_str;
    if(!cstr2string(output_dir, output_dir_size, output_dir_str)) return;
    context_hdl->output_dir.setValue(output_dir_str);
    context_hdl->sendAttributToServer(context_hdl->output_dir);
  }
  
  void cxios_get_context_output_dir(context_Ptr context_hdl, char * output_dir, int output_dir_size)
  {
    if(!string_copy(context_hdl->output_dir.getValue(),output_dir , output_dir_size))
      ERROR("void cxios_get_context_output_dir(context_Ptr context_hdl, char * output_dir, int output_dir_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_context_start_date(context_Ptr context_hdl, const char * start_date, int start_date_size)
  {
    std::string start_date_str;
    if(!cstr2string(start_date, start_date_size, start_date_str)) return;
    context_hdl->start_date.setValue(start_date_str);
    context_hdl->sendAttributToServer(context_hdl->start_date);
  }
  
  void cxios_get_context_start_date(context_Ptr context_hdl, char * start_date, int start_date_size)
  {
    if(!string_copy(context_hdl->start_date.getValue(),start_date , start_date_size))
      ERROR("void cxios_get_context_start_date(context_Ptr context_hdl, char * start_date, int start_date_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_context_timestep(context_Ptr context_hdl, const char * timestep, int timestep_size)
  {
    std::string timestep_str;
    if(!cstr2string(timestep, timestep_size, timestep_str)) return;
    context_hdl->timestep.setValue(timestep_str);
    context_hdl->sendAttributToServer(context_hdl->timestep);
  }
  
  void cxios_get_context_timestep(context_Ptr context_hdl, char * timestep, int timestep_size)
  {
    if(!string_copy(context_hdl->timestep.getValue(),timestep , timestep_size))
      ERROR("void cxios_get_context_timestep(context_Ptr context_hdl, char * timestep, int timestep_size)", <<"Input string is to short");
  }
  
  
  
}
