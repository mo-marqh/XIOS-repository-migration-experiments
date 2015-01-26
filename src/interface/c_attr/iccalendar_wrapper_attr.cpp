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
  typedef xios::CCalendarWrapper*  calendar_wrapper_Ptr;
  
  void cxios_set_calendar_wrapper_start_date(calendar_wrapper_Ptr calendar_wrapper_hdl, const char * start_date, int start_date_size)
  {
    std::string start_date_str;
    if(!cstr2string(start_date, start_date_size, start_date_str)) return;
     CTimer::get("XIOS").resume();
    calendar_wrapper_hdl->start_date.setValue(start_date_str);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_calendar_wrapper_start_date(calendar_wrapper_Ptr calendar_wrapper_hdl, char * start_date, int start_date_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(calendar_wrapper_hdl->start_date.getInheritedValue(),start_date , start_date_size))
      ERROR("void cxios_get_calendar_wrapper_start_date(calendar_wrapper_Ptr calendar_wrapper_hdl, char * start_date, int start_date_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_calendar_wrapper_start_date(calendar_wrapper_Ptr calendar_wrapper_hdl )
  {
     CTimer::get("XIOS").resume();
    return calendar_wrapper_hdl->start_date.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_calendar_wrapper_time_origin(calendar_wrapper_Ptr calendar_wrapper_hdl, const char * time_origin, int time_origin_size)
  {
    std::string time_origin_str;
    if(!cstr2string(time_origin, time_origin_size, time_origin_str)) return;
     CTimer::get("XIOS").resume();
    calendar_wrapper_hdl->time_origin.setValue(time_origin_str);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_calendar_wrapper_time_origin(calendar_wrapper_Ptr calendar_wrapper_hdl, char * time_origin, int time_origin_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(calendar_wrapper_hdl->time_origin.getInheritedValue(),time_origin , time_origin_size))
      ERROR("void cxios_get_calendar_wrapper_time_origin(calendar_wrapper_Ptr calendar_wrapper_hdl, char * time_origin, int time_origin_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_calendar_wrapper_time_origin(calendar_wrapper_Ptr calendar_wrapper_hdl )
  {
     CTimer::get("XIOS").resume();
    return calendar_wrapper_hdl->time_origin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_calendar_wrapper_timestep(calendar_wrapper_Ptr calendar_wrapper_hdl, cxios_duration timestep_c)
  {
    CTimer::get("XIOS").resume();
    calendar_wrapper_hdl->timestep.allocate();
    CDuration& timestep = calendar_wrapper_hdl->timestep.get();
    timestep.year = timestep_c.year;
    timestep.month = timestep_c.month;
    timestep.day = timestep_c.day;
    timestep.hour = timestep_c.hour;
    timestep.minute = timestep_c.minute;
    timestep.second = timestep_c.second;
    timestep.timestep = timestep_c.timestep;
    CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_calendar_wrapper_timestep(calendar_wrapper_Ptr calendar_wrapper_hdl, cxios_duration* timestep_c)
  {
    CTimer::get("XIOS").resume();
    CDuration timestep = calendar_wrapper_hdl->timestep.getInheritedValue();
    timestep_c->year = timestep.year;
    timestep_c->month = timestep.month;
    timestep_c->day = timestep.day;
    timestep_c->hour = timestep.hour;
    timestep_c->minute = timestep.minute;
    timestep_c->second = timestep.second;
    timestep_c->timestep = timestep.timestep;
    CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_calendar_wrapper_timestep(calendar_wrapper_Ptr calendar_wrapper_hdl )
  {
     CTimer::get("XIOS").resume();
    return calendar_wrapper_hdl->timestep.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_calendar_wrapper_type(calendar_wrapper_Ptr calendar_wrapper_hdl, const char * type, int type_size)
  {
    std::string type_str;
    if(!cstr2string(type, type_size, type_str)) return;
     CTimer::get("XIOS").resume();
    calendar_wrapper_hdl->type.fromString(type_str);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_calendar_wrapper_type(calendar_wrapper_Ptr calendar_wrapper_hdl, char * type, int type_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(calendar_wrapper_hdl->type.getInheritedStringValue(),type , type_size))
      ERROR("void cxios_get_calendar_wrapper_type(calendar_wrapper_Ptr calendar_wrapper_hdl, char * type, int type_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_calendar_wrapper_type(calendar_wrapper_Ptr calendar_wrapper_hdl )
  {
     CTimer::get("XIOS").resume();
    return calendar_wrapper_hdl->type.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  
}
