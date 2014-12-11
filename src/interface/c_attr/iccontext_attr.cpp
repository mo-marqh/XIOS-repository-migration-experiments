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
  typedef xios::CContext*  context_Ptr;
  
  void cxios_set_context_calendar_type(context_Ptr context_hdl, const char * calendar_type, int calendar_type_size)
  {
    std::string calendar_type_str;
    if(!cstr2string(calendar_type, calendar_type_size, calendar_type_str)) return;
     CTimer::get("XIOS").resume();
    context_hdl->calendar_type.fromString(calendar_type_str);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_context_calendar_type(context_Ptr context_hdl, char * calendar_type, int calendar_type_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(context_hdl->calendar_type.getInheritedStringValue(),calendar_type , calendar_type_size))
      ERROR("void cxios_get_context_calendar_type(context_Ptr context_hdl, char * calendar_type, int calendar_type_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_context_calendar_type(context_Ptr context_hdl )
  {
     CTimer::get("XIOS").resume();
    return context_hdl->calendar_type.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_context_output_dir(context_Ptr context_hdl, const char * output_dir, int output_dir_size)
  {
    std::string output_dir_str;
    if(!cstr2string(output_dir, output_dir_size, output_dir_str)) return;
     CTimer::get("XIOS").resume();
    context_hdl->output_dir.setValue(output_dir_str);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_context_output_dir(context_Ptr context_hdl, char * output_dir, int output_dir_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(context_hdl->output_dir.getInheritedValue(),output_dir , output_dir_size))
      ERROR("void cxios_get_context_output_dir(context_Ptr context_hdl, char * output_dir, int output_dir_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_context_output_dir(context_Ptr context_hdl )
  {
     CTimer::get("XIOS").resume();
    return context_hdl->output_dir.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_context_start_date(context_Ptr context_hdl, cxios_date start_date_c)
  {
    CTimer::get("XIOS").resume();
    context_hdl->start_date.allocate();
    CDate& start_date = context_hdl->start_date.get();
    start_date.setDate(start_date_c.year,
                           start_date_c.month,
                           start_date_c.day,
                           start_date_c.hour,
                           start_date_c.minute,
                           start_date_c.second);
    if (start_date.hasRelCalendar())
      start_date.checkDate();
    CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_context_start_date(context_Ptr context_hdl, cxios_date* start_date_c)
  {
    CTimer::get("XIOS").resume();
    CDate start_date = context_hdl->start_date.getInheritedValue();
    start_date_c->year = start_date.getYear();
    start_date_c->month = start_date.getMonth();
    start_date_c->day = start_date.getDay();
    start_date_c->hour = start_date.getHour();
    start_date_c->minute = start_date.getMinute();
    start_date_c->second = start_date.getSecond();
    CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_context_start_date(context_Ptr context_hdl )
  {
     CTimer::get("XIOS").resume();
    return context_hdl->start_date.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_context_time_origin(context_Ptr context_hdl, cxios_date time_origin_c)
  {
    CTimer::get("XIOS").resume();
    context_hdl->time_origin.allocate();
    CDate& time_origin = context_hdl->time_origin.get();
    time_origin.setDate(time_origin_c.year,
                           time_origin_c.month,
                           time_origin_c.day,
                           time_origin_c.hour,
                           time_origin_c.minute,
                           time_origin_c.second);
    if (time_origin.hasRelCalendar())
      time_origin.checkDate();
    CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_context_time_origin(context_Ptr context_hdl, cxios_date* time_origin_c)
  {
    CTimer::get("XIOS").resume();
    CDate time_origin = context_hdl->time_origin.getInheritedValue();
    time_origin_c->year = time_origin.getYear();
    time_origin_c->month = time_origin.getMonth();
    time_origin_c->day = time_origin.getDay();
    time_origin_c->hour = time_origin.getHour();
    time_origin_c->minute = time_origin.getMinute();
    time_origin_c->second = time_origin.getSecond();
    CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_context_time_origin(context_Ptr context_hdl )
  {
     CTimer::get("XIOS").resume();
    return context_hdl->time_origin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_context_timestep(context_Ptr context_hdl, cxios_duration timestep_c)
  {
    CTimer::get("XIOS").resume();
    context_hdl->timestep.allocate();
    CDuration& timestep = context_hdl->timestep.get();
    timestep.year = timestep_c.year;
    timestep.month = timestep_c.month;
    timestep.day = timestep_c.day;
    timestep.hour = timestep_c.hour;
    timestep.minute = timestep_c.minute;
    timestep.second = timestep_c.second;
    timestep.timestep = timestep_c.timestep;
    CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_context_timestep(context_Ptr context_hdl, cxios_duration* timestep_c)
  {
    CTimer::get("XIOS").resume();
    CDuration timestep = context_hdl->timestep.getInheritedValue();
    timestep_c->year = timestep.year;
    timestep_c->month = timestep.month;
    timestep_c->day = timestep.day;
    timestep_c->hour = timestep.hour;
    timestep_c->minute = timestep.minute;
    timestep_c->second = timestep.second;
    timestep_c->timestep = timestep.timestep;
    CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_context_timestep(context_Ptr context_hdl )
  {
     CTimer::get("XIOS").resume();
    return context_hdl->timestep.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  
}
