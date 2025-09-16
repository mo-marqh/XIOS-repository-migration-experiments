from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure
from xios.config import lib, String, Int, NpArrayInt, Double, typecheck
from xios.iduration import Duration
from xios.idate import Date
from typing import Union, Optional
from xios.icalendar_wrapper import CalendarWrapper, get_default_calendar_wrapper_handle, set_time_origin_hdl, get_time_origin_hdl, set_start_date_hdl, get_start_date_hdl, update_calendar_timestep, create_calendar, is_created_calendar
from xios.icalendar_wrapper_attr import set_calendar_wrapper_attr_hdl, get_calendar_wrapper_attr_hdl
import numpy as np

#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_update_calendar.argtypes = [c_int]
lib.cxios_update_calendar.restype = None

lib.cxios_get_current_date.argtypes = [POINTER(Date)]
lib.cxios_get_current_date.restype = None

lib.cxios_get_year_length_in_seconds.argtypes = [c_int]
lib.cxios_get_year_length_in_seconds.restype = c_int

lib.cxios_get_day_length_in_seconds.argtypes = [c_int]
lib.cxios_get_day_length_in_seconds.restype = c_int

#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#



@typecheck
def define_calendar(type : Union[String, str], timestep : Optional[Duration] = None, start_date : Optional[Date] = None, 
                    time_origin : Optional[Date] = None, day_length : Optional[Union[Int, int]] = None,
                    month_lengths : Optional[Union[NpArrayInt, np.ndarray]] = None, year_length : Optional[Union[Int, int]] = None,
                    leap_year_month : Optional[Union[Int, int]] = None, leap_year_drift : Optional[Union[float, Double]] = None,
                    leap_year_drift_offset : Optional[Union[Double, float]] = None):
    """
    Defines a new calendar with the specified properties.

    Parameters:
        type (Union[String, str]): The calendar type identifier.
        timestep (Optional[Duration]): The time step duration (optional).
        start_date (Optional[Date]): The start date of the calendar (optional).
        time_origin (Optional[Date]): The time origin of the calendar (optional).
        day_length (Optional[Union[Int, int]]): Length of a day in seconds (optional).
        month_lengths (Optional[Union[NpArrayInt, np.ndarray]]): Array of month lengths (optional).
        year_length (Optional[Union[Int, int]]): Length of the year in days (optional).
        leap_year_month (Optional[Union[Int, int]]): Month of leap year adjustment (optional).
        leap_year_drift (Optional[Union[float, Double]]): Drift adjustment for leap years (optional).
        leap_year_drift_offset (Optional[Union[Double, float]]): Drift offset for leap years (optional).

    Returns:
        None
    """
                        
    type                                                           = String(type)
    if day_length             is not None : daylength              = Int(day_length)
    if month_lengths          is not None : month_lengths          = NpArrayInt(month_lengths)
    if year_length            is not None : year_length            = Int(year_length)
    if leap_year_month        is not None : leap_year_month        = Int(leap_year_month)
    if leap_year_drift        is not None : leap_year_drift        = Double(leap_year_drift)
    if leap_year_drift_offset is not None : leap_year_drift_offset = Double(leap_year_drift)
    
    calendar = CalendarWrapper()
    get_default_calendar_wrapper_handle(calendar)
    set_calendar_wrapper_attr_hdl(calendar, type = type,  timestep = timestep, day_length = day_length,
    month_lengths = month_lengths, year_length = year_length, leap_year_month = leap_year_month,
    leap_year_drift = leap_year_drift, leap_year_drift_offset = leap_year_drift_offset)
    create_calendar(calendar)
    if start_date  : set_start_date_hdl(calendar, start_date)
    if time_origin : set_time_origin_hdl(calendar, time_origin)

@typecheck
def is_defined_calendar():
    """
    Checks whether the calendar has already been defined.

    Returns:
        bool: True if the calendar is created, False otherwise.
    """
    calendar = CalendarWrapper()
    get_default_calendar_wrapper_handle(calendar)
    return is_created_calendar(calendar)
    
@typecheck
def get_calendar_type(calendar_type : String):
    """
    Gets the type of the current calendar.

    Parameters:
        calendar_type (String): The String object to be filled with the calendar type (passed by reference).

    Returns:
        None
    """
    calendar = CalendarWrapper()
    get_default_calendar_wrapper_handle(calendar)
    get_calendar_wrapper_attr_hdl(calendar, type = calendar_type)
    

@typecheck
def set_timestep(timestep : Duration):
    """
    Sets the calendar's time step.

    Parameters:
        timestep (Duration): The duration to set as the time step.

    Returns:
        None
    """
    calendar = CalendarWrapper()
    get_default_calendar_wrapper_handle(calendar)
    set_calendar_wrapper_attr_hdl(calendar, timestep = timestep)
    update_calendar_timestep(calendar)

@typecheck
def get_timestep(timestep : Duration):
    """
    Gets the calendar's time step.

    Parameters:
        timestep (Duration): The Duration object to be filled with the time step (passed by reference).

    Returns:
        None
    """
    calendar = CalendarWrapper()
    get_default_calendar_wrapper_handle(calendar)
    get_calendar_wrapper_attr_hdl(calendar, timestep = timestep)

@typecheck
def set_start_date(start_date : Date):
    """
    Sets the calendar's start date.

    Parameters:
        start_date (Date): The date to set as the start date.

    Returns:
        None
    """

    calendar = CalendarWrapper()

    get_default_calendar_wrapper_handle(calendar)
    set_start_date_hdl(calendar, start_date)

@typecheck
def set_start_date_dur(start_date : Duration):
    """
    Sets the calendar's start date from a duration.

    Parameters:
        start_date (Duration): The duration to convert into a date and set as the start date.

    Returns:
        None
    """
    calendar = CalendarWrapper()
    start_date_date = date_assign_duration(start_date)
    get_default_calendar_wrapper_handle(calendar)
    set_start_date_hdl(calendar, start_date_date)

@typecheck
def get_start_date(start_date : Date):
    """
    Gets the calendar's start date.

    Parameters:
        start_date (Date): The Date object to be filled with the start date (passed by reference).

    Returns:
        None
    """
    calendar = CalendarWrapper()
    get_default_calendar_wrapper_handle(calendar)
    get_start_date_hdl(calendar, start_date)

@typecheck
def set_time_origin(time_origin : Date):
    """
    Sets the calendar's time origin.

    Parameters:
        time_origin (Date): The date to set as the time origin.

    Returns:
        None
    """
    calendar = CalendarWrapper()
    get_default_calendar_wrapper_handle(calendar)
    set_time_origin_hdl(calendar, time_origin)

@typecheck
def set_time_origin_dur(time_origin : Duration):
    """
    Sets the calendar's time origin from a duration.

    Parameters:
        time_origin (Duration): The duration to convert into a date and set as the time origin.

    Returns:
        None
    """

    calendar = CalendarWrapper()
    time_origin_date = date_assign_duration(time_origin)
    get_default_calendar_wrapper_handle(calendar)
    set_time_origin_hdl(calendar, time_origin_date)

@typecheck
def get_time_origin(time_origin : Date):
    """
    Gets the calendar's time origin.

    Parameters:
        time_origin (Date): The Date object to be filled with the time origin (passed by reference).

    Returns:
        None
    """

    calendar = CalendarWrapper()
    get_default_calendar_wrapper_handle(calendar)
    get_time_origin_hdl(calendar, time_origin)

@typecheck
def update_calendar(step : Union[int, Int]):
    """
    Updates the calendar to the given time step.

    Parameters:
        step (Union[int, Int]): The time step index to update the calendar to. Must be non-negative.

    Returns:
        None

    Raises:
        ValueError: If step is negative.
    """
    if step < 0 : raise ValueError("step should be non negative !")
    step = Int(step)
    step_c = step._c_value
    lib.cxios_update_calendar(step_c)

@typecheck
def get_current_date(current_date : Date):
    """
    Gets the current date of the calendar.

    Parameters:
        current_date (Date): The Date object to be filled with the current date (passed by reference).

    Returns:
        None
    """
    lib.cxios_get_current_date(pointer(current_date))
    
@typecheck
def get_year_length_in_seconds(year : Union[int, c_int]):
    """
    Gets the length of the given year in seconds.

    Parameters:
        year (Union[int, c_int]): The year to query.

    Returns:
        int: The length of the year in seconds.
    """
    year = Int(year)
    year_c = year._c_value
    return int(lib.cxios_get_year_length_in_seconds(year_c))
    
@typecheck
def get_day_length_in_seconds(day : Union[int, c_int]):
    """
    Gets the length of a day in seconds.

    Parameters:
        day (Union[int, c_int]): The day to query.

    Returns:
        int: The length of the day in seconds.
    """
    day = Int(day)
    day_c = day._c_value
    return int(lib.cxios_get_day_length_in_seconds(year_c))
