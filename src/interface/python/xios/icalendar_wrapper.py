from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure, pointer
from typing import Union
from xios.config import lib, typecheck, String
from xios.config import CObject as CalendarWrapper
from xios.iduration import Duration
from xios.idate import Date


#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_calendar_wrapper_handle_create.argtypes = [POINTER(CalendarWrapper), c_char_p, c_int]
lib.cxios_calendar_wrapper_handle_create.restype = None

lib.cxios_get_current_calendar_wrapper.argtypes = [POINTER(CalendarWrapper)]
lib.cxios_get_current_calendar_wrapper.restype = None

lib.cxios_calendar_wrapper_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_calendar_wrapper_valid_id.restype = None

lib.cxios_create_calendar.argtypes = [CalendarWrapper]
lib.cxios_create_calendar.restype = None

lib.cxios_is_created_calendar.argtypes = [CalendarWrapper]
lib.cxios_is_created_calendar.restype = bool

lib.cxios_update_calendar_timestep.argtypes = [CalendarWrapper]
lib.cxios_update_calendar_timestep.restype = None

lib.cxios_set_calendar_wrapper_date_start_date.argtypes = [CalendarWrapper, Date]
lib.cxios_set_calendar_wrapper_date_start_date.restype = None

lib.cxios_get_calendar_wrapper_date_start_date.argtypes = [CalendarWrapper, POINTER(Date)]
lib.cxios_get_calendar_wrapper_date_start_date.restype = None

lib.cxios_set_calendar_wrapper_date_time_origin.argtypes = [CalendarWrapper, Date]
lib.cxios_set_calendar_wrapper_date_time_origin.restype = None

lib.cxios_get_calendar_wrapper_date_time_origin.argtypes = [CalendarWrapper, POINTER(Date)]
lib.cxios_get_calendar_wrapper_date_time_origin.restype = None

#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def get_calendar_wrapper_handle(idt: Union[str, String], ret: CalendarWrapper):
    """
    Gets the calendarwrapper handle with the given calendarwrapper id.

    Parameters:
        idt (Union[str, String]): The calendarwrapper identifier as a string or String object.
        ret (CalendarWrapper): The CalendarWrapper object to be filled with the handle (passed by reference).

    Returns:
        None: The function modifies `ret` in place.
    """
    idt = String(idt)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    lib.cxios_calendar_wrapper_handle_create(pointer(ret), idt_c, len_idt_c)

@typecheck
def get_default_calendar_wrapper_handle(ret: CalendarWrapper):
    """
    Gets the calendarwrapper handle of the current context.

    Parameters:
        ret (CalendarWrapper): The CalendarWrapper object to be filled with the handle (passed by reference).

    Returns:
        None: The function modifies `ret` in place.
    """
    lib.cxios_get_current_calendar_wrapper(pointer(ret))

@typecheck
def is_valid_calendar_wrapper(idt: Union[str, String]) -> bool:
    """
    Check that the calendarwrapper is valid.

    Parameters:
        idt (Union[str, String]): The calendarwrapper identifier as a string or String object.

    Returns:
        bool: True if the axis is valid, False otherwise.
    """
    idt = String(idt)
    is_val = xios.Bool(False)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)
    lib.cxios_calendar_wrapper_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)

@typecheck
def create_calendar(hdl: CalendarWrapper):
    """
    create the calendar from the calendarwrapper handle.

    Parameters:
        hdl (CalendarWrapper): The CalendarWrapper object that is used to create the calendar.

    Returns:
        None: The function create the calendar.
    """
    lib.cxios_create_calendar(hdl)

@typecheck
def is_created_calendar(hdl: CalendarWrapper):
    """
    Check that the calendar has been created.

    Parameters:
        hdl (CalendarWrapper): The CalendarWrapper hdl.

    Returns:
        bool: True if the calendar has been created, False otherwise.
    """
    return lib.cxios_is_created_calendar(hdl)

@typecheck
def update_calendar_timestep(hdl: CalendarWrapper):
    """
    Update the calendar such that it is in synchronization with your simulation.

    Parameters:
        hdl (CalendarWrapper): The CalendarWrapper handle.

    Returns:
        None
    """
    lib.cxios_update_calendar_timestep(hdl)

@typecheck
def set_start_date_hdl(hdl: CalendarWrapper, start_date: Date):
    """
    Sets the calendar start date.

    Parameters:
        hdl (CalendarWrapper): The CalendarWrapper handle.
        start_date (Date) : The date to set as a start_date.

    Returns:
        None
    """
    lib.cxios_set_calendar_wrapper_date_start_date(hdl, start_date)

@typecheck
def get_start_date_hdl(hdl: CalendarWrapper, start_date: Date):
    """
    Gets the calendar start date.

    Parameters:
        hdl (CalendarWrapper): The CalendarWrapper handle.
        start_date (Date) : The date to get as a start_date.

    Returns:
        None
    """
    lib.cxios_get_calendar_wrapper_date_start_date(hdl, pointer(start_date))

@typecheck
def set_time_origin_hdl(hdl: CalendarWrapper, time_origin: Date):
    """
    Sets the calendar time_origin.

    Parameters:
        hdl (CalendarWrapper): The CalendarWrapper handle.
        time_origin (Date) : The date to set as a time_origin.

    Returns:
        None
    """
    lib.cxios_set_calendar_wrapper_date_time_origin(hdl, time_origin)

@typecheck
def get_time_origin_hdl(hdl: CalendarWrapper, time_origin: Date):
    """
    Gets the calendar time_origin.

    Parameters:
        hdl (CalendarWrapper): The CalendarWrapper handle.
        time_origin (Date) : The date to get as a time_origin.

    Returns:
        None
    """
    lib.cxios_get_calendar_wrapper_date_time_origin(hdl, pointer(time_origin))


