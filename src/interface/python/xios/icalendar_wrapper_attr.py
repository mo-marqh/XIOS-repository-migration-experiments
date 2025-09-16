# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.icalendar_wrapper import  get_calendar_wrapper_handle
from xios.config import CObject as CalendarWrapper

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_calendar_wrapper_comment.argtypes = [CalendarWrapper, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_calendar_wrapper_comment.restypes = None

lib.cxios_set_calendar_wrapper_comment.argtypes = [CalendarWrapper, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_calendar_wrapper_comment.restypes = None

lib.cxios_is_defined_calendar_wrapper_comment.argtypes = [CalendarWrapper]
lib.cxios_is_defined_calendar_wrapper_comment.restypes = ctypes.c_bool

lib.cxios_get_calendar_wrapper_day_length.argtypes = [CalendarWrapper, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_calendar_wrapper_day_length.restypes = None

lib.cxios_set_calendar_wrapper_day_length.argtypes = [CalendarWrapper, ctypes.c_int]
lib.cxios_set_calendar_wrapper_day_length.restypes = None

lib.cxios_is_defined_calendar_wrapper_day_length.argtypes = [CalendarWrapper]
lib.cxios_is_defined_calendar_wrapper_day_length.restypes = ctypes.c_bool

lib.cxios_get_calendar_wrapper_leap_year_drift.argtypes = [CalendarWrapper, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_calendar_wrapper_leap_year_drift.restypes = None

lib.cxios_set_calendar_wrapper_leap_year_drift.argtypes = [CalendarWrapper, ctypes.c_double]
lib.cxios_set_calendar_wrapper_leap_year_drift.restypes = None

lib.cxios_is_defined_calendar_wrapper_leap_year_drift.argtypes = [CalendarWrapper]
lib.cxios_is_defined_calendar_wrapper_leap_year_drift.restypes = ctypes.c_bool

lib.cxios_get_calendar_wrapper_leap_year_drift_offset.argtypes = [CalendarWrapper, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_calendar_wrapper_leap_year_drift_offset.restypes = None

lib.cxios_set_calendar_wrapper_leap_year_drift_offset.argtypes = [CalendarWrapper, ctypes.c_double]
lib.cxios_set_calendar_wrapper_leap_year_drift_offset.restypes = None

lib.cxios_is_defined_calendar_wrapper_leap_year_drift_offset.argtypes = [CalendarWrapper]
lib.cxios_is_defined_calendar_wrapper_leap_year_drift_offset.restypes = ctypes.c_bool

lib.cxios_get_calendar_wrapper_leap_year_month.argtypes = [CalendarWrapper, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_calendar_wrapper_leap_year_month.restypes = None

lib.cxios_set_calendar_wrapper_leap_year_month.argtypes = [CalendarWrapper, ctypes.c_int]
lib.cxios_set_calendar_wrapper_leap_year_month.restypes = None

lib.cxios_is_defined_calendar_wrapper_leap_year_month.argtypes = [CalendarWrapper]
lib.cxios_is_defined_calendar_wrapper_leap_year_month.restypes = ctypes.c_bool

lib.cxios_get_calendar_wrapper_month_lengths.argtypes = [CalendarWrapper, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_calendar_wrapper_month_lengths.restypes = None

lib.cxios_set_calendar_wrapper_month_lengths.argtypes = [CalendarWrapper, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_calendar_wrapper_month_lengths.restypes = None

lib.cxios_is_defined_calendar_wrapper_month_lengths.argtypes = [CalendarWrapper]
lib.cxios_is_defined_calendar_wrapper_month_lengths.restypes = ctypes.c_bool

lib.cxios_get_calendar_wrapper_start_date.argtypes = [CalendarWrapper, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_calendar_wrapper_start_date.restypes = None

lib.cxios_set_calendar_wrapper_start_date.argtypes = [CalendarWrapper, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_calendar_wrapper_start_date.restypes = None

lib.cxios_is_defined_calendar_wrapper_start_date.argtypes = [CalendarWrapper]
lib.cxios_is_defined_calendar_wrapper_start_date.restypes = ctypes.c_bool

lib.cxios_get_calendar_wrapper_time_origin.argtypes = [CalendarWrapper, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_calendar_wrapper_time_origin.restypes = None

lib.cxios_set_calendar_wrapper_time_origin.argtypes = [CalendarWrapper, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_calendar_wrapper_time_origin.restypes = None

lib.cxios_is_defined_calendar_wrapper_time_origin.argtypes = [CalendarWrapper]
lib.cxios_is_defined_calendar_wrapper_time_origin.restypes = ctypes.c_bool

lib.cxios_get_calendar_wrapper_timestep.argtypes = [CalendarWrapper, ctypes.POINTER(Duration)]
lib.cxios_get_calendar_wrapper_timestep.restypes = None

lib.cxios_set_calendar_wrapper_timestep.argtypes = [CalendarWrapper, Duration]
lib.cxios_set_calendar_wrapper_timestep.restypes = None

lib.cxios_is_defined_calendar_wrapper_timestep.argtypes = [CalendarWrapper]
lib.cxios_is_defined_calendar_wrapper_timestep.restypes = ctypes.c_bool

lib.cxios_get_calendar_wrapper_type.argtypes = [CalendarWrapper, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_calendar_wrapper_type.restypes = None

lib.cxios_set_calendar_wrapper_type.argtypes = [CalendarWrapper, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_calendar_wrapper_type.restypes = None

lib.cxios_is_defined_calendar_wrapper_type.argtypes = [CalendarWrapper]
lib.cxios_is_defined_calendar_wrapper_type.restypes = ctypes.c_bool

lib.cxios_get_calendar_wrapper_year_length.argtypes = [CalendarWrapper, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_calendar_wrapper_year_length.restypes = None

lib.cxios_set_calendar_wrapper_year_length.argtypes = [CalendarWrapper, ctypes.c_int]
lib.cxios_set_calendar_wrapper_year_length.restypes = None

lib.cxios_is_defined_calendar_wrapper_year_length.argtypes = [CalendarWrapper]
lib.cxios_is_defined_calendar_wrapper_year_length.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_calendar_wrapper_attr(calendar_wrapper_id : Union[String, str], comment : Optional[Union[str, String]] = None, day_length : Optional[Union[Int, int]] = None,   
leap_year_drift : Optional[Union[Double, float]] = None, leap_year_drift_offset : Optional[Union[Double, float]] = None,   
leap_year_month : Optional[Union[Int, int]] = None, month_lengths : Optional[Union[np.ndarray, NpArrayInt]] = None,   
start_date : Optional[Union[str, String]] = None, time_origin : Optional[Union[str, String]] = None,   
timestep : Optional[Union[Duration, Duration]] = None, type : Optional[Union[str, String]] = None,   
year_length : Optional[Union[Int, int]] = None):

  
  calendar_wrapper_hdl = CalendarWrapper()
  

  get_calendar_wrapper_handle(calendar_wrapper_id, calendar_wrapper_hdl)
  set_calendar_wrapper_attr_hdl_(calendar_wrapper_hdl, comment, day_length, leap_year_drift, leap_year_drift_offset, leap_year_month,  
   month_lengths, start_date, time_origin, timestep, type, year_length)
  return 



@typecheck
def set_calendar_wrapper_attr_hdl(calendar_wrapper_hdl : CalendarWrapper, comment : Optional[Union[str, String]] = None, day_length : Optional[Union[Int, int]] = None,   
leap_year_drift : Optional[Union[Double, float]] = None, leap_year_drift_offset : Optional[Union[Double, float]] = None,   
leap_year_month : Optional[Union[Int, int]] = None, month_lengths : Optional[Union[np.ndarray, NpArrayInt]] = None,   
start_date : Optional[Union[str, String]] = None, time_origin : Optional[Union[str, String]] = None,   
timestep : Optional[Union[Duration, Duration]] = None, type : Optional[Union[str, String]] = None,   
year_length : Optional[Union[Int, int]] = None):

  
  set_calendar_wrapper_attr_hdl_(calendar_wrapper_hdl, comment, day_length, leap_year_drift, leap_year_drift_offset, leap_year_month,  
   month_lengths, start_date, time_origin, timestep, type, year_length)
  return 



@typecheck
def set_calendar_wrapper_attr_hdl_(calendar_wrapper_hdl : CalendarWrapper, comment_ : Optional[Union[str, String]] = None, day_length_ : Optional[Union[Int, int]] = None,   
leap_year_drift_ : Optional[Union[Double, float]] = None, leap_year_drift_offset_ : Optional[Union[Double, float]] = None,   
leap_year_month_ : Optional[Union[Int, int]] = None, month_lengths_ : Optional[Union[np.ndarray, NpArrayInt]] = None,   
start_date_ : Optional[Union[str, String]] = None, time_origin_ : Optional[Union[str, String]] = None,   
timestep_ : Optional[Union[Duration, Duration]] = None, type_ : Optional[Union[str, String]] = None,   
year_length_ : Optional[Union[Int, int]] = None):

  
  

  if comment_ is not None:
  
    comment_= String(comment_)
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_set_calendar_wrapper_comment(calendar_wrapper_hdl, comment_c, len_comment_c)
    
  

  if day_length_ is not None: 
  
    day_length_ = Int(day_length_)
    day_length_c = day_length_._c_value
    lib.cxios_set_calendar_wrapper_day_length(calendar_wrapper_hdl, day_length_c)
    
  

  if leap_year_drift_ is not None: 
  
    leap_year_drift_ = Double(leap_year_drift_)
    leap_year_drift_c = leap_year_drift_._c_value
    lib.cxios_set_calendar_wrapper_leap_year_drift(calendar_wrapper_hdl, leap_year_drift_c)
    
  

  if leap_year_drift_offset_ is not None: 
  
    leap_year_drift_offset_ = Double(leap_year_drift_offset_)
    leap_year_drift_offset_c = leap_year_drift_offset_._c_value
    lib.cxios_set_calendar_wrapper_leap_year_drift_offset(calendar_wrapper_hdl, leap_year_drift_offset_c)
    
  

  if leap_year_month_ is not None: 
  
    leap_year_month_ = Int(leap_year_month_)
    leap_year_month_c = leap_year_month_._c_value
    lib.cxios_set_calendar_wrapper_leap_year_month(calendar_wrapper_hdl, leap_year_month_c)
    
  

  if month_lengths_ is not None:
    month_lengths_ = NpArrayInt(month_lengths_)
    month_lengths_c = month_lengths_._c_value
    if len(month_lengths_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(month_lengths_.shape)
      shape_c = shape(*month_lengths_.shape)
      lib.cxios_set_calendar_wrapper_month_lengths(calendar_wrapper_hdl, month_lengths_c, shape_c)
  

  if start_date_ is not None:
  
    start_date_= String(start_date_)
    start_date_c = start_date_._c_value
    len_start_date_c = len(ctypes.string_at(start_date_c))
    lib.cxios_set_calendar_wrapper_start_date(calendar_wrapper_hdl, start_date_c, len_start_date_c)
    
  

  if time_origin_ is not None:
  
    time_origin_= String(time_origin_)
    time_origin_c = time_origin_._c_value
    len_time_origin_c = len(ctypes.string_at(time_origin_c))
    lib.cxios_set_calendar_wrapper_time_origin(calendar_wrapper_hdl, time_origin_c, len_time_origin_c)
    
  

  if timestep_ is not None: 
  
    timestep_c = timestep_
    lib.cxios_set_calendar_wrapper_timestep(calendar_wrapper_hdl, timestep_c)
    
  

  if type_ is not None:
  
    type_= String(type_)
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_set_calendar_wrapper_type(calendar_wrapper_hdl, type_c, len_type_c)
    
  

  if year_length_ is not None: 
  
    year_length_ = Int(year_length_)
    year_length_c = year_length_._c_value
    lib.cxios_set_calendar_wrapper_year_length(calendar_wrapper_hdl, year_length_c)
    
  
  return 



@typecheck
def get_calendar_wrapper_attr(calendar_wrapper_id : Union[String, str], comment : Optional[String] = None, day_length : Optional[Int] = None,   
leap_year_drift : Optional[Double] = None, leap_year_drift_offset : Optional[Double] = None,   
leap_year_month : Optional[Int] = None, month_lengths : Optional[NpArrayInt] = None, start_date : Optional[String] = None,   
time_origin : Optional[String] = None, timestep : Optional[Duration] = None, type : Optional[String] = None,   
year_length : Optional[Int] = None):

  
  calendar_wrapper_hdl = CalendarWrapper()
  

  get_calendar_wrapper_handle(calendar_wrapper_id, calendar_wrapper_hdl)
  get_calendar_wrapper_attr_hdl_(calendar_wrapper_hdl, comment, day_length, leap_year_drift, leap_year_drift_offset, leap_year_month,  
   month_lengths, start_date, time_origin, timestep, type, year_length)
  return 



@typecheck
def get_calendar_wrapper_attr_hdl(calendar_wrapper_hdl : CalendarWrapper, comment : Optional[String] = None, day_length : Optional[Int] = None,   
leap_year_drift : Optional[Double] = None, leap_year_drift_offset : Optional[Double] = None,   
leap_year_month : Optional[Int] = None, month_lengths : Optional[NpArrayInt] = None, start_date : Optional[String] = None,   
time_origin : Optional[String] = None, timestep : Optional[Duration] = None, type : Optional[String] = None,   
year_length : Optional[Int] = None):

  
  get_calendar_wrapper_attr_hdl_(calendar_wrapper_hdl, comment, day_length, leap_year_drift, leap_year_drift_offset, leap_year_month,  
   month_lengths, start_date, time_origin, timestep, type, year_length)
  return 



@typecheck
def get_calendar_wrapper_attr_hdl_(calendar_wrapper_hdl : CalendarWrapper, comment_ : Optional[String] = None, day_length_ : Optional[Int] = None,   
leap_year_drift_ : Optional[Double] = None, leap_year_drift_offset_ : Optional[Double] = None,   
leap_year_month_ : Optional[Int] = None, month_lengths_ : Optional[NpArrayInt] = None, start_date_ : Optional[String] = None,   
time_origin_ : Optional[String] = None, timestep_ : Optional[Duration] = None, type_ : Optional[String] = None,   
year_length_ : Optional[Int] = None):

  
  

  if comment_ is not None:
  
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_get_calendar_wrapper_comment(calendar_wrapper_hdl, comment_c, len_comment_c)
    
  

  if day_length_ is not None: 
  
    day_length_c = day_length_._c_value
    lib.cxios_get_calendar_wrapper_day_length(calendar_wrapper_hdl, day_length_c)
    
  

  if leap_year_drift_ is not None: 
  
    leap_year_drift_c = leap_year_drift_._c_value
    lib.cxios_get_calendar_wrapper_leap_year_drift(calendar_wrapper_hdl, leap_year_drift_c)
    
  

  if leap_year_drift_offset_ is not None: 
  
    leap_year_drift_offset_c = leap_year_drift_offset_._c_value
    lib.cxios_get_calendar_wrapper_leap_year_drift_offset(calendar_wrapper_hdl, leap_year_drift_offset_c)
    
  

  if leap_year_month_ is not None: 
  
    leap_year_month_c = leap_year_month_._c_value
    lib.cxios_get_calendar_wrapper_leap_year_month(calendar_wrapper_hdl, leap_year_month_c)
    
  

  if month_lengths_ is not None:
    if month_lengths_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_calendar_wrapper_month_lengths_shape(calendar_wrapper_hdl, shape_c)
      month_lengths_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_calendar_wrapper_month_lengths(calendar_wrapper_hdl, month_lengths_._c_value, shape_c)
    else: 
      month_lengths_c = month_lengths_._c_value
      if len(month_lengths_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(month_lengths_.shape)._c_value
        lib.cxios_get_calendar_wrapper_month_lengths(calendar_wrapper_hdl, month_lengths_c, shape_c)
  

  if start_date_ is not None:
  
    start_date_c = start_date_._c_value
    len_start_date_c = len(ctypes.string_at(start_date_c))
    lib.cxios_get_calendar_wrapper_start_date(calendar_wrapper_hdl, start_date_c, len_start_date_c)
    
  

  if time_origin_ is not None:
  
    time_origin_c = time_origin_._c_value
    len_time_origin_c = len(ctypes.string_at(time_origin_c))
    lib.cxios_get_calendar_wrapper_time_origin(calendar_wrapper_hdl, time_origin_c, len_time_origin_c)
    
  

  if timestep_ is not None: 
  
    timestep_c = ctypes.pointer(timestep_)
    # DATE ?
    lib.cxios_get_calendar_wrapper_timestep(calendar_wrapper_hdl, timestep_c)
    
  

  if type_ is not None:
  
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_get_calendar_wrapper_type(calendar_wrapper_hdl, type_c, len_type_c)
    
  

  if year_length_ is not None: 
  
    year_length_c = year_length_._c_value
    lib.cxios_get_calendar_wrapper_year_length(calendar_wrapper_hdl, year_length_c)
    
  
  return 



@typecheck
def is_defined_calendar_wrapper_attr(calendar_wrapper_id : String, comment : Optional[Bool] = None, day_length : Optional[Bool] = None,   
leap_year_drift : Optional[Bool] = None, leap_year_drift_offset : Optional[Bool] = None, leap_year_month : Optional[Bool] = None,   
month_lengths : Optional[Bool] = None, start_date : Optional[Bool] = None, time_origin : Optional[Bool] = None,   
timestep : Optional[Bool] = None, type : Optional[Bool] = None, year_length : Optional[Bool] = None):

  
  calendar_wrapper_hdl = CalendarWrapper()
  

  get_calendar_wrapper_handle(calendar_wrapper_id, calendar_wrapper_hdl)
  is_defined_calendar_wrapper_attr_hdl_(calendar_wrapper_hdl, comment, day_length, leap_year_drift, leap_year_drift_offset, leap_year_month,  
   month_lengths, start_date, time_origin, timestep, type, year_length)
  return 



@typecheck
def is_defined_calendar_wrapper_attr_hdl(calendar_wrapper_hdl : CalendarWrapper, comment : Optional[Bool] = None, day_length : Optional[Bool] = None,   
leap_year_drift : Optional[Bool] = None, leap_year_drift_offset : Optional[Bool] = None, leap_year_month : Optional[Bool] = None,   
month_lengths : Optional[Bool] = None, start_date : Optional[Bool] = None, time_origin : Optional[Bool] = None,   
timestep : Optional[Bool] = None, type : Optional[Bool] = None, year_length : Optional[Bool] = None):

  
  is_defined_calendar_wrapper_attr_hdl_(calendar_wrapper_hdl, comment, day_length, leap_year_drift, leap_year_drift_offset, leap_year_month,  
   month_lengths, start_date, time_origin, timestep, type, year_length)
  return 



@typecheck
def is_defined_calendar_wrapper_attr_hdl_(calendar_wrapper_hdl : CalendarWrapper, comment_ : Optional[Bool] = None, day_length_ : Optional[Bool] = None,   
leap_year_drift_ : Optional[Bool] = None, leap_year_drift_offset_ : Optional[Bool] = None,   
leap_year_month_ : Optional[Bool] = None, month_lengths_ : Optional[Bool] = None, start_date_ : Optional[Bool] = None,   
time_origin_ : Optional[Bool] = None, timestep_ : Optional[Bool] = None, type_ : Optional[Bool] = None,   
year_length_ : Optional[Bool] = None):

  
  

  if comment_  is not None:
    comment_c = lib.cxios_is_defined_calendar_wrapper_comment(calendar_wrapper_hdl)
    comment_._c_value = ctypes.c_bool(comment_c)
    
  

  if day_length_  is not None:
    day_length_c = lib.cxios_is_defined_calendar_wrapper_day_length(calendar_wrapper_hdl)
    day_length_._c_value = ctypes.c_bool(day_length_c)
    
  

  if leap_year_drift_  is not None:
    leap_year_drift_c = lib.cxios_is_defined_calendar_wrapper_leap_year_drift(calendar_wrapper_hdl)
    leap_year_drift_._c_value = ctypes.c_bool(leap_year_drift_c)
    
  

  if leap_year_drift_offset_  is not None:
    leap_year_drift_offset_c = lib.cxios_is_defined_calendar_wrapper_leap_year_drift_offset(calendar_wrapper_hdl)
    leap_year_drift_offset_._c_value = ctypes.c_bool(leap_year_drift_offset_c)
    
  

  if leap_year_month_  is not None:
    leap_year_month_c = lib.cxios_is_defined_calendar_wrapper_leap_year_month(calendar_wrapper_hdl)
    leap_year_month_._c_value = ctypes.c_bool(leap_year_month_c)
    
  

  if month_lengths_  is not None:
    month_lengths_c = lib.cxios_is_defined_calendar_wrapper_month_lengths(calendar_wrapper_hdl)
    month_lengths_._c_value = ctypes.c_bool(month_lengths_c)
    
  

  if start_date_  is not None:
    start_date_c = lib.cxios_is_defined_calendar_wrapper_start_date(calendar_wrapper_hdl)
    start_date_._c_value = ctypes.c_bool(start_date_c)
    
  

  if time_origin_  is not None:
    time_origin_c = lib.cxios_is_defined_calendar_wrapper_time_origin(calendar_wrapper_hdl)
    time_origin_._c_value = ctypes.c_bool(time_origin_c)
    
  

  if timestep_  is not None:
    timestep_c = lib.cxios_is_defined_calendar_wrapper_timestep(calendar_wrapper_hdl)
    timestep_._c_value = ctypes.c_bool(timestep_c)
    
  

  if type_  is not None:
    type_c = lib.cxios_is_defined_calendar_wrapper_type(calendar_wrapper_hdl)
    type_._c_value = ctypes.c_bool(type_c)
    
  

  if year_length_  is not None:
    year_length_c = lib.cxios_is_defined_calendar_wrapper_year_length(calendar_wrapper_hdl)
    year_length_._c_value = ctypes.c_bool(year_length_c)
    
  
  return 



