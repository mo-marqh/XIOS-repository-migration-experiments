# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.ivariable import  get_variable_handle
from xios.ovariable import Variable

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_variable_name.argtypes = [Variable, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_variable_name.restypes = None

lib.cxios_set_variable_name.argtypes = [Variable, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_variable_name.restypes = None

lib.cxios_is_defined_variable_name.argtypes = [Variable]
lib.cxios_is_defined_variable_name.restypes = ctypes.c_bool

lib.cxios_get_variable_ts_target.argtypes = [Variable, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_variable_ts_target.restypes = None

lib.cxios_set_variable_ts_target.argtypes = [Variable, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_variable_ts_target.restypes = None

lib.cxios_is_defined_variable_ts_target.argtypes = [Variable]
lib.cxios_is_defined_variable_ts_target.restypes = ctypes.c_bool

lib.cxios_get_variable_type.argtypes = [Variable, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_variable_type.restypes = None

lib.cxios_set_variable_type.argtypes = [Variable, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_variable_type.restypes = None

lib.cxios_is_defined_variable_type.argtypes = [Variable]
lib.cxios_is_defined_variable_type.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_variable_attr(variable_id : Union[String, str], name : Optional[Union[str, String]] = None, ts_target : Optional[Union[str, String]] = None,   
type : Optional[Union[str, String]] = None):

  
  variable_hdl = Variable()
  

  get_variable_handle(variable_id, variable_hdl)
  set_variable_attr_hdl_(variable_hdl, name, ts_target, type)
  return 



@typecheck
def set_variable_attr_hdl(variable_hdl : Variable, name : Optional[Union[str, String]] = None, ts_target : Optional[Union[str, String]] = None,   
type : Optional[Union[str, String]] = None):

  
  set_variable_attr_hdl_(variable_hdl, name, ts_target, type)
  return 



@typecheck
def set_variable_attr_hdl_(variable_hdl : Variable, name_ : Optional[Union[str, String]] = None, ts_target_ : Optional[Union[str, String]] = None,   
type_ : Optional[Union[str, String]] = None):

  
  

  if name_ is not None:
  
    name_= String(name_)
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_set_variable_name(variable_hdl, name_c, len_name_c)
    
  

  if ts_target_ is not None:
  
    ts_target_= String(ts_target_)
    ts_target_c = ts_target_._c_value
    len_ts_target_c = len(ctypes.string_at(ts_target_c))
    lib.cxios_set_variable_ts_target(variable_hdl, ts_target_c, len_ts_target_c)
    
  

  if type_ is not None:
  
    type_= String(type_)
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_set_variable_type(variable_hdl, type_c, len_type_c)
    
  
  return 



@typecheck
def get_variable_attr(variable_id : Union[String, str], name : Optional[String] = None, ts_target : Optional[String] = None,   
type : Optional[String] = None):

  
  variable_hdl = Variable()
  

  get_variable_handle(variable_id, variable_hdl)
  get_variable_attr_hdl_(variable_hdl, name, ts_target, type)
  return 



@typecheck
def get_variable_attr_hdl(variable_hdl : Variable, name : Optional[String] = None, ts_target : Optional[String] = None,   
type : Optional[String] = None):

  
  get_variable_attr_hdl_(variable_hdl, name, ts_target, type)
  return 



@typecheck
def get_variable_attr_hdl_(variable_hdl : Variable, name_ : Optional[String] = None, ts_target_ : Optional[String] = None,   
type_ : Optional[String] = None):

  
  

  if name_ is not None:
  
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_get_variable_name(variable_hdl, name_c, len_name_c)
    
  

  if ts_target_ is not None:
  
    ts_target_c = ts_target_._c_value
    len_ts_target_c = len(ctypes.string_at(ts_target_c))
    lib.cxios_get_variable_ts_target(variable_hdl, ts_target_c, len_ts_target_c)
    
  

  if type_ is not None:
  
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_get_variable_type(variable_hdl, type_c, len_type_c)
    
  
  return 



@typecheck
def is_defined_variable_attr(variable_id : String, name : Optional[Bool] = None, ts_target : Optional[Bool] = None, type : Optional[Bool] = None):

  
  variable_hdl = Variable()
  

  get_variable_handle(variable_id, variable_hdl)
  is_defined_variable_attr_hdl_(variable_hdl, name, ts_target, type)
  return 



@typecheck
def is_defined_variable_attr_hdl(variable_hdl : Variable, name : Optional[Bool] = None, ts_target : Optional[Bool] = None,   
type : Optional[Bool] = None):

  
  is_defined_variable_attr_hdl_(variable_hdl, name, ts_target, type)
  return 



@typecheck
def is_defined_variable_attr_hdl_(variable_hdl : Variable, name_ : Optional[Bool] = None, ts_target_ : Optional[Bool] = None,   
type_ : Optional[Bool] = None):

  
  

  if name_  is not None:
    name_c = lib.cxios_is_defined_variable_name(variable_hdl)
    name_._c_value = ctypes.c_bool(name_c)
    
  

  if ts_target_  is not None:
    ts_target_c = lib.cxios_is_defined_variable_ts_target(variable_hdl)
    ts_target_._c_value = ctypes.c_bool(ts_target_c)
    
  

  if type_  is not None:
    type_c = lib.cxios_is_defined_variable_type(variable_hdl)
    type_._c_value = ctypes.c_bool(type_c)
    
  
  return 



