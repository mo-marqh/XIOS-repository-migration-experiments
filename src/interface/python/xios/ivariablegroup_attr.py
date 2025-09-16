# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.ivariable import  get_variablegroup_handle
from xios.ovariablegroup_attr import VariableGroup

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_variablegroup_group_ref.argtypes = [VariableGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_variablegroup_group_ref.restypes = None

lib.cxios_set_variablegroup_group_ref.argtypes = [VariableGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_variablegroup_group_ref.restypes = None

lib.cxios_is_defined_variablegroup_group_ref.argtypes = [VariableGroup]
lib.cxios_is_defined_variablegroup_group_ref.restypes = ctypes.c_bool

lib.cxios_get_variablegroup_name.argtypes = [VariableGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_variablegroup_name.restypes = None

lib.cxios_set_variablegroup_name.argtypes = [VariableGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_variablegroup_name.restypes = None

lib.cxios_is_defined_variablegroup_name.argtypes = [VariableGroup]
lib.cxios_is_defined_variablegroup_name.restypes = ctypes.c_bool

lib.cxios_get_variablegroup_ts_target.argtypes = [VariableGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_variablegroup_ts_target.restypes = None

lib.cxios_set_variablegroup_ts_target.argtypes = [VariableGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_variablegroup_ts_target.restypes = None

lib.cxios_is_defined_variablegroup_ts_target.argtypes = [VariableGroup]
lib.cxios_is_defined_variablegroup_ts_target.restypes = ctypes.c_bool

lib.cxios_get_variablegroup_type.argtypes = [VariableGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_variablegroup_type.restypes = None

lib.cxios_set_variablegroup_type.argtypes = [VariableGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_variablegroup_type.restypes = None

lib.cxios_is_defined_variablegroup_type.argtypes = [VariableGroup]
lib.cxios_is_defined_variablegroup_type.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_variablegroup_attr(variablegroup_id : Union[String, str], group_ref : Optional[Union[str, String]] = None, name : Optional[Union[str, String]] = None,   
ts_target : Optional[Union[str, String]] = None, type : Optional[Union[str, String]] = None):

  
  variablegroup_hdl = VariableGroup()
  

  get_variablegroup_handle(variablegroup_id, variablegroup_hdl)
  set_variablegroup_attr_hdl_(variablegroup_hdl, group_ref, name, ts_target, type)
  return 



@typecheck
def set_variablegroup_attr_hdl(variablegroup_hdl : VariableGroup, group_ref : Optional[Union[str, String]] = None, name : Optional[Union[str, String]] = None,   
ts_target : Optional[Union[str, String]] = None, type : Optional[Union[str, String]] = None):

  
  set_variablegroup_attr_hdl_(variablegroup_hdl, group_ref, name, ts_target, type)
  return 



@typecheck
def set_variablegroup_attr_hdl_(variablegroup_hdl : VariableGroup, group_ref_ : Optional[Union[str, String]] = None, name_ : Optional[Union[str, String]] = None,   
ts_target_ : Optional[Union[str, String]] = None, type_ : Optional[Union[str, String]] = None):

  
  

  if group_ref_ is not None:
  
    group_ref_= String(group_ref_)
    group_ref_c = group_ref_._c_value
    len_group_ref_c = len(ctypes.string_at(group_ref_c))
    lib.cxios_set_variablegroup_group_ref(variablegroup_hdl, group_ref_c, len_group_ref_c)
    
  

  if name_ is not None:
  
    name_= String(name_)
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_set_variablegroup_name(variablegroup_hdl, name_c, len_name_c)
    
  

  if ts_target_ is not None:
  
    ts_target_= String(ts_target_)
    ts_target_c = ts_target_._c_value
    len_ts_target_c = len(ctypes.string_at(ts_target_c))
    lib.cxios_set_variablegroup_ts_target(variablegroup_hdl, ts_target_c, len_ts_target_c)
    
  

  if type_ is not None:
  
    type_= String(type_)
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_set_variablegroup_type(variablegroup_hdl, type_c, len_type_c)
    
  
  return 



@typecheck
def get_variablegroup_attr(variablegroup_id : Union[String, str], group_ref : Optional[String] = None, name : Optional[String] = None,   
ts_target : Optional[String] = None, type : Optional[String] = None):

  
  variablegroup_hdl = VariableGroup()
  

  get_variablegroup_handle(variablegroup_id, variablegroup_hdl)
  get_variablegroup_attr_hdl_(variablegroup_hdl, group_ref, name, ts_target, type)
  return 



@typecheck
def get_variablegroup_attr_hdl(variablegroup_hdl : VariableGroup, group_ref : Optional[String] = None, name : Optional[String] = None,   
ts_target : Optional[String] = None, type : Optional[String] = None):

  
  get_variablegroup_attr_hdl_(variablegroup_hdl, group_ref, name, ts_target, type)
  return 



@typecheck
def get_variablegroup_attr_hdl_(variablegroup_hdl : VariableGroup, group_ref_ : Optional[String] = None, name_ : Optional[String] = None,   
ts_target_ : Optional[String] = None, type_ : Optional[String] = None):

  
  

  if group_ref_ is not None:
  
    group_ref_c = group_ref_._c_value
    len_group_ref_c = len(ctypes.string_at(group_ref_c))
    lib.cxios_get_variablegroup_group_ref(variablegroup_hdl, group_ref_c, len_group_ref_c)
    
  

  if name_ is not None:
  
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_get_variablegroup_name(variablegroup_hdl, name_c, len_name_c)
    
  

  if ts_target_ is not None:
  
    ts_target_c = ts_target_._c_value
    len_ts_target_c = len(ctypes.string_at(ts_target_c))
    lib.cxios_get_variablegroup_ts_target(variablegroup_hdl, ts_target_c, len_ts_target_c)
    
  

  if type_ is not None:
  
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_get_variablegroup_type(variablegroup_hdl, type_c, len_type_c)
    
  
  return 



@typecheck
def is_defined_variablegroup_attr(variablegroup_id : String, group_ref : Optional[Bool] = None, name : Optional[Bool] = None,   
ts_target : Optional[Bool] = None, type : Optional[Bool] = None):

  
  variablegroup_hdl = VariableGroup()
  

  get_variablegroup_handle(variablegroup_id, variablegroup_hdl)
  is_defined_variablegroup_attr_hdl_(variablegroup_hdl, group_ref, name, ts_target, type)
  return 



@typecheck
def is_defined_variablegroup_attr_hdl(variablegroup_hdl : VariableGroup, group_ref : Optional[Bool] = None, name : Optional[Bool] = None,   
ts_target : Optional[Bool] = None, type : Optional[Bool] = None):

  
  is_defined_variablegroup_attr_hdl_(variablegroup_hdl, group_ref, name, ts_target, type)
  return 



@typecheck
def is_defined_variablegroup_attr_hdl_(variablegroup_hdl : VariableGroup, group_ref_ : Optional[Bool] = None, name_ : Optional[Bool] = None,   
ts_target_ : Optional[Bool] = None, type_ : Optional[Bool] = None):

  
  

  if group_ref_  is not None:
    group_ref_c = lib.cxios_is_defined_variablegroup_group_ref(variablegroup_hdl)
    group_ref_._c_value = ctypes.c_bool(group_ref_c)
    
  

  if name_  is not None:
    name_c = lib.cxios_is_defined_variablegroup_name(variablegroup_hdl)
    name_._c_value = ctypes.c_bool(name_c)
    
  

  if ts_target_  is not None:
    ts_target_c = lib.cxios_is_defined_variablegroup_ts_target(variablegroup_hdl)
    ts_target_._c_value = ctypes.c_bool(ts_target_c)
    
  

  if type_  is not None:
    type_c = lib.cxios_is_defined_variablegroup_type(variablegroup_hdl)
    type_._c_value = ctypes.c_bool(type_c)
    
  
  return 



