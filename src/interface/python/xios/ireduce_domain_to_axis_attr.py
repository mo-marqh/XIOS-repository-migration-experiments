# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.ireduce_domain_to_axis import  get_reduce_domain_to_axis_handle
from xios.oreduce_domain_to_axis_attr import ReduceDomainToAxis

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_reduce_domain_to_axis_direction.argtypes = [ReduceDomainToAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_reduce_domain_to_axis_direction.restypes = None

lib.cxios_set_reduce_domain_to_axis_direction.argtypes = [ReduceDomainToAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_reduce_domain_to_axis_direction.restypes = None

lib.cxios_is_defined_reduce_domain_to_axis_direction.argtypes = [ReduceDomainToAxis]
lib.cxios_is_defined_reduce_domain_to_axis_direction.restypes = ctypes.c_bool

lib.cxios_get_reduce_domain_to_axis_local.argtypes = [ReduceDomainToAxis, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_reduce_domain_to_axis_local.restypes = None

lib.cxios_set_reduce_domain_to_axis_local.argtypes = [ReduceDomainToAxis, ctypes.c_bool]
lib.cxios_set_reduce_domain_to_axis_local.restypes = None

lib.cxios_is_defined_reduce_domain_to_axis_local.argtypes = [ReduceDomainToAxis]
lib.cxios_is_defined_reduce_domain_to_axis_local.restypes = ctypes.c_bool

lib.cxios_get_reduce_domain_to_axis_operation.argtypes = [ReduceDomainToAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_reduce_domain_to_axis_operation.restypes = None

lib.cxios_set_reduce_domain_to_axis_operation.argtypes = [ReduceDomainToAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_reduce_domain_to_axis_operation.restypes = None

lib.cxios_is_defined_reduce_domain_to_axis_operation.argtypes = [ReduceDomainToAxis]
lib.cxios_is_defined_reduce_domain_to_axis_operation.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_reduce_domain_to_axis_attr(reduce_domain_to_axis_id : Union[String, str], direction : Optional[Union[str, String]] = None,   
local : Optional[Union[Bool, bool]] = None, operation : Optional[Union[str, String]] = None):

  
  reduce_domain_to_axis_hdl = ReduceDomainToAxis()
  

  get_reduce_domain_to_axis_handle(reduce_domain_to_axis_id, reduce_domain_to_axis_hdl)
  set_reduce_domain_to_axis_attr_hdl_(reduce_domain_to_axis_hdl, direction, local, operation)
  return 



@typecheck
def set_reduce_domain_to_axis_attr_hdl(reduce_domain_to_axis_hdl : ReduceDomainToAxis, direction : Optional[Union[str, String]] = None,   
local : Optional[Union[Bool, bool]] = None, operation : Optional[Union[str, String]] = None):

  
  set_reduce_domain_to_axis_attr_hdl_(reduce_domain_to_axis_hdl, direction, local, operation)
  return 



@typecheck
def set_reduce_domain_to_axis_attr_hdl_(reduce_domain_to_axis_hdl : ReduceDomainToAxis, direction_ : Optional[Union[str, String]] = None,   
local_ : Optional[Union[Bool, bool]] = None, operation_ : Optional[Union[str, String]] = None):

  
  

  if direction_ is not None:
  
    direction_= String(direction_)
    direction_c = direction_._c_value
    len_direction_c = len(ctypes.string_at(direction_c))
    lib.cxios_set_reduce_domain_to_axis_direction(reduce_domain_to_axis_hdl, direction_c, len_direction_c)
    
  

  if local_ is not None: 
  
    local_ = Bool(local_)
    local_c = local_._c_value
    lib.cxios_set_reduce_domain_to_axis_local(reduce_domain_to_axis_hdl, local_c)
    
  

  if operation_ is not None:
  
    operation_= String(operation_)
    operation_c = operation_._c_value
    len_operation_c = len(ctypes.string_at(operation_c))
    lib.cxios_set_reduce_domain_to_axis_operation(reduce_domain_to_axis_hdl, operation_c, len_operation_c)
    
  
  return 



@typecheck
def get_reduce_domain_to_axis_attr(reduce_domain_to_axis_id : Union[String, str], direction : Optional[String] = None, local : Optional[Bool] = None,   
operation : Optional[String] = None):

  
  reduce_domain_to_axis_hdl = ReduceDomainToAxis()
  

  get_reduce_domain_to_axis_handle(reduce_domain_to_axis_id, reduce_domain_to_axis_hdl)
  get_reduce_domain_to_axis_attr_hdl_(reduce_domain_to_axis_hdl, direction, local, operation)
  return 



@typecheck
def get_reduce_domain_to_axis_attr_hdl(reduce_domain_to_axis_hdl : ReduceDomainToAxis, direction : Optional[String] = None, local : Optional[Bool] = None,   
operation : Optional[String] = None):

  
  get_reduce_domain_to_axis_attr_hdl_(reduce_domain_to_axis_hdl, direction, local, operation)
  return 



@typecheck
def get_reduce_domain_to_axis_attr_hdl_(reduce_domain_to_axis_hdl : ReduceDomainToAxis, direction_ : Optional[String] = None, local_ : Optional[Bool] = None,   
operation_ : Optional[String] = None):

  
  

  if direction_ is not None:
  
    direction_c = direction_._c_value
    len_direction_c = len(ctypes.string_at(direction_c))
    lib.cxios_get_reduce_domain_to_axis_direction(reduce_domain_to_axis_hdl, direction_c, len_direction_c)
    
  

  if local_ is not None: 
  
    local_c = local_._c_value
    lib.cxios_get_reduce_domain_to_axis_local(reduce_domain_to_axis_hdl, local_c)
    
  

  if operation_ is not None:
  
    operation_c = operation_._c_value
    len_operation_c = len(ctypes.string_at(operation_c))
    lib.cxios_get_reduce_domain_to_axis_operation(reduce_domain_to_axis_hdl, operation_c, len_operation_c)
    
  
  return 



@typecheck
def is_defined_reduce_domain_to_axis_attr(reduce_domain_to_axis_id : String, direction : Optional[Bool] = None, local : Optional[Bool] = None,   
operation : Optional[Bool] = None):

  
  reduce_domain_to_axis_hdl = ReduceDomainToAxis()
  

  get_reduce_domain_to_axis_handle(reduce_domain_to_axis_id, reduce_domain_to_axis_hdl)
  is_defined_reduce_domain_to_axis_attr_hdl_(reduce_domain_to_axis_hdl, direction, local, operation)
  return 



@typecheck
def is_defined_reduce_domain_to_axis_attr_hdl(reduce_domain_to_axis_hdl : ReduceDomainToAxis, direction : Optional[Bool] = None, local : Optional[Bool] = None,   
operation : Optional[Bool] = None):

  
  is_defined_reduce_domain_to_axis_attr_hdl_(reduce_domain_to_axis_hdl, direction, local, operation)
  return 



@typecheck
def is_defined_reduce_domain_to_axis_attr_hdl_(reduce_domain_to_axis_hdl : ReduceDomainToAxis, direction_ : Optional[Bool] = None, local_ : Optional[Bool] = None,   
operation_ : Optional[Bool] = None):

  
  

  if direction_  is not None:
    direction_c = lib.cxios_is_defined_reduce_domain_to_axis_direction(reduce_domain_to_axis_hdl)
    direction_._c_value = ctypes.c_bool(direction_c)
    
  

  if local_  is not None:
    local_c = lib.cxios_is_defined_reduce_domain_to_axis_local(reduce_domain_to_axis_hdl)
    local_._c_value = ctypes.c_bool(local_c)
    
  

  if operation_  is not None:
    operation_c = lib.cxios_is_defined_reduce_domain_to_axis_operation(reduce_domain_to_axis_hdl)
    operation_._c_value = ctypes.c_bool(operation_c)
    
  
  return 



