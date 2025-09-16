# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.ireduce_axis_to_axis import  get_reduce_axis_to_axis_handle
from xios.oreduce_axis_to_axis_attr import ReduceAxisToAxis

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_reduce_axis_to_axis_operation.argtypes = [ReduceAxisToAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_reduce_axis_to_axis_operation.restypes = None

lib.cxios_set_reduce_axis_to_axis_operation.argtypes = [ReduceAxisToAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_reduce_axis_to_axis_operation.restypes = None

lib.cxios_is_defined_reduce_axis_to_axis_operation.argtypes = [ReduceAxisToAxis]
lib.cxios_is_defined_reduce_axis_to_axis_operation.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_reduce_axis_to_axis_attr(reduce_axis_to_axis_id : Union[String, str], operation : Optional[Union[str, String]] = None):

  
  reduce_axis_to_axis_hdl = ReduceAxisToAxis()
  

  get_reduce_axis_to_axis_handle(reduce_axis_to_axis_id, reduce_axis_to_axis_hdl)
  set_reduce_axis_to_axis_attr_hdl_(reduce_axis_to_axis_hdl, operation)
  return 



@typecheck
def set_reduce_axis_to_axis_attr_hdl(reduce_axis_to_axis_hdl : ReduceAxisToAxis, operation : Optional[Union[str, String]] = None):

  
  set_reduce_axis_to_axis_attr_hdl_(reduce_axis_to_axis_hdl, operation)
  return 



@typecheck
def set_reduce_axis_to_axis_attr_hdl_(reduce_axis_to_axis_hdl : ReduceAxisToAxis, operation_ : Optional[Union[str, String]] = None):

  
  

  if operation_ is not None:
  
    operation_= String(operation_)
    operation_c = operation_._c_value
    len_operation_c = len(ctypes.string_at(operation_c))
    lib.cxios_set_reduce_axis_to_axis_operation(reduce_axis_to_axis_hdl, operation_c, len_operation_c)
    
  
  return 



@typecheck
def get_reduce_axis_to_axis_attr(reduce_axis_to_axis_id : Union[String, str], operation : Optional[String] = None):

  
  reduce_axis_to_axis_hdl = ReduceAxisToAxis()
  

  get_reduce_axis_to_axis_handle(reduce_axis_to_axis_id, reduce_axis_to_axis_hdl)
  get_reduce_axis_to_axis_attr_hdl_(reduce_axis_to_axis_hdl, operation)
  return 



@typecheck
def get_reduce_axis_to_axis_attr_hdl(reduce_axis_to_axis_hdl : ReduceAxisToAxis, operation : Optional[String] = None):

  
  get_reduce_axis_to_axis_attr_hdl_(reduce_axis_to_axis_hdl, operation)
  return 



@typecheck
def get_reduce_axis_to_axis_attr_hdl_(reduce_axis_to_axis_hdl : ReduceAxisToAxis, operation_ : Optional[String] = None):

  
  

  if operation_ is not None:
  
    operation_c = operation_._c_value
    len_operation_c = len(ctypes.string_at(operation_c))
    lib.cxios_get_reduce_axis_to_axis_operation(reduce_axis_to_axis_hdl, operation_c, len_operation_c)
    
  
  return 



@typecheck
def is_defined_reduce_axis_to_axis_attr(reduce_axis_to_axis_id : String, operation : Optional[Bool] = None):

  
  reduce_axis_to_axis_hdl = ReduceAxisToAxis()
  

  get_reduce_axis_to_axis_handle(reduce_axis_to_axis_id, reduce_axis_to_axis_hdl)
  is_defined_reduce_axis_to_axis_attr_hdl_(reduce_axis_to_axis_hdl, operation)
  return 



@typecheck
def is_defined_reduce_axis_to_axis_attr_hdl(reduce_axis_to_axis_hdl : ReduceAxisToAxis, operation : Optional[Bool] = None):

  
  is_defined_reduce_axis_to_axis_attr_hdl_(reduce_axis_to_axis_hdl, operation)
  return 



@typecheck
def is_defined_reduce_axis_to_axis_attr_hdl_(reduce_axis_to_axis_hdl : ReduceAxisToAxis, operation_ : Optional[Bool] = None):

  
  

  if operation_  is not None:
    operation_c = lib.cxios_is_defined_reduce_axis_to_axis_operation(reduce_axis_to_axis_hdl)
    operation_._c_value = ctypes.c_bool(operation_c)
    
  
  return 



