# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.ireduce_axis_to_scalar import  get_reduce_axis_to_scalar_handle
from xios.oreduce_axis_to_scalar_attr import ReduceAxisToScalar

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_reduce_axis_to_scalar_local.argtypes = [ReduceAxisToScalar, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_reduce_axis_to_scalar_local.restypes = None

lib.cxios_set_reduce_axis_to_scalar_local.argtypes = [ReduceAxisToScalar, ctypes.c_bool]
lib.cxios_set_reduce_axis_to_scalar_local.restypes = None

lib.cxios_is_defined_reduce_axis_to_scalar_local.argtypes = [ReduceAxisToScalar]
lib.cxios_is_defined_reduce_axis_to_scalar_local.restypes = ctypes.c_bool

lib.cxios_get_reduce_axis_to_scalar_operation.argtypes = [ReduceAxisToScalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_reduce_axis_to_scalar_operation.restypes = None

lib.cxios_set_reduce_axis_to_scalar_operation.argtypes = [ReduceAxisToScalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_reduce_axis_to_scalar_operation.restypes = None

lib.cxios_is_defined_reduce_axis_to_scalar_operation.argtypes = [ReduceAxisToScalar]
lib.cxios_is_defined_reduce_axis_to_scalar_operation.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_reduce_axis_to_scalar_attr(reduce_axis_to_scalar_id : Union[String, str], local : Optional[Union[Bool, bool]] = None,   
operation : Optional[Union[str, String]] = None):

  
  reduce_axis_to_scalar_hdl = ReduceAxisToScalar()
  

  get_reduce_axis_to_scalar_handle(reduce_axis_to_scalar_id, reduce_axis_to_scalar_hdl)
  set_reduce_axis_to_scalar_attr_hdl_(reduce_axis_to_scalar_hdl, local, operation)
  return 



@typecheck
def set_reduce_axis_to_scalar_attr_hdl(reduce_axis_to_scalar_hdl : ReduceAxisToScalar, local : Optional[Union[Bool, bool]] = None,   
operation : Optional[Union[str, String]] = None):

  
  set_reduce_axis_to_scalar_attr_hdl_(reduce_axis_to_scalar_hdl, local, operation)
  return 



@typecheck
def set_reduce_axis_to_scalar_attr_hdl_(reduce_axis_to_scalar_hdl : ReduceAxisToScalar, local_ : Optional[Union[Bool, bool]] = None,   
operation_ : Optional[Union[str, String]] = None):

  
  

  if local_ is not None: 
  
    local_ = Bool(local_)
    local_c = local_._c_value
    lib.cxios_set_reduce_axis_to_scalar_local(reduce_axis_to_scalar_hdl, local_c)
    
  

  if operation_ is not None:
  
    operation_= String(operation_)
    operation_c = operation_._c_value
    len_operation_c = len(ctypes.string_at(operation_c))
    lib.cxios_set_reduce_axis_to_scalar_operation(reduce_axis_to_scalar_hdl, operation_c, len_operation_c)
    
  
  return 



@typecheck
def get_reduce_axis_to_scalar_attr(reduce_axis_to_scalar_id : Union[String, str], local : Optional[Bool] = None, operation : Optional[String] = None):

  
  reduce_axis_to_scalar_hdl = ReduceAxisToScalar()
  

  get_reduce_axis_to_scalar_handle(reduce_axis_to_scalar_id, reduce_axis_to_scalar_hdl)
  get_reduce_axis_to_scalar_attr_hdl_(reduce_axis_to_scalar_hdl, local, operation)
  return 



@typecheck
def get_reduce_axis_to_scalar_attr_hdl(reduce_axis_to_scalar_hdl : ReduceAxisToScalar, local : Optional[Bool] = None, operation : Optional[String] = None):

  
  get_reduce_axis_to_scalar_attr_hdl_(reduce_axis_to_scalar_hdl, local, operation)
  return 



@typecheck
def get_reduce_axis_to_scalar_attr_hdl_(reduce_axis_to_scalar_hdl : ReduceAxisToScalar, local_ : Optional[Bool] = None, operation_ : Optional[String] = None):

  
  

  if local_ is not None: 
  
    local_c = local_._c_value
    lib.cxios_get_reduce_axis_to_scalar_local(reduce_axis_to_scalar_hdl, local_c)
    
  

  if operation_ is not None:
  
    operation_c = operation_._c_value
    len_operation_c = len(ctypes.string_at(operation_c))
    lib.cxios_get_reduce_axis_to_scalar_operation(reduce_axis_to_scalar_hdl, operation_c, len_operation_c)
    
  
  return 



@typecheck
def is_defined_reduce_axis_to_scalar_attr(reduce_axis_to_scalar_id : String, local : Optional[Bool] = None, operation : Optional[Bool] = None):

  
  reduce_axis_to_scalar_hdl = ReduceAxisToScalar()
  

  get_reduce_axis_to_scalar_handle(reduce_axis_to_scalar_id, reduce_axis_to_scalar_hdl)
  is_defined_reduce_axis_to_scalar_attr_hdl_(reduce_axis_to_scalar_hdl, local, operation)
  return 



@typecheck
def is_defined_reduce_axis_to_scalar_attr_hdl(reduce_axis_to_scalar_hdl : ReduceAxisToScalar, local : Optional[Bool] = None, operation : Optional[Bool] = None):

  
  is_defined_reduce_axis_to_scalar_attr_hdl_(reduce_axis_to_scalar_hdl, local, operation)
  return 



@typecheck
def is_defined_reduce_axis_to_scalar_attr_hdl_(reduce_axis_to_scalar_hdl : ReduceAxisToScalar, local_ : Optional[Bool] = None, operation_ : Optional[Bool] = None):

  
  

  if local_  is not None:
    local_c = lib.cxios_is_defined_reduce_axis_to_scalar_local(reduce_axis_to_scalar_hdl)
    local_._c_value = ctypes.c_bool(local_c)
    
  

  if operation_  is not None:
    operation_c = lib.cxios_is_defined_reduce_axis_to_scalar_operation(reduce_axis_to_scalar_hdl)
    operation_._c_value = ctypes.c_bool(operation_c)
    
  
  return 



