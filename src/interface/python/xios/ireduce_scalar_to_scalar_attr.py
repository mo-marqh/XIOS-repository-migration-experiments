# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.ireduce_scalar_to_scalar import  get_reduce_scalar_to_scalar_handle
from xios.oreduce_scalar_to_scalar_attr import ReduceScalarToScalar

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_reduce_scalar_to_scalar_operation.argtypes = [ReduceScalarToScalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_reduce_scalar_to_scalar_operation.restypes = None

lib.cxios_set_reduce_scalar_to_scalar_operation.argtypes = [ReduceScalarToScalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_reduce_scalar_to_scalar_operation.restypes = None

lib.cxios_is_defined_reduce_scalar_to_scalar_operation.argtypes = [ReduceScalarToScalar]
lib.cxios_is_defined_reduce_scalar_to_scalar_operation.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_reduce_scalar_to_scalar_attr(reduce_scalar_to_scalar_id : Union[String, str], operation : Optional[Union[str, String]] = None):

  
  reduce_scalar_to_scalar_hdl = ReduceScalarToScalar()
  

  get_reduce_scalar_to_scalar_handle(reduce_scalar_to_scalar_id, reduce_scalar_to_scalar_hdl)
  set_reduce_scalar_to_scalar_attr_hdl_(reduce_scalar_to_scalar_hdl, operation)
  return 



@typecheck
def set_reduce_scalar_to_scalar_attr_hdl(reduce_scalar_to_scalar_hdl : ReduceScalarToScalar, operation : Optional[Union[str, String]] = None):

  
  set_reduce_scalar_to_scalar_attr_hdl_(reduce_scalar_to_scalar_hdl, operation)
  return 



@typecheck
def set_reduce_scalar_to_scalar_attr_hdl_(reduce_scalar_to_scalar_hdl : ReduceScalarToScalar, operation_ : Optional[Union[str, String]] = None):

  
  

  if operation_ is not None:
  
    operation_= String(operation_)
    operation_c = operation_._c_value
    len_operation_c = len(ctypes.string_at(operation_c))
    lib.cxios_set_reduce_scalar_to_scalar_operation(reduce_scalar_to_scalar_hdl, operation_c, len_operation_c)
    
  
  return 



@typecheck
def get_reduce_scalar_to_scalar_attr(reduce_scalar_to_scalar_id : Union[String, str], operation : Optional[String] = None):

  
  reduce_scalar_to_scalar_hdl = ReduceScalarToScalar()
  

  get_reduce_scalar_to_scalar_handle(reduce_scalar_to_scalar_id, reduce_scalar_to_scalar_hdl)
  get_reduce_scalar_to_scalar_attr_hdl_(reduce_scalar_to_scalar_hdl, operation)
  return 



@typecheck
def get_reduce_scalar_to_scalar_attr_hdl(reduce_scalar_to_scalar_hdl : ReduceScalarToScalar, operation : Optional[String] = None):

  
  get_reduce_scalar_to_scalar_attr_hdl_(reduce_scalar_to_scalar_hdl, operation)
  return 



@typecheck
def get_reduce_scalar_to_scalar_attr_hdl_(reduce_scalar_to_scalar_hdl : ReduceScalarToScalar, operation_ : Optional[String] = None):

  
  

  if operation_ is not None:
  
    operation_c = operation_._c_value
    len_operation_c = len(ctypes.string_at(operation_c))
    lib.cxios_get_reduce_scalar_to_scalar_operation(reduce_scalar_to_scalar_hdl, operation_c, len_operation_c)
    
  
  return 



@typecheck
def is_defined_reduce_scalar_to_scalar_attr(reduce_scalar_to_scalar_id : String, operation : Optional[Bool] = None):

  
  reduce_scalar_to_scalar_hdl = ReduceScalarToScalar()
  

  get_reduce_scalar_to_scalar_handle(reduce_scalar_to_scalar_id, reduce_scalar_to_scalar_hdl)
  is_defined_reduce_scalar_to_scalar_attr_hdl_(reduce_scalar_to_scalar_hdl, operation)
  return 



@typecheck
def is_defined_reduce_scalar_to_scalar_attr_hdl(reduce_scalar_to_scalar_hdl : ReduceScalarToScalar, operation : Optional[Bool] = None):

  
  is_defined_reduce_scalar_to_scalar_attr_hdl_(reduce_scalar_to_scalar_hdl, operation)
  return 



@typecheck
def is_defined_reduce_scalar_to_scalar_attr_hdl_(reduce_scalar_to_scalar_hdl : ReduceScalarToScalar, operation_ : Optional[Bool] = None):

  
  

  if operation_  is not None:
    operation_c = lib.cxios_is_defined_reduce_scalar_to_scalar_operation(reduce_scalar_to_scalar_hdl)
    operation_._c_value = ctypes.c_bool(operation_c)
    
  
  return 



