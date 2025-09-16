# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.iredistribute_axis import  get_redistribute_axis_handle
from xios.oredistribute_axis_attr import RedistributeAxis

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_redistribute_axis_index.argtypes = [RedistributeAxis, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_redistribute_axis_index.restypes = None

lib.cxios_set_redistribute_axis_index.argtypes = [RedistributeAxis, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_redistribute_axis_index.restypes = None

lib.cxios_is_defined_redistribute_axis_index.argtypes = [RedistributeAxis]
lib.cxios_is_defined_redistribute_axis_index.restypes = ctypes.c_bool

lib.cxios_get_redistribute_axis_mask.argtypes = [RedistributeAxis, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_redistribute_axis_mask.restypes = None

lib.cxios_set_redistribute_axis_mask.argtypes = [RedistributeAxis, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_redistribute_axis_mask.restypes = None

lib.cxios_is_defined_redistribute_axis_mask.argtypes = [RedistributeAxis]
lib.cxios_is_defined_redistribute_axis_mask.restypes = ctypes.c_bool

lib.cxios_get_redistribute_axis_type.argtypes = [RedistributeAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_redistribute_axis_type.restypes = None

lib.cxios_set_redistribute_axis_type.argtypes = [RedistributeAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_redistribute_axis_type.restypes = None

lib.cxios_is_defined_redistribute_axis_type.argtypes = [RedistributeAxis]
lib.cxios_is_defined_redistribute_axis_type.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_redistribute_axis_attr(redistribute_axis_id : Union[String, str], index : Optional[Union[np.ndarray, NpArrayInt]] = None,   
mask : Optional[Union[np.ndarray, NpArrayBool]] = None, type : Optional[Union[str, String]] = None):

  
  redistribute_axis_hdl = RedistributeAxis()
  

  get_redistribute_axis_handle(redistribute_axis_id, redistribute_axis_hdl)
  set_redistribute_axis_attr_hdl_(redistribute_axis_hdl, index, mask, type)
  return 



@typecheck
def set_redistribute_axis_attr_hdl(redistribute_axis_hdl : RedistributeAxis, index : Optional[Union[np.ndarray, NpArrayInt]] = None,   
mask : Optional[Union[np.ndarray, NpArrayBool]] = None, type : Optional[Union[str, String]] = None):

  
  set_redistribute_axis_attr_hdl_(redistribute_axis_hdl, index, mask, type)
  return 



@typecheck
def set_redistribute_axis_attr_hdl_(redistribute_axis_hdl : RedistributeAxis, index_ : Optional[Union[np.ndarray, NpArrayInt]] = None,   
mask_ : Optional[Union[np.ndarray, NpArrayBool]] = None, type_ : Optional[Union[str, String]] = None):

  
  

  if index_ is not None:
    index_ = NpArrayInt(index_)
    index_c = index_._c_value
    if len(index_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(index_.shape)
      shape_c = shape(*index_.shape)
      lib.cxios_set_redistribute_axis_index(redistribute_axis_hdl, index_c, shape_c)
  

  if mask_ is not None:
    mask_ = NpArrayBool(mask_)
    mask_c = mask_._c_value
    if len(mask_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(mask_.shape)
      shape_c = shape(*mask_.shape)
      lib.cxios_set_redistribute_axis_mask(redistribute_axis_hdl, mask_c, shape_c)
  

  if type_ is not None:
  
    type_= String(type_)
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_set_redistribute_axis_type(redistribute_axis_hdl, type_c, len_type_c)
    
  
  return 



@typecheck
def get_redistribute_axis_attr(redistribute_axis_id : Union[String, str], index : Optional[NpArrayInt] = None, mask : Optional[NpArrayBool] = None,   
type : Optional[String] = None):

  
  redistribute_axis_hdl = RedistributeAxis()
  

  get_redistribute_axis_handle(redistribute_axis_id, redistribute_axis_hdl)
  get_redistribute_axis_attr_hdl_(redistribute_axis_hdl, index, mask, type)
  return 



@typecheck
def get_redistribute_axis_attr_hdl(redistribute_axis_hdl : RedistributeAxis, index : Optional[NpArrayInt] = None, mask : Optional[NpArrayBool] = None,   
type : Optional[String] = None):

  
  get_redistribute_axis_attr_hdl_(redistribute_axis_hdl, index, mask, type)
  return 



@typecheck
def get_redistribute_axis_attr_hdl_(redistribute_axis_hdl : RedistributeAxis, index_ : Optional[NpArrayInt] = None, mask_ : Optional[NpArrayBool] = None,   
type_ : Optional[String] = None):

  
  

  if index_ is not None:
    if index_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_redistribute_axis_index_shape(redistribute_axis_hdl, shape_c)
      index_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_redistribute_axis_index(redistribute_axis_hdl, index_._c_value, shape_c)
    else: 
      index_c = index_._c_value
      if len(index_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(index_.shape)._c_value
        lib.cxios_get_redistribute_axis_index(redistribute_axis_hdl, index_c, shape_c)
  

  if mask_ is not None:
    if mask_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_redistribute_axis_mask_shape(redistribute_axis_hdl, shape_c)
      mask_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_redistribute_axis_mask(redistribute_axis_hdl, mask_._c_value, shape_c)
    else: 
      mask_c = mask_._c_value
      if len(mask_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(mask_.shape)._c_value
        lib.cxios_get_redistribute_axis_mask(redistribute_axis_hdl, mask_c, shape_c)
  

  if type_ is not None:
  
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_get_redistribute_axis_type(redistribute_axis_hdl, type_c, len_type_c)
    
  
  return 



@typecheck
def is_defined_redistribute_axis_attr(redistribute_axis_id : String, index : Optional[Bool] = None, mask : Optional[Bool] = None,   
type : Optional[Bool] = None):

  
  redistribute_axis_hdl = RedistributeAxis()
  

  get_redistribute_axis_handle(redistribute_axis_id, redistribute_axis_hdl)
  is_defined_redistribute_axis_attr_hdl_(redistribute_axis_hdl, index, mask, type)
  return 



@typecheck
def is_defined_redistribute_axis_attr_hdl(redistribute_axis_hdl : RedistributeAxis, index : Optional[Bool] = None, mask : Optional[Bool] = None,   
type : Optional[Bool] = None):

  
  is_defined_redistribute_axis_attr_hdl_(redistribute_axis_hdl, index, mask, type)
  return 



@typecheck
def is_defined_redistribute_axis_attr_hdl_(redistribute_axis_hdl : RedistributeAxis, index_ : Optional[Bool] = None, mask_ : Optional[Bool] = None,   
type_ : Optional[Bool] = None):

  
  

  if index_  is not None:
    index_c = lib.cxios_is_defined_redistribute_axis_index(redistribute_axis_hdl)
    index_._c_value = ctypes.c_bool(index_c)
    
  

  if mask_  is not None:
    mask_c = lib.cxios_is_defined_redistribute_axis_mask(redistribute_axis_hdl)
    mask_._c_value = ctypes.c_bool(mask_c)
    
  

  if type_  is not None:
    type_c = lib.cxios_is_defined_redistribute_axis_type(redistribute_axis_hdl)
    type_._c_value = ctypes.c_bool(type_c)
    
  
  return 



