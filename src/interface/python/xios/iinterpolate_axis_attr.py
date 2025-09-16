# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.iinterpolate_axis import  get_interpolate_axis_handle
from xios.ointerpolate_axis_attr import InterpolateAxis

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_interpolate_axis_coordinate.argtypes = [InterpolateAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_interpolate_axis_coordinate.restypes = None

lib.cxios_set_interpolate_axis_coordinate.argtypes = [InterpolateAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_interpolate_axis_coordinate.restypes = None

lib.cxios_is_defined_interpolate_axis_coordinate.argtypes = [InterpolateAxis]
lib.cxios_is_defined_interpolate_axis_coordinate.restypes = ctypes.c_bool

lib.cxios_get_interpolate_axis_coordinate_dst.argtypes = [InterpolateAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_interpolate_axis_coordinate_dst.restypes = None

lib.cxios_set_interpolate_axis_coordinate_dst.argtypes = [InterpolateAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_interpolate_axis_coordinate_dst.restypes = None

lib.cxios_is_defined_interpolate_axis_coordinate_dst.argtypes = [InterpolateAxis]
lib.cxios_is_defined_interpolate_axis_coordinate_dst.restypes = ctypes.c_bool

lib.cxios_get_interpolate_axis_coordinate_src.argtypes = [InterpolateAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_interpolate_axis_coordinate_src.restypes = None

lib.cxios_set_interpolate_axis_coordinate_src.argtypes = [InterpolateAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_interpolate_axis_coordinate_src.restypes = None

lib.cxios_is_defined_interpolate_axis_coordinate_src.argtypes = [InterpolateAxis]
lib.cxios_is_defined_interpolate_axis_coordinate_src.restypes = ctypes.c_bool

lib.cxios_get_interpolate_axis_extrapolate.argtypes = [InterpolateAxis, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_interpolate_axis_extrapolate.restypes = None

lib.cxios_set_interpolate_axis_extrapolate.argtypes = [InterpolateAxis, ctypes.c_bool]
lib.cxios_set_interpolate_axis_extrapolate.restypes = None

lib.cxios_is_defined_interpolate_axis_extrapolate.argtypes = [InterpolateAxis]
lib.cxios_is_defined_interpolate_axis_extrapolate.restypes = ctypes.c_bool

lib.cxios_get_interpolate_axis_order.argtypes = [InterpolateAxis, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_interpolate_axis_order.restypes = None

lib.cxios_set_interpolate_axis_order.argtypes = [InterpolateAxis, ctypes.c_int]
lib.cxios_set_interpolate_axis_order.restypes = None

lib.cxios_is_defined_interpolate_axis_order.argtypes = [InterpolateAxis]
lib.cxios_is_defined_interpolate_axis_order.restypes = ctypes.c_bool

lib.cxios_get_interpolate_axis_type.argtypes = [InterpolateAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_interpolate_axis_type.restypes = None

lib.cxios_set_interpolate_axis_type.argtypes = [InterpolateAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_interpolate_axis_type.restypes = None

lib.cxios_is_defined_interpolate_axis_type.argtypes = [InterpolateAxis]
lib.cxios_is_defined_interpolate_axis_type.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_interpolate_axis_attr(interpolate_axis_id : Union[String, str], coordinate : Optional[Union[str, String]] = None,   
coordinate_dst : Optional[Union[str, String]] = None, coordinate_src : Optional[Union[str, String]] = None,   
extrapolate : Optional[Union[Bool, bool]] = None, order : Optional[Union[Int, int]] = None,   
type : Optional[Union[str, String]] = None):

  
  interpolate_axis_hdl = InterpolateAxis()
  

  get_interpolate_axis_handle(interpolate_axis_id, interpolate_axis_hdl)
  set_interpolate_axis_attr_hdl_(interpolate_axis_hdl, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type)
  return 



@typecheck
def set_interpolate_axis_attr_hdl(interpolate_axis_hdl : InterpolateAxis, coordinate : Optional[Union[str, String]] = None,   
coordinate_dst : Optional[Union[str, String]] = None, coordinate_src : Optional[Union[str, String]] = None,   
extrapolate : Optional[Union[Bool, bool]] = None, order : Optional[Union[Int, int]] = None,   
type : Optional[Union[str, String]] = None):

  
  set_interpolate_axis_attr_hdl_(interpolate_axis_hdl, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type)
  return 



@typecheck
def set_interpolate_axis_attr_hdl_(interpolate_axis_hdl : InterpolateAxis, coordinate_ : Optional[Union[str, String]] = None,   
coordinate_dst_ : Optional[Union[str, String]] = None, coordinate_src_ : Optional[Union[str, String]] = None,   
extrapolate_ : Optional[Union[Bool, bool]] = None, order_ : Optional[Union[Int, int]] = None,   
type_ : Optional[Union[str, String]] = None):

  
  

  if coordinate_ is not None:
  
    coordinate_= String(coordinate_)
    coordinate_c = coordinate_._c_value
    len_coordinate_c = len(ctypes.string_at(coordinate_c))
    lib.cxios_set_interpolate_axis_coordinate(interpolate_axis_hdl, coordinate_c, len_coordinate_c)
    
  

  if coordinate_dst_ is not None:
  
    coordinate_dst_= String(coordinate_dst_)
    coordinate_dst_c = coordinate_dst_._c_value
    len_coordinate_dst_c = len(ctypes.string_at(coordinate_dst_c))
    lib.cxios_set_interpolate_axis_coordinate_dst(interpolate_axis_hdl, coordinate_dst_c, len_coordinate_dst_c)
    
  

  if coordinate_src_ is not None:
  
    coordinate_src_= String(coordinate_src_)
    coordinate_src_c = coordinate_src_._c_value
    len_coordinate_src_c = len(ctypes.string_at(coordinate_src_c))
    lib.cxios_set_interpolate_axis_coordinate_src(interpolate_axis_hdl, coordinate_src_c, len_coordinate_src_c)
    
  

  if extrapolate_ is not None: 
  
    extrapolate_ = Bool(extrapolate_)
    extrapolate_c = extrapolate_._c_value
    lib.cxios_set_interpolate_axis_extrapolate(interpolate_axis_hdl, extrapolate_c)
    
  

  if order_ is not None: 
  
    order_ = Int(order_)
    order_c = order_._c_value
    lib.cxios_set_interpolate_axis_order(interpolate_axis_hdl, order_c)
    
  

  if type_ is not None:
  
    type_= String(type_)
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_set_interpolate_axis_type(interpolate_axis_hdl, type_c, len_type_c)
    
  
  return 



@typecheck
def get_interpolate_axis_attr(interpolate_axis_id : Union[String, str], coordinate : Optional[String] = None, coordinate_dst : Optional[String] = None,   
coordinate_src : Optional[String] = None, extrapolate : Optional[Bool] = None, order : Optional[Int] = None,   
type : Optional[String] = None):

  
  interpolate_axis_hdl = InterpolateAxis()
  

  get_interpolate_axis_handle(interpolate_axis_id, interpolate_axis_hdl)
  get_interpolate_axis_attr_hdl_(interpolate_axis_hdl, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type)
  return 



@typecheck
def get_interpolate_axis_attr_hdl(interpolate_axis_hdl : InterpolateAxis, coordinate : Optional[String] = None, coordinate_dst : Optional[String] = None,   
coordinate_src : Optional[String] = None, extrapolate : Optional[Bool] = None, order : Optional[Int] = None,   
type : Optional[String] = None):

  
  get_interpolate_axis_attr_hdl_(interpolate_axis_hdl, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type)
  return 



@typecheck
def get_interpolate_axis_attr_hdl_(interpolate_axis_hdl : InterpolateAxis, coordinate_ : Optional[String] = None, coordinate_dst_ : Optional[String] = None,   
coordinate_src_ : Optional[String] = None, extrapolate_ : Optional[Bool] = None, order_ : Optional[Int] = None,   
type_ : Optional[String] = None):

  
  

  if coordinate_ is not None:
  
    coordinate_c = coordinate_._c_value
    len_coordinate_c = len(ctypes.string_at(coordinate_c))
    lib.cxios_get_interpolate_axis_coordinate(interpolate_axis_hdl, coordinate_c, len_coordinate_c)
    
  

  if coordinate_dst_ is not None:
  
    coordinate_dst_c = coordinate_dst_._c_value
    len_coordinate_dst_c = len(ctypes.string_at(coordinate_dst_c))
    lib.cxios_get_interpolate_axis_coordinate_dst(interpolate_axis_hdl, coordinate_dst_c, len_coordinate_dst_c)
    
  

  if coordinate_src_ is not None:
  
    coordinate_src_c = coordinate_src_._c_value
    len_coordinate_src_c = len(ctypes.string_at(coordinate_src_c))
    lib.cxios_get_interpolate_axis_coordinate_src(interpolate_axis_hdl, coordinate_src_c, len_coordinate_src_c)
    
  

  if extrapolate_ is not None: 
  
    extrapolate_c = extrapolate_._c_value
    lib.cxios_get_interpolate_axis_extrapolate(interpolate_axis_hdl, extrapolate_c)
    
  

  if order_ is not None: 
  
    order_c = order_._c_value
    lib.cxios_get_interpolate_axis_order(interpolate_axis_hdl, order_c)
    
  

  if type_ is not None:
  
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_get_interpolate_axis_type(interpolate_axis_hdl, type_c, len_type_c)
    
  
  return 



@typecheck
def is_defined_interpolate_axis_attr(interpolate_axis_id : String, coordinate : Optional[Bool] = None, coordinate_dst : Optional[Bool] = None,   
coordinate_src : Optional[Bool] = None, extrapolate : Optional[Bool] = None, order : Optional[Bool] = None,   
type : Optional[Bool] = None):

  
  interpolate_axis_hdl = InterpolateAxis()
  

  get_interpolate_axis_handle(interpolate_axis_id, interpolate_axis_hdl)
  is_defined_interpolate_axis_attr_hdl_(interpolate_axis_hdl, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type)
  return 



@typecheck
def is_defined_interpolate_axis_attr_hdl(interpolate_axis_hdl : InterpolateAxis, coordinate : Optional[Bool] = None, coordinate_dst : Optional[Bool] = None,   
coordinate_src : Optional[Bool] = None, extrapolate : Optional[Bool] = None, order : Optional[Bool] = None,   
type : Optional[Bool] = None):

  
  is_defined_interpolate_axis_attr_hdl_(interpolate_axis_hdl, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type)
  return 



@typecheck
def is_defined_interpolate_axis_attr_hdl_(interpolate_axis_hdl : InterpolateAxis, coordinate_ : Optional[Bool] = None, coordinate_dst_ : Optional[Bool] = None,   
coordinate_src_ : Optional[Bool] = None, extrapolate_ : Optional[Bool] = None, order_ : Optional[Bool] = None,   
type_ : Optional[Bool] = None):

  
  

  if coordinate_  is not None:
    coordinate_c = lib.cxios_is_defined_interpolate_axis_coordinate(interpolate_axis_hdl)
    coordinate_._c_value = ctypes.c_bool(coordinate_c)
    
  

  if coordinate_dst_  is not None:
    coordinate_dst_c = lib.cxios_is_defined_interpolate_axis_coordinate_dst(interpolate_axis_hdl)
    coordinate_dst_._c_value = ctypes.c_bool(coordinate_dst_c)
    
  

  if coordinate_src_  is not None:
    coordinate_src_c = lib.cxios_is_defined_interpolate_axis_coordinate_src(interpolate_axis_hdl)
    coordinate_src_._c_value = ctypes.c_bool(coordinate_src_c)
    
  

  if extrapolate_  is not None:
    extrapolate_c = lib.cxios_is_defined_interpolate_axis_extrapolate(interpolate_axis_hdl)
    extrapolate_._c_value = ctypes.c_bool(extrapolate_c)
    
  

  if order_  is not None:
    order_c = lib.cxios_is_defined_interpolate_axis_order(interpolate_axis_hdl)
    order_._c_value = ctypes.c_bool(order_c)
    
  

  if type_  is not None:
    type_c = lib.cxios_is_defined_interpolate_axis_type(interpolate_axis_hdl)
    type_._c_value = ctypes.c_bool(type_c)
    
  
  return 



