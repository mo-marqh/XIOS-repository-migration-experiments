# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.izoom_axis import  get_zoom_axis_handle
from xios.ozoom_axis_attr import ZoomAxis

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_zoom_axis_begin.argtypes = [ZoomAxis, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_zoom_axis_begin.restypes = None

lib.cxios_set_zoom_axis_begin.argtypes = [ZoomAxis, ctypes.c_int]
lib.cxios_set_zoom_axis_begin.restypes = None

lib.cxios_is_defined_zoom_axis_begin.argtypes = [ZoomAxis]
lib.cxios_is_defined_zoom_axis_begin.restypes = ctypes.c_bool

lib.cxios_get_zoom_axis_index.argtypes = [ZoomAxis, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_zoom_axis_index.restypes = None

lib.cxios_set_zoom_axis_index.argtypes = [ZoomAxis, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_zoom_axis_index.restypes = None

lib.cxios_is_defined_zoom_axis_index.argtypes = [ZoomAxis]
lib.cxios_is_defined_zoom_axis_index.restypes = ctypes.c_bool

lib.cxios_get_zoom_axis_n.argtypes = [ZoomAxis, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_zoom_axis_n.restypes = None

lib.cxios_set_zoom_axis_n.argtypes = [ZoomAxis, ctypes.c_int]
lib.cxios_set_zoom_axis_n.restypes = None

lib.cxios_is_defined_zoom_axis_n.argtypes = [ZoomAxis]
lib.cxios_is_defined_zoom_axis_n.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_zoom_axis_attr(zoom_axis_id : Union[String, str], begin : Optional[Union[Int, int]] = None, index : Optional[Union[np.ndarray, NpArrayInt]] = None,   
n : Optional[Union[Int, int]] = None):

  
  zoom_axis_hdl = ZoomAxis()
  

  get_zoom_axis_handle(zoom_axis_id, zoom_axis_hdl)
  set_zoom_axis_attr_hdl_(zoom_axis_hdl, begin, index, n)
  return 



@typecheck
def set_zoom_axis_attr_hdl(zoom_axis_hdl : ZoomAxis, begin : Optional[Union[Int, int]] = None, index : Optional[Union[np.ndarray, NpArrayInt]] = None,   
n : Optional[Union[Int, int]] = None):

  
  set_zoom_axis_attr_hdl_(zoom_axis_hdl, begin, index, n)
  return 



@typecheck
def set_zoom_axis_attr_hdl_(zoom_axis_hdl : ZoomAxis, begin_ : Optional[Union[Int, int]] = None, index_ : Optional[Union[np.ndarray, NpArrayInt]] = None,   
n_ : Optional[Union[Int, int]] = None):

  
  

  if begin_ is not None: 
  
    begin_ = Int(begin_)
    begin_c = begin_._c_value
    lib.cxios_set_zoom_axis_begin(zoom_axis_hdl, begin_c)
    
  

  if index_ is not None:
    index_ = NpArrayInt(index_)
    index_c = index_._c_value
    if len(index_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(index_.shape)
      shape_c = shape(*index_.shape)
      lib.cxios_set_zoom_axis_index(zoom_axis_hdl, index_c, shape_c)
  

  if n_ is not None: 
  
    n_ = Int(n_)
    n_c = n_._c_value
    lib.cxios_set_zoom_axis_n(zoom_axis_hdl, n_c)
    
  
  return 



@typecheck
def get_zoom_axis_attr(zoom_axis_id : Union[String, str], begin : Optional[Int] = None, index : Optional[NpArrayInt] = None,   
n : Optional[Int] = None):

  
  zoom_axis_hdl = ZoomAxis()
  

  get_zoom_axis_handle(zoom_axis_id, zoom_axis_hdl)
  get_zoom_axis_attr_hdl_(zoom_axis_hdl, begin, index, n)
  return 



@typecheck
def get_zoom_axis_attr_hdl(zoom_axis_hdl : ZoomAxis, begin : Optional[Int] = None, index : Optional[NpArrayInt] = None,   
n : Optional[Int] = None):

  
  get_zoom_axis_attr_hdl_(zoom_axis_hdl, begin, index, n)
  return 



@typecheck
def get_zoom_axis_attr_hdl_(zoom_axis_hdl : ZoomAxis, begin_ : Optional[Int] = None, index_ : Optional[NpArrayInt] = None,   
n_ : Optional[Int] = None):

  
  

  if begin_ is not None: 
  
    begin_c = begin_._c_value
    lib.cxios_get_zoom_axis_begin(zoom_axis_hdl, begin_c)
    
  

  if index_ is not None:
    if index_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_zoom_axis_index_shape(zoom_axis_hdl, shape_c)
      index_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_zoom_axis_index(zoom_axis_hdl, index_._c_value, shape_c)
    else: 
      index_c = index_._c_value
      if len(index_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(index_.shape)._c_value
        lib.cxios_get_zoom_axis_index(zoom_axis_hdl, index_c, shape_c)
  

  if n_ is not None: 
  
    n_c = n_._c_value
    lib.cxios_get_zoom_axis_n(zoom_axis_hdl, n_c)
    
  
  return 



@typecheck
def is_defined_zoom_axis_attr(zoom_axis_id : String, begin : Optional[Bool] = None, index : Optional[Bool] = None, n : Optional[Bool] = None):

  
  zoom_axis_hdl = ZoomAxis()
  

  get_zoom_axis_handle(zoom_axis_id, zoom_axis_hdl)
  is_defined_zoom_axis_attr_hdl_(zoom_axis_hdl, begin, index, n)
  return 



@typecheck
def is_defined_zoom_axis_attr_hdl(zoom_axis_hdl : ZoomAxis, begin : Optional[Bool] = None, index : Optional[Bool] = None, n : Optional[Bool] = None):

  
  is_defined_zoom_axis_attr_hdl_(zoom_axis_hdl, begin, index, n)
  return 



@typecheck
def is_defined_zoom_axis_attr_hdl_(zoom_axis_hdl : ZoomAxis, begin_ : Optional[Bool] = None, index_ : Optional[Bool] = None,   
n_ : Optional[Bool] = None):

  
  

  if begin_  is not None:
    begin_c = lib.cxios_is_defined_zoom_axis_begin(zoom_axis_hdl)
    begin_._c_value = ctypes.c_bool(begin_c)
    
  

  if index_  is not None:
    index_c = lib.cxios_is_defined_zoom_axis_index(zoom_axis_hdl)
    index_._c_value = ctypes.c_bool(index_c)
    
  

  if n_  is not None:
    n_c = lib.cxios_is_defined_zoom_axis_n(zoom_axis_hdl)
    n_._c_value = ctypes.c_bool(n_c)
    
  
  return 



