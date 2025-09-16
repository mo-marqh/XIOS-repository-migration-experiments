# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.iextract_axis_to_scalar import  get_extract_axis_to_scalar_handle
from xios.oextract_axis_to_scalar_attr import ExtractAxisToScalar

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_extract_axis_to_scalar_position.argtypes = [ExtractAxisToScalar, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_extract_axis_to_scalar_position.restypes = None

lib.cxios_set_extract_axis_to_scalar_position.argtypes = [ExtractAxisToScalar, ctypes.c_int]
lib.cxios_set_extract_axis_to_scalar_position.restypes = None

lib.cxios_is_defined_extract_axis_to_scalar_position.argtypes = [ExtractAxisToScalar]
lib.cxios_is_defined_extract_axis_to_scalar_position.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_extract_axis_to_scalar_attr(extract_axis_to_scalar_id : Union[String, str], position : Optional[Union[Int, int]] = None):

  
  extract_axis_to_scalar_hdl = ExtractAxisToScalar()
  

  get_extract_axis_to_scalar_handle(extract_axis_to_scalar_id, extract_axis_to_scalar_hdl)
  set_extract_axis_to_scalar_attr_hdl_(extract_axis_to_scalar_hdl, position)
  return 



@typecheck
def set_extract_axis_to_scalar_attr_hdl(extract_axis_to_scalar_hdl : ExtractAxisToScalar, position : Optional[Union[Int, int]] = None):

  
  set_extract_axis_to_scalar_attr_hdl_(extract_axis_to_scalar_hdl, position)
  return 



@typecheck
def set_extract_axis_to_scalar_attr_hdl_(extract_axis_to_scalar_hdl : ExtractAxisToScalar, position_ : Optional[Union[Int, int]] = None):

  
  

  if position_ is not None: 
  
    position_ = Int(position_)
    position_c = position_._c_value
    lib.cxios_set_extract_axis_to_scalar_position(extract_axis_to_scalar_hdl, position_c)
    
  
  return 



@typecheck
def get_extract_axis_to_scalar_attr(extract_axis_to_scalar_id : Union[String, str], position : Optional[Int] = None):

  
  extract_axis_to_scalar_hdl = ExtractAxisToScalar()
  

  get_extract_axis_to_scalar_handle(extract_axis_to_scalar_id, extract_axis_to_scalar_hdl)
  get_extract_axis_to_scalar_attr_hdl_(extract_axis_to_scalar_hdl, position)
  return 



@typecheck
def get_extract_axis_to_scalar_attr_hdl(extract_axis_to_scalar_hdl : ExtractAxisToScalar, position : Optional[Int] = None):

  
  get_extract_axis_to_scalar_attr_hdl_(extract_axis_to_scalar_hdl, position)
  return 



@typecheck
def get_extract_axis_to_scalar_attr_hdl_(extract_axis_to_scalar_hdl : ExtractAxisToScalar, position_ : Optional[Int] = None):

  
  

  if position_ is not None: 
  
    position_c = position_._c_value
    lib.cxios_get_extract_axis_to_scalar_position(extract_axis_to_scalar_hdl, position_c)
    
  
  return 



@typecheck
def is_defined_extract_axis_to_scalar_attr(extract_axis_to_scalar_id : String, position : Optional[Bool] = None):

  
  extract_axis_to_scalar_hdl = ExtractAxisToScalar()
  

  get_extract_axis_to_scalar_handle(extract_axis_to_scalar_id, extract_axis_to_scalar_hdl)
  is_defined_extract_axis_to_scalar_attr_hdl_(extract_axis_to_scalar_hdl, position)
  return 



@typecheck
def is_defined_extract_axis_to_scalar_attr_hdl(extract_axis_to_scalar_hdl : ExtractAxisToScalar, position : Optional[Bool] = None):

  
  is_defined_extract_axis_to_scalar_attr_hdl_(extract_axis_to_scalar_hdl, position)
  return 



@typecheck
def is_defined_extract_axis_to_scalar_attr_hdl_(extract_axis_to_scalar_hdl : ExtractAxisToScalar, position_ : Optional[Bool] = None):

  
  

  if position_  is not None:
    position_c = lib.cxios_is_defined_extract_axis_to_scalar_position(extract_axis_to_scalar_hdl)
    position_._c_value = ctypes.c_bool(position_c)
    
  
  return 



