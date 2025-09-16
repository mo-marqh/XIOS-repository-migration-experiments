# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.iextract_domain_to_axis import  get_extract_domain_to_axis_handle
from xios.oextract_domain_to_axis_attr import ExtractDomainToAxis

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_extract_domain_to_axis_direction.argtypes = [ExtractDomainToAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_extract_domain_to_axis_direction.restypes = None

lib.cxios_set_extract_domain_to_axis_direction.argtypes = [ExtractDomainToAxis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_extract_domain_to_axis_direction.restypes = None

lib.cxios_is_defined_extract_domain_to_axis_direction.argtypes = [ExtractDomainToAxis]
lib.cxios_is_defined_extract_domain_to_axis_direction.restypes = ctypes.c_bool

lib.cxios_get_extract_domain_to_axis_position.argtypes = [ExtractDomainToAxis, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_extract_domain_to_axis_position.restypes = None

lib.cxios_set_extract_domain_to_axis_position.argtypes = [ExtractDomainToAxis, ctypes.c_int]
lib.cxios_set_extract_domain_to_axis_position.restypes = None

lib.cxios_is_defined_extract_domain_to_axis_position.argtypes = [ExtractDomainToAxis]
lib.cxios_is_defined_extract_domain_to_axis_position.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_extract_domain_to_axis_attr(extract_domain_to_axis_id : Union[String, str], direction : Optional[Union[str, String]] = None,   
position : Optional[Union[Int, int]] = None):

  
  extract_domain_to_axis_hdl = ExtractDomainToAxis()
  

  get_extract_domain_to_axis_handle(extract_domain_to_axis_id, extract_domain_to_axis_hdl)
  set_extract_domain_to_axis_attr_hdl_(extract_domain_to_axis_hdl, direction, position)
  return 



@typecheck
def set_extract_domain_to_axis_attr_hdl(extract_domain_to_axis_hdl : ExtractDomainToAxis, direction : Optional[Union[str, String]] = None,   
position : Optional[Union[Int, int]] = None):

  
  set_extract_domain_to_axis_attr_hdl_(extract_domain_to_axis_hdl, direction, position)
  return 



@typecheck
def set_extract_domain_to_axis_attr_hdl_(extract_domain_to_axis_hdl : ExtractDomainToAxis, direction_ : Optional[Union[str, String]] = None,   
position_ : Optional[Union[Int, int]] = None):

  
  

  if direction_ is not None:
  
    direction_= String(direction_)
    direction_c = direction_._c_value
    len_direction_c = len(ctypes.string_at(direction_c))
    lib.cxios_set_extract_domain_to_axis_direction(extract_domain_to_axis_hdl, direction_c, len_direction_c)
    
  

  if position_ is not None: 
  
    position_ = Int(position_)
    position_c = position_._c_value
    lib.cxios_set_extract_domain_to_axis_position(extract_domain_to_axis_hdl, position_c)
    
  
  return 



@typecheck
def get_extract_domain_to_axis_attr(extract_domain_to_axis_id : Union[String, str], direction : Optional[String] = None, position : Optional[Int] = None):

  
  extract_domain_to_axis_hdl = ExtractDomainToAxis()
  

  get_extract_domain_to_axis_handle(extract_domain_to_axis_id, extract_domain_to_axis_hdl)
  get_extract_domain_to_axis_attr_hdl_(extract_domain_to_axis_hdl, direction, position)
  return 



@typecheck
def get_extract_domain_to_axis_attr_hdl(extract_domain_to_axis_hdl : ExtractDomainToAxis, direction : Optional[String] = None, position : Optional[Int] = None):

  
  get_extract_domain_to_axis_attr_hdl_(extract_domain_to_axis_hdl, direction, position)
  return 



@typecheck
def get_extract_domain_to_axis_attr_hdl_(extract_domain_to_axis_hdl : ExtractDomainToAxis, direction_ : Optional[String] = None, position_ : Optional[Int] = None):

  
  

  if direction_ is not None:
  
    direction_c = direction_._c_value
    len_direction_c = len(ctypes.string_at(direction_c))
    lib.cxios_get_extract_domain_to_axis_direction(extract_domain_to_axis_hdl, direction_c, len_direction_c)
    
  

  if position_ is not None: 
  
    position_c = position_._c_value
    lib.cxios_get_extract_domain_to_axis_position(extract_domain_to_axis_hdl, position_c)
    
  
  return 



@typecheck
def is_defined_extract_domain_to_axis_attr(extract_domain_to_axis_id : String, direction : Optional[Bool] = None, position : Optional[Bool] = None):

  
  extract_domain_to_axis_hdl = ExtractDomainToAxis()
  

  get_extract_domain_to_axis_handle(extract_domain_to_axis_id, extract_domain_to_axis_hdl)
  is_defined_extract_domain_to_axis_attr_hdl_(extract_domain_to_axis_hdl, direction, position)
  return 



@typecheck
def is_defined_extract_domain_to_axis_attr_hdl(extract_domain_to_axis_hdl : ExtractDomainToAxis, direction : Optional[Bool] = None, position : Optional[Bool] = None):

  
  is_defined_extract_domain_to_axis_attr_hdl_(extract_domain_to_axis_hdl, direction, position)
  return 



@typecheck
def is_defined_extract_domain_to_axis_attr_hdl_(extract_domain_to_axis_hdl : ExtractDomainToAxis, direction_ : Optional[Bool] = None, position_ : Optional[Bool] = None):

  
  

  if direction_  is not None:
    direction_c = lib.cxios_is_defined_extract_domain_to_axis_direction(extract_domain_to_axis_hdl)
    direction_._c_value = ctypes.c_bool(direction_c)
    
  

  if position_  is not None:
    position_c = lib.cxios_is_defined_extract_domain_to_axis_position(extract_domain_to_axis_hdl)
    position_._c_value = ctypes.c_bool(position_c)
    
  
  return 



