# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.iinterpolate_domain import  get_interpolate_domain_handle
from xios.ointerpolate_domain_attr import InterpolateDomain

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_interpolate_domain_detect_missing_value.argtypes = [InterpolateDomain, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_interpolate_domain_detect_missing_value.restypes = None

lib.cxios_set_interpolate_domain_detect_missing_value.argtypes = [InterpolateDomain, ctypes.c_bool]
lib.cxios_set_interpolate_domain_detect_missing_value.restypes = None

lib.cxios_is_defined_interpolate_domain_detect_missing_value.argtypes = [InterpolateDomain]
lib.cxios_is_defined_interpolate_domain_detect_missing_value.restypes = ctypes.c_bool

lib.cxios_get_interpolate_domain_mode.argtypes = [InterpolateDomain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_interpolate_domain_mode.restypes = None

lib.cxios_set_interpolate_domain_mode.argtypes = [InterpolateDomain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_interpolate_domain_mode.restypes = None

lib.cxios_is_defined_interpolate_domain_mode.argtypes = [InterpolateDomain]
lib.cxios_is_defined_interpolate_domain_mode.restypes = ctypes.c_bool

lib.cxios_get_interpolate_domain_order.argtypes = [InterpolateDomain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_interpolate_domain_order.restypes = None

lib.cxios_set_interpolate_domain_order.argtypes = [InterpolateDomain, ctypes.c_int]
lib.cxios_set_interpolate_domain_order.restypes = None

lib.cxios_is_defined_interpolate_domain_order.argtypes = [InterpolateDomain]
lib.cxios_is_defined_interpolate_domain_order.restypes = ctypes.c_bool

lib.cxios_get_interpolate_domain_quantity.argtypes = [InterpolateDomain, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_interpolate_domain_quantity.restypes = None

lib.cxios_set_interpolate_domain_quantity.argtypes = [InterpolateDomain, ctypes.c_bool]
lib.cxios_set_interpolate_domain_quantity.restypes = None

lib.cxios_is_defined_interpolate_domain_quantity.argtypes = [InterpolateDomain]
lib.cxios_is_defined_interpolate_domain_quantity.restypes = ctypes.c_bool

lib.cxios_get_interpolate_domain_read_write_convention.argtypes = [InterpolateDomain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_interpolate_domain_read_write_convention.restypes = None

lib.cxios_set_interpolate_domain_read_write_convention.argtypes = [InterpolateDomain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_interpolate_domain_read_write_convention.restypes = None

lib.cxios_is_defined_interpolate_domain_read_write_convention.argtypes = [InterpolateDomain]
lib.cxios_is_defined_interpolate_domain_read_write_convention.restypes = ctypes.c_bool

lib.cxios_get_interpolate_domain_renormalize.argtypes = [InterpolateDomain, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_interpolate_domain_renormalize.restypes = None

lib.cxios_set_interpolate_domain_renormalize.argtypes = [InterpolateDomain, ctypes.c_bool]
lib.cxios_set_interpolate_domain_renormalize.restypes = None

lib.cxios_is_defined_interpolate_domain_renormalize.argtypes = [InterpolateDomain]
lib.cxios_is_defined_interpolate_domain_renormalize.restypes = ctypes.c_bool

lib.cxios_get_interpolate_domain_use_area.argtypes = [InterpolateDomain, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_interpolate_domain_use_area.restypes = None

lib.cxios_set_interpolate_domain_use_area.argtypes = [InterpolateDomain, ctypes.c_bool]
lib.cxios_set_interpolate_domain_use_area.restypes = None

lib.cxios_is_defined_interpolate_domain_use_area.argtypes = [InterpolateDomain]
lib.cxios_is_defined_interpolate_domain_use_area.restypes = ctypes.c_bool

lib.cxios_get_interpolate_domain_weight_filename.argtypes = [InterpolateDomain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_interpolate_domain_weight_filename.restypes = None

lib.cxios_set_interpolate_domain_weight_filename.argtypes = [InterpolateDomain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_interpolate_domain_weight_filename.restypes = None

lib.cxios_is_defined_interpolate_domain_weight_filename.argtypes = [InterpolateDomain]
lib.cxios_is_defined_interpolate_domain_weight_filename.restypes = ctypes.c_bool

lib.cxios_get_interpolate_domain_write_weight.argtypes = [InterpolateDomain, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_interpolate_domain_write_weight.restypes = None

lib.cxios_set_interpolate_domain_write_weight.argtypes = [InterpolateDomain, ctypes.c_bool]
lib.cxios_set_interpolate_domain_write_weight.restypes = None

lib.cxios_is_defined_interpolate_domain_write_weight.argtypes = [InterpolateDomain]
lib.cxios_is_defined_interpolate_domain_write_weight.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_interpolate_domain_attr(interpolate_domain_id : Union[String, str], detect_missing_value : Optional[Union[Bool, bool]] = None,   
mode : Optional[Union[str, String]] = None, order : Optional[Union[Int, int]] = None, quantity : Optional[Union[Bool, bool]] = None,   
read_write_convention : Optional[Union[str, String]] = None, renormalize : Optional[Union[Bool, bool]] = None,   
use_area : Optional[Union[Bool, bool]] = None, weight_filename : Optional[Union[str, String]] = None,   
write_weight : Optional[Union[Bool, bool]] = None):

  
  interpolate_domain_hdl = InterpolateDomain()
  

  get_interpolate_domain_handle(interpolate_domain_id, interpolate_domain_hdl)
  set_interpolate_domain_attr_hdl_(interpolate_domain_hdl, detect_missing_value, mode, order, quantity, read_write_convention,  
   renormalize, use_area, weight_filename, write_weight)
  return 



@typecheck
def set_interpolate_domain_attr_hdl(interpolate_domain_hdl : InterpolateDomain, detect_missing_value : Optional[Union[Bool, bool]] = None,   
mode : Optional[Union[str, String]] = None, order : Optional[Union[Int, int]] = None, quantity : Optional[Union[Bool, bool]] = None,   
read_write_convention : Optional[Union[str, String]] = None, renormalize : Optional[Union[Bool, bool]] = None,   
use_area : Optional[Union[Bool, bool]] = None, weight_filename : Optional[Union[str, String]] = None,   
write_weight : Optional[Union[Bool, bool]] = None):

  
  set_interpolate_domain_attr_hdl_(interpolate_domain_hdl, detect_missing_value, mode, order, quantity, read_write_convention,  
   renormalize, use_area, weight_filename, write_weight)
  return 



@typecheck
def set_interpolate_domain_attr_hdl_(interpolate_domain_hdl : InterpolateDomain, detect_missing_value_ : Optional[Union[Bool, bool]] = None,   
mode_ : Optional[Union[str, String]] = None, order_ : Optional[Union[Int, int]] = None, quantity_ : Optional[Union[Bool, bool]] = None,   
read_write_convention_ : Optional[Union[str, String]] = None, renormalize_ : Optional[Union[Bool, bool]] = None,   
use_area_ : Optional[Union[Bool, bool]] = None, weight_filename_ : Optional[Union[str, String]] = None,   
write_weight_ : Optional[Union[Bool, bool]] = None):

  
  

  if detect_missing_value_ is not None: 
  
    detect_missing_value_ = Bool(detect_missing_value_)
    detect_missing_value_c = detect_missing_value_._c_value
    lib.cxios_set_interpolate_domain_detect_missing_value(interpolate_domain_hdl, detect_missing_value_c)
    
  

  if mode_ is not None:
  
    mode_= String(mode_)
    mode_c = mode_._c_value
    len_mode_c = len(ctypes.string_at(mode_c))
    lib.cxios_set_interpolate_domain_mode(interpolate_domain_hdl, mode_c, len_mode_c)
    
  

  if order_ is not None: 
  
    order_ = Int(order_)
    order_c = order_._c_value
    lib.cxios_set_interpolate_domain_order(interpolate_domain_hdl, order_c)
    
  

  if quantity_ is not None: 
  
    quantity_ = Bool(quantity_)
    quantity_c = quantity_._c_value
    lib.cxios_set_interpolate_domain_quantity(interpolate_domain_hdl, quantity_c)
    
  

  if read_write_convention_ is not None:
  
    read_write_convention_= String(read_write_convention_)
    read_write_convention_c = read_write_convention_._c_value
    len_read_write_convention_c = len(ctypes.string_at(read_write_convention_c))
    lib.cxios_set_interpolate_domain_read_write_convention(interpolate_domain_hdl, read_write_convention_c, len_read_write_convention_c)
    
  

  if renormalize_ is not None: 
  
    renormalize_ = Bool(renormalize_)
    renormalize_c = renormalize_._c_value
    lib.cxios_set_interpolate_domain_renormalize(interpolate_domain_hdl, renormalize_c)
    
  

  if use_area_ is not None: 
  
    use_area_ = Bool(use_area_)
    use_area_c = use_area_._c_value
    lib.cxios_set_interpolate_domain_use_area(interpolate_domain_hdl, use_area_c)
    
  

  if weight_filename_ is not None:
  
    weight_filename_= String(weight_filename_)
    weight_filename_c = weight_filename_._c_value
    len_weight_filename_c = len(ctypes.string_at(weight_filename_c))
    lib.cxios_set_interpolate_domain_weight_filename(interpolate_domain_hdl, weight_filename_c, len_weight_filename_c)
    
  

  if write_weight_ is not None: 
  
    write_weight_ = Bool(write_weight_)
    write_weight_c = write_weight_._c_value
    lib.cxios_set_interpolate_domain_write_weight(interpolate_domain_hdl, write_weight_c)
    
  
  return 



@typecheck
def get_interpolate_domain_attr(interpolate_domain_id : Union[String, str], detect_missing_value : Optional[Bool] = None,   
mode : Optional[String] = None, order : Optional[Int] = None, quantity : Optional[Bool] = None,   
read_write_convention : Optional[String] = None, renormalize : Optional[Bool] = None, use_area : Optional[Bool] = None,   
weight_filename : Optional[String] = None, write_weight : Optional[Bool] = None):

  
  interpolate_domain_hdl = InterpolateDomain()
  

  get_interpolate_domain_handle(interpolate_domain_id, interpolate_domain_hdl)
  get_interpolate_domain_attr_hdl_(interpolate_domain_hdl, detect_missing_value, mode, order, quantity, read_write_convention,  
   renormalize, use_area, weight_filename, write_weight)
  return 



@typecheck
def get_interpolate_domain_attr_hdl(interpolate_domain_hdl : InterpolateDomain, detect_missing_value : Optional[Bool] = None,   
mode : Optional[String] = None, order : Optional[Int] = None, quantity : Optional[Bool] = None,   
read_write_convention : Optional[String] = None, renormalize : Optional[Bool] = None, use_area : Optional[Bool] = None,   
weight_filename : Optional[String] = None, write_weight : Optional[Bool] = None):

  
  get_interpolate_domain_attr_hdl_(interpolate_domain_hdl, detect_missing_value, mode, order, quantity, read_write_convention,  
   renormalize, use_area, weight_filename, write_weight)
  return 



@typecheck
def get_interpolate_domain_attr_hdl_(interpolate_domain_hdl : InterpolateDomain, detect_missing_value_ : Optional[Bool] = None,   
mode_ : Optional[String] = None, order_ : Optional[Int] = None, quantity_ : Optional[Bool] = None,   
read_write_convention_ : Optional[String] = None, renormalize_ : Optional[Bool] = None, use_area_ : Optional[Bool] = None,   
weight_filename_ : Optional[String] = None, write_weight_ : Optional[Bool] = None):

  
  

  if detect_missing_value_ is not None: 
  
    detect_missing_value_c = detect_missing_value_._c_value
    lib.cxios_get_interpolate_domain_detect_missing_value(interpolate_domain_hdl, detect_missing_value_c)
    
  

  if mode_ is not None:
  
    mode_c = mode_._c_value
    len_mode_c = len(ctypes.string_at(mode_c))
    lib.cxios_get_interpolate_domain_mode(interpolate_domain_hdl, mode_c, len_mode_c)
    
  

  if order_ is not None: 
  
    order_c = order_._c_value
    lib.cxios_get_interpolate_domain_order(interpolate_domain_hdl, order_c)
    
  

  if quantity_ is not None: 
  
    quantity_c = quantity_._c_value
    lib.cxios_get_interpolate_domain_quantity(interpolate_domain_hdl, quantity_c)
    
  

  if read_write_convention_ is not None:
  
    read_write_convention_c = read_write_convention_._c_value
    len_read_write_convention_c = len(ctypes.string_at(read_write_convention_c))
    lib.cxios_get_interpolate_domain_read_write_convention(interpolate_domain_hdl, read_write_convention_c, len_read_write_convention_c)
    
  

  if renormalize_ is not None: 
  
    renormalize_c = renormalize_._c_value
    lib.cxios_get_interpolate_domain_renormalize(interpolate_domain_hdl, renormalize_c)
    
  

  if use_area_ is not None: 
  
    use_area_c = use_area_._c_value
    lib.cxios_get_interpolate_domain_use_area(interpolate_domain_hdl, use_area_c)
    
  

  if weight_filename_ is not None:
  
    weight_filename_c = weight_filename_._c_value
    len_weight_filename_c = len(ctypes.string_at(weight_filename_c))
    lib.cxios_get_interpolate_domain_weight_filename(interpolate_domain_hdl, weight_filename_c, len_weight_filename_c)
    
  

  if write_weight_ is not None: 
  
    write_weight_c = write_weight_._c_value
    lib.cxios_get_interpolate_domain_write_weight(interpolate_domain_hdl, write_weight_c)
    
  
  return 



@typecheck
def is_defined_interpolate_domain_attr(interpolate_domain_id : String, detect_missing_value : Optional[Bool] = None, mode : Optional[Bool] = None,   
order : Optional[Bool] = None, quantity : Optional[Bool] = None, read_write_convention : Optional[Bool] = None,   
renormalize : Optional[Bool] = None, use_area : Optional[Bool] = None, weight_filename : Optional[Bool] = None,   
write_weight : Optional[Bool] = None):

  
  interpolate_domain_hdl = InterpolateDomain()
  

  get_interpolate_domain_handle(interpolate_domain_id, interpolate_domain_hdl)
  is_defined_interpolate_domain_attr_hdl_(interpolate_domain_hdl, detect_missing_value, mode, order, quantity, read_write_convention,  
   renormalize, use_area, weight_filename, write_weight)
  return 



@typecheck
def is_defined_interpolate_domain_attr_hdl(interpolate_domain_hdl : InterpolateDomain, detect_missing_value : Optional[Bool] = None,   
mode : Optional[Bool] = None, order : Optional[Bool] = None, quantity : Optional[Bool] = None,   
read_write_convention : Optional[Bool] = None, renormalize : Optional[Bool] = None, use_area : Optional[Bool] = None,   
weight_filename : Optional[Bool] = None, write_weight : Optional[Bool] = None):

  
  is_defined_interpolate_domain_attr_hdl_(interpolate_domain_hdl, detect_missing_value, mode, order, quantity, read_write_convention,  
   renormalize, use_area, weight_filename, write_weight)
  return 



@typecheck
def is_defined_interpolate_domain_attr_hdl_(interpolate_domain_hdl : InterpolateDomain, detect_missing_value_ : Optional[Bool] = None,   
mode_ : Optional[Bool] = None, order_ : Optional[Bool] = None, quantity_ : Optional[Bool] = None,   
read_write_convention_ : Optional[Bool] = None, renormalize_ : Optional[Bool] = None, use_area_ : Optional[Bool] = None,   
weight_filename_ : Optional[Bool] = None, write_weight_ : Optional[Bool] = None):

  
  

  if detect_missing_value_  is not None:
    detect_missing_value_c = lib.cxios_is_defined_interpolate_domain_detect_missing_value(interpolate_domain_hdl)
    detect_missing_value_._c_value = ctypes.c_bool(detect_missing_value_c)
    
  

  if mode_  is not None:
    mode_c = lib.cxios_is_defined_interpolate_domain_mode(interpolate_domain_hdl)
    mode_._c_value = ctypes.c_bool(mode_c)
    
  

  if order_  is not None:
    order_c = lib.cxios_is_defined_interpolate_domain_order(interpolate_domain_hdl)
    order_._c_value = ctypes.c_bool(order_c)
    
  

  if quantity_  is not None:
    quantity_c = lib.cxios_is_defined_interpolate_domain_quantity(interpolate_domain_hdl)
    quantity_._c_value = ctypes.c_bool(quantity_c)
    
  

  if read_write_convention_  is not None:
    read_write_convention_c = lib.cxios_is_defined_interpolate_domain_read_write_convention(interpolate_domain_hdl)
    read_write_convention_._c_value = ctypes.c_bool(read_write_convention_c)
    
  

  if renormalize_  is not None:
    renormalize_c = lib.cxios_is_defined_interpolate_domain_renormalize(interpolate_domain_hdl)
    renormalize_._c_value = ctypes.c_bool(renormalize_c)
    
  

  if use_area_  is not None:
    use_area_c = lib.cxios_is_defined_interpolate_domain_use_area(interpolate_domain_hdl)
    use_area_._c_value = ctypes.c_bool(use_area_c)
    
  

  if weight_filename_  is not None:
    weight_filename_c = lib.cxios_is_defined_interpolate_domain_weight_filename(interpolate_domain_hdl)
    weight_filename_._c_value = ctypes.c_bool(weight_filename_c)
    
  

  if write_weight_  is not None:
    write_weight_c = lib.cxios_is_defined_interpolate_domain_write_weight(interpolate_domain_hdl)
    write_weight_._c_value = ctypes.c_bool(write_weight_c)
    
  
  return 



