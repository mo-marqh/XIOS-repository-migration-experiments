# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.ireorder_domain import  get_reorder_domain_handle
from xios.oreorder_domain_attr import ReorderDomain

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_reorder_domain_invert_lat.argtypes = [ReorderDomain, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_reorder_domain_invert_lat.restypes = None

lib.cxios_set_reorder_domain_invert_lat.argtypes = [ReorderDomain, ctypes.c_bool]
lib.cxios_set_reorder_domain_invert_lat.restypes = None

lib.cxios_is_defined_reorder_domain_invert_lat.argtypes = [ReorderDomain]
lib.cxios_is_defined_reorder_domain_invert_lat.restypes = ctypes.c_bool

lib.cxios_get_reorder_domain_max_lon.argtypes = [ReorderDomain, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_reorder_domain_max_lon.restypes = None

lib.cxios_set_reorder_domain_max_lon.argtypes = [ReorderDomain, ctypes.c_double]
lib.cxios_set_reorder_domain_max_lon.restypes = None

lib.cxios_is_defined_reorder_domain_max_lon.argtypes = [ReorderDomain]
lib.cxios_is_defined_reorder_domain_max_lon.restypes = ctypes.c_bool

lib.cxios_get_reorder_domain_min_lon.argtypes = [ReorderDomain, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_reorder_domain_min_lon.restypes = None

lib.cxios_set_reorder_domain_min_lon.argtypes = [ReorderDomain, ctypes.c_double]
lib.cxios_set_reorder_domain_min_lon.restypes = None

lib.cxios_is_defined_reorder_domain_min_lon.argtypes = [ReorderDomain]
lib.cxios_is_defined_reorder_domain_min_lon.restypes = ctypes.c_bool

lib.cxios_get_reorder_domain_shift_lon_fraction.argtypes = [ReorderDomain, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_reorder_domain_shift_lon_fraction.restypes = None

lib.cxios_set_reorder_domain_shift_lon_fraction.argtypes = [ReorderDomain, ctypes.c_double]
lib.cxios_set_reorder_domain_shift_lon_fraction.restypes = None

lib.cxios_is_defined_reorder_domain_shift_lon_fraction.argtypes = [ReorderDomain]
lib.cxios_is_defined_reorder_domain_shift_lon_fraction.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_reorder_domain_attr(reorder_domain_id : Union[String, str], invert_lat : Optional[Union[Bool, bool]] = None, max_lon : Optional[Union[Double, float]] = None,   
min_lon : Optional[Union[Double, float]] = None, shift_lon_fraction : Optional[Union[Double, float]] = None):

  
  reorder_domain_hdl = ReorderDomain()
  

  get_reorder_domain_handle(reorder_domain_id, reorder_domain_hdl)
  set_reorder_domain_attr_hdl_(reorder_domain_hdl, invert_lat, max_lon, min_lon, shift_lon_fraction)
  return 



@typecheck
def set_reorder_domain_attr_hdl(reorder_domain_hdl : ReorderDomain, invert_lat : Optional[Union[Bool, bool]] = None, max_lon : Optional[Union[Double, float]] = None,   
min_lon : Optional[Union[Double, float]] = None, shift_lon_fraction : Optional[Union[Double, float]] = None):

  
  set_reorder_domain_attr_hdl_(reorder_domain_hdl, invert_lat, max_lon, min_lon, shift_lon_fraction)
  return 



@typecheck
def set_reorder_domain_attr_hdl_(reorder_domain_hdl : ReorderDomain, invert_lat_ : Optional[Union[Bool, bool]] = None, max_lon_ : Optional[Union[Double, float]] = None,   
min_lon_ : Optional[Union[Double, float]] = None, shift_lon_fraction_ : Optional[Union[Double, float]] = None):

  
  

  if invert_lat_ is not None: 
  
    invert_lat_ = Bool(invert_lat_)
    invert_lat_c = invert_lat_._c_value
    lib.cxios_set_reorder_domain_invert_lat(reorder_domain_hdl, invert_lat_c)
    
  

  if max_lon_ is not None: 
  
    max_lon_ = Double(max_lon_)
    max_lon_c = max_lon_._c_value
    lib.cxios_set_reorder_domain_max_lon(reorder_domain_hdl, max_lon_c)
    
  

  if min_lon_ is not None: 
  
    min_lon_ = Double(min_lon_)
    min_lon_c = min_lon_._c_value
    lib.cxios_set_reorder_domain_min_lon(reorder_domain_hdl, min_lon_c)
    
  

  if shift_lon_fraction_ is not None: 
  
    shift_lon_fraction_ = Double(shift_lon_fraction_)
    shift_lon_fraction_c = shift_lon_fraction_._c_value
    lib.cxios_set_reorder_domain_shift_lon_fraction(reorder_domain_hdl, shift_lon_fraction_c)
    
  
  return 



@typecheck
def get_reorder_domain_attr(reorder_domain_id : Union[String, str], invert_lat : Optional[Bool] = None, max_lon : Optional[Double] = None,   
min_lon : Optional[Double] = None, shift_lon_fraction : Optional[Double] = None):

  
  reorder_domain_hdl = ReorderDomain()
  

  get_reorder_domain_handle(reorder_domain_id, reorder_domain_hdl)
  get_reorder_domain_attr_hdl_(reorder_domain_hdl, invert_lat, max_lon, min_lon, shift_lon_fraction)
  return 



@typecheck
def get_reorder_domain_attr_hdl(reorder_domain_hdl : ReorderDomain, invert_lat : Optional[Bool] = None, max_lon : Optional[Double] = None,   
min_lon : Optional[Double] = None, shift_lon_fraction : Optional[Double] = None):

  
  get_reorder_domain_attr_hdl_(reorder_domain_hdl, invert_lat, max_lon, min_lon, shift_lon_fraction)
  return 



@typecheck
def get_reorder_domain_attr_hdl_(reorder_domain_hdl : ReorderDomain, invert_lat_ : Optional[Bool] = None, max_lon_ : Optional[Double] = None,   
min_lon_ : Optional[Double] = None, shift_lon_fraction_ : Optional[Double] = None):

  
  

  if invert_lat_ is not None: 
  
    invert_lat_c = invert_lat_._c_value
    lib.cxios_get_reorder_domain_invert_lat(reorder_domain_hdl, invert_lat_c)
    
  

  if max_lon_ is not None: 
  
    max_lon_c = max_lon_._c_value
    lib.cxios_get_reorder_domain_max_lon(reorder_domain_hdl, max_lon_c)
    
  

  if min_lon_ is not None: 
  
    min_lon_c = min_lon_._c_value
    lib.cxios_get_reorder_domain_min_lon(reorder_domain_hdl, min_lon_c)
    
  

  if shift_lon_fraction_ is not None: 
  
    shift_lon_fraction_c = shift_lon_fraction_._c_value
    lib.cxios_get_reorder_domain_shift_lon_fraction(reorder_domain_hdl, shift_lon_fraction_c)
    
  
  return 



@typecheck
def is_defined_reorder_domain_attr(reorder_domain_id : String, invert_lat : Optional[Bool] = None, max_lon : Optional[Bool] = None,   
min_lon : Optional[Bool] = None, shift_lon_fraction : Optional[Bool] = None):

  
  reorder_domain_hdl = ReorderDomain()
  

  get_reorder_domain_handle(reorder_domain_id, reorder_domain_hdl)
  is_defined_reorder_domain_attr_hdl_(reorder_domain_hdl, invert_lat, max_lon, min_lon, shift_lon_fraction)
  return 



@typecheck
def is_defined_reorder_domain_attr_hdl(reorder_domain_hdl : ReorderDomain, invert_lat : Optional[Bool] = None, max_lon : Optional[Bool] = None,   
min_lon : Optional[Bool] = None, shift_lon_fraction : Optional[Bool] = None):

  
  is_defined_reorder_domain_attr_hdl_(reorder_domain_hdl, invert_lat, max_lon, min_lon, shift_lon_fraction)
  return 



@typecheck
def is_defined_reorder_domain_attr_hdl_(reorder_domain_hdl : ReorderDomain, invert_lat_ : Optional[Bool] = None, max_lon_ : Optional[Bool] = None,   
min_lon_ : Optional[Bool] = None, shift_lon_fraction_ : Optional[Bool] = None):

  
  

  if invert_lat_  is not None:
    invert_lat_c = lib.cxios_is_defined_reorder_domain_invert_lat(reorder_domain_hdl)
    invert_lat_._c_value = ctypes.c_bool(invert_lat_c)
    
  

  if max_lon_  is not None:
    max_lon_c = lib.cxios_is_defined_reorder_domain_max_lon(reorder_domain_hdl)
    max_lon_._c_value = ctypes.c_bool(max_lon_c)
    
  

  if min_lon_  is not None:
    min_lon_c = lib.cxios_is_defined_reorder_domain_min_lon(reorder_domain_hdl)
    min_lon_._c_value = ctypes.c_bool(min_lon_c)
    
  

  if shift_lon_fraction_  is not None:
    shift_lon_fraction_c = lib.cxios_is_defined_reorder_domain_shift_lon_fraction(reorder_domain_hdl)
    shift_lon_fraction_._c_value = ctypes.c_bool(shift_lon_fraction_c)
    
  
  return 



