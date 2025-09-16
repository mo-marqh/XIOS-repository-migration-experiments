# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.igenerate_rectilinear_domain import  get_generate_rectilinear_domain_handle
from xios.ogenerate_rectilinear_domain_attr import GenerateRectilinearDomain

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_generate_rectilinear_domain_bounds_lat_end.argtypes = [GenerateRectilinearDomain, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_generate_rectilinear_domain_bounds_lat_end.restypes = None

lib.cxios_set_generate_rectilinear_domain_bounds_lat_end.argtypes = [GenerateRectilinearDomain, ctypes.c_double]
lib.cxios_set_generate_rectilinear_domain_bounds_lat_end.restypes = None

lib.cxios_is_defined_generate_rectilinear_domain_bounds_lat_end.argtypes = [GenerateRectilinearDomain]
lib.cxios_is_defined_generate_rectilinear_domain_bounds_lat_end.restypes = ctypes.c_bool

lib.cxios_get_generate_rectilinear_domain_bounds_lat_start.argtypes = [GenerateRectilinearDomain, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_generate_rectilinear_domain_bounds_lat_start.restypes = None

lib.cxios_set_generate_rectilinear_domain_bounds_lat_start.argtypes = [GenerateRectilinearDomain, ctypes.c_double]
lib.cxios_set_generate_rectilinear_domain_bounds_lat_start.restypes = None

lib.cxios_is_defined_generate_rectilinear_domain_bounds_lat_start.argtypes = [GenerateRectilinearDomain]
lib.cxios_is_defined_generate_rectilinear_domain_bounds_lat_start.restypes = ctypes.c_bool

lib.cxios_get_generate_rectilinear_domain_bounds_lon_end.argtypes = [GenerateRectilinearDomain, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_generate_rectilinear_domain_bounds_lon_end.restypes = None

lib.cxios_set_generate_rectilinear_domain_bounds_lon_end.argtypes = [GenerateRectilinearDomain, ctypes.c_double]
lib.cxios_set_generate_rectilinear_domain_bounds_lon_end.restypes = None

lib.cxios_is_defined_generate_rectilinear_domain_bounds_lon_end.argtypes = [GenerateRectilinearDomain]
lib.cxios_is_defined_generate_rectilinear_domain_bounds_lon_end.restypes = ctypes.c_bool

lib.cxios_get_generate_rectilinear_domain_bounds_lon_start.argtypes = [GenerateRectilinearDomain, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_generate_rectilinear_domain_bounds_lon_start.restypes = None

lib.cxios_set_generate_rectilinear_domain_bounds_lon_start.argtypes = [GenerateRectilinearDomain, ctypes.c_double]
lib.cxios_set_generate_rectilinear_domain_bounds_lon_start.restypes = None

lib.cxios_is_defined_generate_rectilinear_domain_bounds_lon_start.argtypes = [GenerateRectilinearDomain]
lib.cxios_is_defined_generate_rectilinear_domain_bounds_lon_start.restypes = ctypes.c_bool

lib.cxios_get_generate_rectilinear_domain_lat_end.argtypes = [GenerateRectilinearDomain, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_generate_rectilinear_domain_lat_end.restypes = None

lib.cxios_set_generate_rectilinear_domain_lat_end.argtypes = [GenerateRectilinearDomain, ctypes.c_double]
lib.cxios_set_generate_rectilinear_domain_lat_end.restypes = None

lib.cxios_is_defined_generate_rectilinear_domain_lat_end.argtypes = [GenerateRectilinearDomain]
lib.cxios_is_defined_generate_rectilinear_domain_lat_end.restypes = ctypes.c_bool

lib.cxios_get_generate_rectilinear_domain_lat_start.argtypes = [GenerateRectilinearDomain, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_generate_rectilinear_domain_lat_start.restypes = None

lib.cxios_set_generate_rectilinear_domain_lat_start.argtypes = [GenerateRectilinearDomain, ctypes.c_double]
lib.cxios_set_generate_rectilinear_domain_lat_start.restypes = None

lib.cxios_is_defined_generate_rectilinear_domain_lat_start.argtypes = [GenerateRectilinearDomain]
lib.cxios_is_defined_generate_rectilinear_domain_lat_start.restypes = ctypes.c_bool

lib.cxios_get_generate_rectilinear_domain_lon_end.argtypes = [GenerateRectilinearDomain, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_generate_rectilinear_domain_lon_end.restypes = None

lib.cxios_set_generate_rectilinear_domain_lon_end.argtypes = [GenerateRectilinearDomain, ctypes.c_double]
lib.cxios_set_generate_rectilinear_domain_lon_end.restypes = None

lib.cxios_is_defined_generate_rectilinear_domain_lon_end.argtypes = [GenerateRectilinearDomain]
lib.cxios_is_defined_generate_rectilinear_domain_lon_end.restypes = ctypes.c_bool

lib.cxios_get_generate_rectilinear_domain_lon_start.argtypes = [GenerateRectilinearDomain, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_generate_rectilinear_domain_lon_start.restypes = None

lib.cxios_set_generate_rectilinear_domain_lon_start.argtypes = [GenerateRectilinearDomain, ctypes.c_double]
lib.cxios_set_generate_rectilinear_domain_lon_start.restypes = None

lib.cxios_is_defined_generate_rectilinear_domain_lon_start.argtypes = [GenerateRectilinearDomain]
lib.cxios_is_defined_generate_rectilinear_domain_lon_start.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_generate_rectilinear_domain_attr(generate_rectilinear_domain_id : Union[String, str], bounds_lat_end : Optional[Union[Double, float]] = None,   
bounds_lat_start : Optional[Union[Double, float]] = None, bounds_lon_end : Optional[Union[Double, float]] = None,   
bounds_lon_start : Optional[Union[Double, float]] = None, lat_end : Optional[Union[Double, float]] = None,   
lat_start : Optional[Union[Double, float]] = None, lon_end : Optional[Union[Double, float]] = None,   
lon_start : Optional[Union[Double, float]] = None):

  
  generate_rectilinear_domain_hdl = GenerateRectilinearDomain()
  

  get_generate_rectilinear_domain_handle(generate_rectilinear_domain_id, generate_rectilinear_domain_hdl)
  set_generate_rectilinear_domain_attr_hdl_(generate_rectilinear_domain_hdl, bounds_lat_end, bounds_lat_start, bounds_lon_end, bounds_lon_start,  
   lat_end, lat_start, lon_end, lon_start)
  return 



@typecheck
def set_generate_rectilinear_domain_attr_hdl(generate_rectilinear_domain_hdl : GenerateRectilinearDomain, bounds_lat_end : Optional[Union[Double, float]] = None,   
bounds_lat_start : Optional[Union[Double, float]] = None, bounds_lon_end : Optional[Union[Double, float]] = None,   
bounds_lon_start : Optional[Union[Double, float]] = None, lat_end : Optional[Union[Double, float]] = None,   
lat_start : Optional[Union[Double, float]] = None, lon_end : Optional[Union[Double, float]] = None,   
lon_start : Optional[Union[Double, float]] = None):

  
  set_generate_rectilinear_domain_attr_hdl_(generate_rectilinear_domain_hdl, bounds_lat_end, bounds_lat_start, bounds_lon_end, bounds_lon_start,  
   lat_end, lat_start, lon_end, lon_start)
  return 



@typecheck
def set_generate_rectilinear_domain_attr_hdl_(generate_rectilinear_domain_hdl : GenerateRectilinearDomain, bounds_lat_end_ : Optional[Union[Double, float]] = None,   
bounds_lat_start_ : Optional[Union[Double, float]] = None, bounds_lon_end_ : Optional[Union[Double, float]] = None,   
bounds_lon_start_ : Optional[Union[Double, float]] = None, lat_end_ : Optional[Union[Double, float]] = None,   
lat_start_ : Optional[Union[Double, float]] = None, lon_end_ : Optional[Union[Double, float]] = None,   
lon_start_ : Optional[Union[Double, float]] = None):

  
  

  if bounds_lat_end_ is not None: 
  
    bounds_lat_end_ = Double(bounds_lat_end_)
    bounds_lat_end_c = bounds_lat_end_._c_value
    lib.cxios_set_generate_rectilinear_domain_bounds_lat_end(generate_rectilinear_domain_hdl, bounds_lat_end_c)
    
  

  if bounds_lat_start_ is not None: 
  
    bounds_lat_start_ = Double(bounds_lat_start_)
    bounds_lat_start_c = bounds_lat_start_._c_value
    lib.cxios_set_generate_rectilinear_domain_bounds_lat_start(generate_rectilinear_domain_hdl, bounds_lat_start_c)
    
  

  if bounds_lon_end_ is not None: 
  
    bounds_lon_end_ = Double(bounds_lon_end_)
    bounds_lon_end_c = bounds_lon_end_._c_value
    lib.cxios_set_generate_rectilinear_domain_bounds_lon_end(generate_rectilinear_domain_hdl, bounds_lon_end_c)
    
  

  if bounds_lon_start_ is not None: 
  
    bounds_lon_start_ = Double(bounds_lon_start_)
    bounds_lon_start_c = bounds_lon_start_._c_value
    lib.cxios_set_generate_rectilinear_domain_bounds_lon_start(generate_rectilinear_domain_hdl, bounds_lon_start_c)
    
  

  if lat_end_ is not None: 
  
    lat_end_ = Double(lat_end_)
    lat_end_c = lat_end_._c_value
    lib.cxios_set_generate_rectilinear_domain_lat_end(generate_rectilinear_domain_hdl, lat_end_c)
    
  

  if lat_start_ is not None: 
  
    lat_start_ = Double(lat_start_)
    lat_start_c = lat_start_._c_value
    lib.cxios_set_generate_rectilinear_domain_lat_start(generate_rectilinear_domain_hdl, lat_start_c)
    
  

  if lon_end_ is not None: 
  
    lon_end_ = Double(lon_end_)
    lon_end_c = lon_end_._c_value
    lib.cxios_set_generate_rectilinear_domain_lon_end(generate_rectilinear_domain_hdl, lon_end_c)
    
  

  if lon_start_ is not None: 
  
    lon_start_ = Double(lon_start_)
    lon_start_c = lon_start_._c_value
    lib.cxios_set_generate_rectilinear_domain_lon_start(generate_rectilinear_domain_hdl, lon_start_c)
    
  
  return 



@typecheck
def get_generate_rectilinear_domain_attr(generate_rectilinear_domain_id : Union[String, str], bounds_lat_end : Optional[Double] = None,   
bounds_lat_start : Optional[Double] = None, bounds_lon_end : Optional[Double] = None, bounds_lon_start : Optional[Double] = None,   
lat_end : Optional[Double] = None, lat_start : Optional[Double] = None, lon_end : Optional[Double] = None,   
lon_start : Optional[Double] = None):

  
  generate_rectilinear_domain_hdl = GenerateRectilinearDomain()
  

  get_generate_rectilinear_domain_handle(generate_rectilinear_domain_id, generate_rectilinear_domain_hdl)
  get_generate_rectilinear_domain_attr_hdl_(generate_rectilinear_domain_hdl, bounds_lat_end, bounds_lat_start, bounds_lon_end, bounds_lon_start,  
   lat_end, lat_start, lon_end, lon_start)
  return 



@typecheck
def get_generate_rectilinear_domain_attr_hdl(generate_rectilinear_domain_hdl : GenerateRectilinearDomain, bounds_lat_end : Optional[Double] = None,   
bounds_lat_start : Optional[Double] = None, bounds_lon_end : Optional[Double] = None, bounds_lon_start : Optional[Double] = None,   
lat_end : Optional[Double] = None, lat_start : Optional[Double] = None, lon_end : Optional[Double] = None,   
lon_start : Optional[Double] = None):

  
  get_generate_rectilinear_domain_attr_hdl_(generate_rectilinear_domain_hdl, bounds_lat_end, bounds_lat_start, bounds_lon_end, bounds_lon_start,  
   lat_end, lat_start, lon_end, lon_start)
  return 



@typecheck
def get_generate_rectilinear_domain_attr_hdl_(generate_rectilinear_domain_hdl : GenerateRectilinearDomain, bounds_lat_end_ : Optional[Double] = None,   
bounds_lat_start_ : Optional[Double] = None, bounds_lon_end_ : Optional[Double] = None, bounds_lon_start_ : Optional[Double] = None,   
lat_end_ : Optional[Double] = None, lat_start_ : Optional[Double] = None, lon_end_ : Optional[Double] = None,   
lon_start_ : Optional[Double] = None):

  
  

  if bounds_lat_end_ is not None: 
  
    bounds_lat_end_c = bounds_lat_end_._c_value
    lib.cxios_get_generate_rectilinear_domain_bounds_lat_end(generate_rectilinear_domain_hdl, bounds_lat_end_c)
    
  

  if bounds_lat_start_ is not None: 
  
    bounds_lat_start_c = bounds_lat_start_._c_value
    lib.cxios_get_generate_rectilinear_domain_bounds_lat_start(generate_rectilinear_domain_hdl, bounds_lat_start_c)
    
  

  if bounds_lon_end_ is not None: 
  
    bounds_lon_end_c = bounds_lon_end_._c_value
    lib.cxios_get_generate_rectilinear_domain_bounds_lon_end(generate_rectilinear_domain_hdl, bounds_lon_end_c)
    
  

  if bounds_lon_start_ is not None: 
  
    bounds_lon_start_c = bounds_lon_start_._c_value
    lib.cxios_get_generate_rectilinear_domain_bounds_lon_start(generate_rectilinear_domain_hdl, bounds_lon_start_c)
    
  

  if lat_end_ is not None: 
  
    lat_end_c = lat_end_._c_value
    lib.cxios_get_generate_rectilinear_domain_lat_end(generate_rectilinear_domain_hdl, lat_end_c)
    
  

  if lat_start_ is not None: 
  
    lat_start_c = lat_start_._c_value
    lib.cxios_get_generate_rectilinear_domain_lat_start(generate_rectilinear_domain_hdl, lat_start_c)
    
  

  if lon_end_ is not None: 
  
    lon_end_c = lon_end_._c_value
    lib.cxios_get_generate_rectilinear_domain_lon_end(generate_rectilinear_domain_hdl, lon_end_c)
    
  

  if lon_start_ is not None: 
  
    lon_start_c = lon_start_._c_value
    lib.cxios_get_generate_rectilinear_domain_lon_start(generate_rectilinear_domain_hdl, lon_start_c)
    
  
  return 



@typecheck
def is_defined_generate_rectilinear_domain_attr(generate_rectilinear_domain_id : String, bounds_lat_end : Optional[Bool] = None, bounds_lat_start : Optional[Bool] = None,   
bounds_lon_end : Optional[Bool] = None, bounds_lon_start : Optional[Bool] = None, lat_end : Optional[Bool] = None,   
lat_start : Optional[Bool] = None, lon_end : Optional[Bool] = None, lon_start : Optional[Bool] = None):

  
  generate_rectilinear_domain_hdl = GenerateRectilinearDomain()
  

  get_generate_rectilinear_domain_handle(generate_rectilinear_domain_id, generate_rectilinear_domain_hdl)
  is_defined_generate_rectilinear_domain_attr_hdl_(generate_rectilinear_domain_hdl, bounds_lat_end, bounds_lat_start, bounds_lon_end, bounds_lon_start,  
   lat_end, lat_start, lon_end, lon_start)
  return 



@typecheck
def is_defined_generate_rectilinear_domain_attr_hdl(generate_rectilinear_domain_hdl : GenerateRectilinearDomain, bounds_lat_end : Optional[Bool] = None,   
bounds_lat_start : Optional[Bool] = None, bounds_lon_end : Optional[Bool] = None, bounds_lon_start : Optional[Bool] = None,   
lat_end : Optional[Bool] = None, lat_start : Optional[Bool] = None, lon_end : Optional[Bool] = None,   
lon_start : Optional[Bool] = None):

  
  is_defined_generate_rectilinear_domain_attr_hdl_(generate_rectilinear_domain_hdl, bounds_lat_end, bounds_lat_start, bounds_lon_end, bounds_lon_start,  
   lat_end, lat_start, lon_end, lon_start)
  return 



@typecheck
def is_defined_generate_rectilinear_domain_attr_hdl_(generate_rectilinear_domain_hdl : GenerateRectilinearDomain, bounds_lat_end_ : Optional[Bool] = None,   
bounds_lat_start_ : Optional[Bool] = None, bounds_lon_end_ : Optional[Bool] = None, bounds_lon_start_ : Optional[Bool] = None,   
lat_end_ : Optional[Bool] = None, lat_start_ : Optional[Bool] = None, lon_end_ : Optional[Bool] = None,   
lon_start_ : Optional[Bool] = None):

  
  

  if bounds_lat_end_  is not None:
    bounds_lat_end_c = lib.cxios_is_defined_generate_rectilinear_domain_bounds_lat_end(generate_rectilinear_domain_hdl)
    bounds_lat_end_._c_value = ctypes.c_bool(bounds_lat_end_c)
    
  

  if bounds_lat_start_  is not None:
    bounds_lat_start_c = lib.cxios_is_defined_generate_rectilinear_domain_bounds_lat_start(generate_rectilinear_domain_hdl)
    bounds_lat_start_._c_value = ctypes.c_bool(bounds_lat_start_c)
    
  

  if bounds_lon_end_  is not None:
    bounds_lon_end_c = lib.cxios_is_defined_generate_rectilinear_domain_bounds_lon_end(generate_rectilinear_domain_hdl)
    bounds_lon_end_._c_value = ctypes.c_bool(bounds_lon_end_c)
    
  

  if bounds_lon_start_  is not None:
    bounds_lon_start_c = lib.cxios_is_defined_generate_rectilinear_domain_bounds_lon_start(generate_rectilinear_domain_hdl)
    bounds_lon_start_._c_value = ctypes.c_bool(bounds_lon_start_c)
    
  

  if lat_end_  is not None:
    lat_end_c = lib.cxios_is_defined_generate_rectilinear_domain_lat_end(generate_rectilinear_domain_hdl)
    lat_end_._c_value = ctypes.c_bool(lat_end_c)
    
  

  if lat_start_  is not None:
    lat_start_c = lib.cxios_is_defined_generate_rectilinear_domain_lat_start(generate_rectilinear_domain_hdl)
    lat_start_._c_value = ctypes.c_bool(lat_start_c)
    
  

  if lon_end_  is not None:
    lon_end_c = lib.cxios_is_defined_generate_rectilinear_domain_lon_end(generate_rectilinear_domain_hdl)
    lon_end_._c_value = ctypes.c_bool(lon_end_c)
    
  

  if lon_start_  is not None:
    lon_start_c = lib.cxios_is_defined_generate_rectilinear_domain_lon_start(generate_rectilinear_domain_hdl)
    lon_start_._c_value = ctypes.c_bool(lon_start_c)
    
  
  return 



