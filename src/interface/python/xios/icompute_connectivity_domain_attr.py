# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.icompute_connectivity_domain import  get_compute_connectivity_domain_handle
from xios.ocompute_connectivity_domain_attr import ComputeConnectivityDomain

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_compute_connectivity_domain_local_neighbor.argtypes = [ComputeConnectivityDomain, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_compute_connectivity_domain_local_neighbor.restypes = None

lib.cxios_set_compute_connectivity_domain_local_neighbor.argtypes = [ComputeConnectivityDomain, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_compute_connectivity_domain_local_neighbor.restypes = None

lib.cxios_is_defined_compute_connectivity_domain_local_neighbor.argtypes = [ComputeConnectivityDomain]
lib.cxios_is_defined_compute_connectivity_domain_local_neighbor.restypes = ctypes.c_bool

lib.cxios_get_compute_connectivity_domain_n_neighbor.argtypes = [ComputeConnectivityDomain, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_compute_connectivity_domain_n_neighbor.restypes = None

lib.cxios_set_compute_connectivity_domain_n_neighbor.argtypes = [ComputeConnectivityDomain, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_compute_connectivity_domain_n_neighbor.restypes = None

lib.cxios_is_defined_compute_connectivity_domain_n_neighbor.argtypes = [ComputeConnectivityDomain]
lib.cxios_is_defined_compute_connectivity_domain_n_neighbor.restypes = ctypes.c_bool

lib.cxios_get_compute_connectivity_domain_n_neighbor_max.argtypes = [ComputeConnectivityDomain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_compute_connectivity_domain_n_neighbor_max.restypes = None

lib.cxios_set_compute_connectivity_domain_n_neighbor_max.argtypes = [ComputeConnectivityDomain, ctypes.c_int]
lib.cxios_set_compute_connectivity_domain_n_neighbor_max.restypes = None

lib.cxios_is_defined_compute_connectivity_domain_n_neighbor_max.argtypes = [ComputeConnectivityDomain]
lib.cxios_is_defined_compute_connectivity_domain_n_neighbor_max.restypes = ctypes.c_bool

lib.cxios_get_compute_connectivity_domain_type.argtypes = [ComputeConnectivityDomain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_compute_connectivity_domain_type.restypes = None

lib.cxios_set_compute_connectivity_domain_type.argtypes = [ComputeConnectivityDomain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_compute_connectivity_domain_type.restypes = None

lib.cxios_is_defined_compute_connectivity_domain_type.argtypes = [ComputeConnectivityDomain]
lib.cxios_is_defined_compute_connectivity_domain_type.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_compute_connectivity_domain_attr(compute_connectivity_domain_id : Union[String, str], local_neighbor : Optional[Union[np.ndarray, NpArrayInt]] = None,   
n_neighbor : Optional[Union[np.ndarray, NpArrayInt]] = None, n_neighbor_max : Optional[Union[Int, int]] = None,   
type : Optional[Union[str, String]] = None):

  
  compute_connectivity_domain_hdl = ComputeConnectivityDomain()
  

  get_compute_connectivity_domain_handle(compute_connectivity_domain_id, compute_connectivity_domain_hdl)
  set_compute_connectivity_domain_attr_hdl_(compute_connectivity_domain_hdl, local_neighbor, n_neighbor, n_neighbor_max, type)
  return 



@typecheck
def set_compute_connectivity_domain_attr_hdl(compute_connectivity_domain_hdl : ComputeConnectivityDomain, local_neighbor : Optional[Union[np.ndarray, NpArrayInt]] = None,   
n_neighbor : Optional[Union[np.ndarray, NpArrayInt]] = None, n_neighbor_max : Optional[Union[Int, int]] = None,   
type : Optional[Union[str, String]] = None):

  
  set_compute_connectivity_domain_attr_hdl_(compute_connectivity_domain_hdl, local_neighbor, n_neighbor, n_neighbor_max, type)
  return 



@typecheck
def set_compute_connectivity_domain_attr_hdl_(compute_connectivity_domain_hdl : ComputeConnectivityDomain, local_neighbor_ : Optional[Union[np.ndarray, NpArrayInt]] = None,   
n_neighbor_ : Optional[Union[np.ndarray, NpArrayInt]] = None, n_neighbor_max_ : Optional[Union[Int, int]] = None,   
type_ : Optional[Union[str, String]] = None):

  
  

  if local_neighbor_ is not None:
    local_neighbor_ = NpArrayInt(local_neighbor_)
    local_neighbor_c = local_neighbor_._c_value
    if len(local_neighbor_.shape) != 2 : 
      raise ValueError('The array dimension should be 2')
    else : 
      shape = ctypes.c_int * len(local_neighbor_.shape)
      shape_c = shape(*local_neighbor_.shape)
      lib.cxios_set_compute_connectivity_domain_local_neighbor(compute_connectivity_domain_hdl, local_neighbor_c, shape_c)
  

  if n_neighbor_ is not None:
    n_neighbor_ = NpArrayInt(n_neighbor_)
    n_neighbor_c = n_neighbor_._c_value
    if len(n_neighbor_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(n_neighbor_.shape)
      shape_c = shape(*n_neighbor_.shape)
      lib.cxios_set_compute_connectivity_domain_n_neighbor(compute_connectivity_domain_hdl, n_neighbor_c, shape_c)
  

  if n_neighbor_max_ is not None: 
  
    n_neighbor_max_ = Int(n_neighbor_max_)
    n_neighbor_max_c = n_neighbor_max_._c_value
    lib.cxios_set_compute_connectivity_domain_n_neighbor_max(compute_connectivity_domain_hdl, n_neighbor_max_c)
    
  

  if type_ is not None:
  
    type_= String(type_)
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_set_compute_connectivity_domain_type(compute_connectivity_domain_hdl, type_c, len_type_c)
    
  
  return 



@typecheck
def get_compute_connectivity_domain_attr(compute_connectivity_domain_id : Union[String, str], local_neighbor : Optional[NpArrayInt] = None,   
n_neighbor : Optional[NpArrayInt] = None, n_neighbor_max : Optional[Int] = None, type : Optional[String] = None):

  
  compute_connectivity_domain_hdl = ComputeConnectivityDomain()
  

  get_compute_connectivity_domain_handle(compute_connectivity_domain_id, compute_connectivity_domain_hdl)
  get_compute_connectivity_domain_attr_hdl_(compute_connectivity_domain_hdl, local_neighbor, n_neighbor, n_neighbor_max, type)
  return 



@typecheck
def get_compute_connectivity_domain_attr_hdl(compute_connectivity_domain_hdl : ComputeConnectivityDomain, local_neighbor : Optional[NpArrayInt] = None,   
n_neighbor : Optional[NpArrayInt] = None, n_neighbor_max : Optional[Int] = None, type : Optional[String] = None):

  
  get_compute_connectivity_domain_attr_hdl_(compute_connectivity_domain_hdl, local_neighbor, n_neighbor, n_neighbor_max, type)
  return 



@typecheck
def get_compute_connectivity_domain_attr_hdl_(compute_connectivity_domain_hdl : ComputeConnectivityDomain, local_neighbor_ : Optional[NpArrayInt] = None,   
n_neighbor_ : Optional[NpArrayInt] = None, n_neighbor_max_ : Optional[Int] = None, type_ : Optional[String] = None):

  
  

  if local_neighbor_ is not None:
    if local_neighbor_.shape_is_needed: 
      shape_c = (ctypes.c_int * 2)()
      lib.cxios_compute_connectivity_domain_local_neighbor_shape(compute_connectivity_domain_hdl, shape_c)
      local_neighbor_.arr = np.zeros((shape_c[0], shape_c[1]) , order = 'F')
      lib.cxios_get_compute_connectivity_domain_local_neighbor(compute_connectivity_domain_hdl, local_neighbor_._c_value, shape_c)
    else: 
      local_neighbor_c = local_neighbor_._c_value
      if len(local_neighbor_.shape) != 2 : 
        raise ValueError('The array dimension should be 2')
      else : 
        shape_c = NpArrayInt(local_neighbor_.shape)._c_value
        lib.cxios_get_compute_connectivity_domain_local_neighbor(compute_connectivity_domain_hdl, local_neighbor_c, shape_c)
  

  if n_neighbor_ is not None:
    if n_neighbor_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_compute_connectivity_domain_n_neighbor_shape(compute_connectivity_domain_hdl, shape_c)
      n_neighbor_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_compute_connectivity_domain_n_neighbor(compute_connectivity_domain_hdl, n_neighbor_._c_value, shape_c)
    else: 
      n_neighbor_c = n_neighbor_._c_value
      if len(n_neighbor_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(n_neighbor_.shape)._c_value
        lib.cxios_get_compute_connectivity_domain_n_neighbor(compute_connectivity_domain_hdl, n_neighbor_c, shape_c)
  

  if n_neighbor_max_ is not None: 
  
    n_neighbor_max_c = n_neighbor_max_._c_value
    lib.cxios_get_compute_connectivity_domain_n_neighbor_max(compute_connectivity_domain_hdl, n_neighbor_max_c)
    
  

  if type_ is not None:
  
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_get_compute_connectivity_domain_type(compute_connectivity_domain_hdl, type_c, len_type_c)
    
  
  return 



@typecheck
def is_defined_compute_connectivity_domain_attr(compute_connectivity_domain_id : String, local_neighbor : Optional[Bool] = None, n_neighbor : Optional[Bool] = None,   
n_neighbor_max : Optional[Bool] = None, type : Optional[Bool] = None):

  
  compute_connectivity_domain_hdl = ComputeConnectivityDomain()
  

  get_compute_connectivity_domain_handle(compute_connectivity_domain_id, compute_connectivity_domain_hdl)
  is_defined_compute_connectivity_domain_attr_hdl_(compute_connectivity_domain_hdl, local_neighbor, n_neighbor, n_neighbor_max, type)
  return 



@typecheck
def is_defined_compute_connectivity_domain_attr_hdl(compute_connectivity_domain_hdl : ComputeConnectivityDomain, local_neighbor : Optional[Bool] = None,   
n_neighbor : Optional[Bool] = None, n_neighbor_max : Optional[Bool] = None, type : Optional[Bool] = None):

  
  is_defined_compute_connectivity_domain_attr_hdl_(compute_connectivity_domain_hdl, local_neighbor, n_neighbor, n_neighbor_max, type)
  return 



@typecheck
def is_defined_compute_connectivity_domain_attr_hdl_(compute_connectivity_domain_hdl : ComputeConnectivityDomain, local_neighbor_ : Optional[Bool] = None,   
n_neighbor_ : Optional[Bool] = None, n_neighbor_max_ : Optional[Bool] = None, type_ : Optional[Bool] = None):

  
  

  if local_neighbor_  is not None:
    local_neighbor_c = lib.cxios_is_defined_compute_connectivity_domain_local_neighbor(compute_connectivity_domain_hdl)
    local_neighbor_._c_value = ctypes.c_bool(local_neighbor_c)
    
  

  if n_neighbor_  is not None:
    n_neighbor_c = lib.cxios_is_defined_compute_connectivity_domain_n_neighbor(compute_connectivity_domain_hdl)
    n_neighbor_._c_value = ctypes.c_bool(n_neighbor_c)
    
  

  if n_neighbor_max_  is not None:
    n_neighbor_max_c = lib.cxios_is_defined_compute_connectivity_domain_n_neighbor_max(compute_connectivity_domain_hdl)
    n_neighbor_max_._c_value = ctypes.c_bool(n_neighbor_max_c)
    
  

  if type_  is not None:
    type_c = lib.cxios_is_defined_compute_connectivity_domain_type(compute_connectivity_domain_hdl)
    type_._c_value = ctypes.c_bool(type_c)
    
  
  return 



