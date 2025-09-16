# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.iexpand_domain import  get_expand_domain_handle
from xios.oexpand_domain_attr import ExpandDomain

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_expand_domain_i_periodic.argtypes = [ExpandDomain, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_expand_domain_i_periodic.restypes = None

lib.cxios_set_expand_domain_i_periodic.argtypes = [ExpandDomain, ctypes.c_bool]
lib.cxios_set_expand_domain_i_periodic.restypes = None

lib.cxios_is_defined_expand_domain_i_periodic.argtypes = [ExpandDomain]
lib.cxios_is_defined_expand_domain_i_periodic.restypes = ctypes.c_bool

lib.cxios_get_expand_domain_j_periodic.argtypes = [ExpandDomain, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_expand_domain_j_periodic.restypes = None

lib.cxios_set_expand_domain_j_periodic.argtypes = [ExpandDomain, ctypes.c_bool]
lib.cxios_set_expand_domain_j_periodic.restypes = None

lib.cxios_is_defined_expand_domain_j_periodic.argtypes = [ExpandDomain]
lib.cxios_is_defined_expand_domain_j_periodic.restypes = ctypes.c_bool

lib.cxios_get_expand_domain_order.argtypes = [ExpandDomain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_expand_domain_order.restypes = None

lib.cxios_set_expand_domain_order.argtypes = [ExpandDomain, ctypes.c_int]
lib.cxios_set_expand_domain_order.restypes = None

lib.cxios_is_defined_expand_domain_order.argtypes = [ExpandDomain]
lib.cxios_is_defined_expand_domain_order.restypes = ctypes.c_bool

lib.cxios_get_expand_domain_type.argtypes = [ExpandDomain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_expand_domain_type.restypes = None

lib.cxios_set_expand_domain_type.argtypes = [ExpandDomain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_expand_domain_type.restypes = None

lib.cxios_is_defined_expand_domain_type.argtypes = [ExpandDomain]
lib.cxios_is_defined_expand_domain_type.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_expand_domain_attr(expand_domain_id : Union[String, str], i_periodic : Optional[Union[Bool, bool]] = None, j_periodic : Optional[Union[Bool, bool]] = None,   
order : Optional[Union[Int, int]] = None, type : Optional[Union[str, String]] = None):

  
  expand_domain_hdl = ExpandDomain()
  

  get_expand_domain_handle(expand_domain_id, expand_domain_hdl)
  set_expand_domain_attr_hdl_(expand_domain_hdl, i_periodic, j_periodic, order, type)
  return 



@typecheck
def set_expand_domain_attr_hdl(expand_domain_hdl : ExpandDomain, i_periodic : Optional[Union[Bool, bool]] = None, j_periodic : Optional[Union[Bool, bool]] = None,   
order : Optional[Union[Int, int]] = None, type : Optional[Union[str, String]] = None):

  
  set_expand_domain_attr_hdl_(expand_domain_hdl, i_periodic, j_periodic, order, type)
  return 



@typecheck
def set_expand_domain_attr_hdl_(expand_domain_hdl : ExpandDomain, i_periodic_ : Optional[Union[Bool, bool]] = None, j_periodic_ : Optional[Union[Bool, bool]] = None,   
order_ : Optional[Union[Int, int]] = None, type_ : Optional[Union[str, String]] = None):

  
  

  if i_periodic_ is not None: 
  
    i_periodic_ = Bool(i_periodic_)
    i_periodic_c = i_periodic_._c_value
    lib.cxios_set_expand_domain_i_periodic(expand_domain_hdl, i_periodic_c)
    
  

  if j_periodic_ is not None: 
  
    j_periodic_ = Bool(j_periodic_)
    j_periodic_c = j_periodic_._c_value
    lib.cxios_set_expand_domain_j_periodic(expand_domain_hdl, j_periodic_c)
    
  

  if order_ is not None: 
  
    order_ = Int(order_)
    order_c = order_._c_value
    lib.cxios_set_expand_domain_order(expand_domain_hdl, order_c)
    
  

  if type_ is not None:
  
    type_= String(type_)
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_set_expand_domain_type(expand_domain_hdl, type_c, len_type_c)
    
  
  return 



@typecheck
def get_expand_domain_attr(expand_domain_id : Union[String, str], i_periodic : Optional[Bool] = None, j_periodic : Optional[Bool] = None,   
order : Optional[Int] = None, type : Optional[String] = None):

  
  expand_domain_hdl = ExpandDomain()
  

  get_expand_domain_handle(expand_domain_id, expand_domain_hdl)
  get_expand_domain_attr_hdl_(expand_domain_hdl, i_periodic, j_periodic, order, type)
  return 



@typecheck
def get_expand_domain_attr_hdl(expand_domain_hdl : ExpandDomain, i_periodic : Optional[Bool] = None, j_periodic : Optional[Bool] = None,   
order : Optional[Int] = None, type : Optional[String] = None):

  
  get_expand_domain_attr_hdl_(expand_domain_hdl, i_periodic, j_periodic, order, type)
  return 



@typecheck
def get_expand_domain_attr_hdl_(expand_domain_hdl : ExpandDomain, i_periodic_ : Optional[Bool] = None, j_periodic_ : Optional[Bool] = None,   
order_ : Optional[Int] = None, type_ : Optional[String] = None):

  
  

  if i_periodic_ is not None: 
  
    i_periodic_c = i_periodic_._c_value
    lib.cxios_get_expand_domain_i_periodic(expand_domain_hdl, i_periodic_c)
    
  

  if j_periodic_ is not None: 
  
    j_periodic_c = j_periodic_._c_value
    lib.cxios_get_expand_domain_j_periodic(expand_domain_hdl, j_periodic_c)
    
  

  if order_ is not None: 
  
    order_c = order_._c_value
    lib.cxios_get_expand_domain_order(expand_domain_hdl, order_c)
    
  

  if type_ is not None:
  
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_get_expand_domain_type(expand_domain_hdl, type_c, len_type_c)
    
  
  return 



@typecheck
def is_defined_expand_domain_attr(expand_domain_id : String, i_periodic : Optional[Bool] = None, j_periodic : Optional[Bool] = None,   
order : Optional[Bool] = None, type : Optional[Bool] = None):

  
  expand_domain_hdl = ExpandDomain()
  

  get_expand_domain_handle(expand_domain_id, expand_domain_hdl)
  is_defined_expand_domain_attr_hdl_(expand_domain_hdl, i_periodic, j_periodic, order, type)
  return 



@typecheck
def is_defined_expand_domain_attr_hdl(expand_domain_hdl : ExpandDomain, i_periodic : Optional[Bool] = None, j_periodic : Optional[Bool] = None,   
order : Optional[Bool] = None, type : Optional[Bool] = None):

  
  is_defined_expand_domain_attr_hdl_(expand_domain_hdl, i_periodic, j_periodic, order, type)
  return 



@typecheck
def is_defined_expand_domain_attr_hdl_(expand_domain_hdl : ExpandDomain, i_periodic_ : Optional[Bool] = None, j_periodic_ : Optional[Bool] = None,   
order_ : Optional[Bool] = None, type_ : Optional[Bool] = None):

  
  

  if i_periodic_  is not None:
    i_periodic_c = lib.cxios_is_defined_expand_domain_i_periodic(expand_domain_hdl)
    i_periodic_._c_value = ctypes.c_bool(i_periodic_c)
    
  

  if j_periodic_  is not None:
    j_periodic_c = lib.cxios_is_defined_expand_domain_j_periodic(expand_domain_hdl)
    j_periodic_._c_value = ctypes.c_bool(j_periodic_c)
    
  

  if order_  is not None:
    order_c = lib.cxios_is_defined_expand_domain_order(expand_domain_hdl)
    order_._c_value = ctypes.c_bool(order_c)
    
  

  if type_  is not None:
    type_c = lib.cxios_is_defined_expand_domain_type(expand_domain_hdl)
    type_._c_value = ctypes.c_bool(type_c)
    
  
  return 



