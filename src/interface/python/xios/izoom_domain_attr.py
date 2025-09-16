# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.izoom_domain import  get_zoom_domain_handle
from xios.ozoom_domain_attr import ZoomDomain

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_zoom_domain_ibegin.argtypes = [ZoomDomain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_zoom_domain_ibegin.restypes = None

lib.cxios_set_zoom_domain_ibegin.argtypes = [ZoomDomain, ctypes.c_int]
lib.cxios_set_zoom_domain_ibegin.restypes = None

lib.cxios_is_defined_zoom_domain_ibegin.argtypes = [ZoomDomain]
lib.cxios_is_defined_zoom_domain_ibegin.restypes = ctypes.c_bool

lib.cxios_get_zoom_domain_jbegin.argtypes = [ZoomDomain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_zoom_domain_jbegin.restypes = None

lib.cxios_set_zoom_domain_jbegin.argtypes = [ZoomDomain, ctypes.c_int]
lib.cxios_set_zoom_domain_jbegin.restypes = None

lib.cxios_is_defined_zoom_domain_jbegin.argtypes = [ZoomDomain]
lib.cxios_is_defined_zoom_domain_jbegin.restypes = ctypes.c_bool

lib.cxios_get_zoom_domain_ni.argtypes = [ZoomDomain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_zoom_domain_ni.restypes = None

lib.cxios_set_zoom_domain_ni.argtypes = [ZoomDomain, ctypes.c_int]
lib.cxios_set_zoom_domain_ni.restypes = None

lib.cxios_is_defined_zoom_domain_ni.argtypes = [ZoomDomain]
lib.cxios_is_defined_zoom_domain_ni.restypes = ctypes.c_bool

lib.cxios_get_zoom_domain_nj.argtypes = [ZoomDomain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_zoom_domain_nj.restypes = None

lib.cxios_set_zoom_domain_nj.argtypes = [ZoomDomain, ctypes.c_int]
lib.cxios_set_zoom_domain_nj.restypes = None

lib.cxios_is_defined_zoom_domain_nj.argtypes = [ZoomDomain]
lib.cxios_is_defined_zoom_domain_nj.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_zoom_domain_attr(zoom_domain_id : Union[String, str], ibegin : Optional[Union[Int, int]] = None, jbegin : Optional[Union[Int, int]] = None,   
ni : Optional[Union[Int, int]] = None, nj : Optional[Union[Int, int]] = None):

  
  zoom_domain_hdl = ZoomDomain()
  

  get_zoom_domain_handle(zoom_domain_id, zoom_domain_hdl)
  set_zoom_domain_attr_hdl_(zoom_domain_hdl, ibegin, jbegin, ni, nj)
  return 



@typecheck
def set_zoom_domain_attr_hdl(zoom_domain_hdl : ZoomDomain, ibegin : Optional[Union[Int, int]] = None, jbegin : Optional[Union[Int, int]] = None,   
ni : Optional[Union[Int, int]] = None, nj : Optional[Union[Int, int]] = None):

  
  set_zoom_domain_attr_hdl_(zoom_domain_hdl, ibegin, jbegin, ni, nj)
  return 



@typecheck
def set_zoom_domain_attr_hdl_(zoom_domain_hdl : ZoomDomain, ibegin_ : Optional[Union[Int, int]] = None, jbegin_ : Optional[Union[Int, int]] = None,   
ni_ : Optional[Union[Int, int]] = None, nj_ : Optional[Union[Int, int]] = None):

  
  

  if ibegin_ is not None: 
  
    ibegin_ = Int(ibegin_)
    ibegin_c = ibegin_._c_value
    lib.cxios_set_zoom_domain_ibegin(zoom_domain_hdl, ibegin_c)
    
  

  if jbegin_ is not None: 
  
    jbegin_ = Int(jbegin_)
    jbegin_c = jbegin_._c_value
    lib.cxios_set_zoom_domain_jbegin(zoom_domain_hdl, jbegin_c)
    
  

  if ni_ is not None: 
  
    ni_ = Int(ni_)
    ni_c = ni_._c_value
    lib.cxios_set_zoom_domain_ni(zoom_domain_hdl, ni_c)
    
  

  if nj_ is not None: 
  
    nj_ = Int(nj_)
    nj_c = nj_._c_value
    lib.cxios_set_zoom_domain_nj(zoom_domain_hdl, nj_c)
    
  
  return 



@typecheck
def get_zoom_domain_attr(zoom_domain_id : Union[String, str], ibegin : Optional[Int] = None, jbegin : Optional[Int] = None,   
ni : Optional[Int] = None, nj : Optional[Int] = None):

  
  zoom_domain_hdl = ZoomDomain()
  

  get_zoom_domain_handle(zoom_domain_id, zoom_domain_hdl)
  get_zoom_domain_attr_hdl_(zoom_domain_hdl, ibegin, jbegin, ni, nj)
  return 



@typecheck
def get_zoom_domain_attr_hdl(zoom_domain_hdl : ZoomDomain, ibegin : Optional[Int] = None, jbegin : Optional[Int] = None,   
ni : Optional[Int] = None, nj : Optional[Int] = None):

  
  get_zoom_domain_attr_hdl_(zoom_domain_hdl, ibegin, jbegin, ni, nj)
  return 



@typecheck
def get_zoom_domain_attr_hdl_(zoom_domain_hdl : ZoomDomain, ibegin_ : Optional[Int] = None, jbegin_ : Optional[Int] = None,   
ni_ : Optional[Int] = None, nj_ : Optional[Int] = None):

  
  

  if ibegin_ is not None: 
  
    ibegin_c = ibegin_._c_value
    lib.cxios_get_zoom_domain_ibegin(zoom_domain_hdl, ibegin_c)
    
  

  if jbegin_ is not None: 
  
    jbegin_c = jbegin_._c_value
    lib.cxios_get_zoom_domain_jbegin(zoom_domain_hdl, jbegin_c)
    
  

  if ni_ is not None: 
  
    ni_c = ni_._c_value
    lib.cxios_get_zoom_domain_ni(zoom_domain_hdl, ni_c)
    
  

  if nj_ is not None: 
  
    nj_c = nj_._c_value
    lib.cxios_get_zoom_domain_nj(zoom_domain_hdl, nj_c)
    
  
  return 



@typecheck
def is_defined_zoom_domain_attr(zoom_domain_id : String, ibegin : Optional[Bool] = None, jbegin : Optional[Bool] = None, ni : Optional[Bool] = None,   
nj : Optional[Bool] = None):

  
  zoom_domain_hdl = ZoomDomain()
  

  get_zoom_domain_handle(zoom_domain_id, zoom_domain_hdl)
  is_defined_zoom_domain_attr_hdl_(zoom_domain_hdl, ibegin, jbegin, ni, nj)
  return 



@typecheck
def is_defined_zoom_domain_attr_hdl(zoom_domain_hdl : ZoomDomain, ibegin : Optional[Bool] = None, jbegin : Optional[Bool] = None,   
ni : Optional[Bool] = None, nj : Optional[Bool] = None):

  
  is_defined_zoom_domain_attr_hdl_(zoom_domain_hdl, ibegin, jbegin, ni, nj)
  return 



@typecheck
def is_defined_zoom_domain_attr_hdl_(zoom_domain_hdl : ZoomDomain, ibegin_ : Optional[Bool] = None, jbegin_ : Optional[Bool] = None,   
ni_ : Optional[Bool] = None, nj_ : Optional[Bool] = None):

  
  

  if ibegin_  is not None:
    ibegin_c = lib.cxios_is_defined_zoom_domain_ibegin(zoom_domain_hdl)
    ibegin_._c_value = ctypes.c_bool(ibegin_c)
    
  

  if jbegin_  is not None:
    jbegin_c = lib.cxios_is_defined_zoom_domain_jbegin(zoom_domain_hdl)
    jbegin_._c_value = ctypes.c_bool(jbegin_c)
    
  

  if ni_  is not None:
    ni_c = lib.cxios_is_defined_zoom_domain_ni(zoom_domain_hdl)
    ni_._c_value = ctypes.c_bool(ni_c)
    
  

  if nj_  is not None:
    nj_c = lib.cxios_is_defined_zoom_domain_nj(zoom_domain_hdl)
    nj_._c_value = ctypes.c_bool(nj_c)
    
  
  return 



