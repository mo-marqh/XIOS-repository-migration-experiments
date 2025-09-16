# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.iextract_domain import  get_extract_domain_handle
from xios.oextract_domain_attr import ExtractDomain

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_extract_domain_ibegin.argtypes = [ExtractDomain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_extract_domain_ibegin.restypes = None

lib.cxios_set_extract_domain_ibegin.argtypes = [ExtractDomain, ctypes.c_int]
lib.cxios_set_extract_domain_ibegin.restypes = None

lib.cxios_is_defined_extract_domain_ibegin.argtypes = [ExtractDomain]
lib.cxios_is_defined_extract_domain_ibegin.restypes = ctypes.c_bool

lib.cxios_get_extract_domain_jbegin.argtypes = [ExtractDomain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_extract_domain_jbegin.restypes = None

lib.cxios_set_extract_domain_jbegin.argtypes = [ExtractDomain, ctypes.c_int]
lib.cxios_set_extract_domain_jbegin.restypes = None

lib.cxios_is_defined_extract_domain_jbegin.argtypes = [ExtractDomain]
lib.cxios_is_defined_extract_domain_jbegin.restypes = ctypes.c_bool

lib.cxios_get_extract_domain_ni.argtypes = [ExtractDomain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_extract_domain_ni.restypes = None

lib.cxios_set_extract_domain_ni.argtypes = [ExtractDomain, ctypes.c_int]
lib.cxios_set_extract_domain_ni.restypes = None

lib.cxios_is_defined_extract_domain_ni.argtypes = [ExtractDomain]
lib.cxios_is_defined_extract_domain_ni.restypes = ctypes.c_bool

lib.cxios_get_extract_domain_nj.argtypes = [ExtractDomain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_extract_domain_nj.restypes = None

lib.cxios_set_extract_domain_nj.argtypes = [ExtractDomain, ctypes.c_int]
lib.cxios_set_extract_domain_nj.restypes = None

lib.cxios_is_defined_extract_domain_nj.argtypes = [ExtractDomain]
lib.cxios_is_defined_extract_domain_nj.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_extract_domain_attr(extract_domain_id : Union[String, str], ibegin : Optional[Union[Int, int]] = None, jbegin : Optional[Union[Int, int]] = None,   
ni : Optional[Union[Int, int]] = None, nj : Optional[Union[Int, int]] = None):

  
  extract_domain_hdl = ExtractDomain()
  

  get_extract_domain_handle(extract_domain_id, extract_domain_hdl)
  set_extract_domain_attr_hdl_(extract_domain_hdl, ibegin, jbegin, ni, nj)
  return 



@typecheck
def set_extract_domain_attr_hdl(extract_domain_hdl : ExtractDomain, ibegin : Optional[Union[Int, int]] = None, jbegin : Optional[Union[Int, int]] = None,   
ni : Optional[Union[Int, int]] = None, nj : Optional[Union[Int, int]] = None):

  
  set_extract_domain_attr_hdl_(extract_domain_hdl, ibegin, jbegin, ni, nj)
  return 



@typecheck
def set_extract_domain_attr_hdl_(extract_domain_hdl : ExtractDomain, ibegin_ : Optional[Union[Int, int]] = None, jbegin_ : Optional[Union[Int, int]] = None,   
ni_ : Optional[Union[Int, int]] = None, nj_ : Optional[Union[Int, int]] = None):

  
  

  if ibegin_ is not None: 
  
    ibegin_ = Int(ibegin_)
    ibegin_c = ibegin_._c_value
    lib.cxios_set_extract_domain_ibegin(extract_domain_hdl, ibegin_c)
    
  

  if jbegin_ is not None: 
  
    jbegin_ = Int(jbegin_)
    jbegin_c = jbegin_._c_value
    lib.cxios_set_extract_domain_jbegin(extract_domain_hdl, jbegin_c)
    
  

  if ni_ is not None: 
  
    ni_ = Int(ni_)
    ni_c = ni_._c_value
    lib.cxios_set_extract_domain_ni(extract_domain_hdl, ni_c)
    
  

  if nj_ is not None: 
  
    nj_ = Int(nj_)
    nj_c = nj_._c_value
    lib.cxios_set_extract_domain_nj(extract_domain_hdl, nj_c)
    
  
  return 



@typecheck
def get_extract_domain_attr(extract_domain_id : Union[String, str], ibegin : Optional[Int] = None, jbegin : Optional[Int] = None,   
ni : Optional[Int] = None, nj : Optional[Int] = None):

  
  extract_domain_hdl = ExtractDomain()
  

  get_extract_domain_handle(extract_domain_id, extract_domain_hdl)
  get_extract_domain_attr_hdl_(extract_domain_hdl, ibegin, jbegin, ni, nj)
  return 



@typecheck
def get_extract_domain_attr_hdl(extract_domain_hdl : ExtractDomain, ibegin : Optional[Int] = None, jbegin : Optional[Int] = None,   
ni : Optional[Int] = None, nj : Optional[Int] = None):

  
  get_extract_domain_attr_hdl_(extract_domain_hdl, ibegin, jbegin, ni, nj)
  return 



@typecheck
def get_extract_domain_attr_hdl_(extract_domain_hdl : ExtractDomain, ibegin_ : Optional[Int] = None, jbegin_ : Optional[Int] = None,   
ni_ : Optional[Int] = None, nj_ : Optional[Int] = None):

  
  

  if ibegin_ is not None: 
  
    ibegin_c = ibegin_._c_value
    lib.cxios_get_extract_domain_ibegin(extract_domain_hdl, ibegin_c)
    
  

  if jbegin_ is not None: 
  
    jbegin_c = jbegin_._c_value
    lib.cxios_get_extract_domain_jbegin(extract_domain_hdl, jbegin_c)
    
  

  if ni_ is not None: 
  
    ni_c = ni_._c_value
    lib.cxios_get_extract_domain_ni(extract_domain_hdl, ni_c)
    
  

  if nj_ is not None: 
  
    nj_c = nj_._c_value
    lib.cxios_get_extract_domain_nj(extract_domain_hdl, nj_c)
    
  
  return 



@typecheck
def is_defined_extract_domain_attr(extract_domain_id : String, ibegin : Optional[Bool] = None, jbegin : Optional[Bool] = None,   
ni : Optional[Bool] = None, nj : Optional[Bool] = None):

  
  extract_domain_hdl = ExtractDomain()
  

  get_extract_domain_handle(extract_domain_id, extract_domain_hdl)
  is_defined_extract_domain_attr_hdl_(extract_domain_hdl, ibegin, jbegin, ni, nj)
  return 



@typecheck
def is_defined_extract_domain_attr_hdl(extract_domain_hdl : ExtractDomain, ibegin : Optional[Bool] = None, jbegin : Optional[Bool] = None,   
ni : Optional[Bool] = None, nj : Optional[Bool] = None):

  
  is_defined_extract_domain_attr_hdl_(extract_domain_hdl, ibegin, jbegin, ni, nj)
  return 



@typecheck
def is_defined_extract_domain_attr_hdl_(extract_domain_hdl : ExtractDomain, ibegin_ : Optional[Bool] = None, jbegin_ : Optional[Bool] = None,   
ni_ : Optional[Bool] = None, nj_ : Optional[Bool] = None):

  
  

  if ibegin_  is not None:
    ibegin_c = lib.cxios_is_defined_extract_domain_ibegin(extract_domain_hdl)
    ibegin_._c_value = ctypes.c_bool(ibegin_c)
    
  

  if jbegin_  is not None:
    jbegin_c = lib.cxios_is_defined_extract_domain_jbegin(extract_domain_hdl)
    jbegin_._c_value = ctypes.c_bool(jbegin_c)
    
  

  if ni_  is not None:
    ni_c = lib.cxios_is_defined_extract_domain_ni(extract_domain_hdl)
    ni_._c_value = ctypes.c_bool(ni_c)
    
  

  if nj_  is not None:
    nj_c = lib.cxios_is_defined_extract_domain_nj(extract_domain_hdl)
    nj_._c_value = ctypes.c_bool(nj_c)
    
  
  return 



