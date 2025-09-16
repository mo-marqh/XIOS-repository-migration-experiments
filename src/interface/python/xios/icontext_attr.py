# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.icontext import  get_context_handle
from xios.ocontext import Context

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_context_attached_mode.argtypes = [Context, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_context_attached_mode.restypes = None

lib.cxios_set_context_attached_mode.argtypes = [Context, ctypes.c_bool]
lib.cxios_set_context_attached_mode.restypes = None

lib.cxios_is_defined_context_attached_mode.argtypes = [Context]
lib.cxios_is_defined_context_attached_mode.restypes = ctypes.c_bool

lib.cxios_get_context_default_gatherer.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_context_default_gatherer.restypes = None

lib.cxios_set_context_default_gatherer.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_context_default_gatherer.restypes = None

lib.cxios_is_defined_context_default_gatherer.argtypes = [Context]
lib.cxios_is_defined_context_default_gatherer.restypes = ctypes.c_bool

lib.cxios_get_context_default_pool.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_context_default_pool.restypes = None

lib.cxios_set_context_default_pool.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_context_default_pool.restypes = None

lib.cxios_is_defined_context_default_pool.argtypes = [Context]
lib.cxios_is_defined_context_default_pool.restypes = ctypes.c_bool

lib.cxios_get_context_default_pool_gatherer.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_context_default_pool_gatherer.restypes = None

lib.cxios_set_context_default_pool_gatherer.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_context_default_pool_gatherer.restypes = None

lib.cxios_is_defined_context_default_pool_gatherer.argtypes = [Context]
lib.cxios_is_defined_context_default_pool_gatherer.restypes = ctypes.c_bool

lib.cxios_get_context_default_pool_reader.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_context_default_pool_reader.restypes = None

lib.cxios_set_context_default_pool_reader.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_context_default_pool_reader.restypes = None

lib.cxios_is_defined_context_default_pool_reader.argtypes = [Context]
lib.cxios_is_defined_context_default_pool_reader.restypes = ctypes.c_bool

lib.cxios_get_context_default_pool_writer.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_context_default_pool_writer.restypes = None

lib.cxios_set_context_default_pool_writer.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_context_default_pool_writer.restypes = None

lib.cxios_is_defined_context_default_pool_writer.argtypes = [Context]
lib.cxios_is_defined_context_default_pool_writer.restypes = ctypes.c_bool

lib.cxios_get_context_default_reader.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_context_default_reader.restypes = None

lib.cxios_set_context_default_reader.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_context_default_reader.restypes = None

lib.cxios_is_defined_context_default_reader.argtypes = [Context]
lib.cxios_is_defined_context_default_reader.restypes = ctypes.c_bool

lib.cxios_get_context_default_using_server2.argtypes = [Context, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_context_default_using_server2.restypes = None

lib.cxios_set_context_default_using_server2.argtypes = [Context, ctypes.c_bool]
lib.cxios_set_context_default_using_server2.restypes = None

lib.cxios_is_defined_context_default_using_server2.argtypes = [Context]
lib.cxios_is_defined_context_default_using_server2.restypes = ctypes.c_bool

lib.cxios_get_context_default_writer.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_context_default_writer.restypes = None

lib.cxios_set_context_default_writer.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_context_default_writer.restypes = None

lib.cxios_is_defined_context_default_writer.argtypes = [Context]
lib.cxios_is_defined_context_default_writer.restypes = ctypes.c_bool

lib.cxios_get_context_output_dir.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_context_output_dir.restypes = None

lib.cxios_set_context_output_dir.argtypes = [Context, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_context_output_dir.restypes = None

lib.cxios_is_defined_context_output_dir.argtypes = [Context]
lib.cxios_is_defined_context_output_dir.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_context_attr(context_id : Union[String, str], attached_mode : Optional[Union[Bool, bool]] = None, default_gatherer : Optional[Union[str, String]] = None,   
default_pool : Optional[Union[str, String]] = None, default_pool_gatherer : Optional[Union[str, String]] = None,   
default_pool_reader : Optional[Union[str, String]] = None, default_pool_writer : Optional[Union[str, String]] = None,   
default_reader : Optional[Union[str, String]] = None, default_using_server2 : Optional[Union[Bool, bool]] = None,   
default_writer : Optional[Union[str, String]] = None, output_dir : Optional[Union[str, String]] = None):

  
  context_hdl = Context()
  

  get_context_handle(context_id, context_hdl)
  set_context_attr_hdl_(context_hdl, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader,  
   default_pool_writer, default_reader, default_using_server2, default_writer, output_dir)
  return 



@typecheck
def set_context_attr_hdl(context_hdl : Context, attached_mode : Optional[Union[Bool, bool]] = None, default_gatherer : Optional[Union[str, String]] = None,   
default_pool : Optional[Union[str, String]] = None, default_pool_gatherer : Optional[Union[str, String]] = None,   
default_pool_reader : Optional[Union[str, String]] = None, default_pool_writer : Optional[Union[str, String]] = None,   
default_reader : Optional[Union[str, String]] = None, default_using_server2 : Optional[Union[Bool, bool]] = None,   
default_writer : Optional[Union[str, String]] = None, output_dir : Optional[Union[str, String]] = None):

  
  set_context_attr_hdl_(context_hdl, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader,  
   default_pool_writer, default_reader, default_using_server2, default_writer, output_dir)
  return 



@typecheck
def set_context_attr_hdl_(context_hdl : Context, attached_mode_ : Optional[Union[Bool, bool]] = None, default_gatherer_ : Optional[Union[str, String]] = None,   
default_pool_ : Optional[Union[str, String]] = None, default_pool_gatherer_ : Optional[Union[str, String]] = None,   
default_pool_reader_ : Optional[Union[str, String]] = None, default_pool_writer_ : Optional[Union[str, String]] = None,   
default_reader_ : Optional[Union[str, String]] = None, default_using_server2_ : Optional[Union[Bool, bool]] = None,   
default_writer_ : Optional[Union[str, String]] = None, output_dir_ : Optional[Union[str, String]] = None):

  
  

  if attached_mode_ is not None: 
  
    attached_mode_ = Bool(attached_mode_)
    attached_mode_c = attached_mode_._c_value
    lib.cxios_set_context_attached_mode(context_hdl, attached_mode_c)
    
  

  if default_gatherer_ is not None:
  
    default_gatherer_= String(default_gatherer_)
    default_gatherer_c = default_gatherer_._c_value
    len_default_gatherer_c = len(ctypes.string_at(default_gatherer_c))
    lib.cxios_set_context_default_gatherer(context_hdl, default_gatherer_c, len_default_gatherer_c)
    
  

  if default_pool_ is not None:
  
    default_pool_= String(default_pool_)
    default_pool_c = default_pool_._c_value
    len_default_pool_c = len(ctypes.string_at(default_pool_c))
    lib.cxios_set_context_default_pool(context_hdl, default_pool_c, len_default_pool_c)
    
  

  if default_pool_gatherer_ is not None:
  
    default_pool_gatherer_= String(default_pool_gatherer_)
    default_pool_gatherer_c = default_pool_gatherer_._c_value
    len_default_pool_gatherer_c = len(ctypes.string_at(default_pool_gatherer_c))
    lib.cxios_set_context_default_pool_gatherer(context_hdl, default_pool_gatherer_c, len_default_pool_gatherer_c)
    
  

  if default_pool_reader_ is not None:
  
    default_pool_reader_= String(default_pool_reader_)
    default_pool_reader_c = default_pool_reader_._c_value
    len_default_pool_reader_c = len(ctypes.string_at(default_pool_reader_c))
    lib.cxios_set_context_default_pool_reader(context_hdl, default_pool_reader_c, len_default_pool_reader_c)
    
  

  if default_pool_writer_ is not None:
  
    default_pool_writer_= String(default_pool_writer_)
    default_pool_writer_c = default_pool_writer_._c_value
    len_default_pool_writer_c = len(ctypes.string_at(default_pool_writer_c))
    lib.cxios_set_context_default_pool_writer(context_hdl, default_pool_writer_c, len_default_pool_writer_c)
    
  

  if default_reader_ is not None:
  
    default_reader_= String(default_reader_)
    default_reader_c = default_reader_._c_value
    len_default_reader_c = len(ctypes.string_at(default_reader_c))
    lib.cxios_set_context_default_reader(context_hdl, default_reader_c, len_default_reader_c)
    
  

  if default_using_server2_ is not None: 
  
    default_using_server2_ = Bool(default_using_server2_)
    default_using_server2_c = default_using_server2_._c_value
    lib.cxios_set_context_default_using_server2(context_hdl, default_using_server2_c)
    
  

  if default_writer_ is not None:
  
    default_writer_= String(default_writer_)
    default_writer_c = default_writer_._c_value
    len_default_writer_c = len(ctypes.string_at(default_writer_c))
    lib.cxios_set_context_default_writer(context_hdl, default_writer_c, len_default_writer_c)
    
  

  if output_dir_ is not None:
  
    output_dir_= String(output_dir_)
    output_dir_c = output_dir_._c_value
    len_output_dir_c = len(ctypes.string_at(output_dir_c))
    lib.cxios_set_context_output_dir(context_hdl, output_dir_c, len_output_dir_c)
    
  
  return 



@typecheck
def get_context_attr(context_id : Union[String, str], attached_mode : Optional[Bool] = None, default_gatherer : Optional[String] = None,   
default_pool : Optional[String] = None, default_pool_gatherer : Optional[String] = None, default_pool_reader : Optional[String] = None,   
default_pool_writer : Optional[String] = None, default_reader : Optional[String] = None, default_using_server2 : Optional[Bool] = None,   
default_writer : Optional[String] = None, output_dir : Optional[String] = None):

  
  context_hdl = Context()
  

  get_context_handle(context_id, context_hdl)
  get_context_attr_hdl_(context_hdl, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader,  
   default_pool_writer, default_reader, default_using_server2, default_writer, output_dir)
  return 



@typecheck
def get_context_attr_hdl(context_hdl : Context, attached_mode : Optional[Bool] = None, default_gatherer : Optional[String] = None,   
default_pool : Optional[String] = None, default_pool_gatherer : Optional[String] = None, default_pool_reader : Optional[String] = None,   
default_pool_writer : Optional[String] = None, default_reader : Optional[String] = None, default_using_server2 : Optional[Bool] = None,   
default_writer : Optional[String] = None, output_dir : Optional[String] = None):

  
  get_context_attr_hdl_(context_hdl, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader,  
   default_pool_writer, default_reader, default_using_server2, default_writer, output_dir)
  return 



@typecheck
def get_context_attr_hdl_(context_hdl : Context, attached_mode_ : Optional[Bool] = None, default_gatherer_ : Optional[String] = None,   
default_pool_ : Optional[String] = None, default_pool_gatherer_ : Optional[String] = None,   
default_pool_reader_ : Optional[String] = None, default_pool_writer_ : Optional[String] = None,   
default_reader_ : Optional[String] = None, default_using_server2_ : Optional[Bool] = None,   
default_writer_ : Optional[String] = None, output_dir_ : Optional[String] = None):

  
  

  if attached_mode_ is not None: 
  
    attached_mode_c = attached_mode_._c_value
    lib.cxios_get_context_attached_mode(context_hdl, attached_mode_c)
    
  

  if default_gatherer_ is not None:
  
    default_gatherer_c = default_gatherer_._c_value
    len_default_gatherer_c = len(ctypes.string_at(default_gatherer_c))
    lib.cxios_get_context_default_gatherer(context_hdl, default_gatherer_c, len_default_gatherer_c)
    
  

  if default_pool_ is not None:
  
    default_pool_c = default_pool_._c_value
    len_default_pool_c = len(ctypes.string_at(default_pool_c))
    lib.cxios_get_context_default_pool(context_hdl, default_pool_c, len_default_pool_c)
    
  

  if default_pool_gatherer_ is not None:
  
    default_pool_gatherer_c = default_pool_gatherer_._c_value
    len_default_pool_gatherer_c = len(ctypes.string_at(default_pool_gatherer_c))
    lib.cxios_get_context_default_pool_gatherer(context_hdl, default_pool_gatherer_c, len_default_pool_gatherer_c)
    
  

  if default_pool_reader_ is not None:
  
    default_pool_reader_c = default_pool_reader_._c_value
    len_default_pool_reader_c = len(ctypes.string_at(default_pool_reader_c))
    lib.cxios_get_context_default_pool_reader(context_hdl, default_pool_reader_c, len_default_pool_reader_c)
    
  

  if default_pool_writer_ is not None:
  
    default_pool_writer_c = default_pool_writer_._c_value
    len_default_pool_writer_c = len(ctypes.string_at(default_pool_writer_c))
    lib.cxios_get_context_default_pool_writer(context_hdl, default_pool_writer_c, len_default_pool_writer_c)
    
  

  if default_reader_ is not None:
  
    default_reader_c = default_reader_._c_value
    len_default_reader_c = len(ctypes.string_at(default_reader_c))
    lib.cxios_get_context_default_reader(context_hdl, default_reader_c, len_default_reader_c)
    
  

  if default_using_server2_ is not None: 
  
    default_using_server2_c = default_using_server2_._c_value
    lib.cxios_get_context_default_using_server2(context_hdl, default_using_server2_c)
    
  

  if default_writer_ is not None:
  
    default_writer_c = default_writer_._c_value
    len_default_writer_c = len(ctypes.string_at(default_writer_c))
    lib.cxios_get_context_default_writer(context_hdl, default_writer_c, len_default_writer_c)
    
  

  if output_dir_ is not None:
  
    output_dir_c = output_dir_._c_value
    len_output_dir_c = len(ctypes.string_at(output_dir_c))
    lib.cxios_get_context_output_dir(context_hdl, output_dir_c, len_output_dir_c)
    
  
  return 



@typecheck
def is_defined_context_attr(context_id : String, attached_mode : Optional[Bool] = None, default_gatherer : Optional[Bool] = None,   
default_pool : Optional[Bool] = None, default_pool_gatherer : Optional[Bool] = None, default_pool_reader : Optional[Bool] = None,   
default_pool_writer : Optional[Bool] = None, default_reader : Optional[Bool] = None, default_using_server2 : Optional[Bool] = None,   
default_writer : Optional[Bool] = None, output_dir : Optional[Bool] = None):

  
  context_hdl = Context()
  

  get_context_handle(context_id, context_hdl)
  is_defined_context_attr_hdl_(context_hdl, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader,  
   default_pool_writer, default_reader, default_using_server2, default_writer, output_dir)
  return 



@typecheck
def is_defined_context_attr_hdl(context_hdl : Context, attached_mode : Optional[Bool] = None, default_gatherer : Optional[Bool] = None,   
default_pool : Optional[Bool] = None, default_pool_gatherer : Optional[Bool] = None, default_pool_reader : Optional[Bool] = None,   
default_pool_writer : Optional[Bool] = None, default_reader : Optional[Bool] = None, default_using_server2 : Optional[Bool] = None,   
default_writer : Optional[Bool] = None, output_dir : Optional[Bool] = None):

  
  is_defined_context_attr_hdl_(context_hdl, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader,  
   default_pool_writer, default_reader, default_using_server2, default_writer, output_dir)
  return 



@typecheck
def is_defined_context_attr_hdl_(context_hdl : Context, attached_mode_ : Optional[Bool] = None, default_gatherer_ : Optional[Bool] = None,   
default_pool_ : Optional[Bool] = None, default_pool_gatherer_ : Optional[Bool] = None, default_pool_reader_ : Optional[Bool] = None,   
default_pool_writer_ : Optional[Bool] = None, default_reader_ : Optional[Bool] = None, default_using_server2_ : Optional[Bool] = None,   
default_writer_ : Optional[Bool] = None, output_dir_ : Optional[Bool] = None):

  
  

  if attached_mode_  is not None:
    attached_mode_c = lib.cxios_is_defined_context_attached_mode(context_hdl)
    attached_mode_._c_value = ctypes.c_bool(attached_mode_c)
    
  

  if default_gatherer_  is not None:
    default_gatherer_c = lib.cxios_is_defined_context_default_gatherer(context_hdl)
    default_gatherer_._c_value = ctypes.c_bool(default_gatherer_c)
    
  

  if default_pool_  is not None:
    default_pool_c = lib.cxios_is_defined_context_default_pool(context_hdl)
    default_pool_._c_value = ctypes.c_bool(default_pool_c)
    
  

  if default_pool_gatherer_  is not None:
    default_pool_gatherer_c = lib.cxios_is_defined_context_default_pool_gatherer(context_hdl)
    default_pool_gatherer_._c_value = ctypes.c_bool(default_pool_gatherer_c)
    
  

  if default_pool_reader_  is not None:
    default_pool_reader_c = lib.cxios_is_defined_context_default_pool_reader(context_hdl)
    default_pool_reader_._c_value = ctypes.c_bool(default_pool_reader_c)
    
  

  if default_pool_writer_  is not None:
    default_pool_writer_c = lib.cxios_is_defined_context_default_pool_writer(context_hdl)
    default_pool_writer_._c_value = ctypes.c_bool(default_pool_writer_c)
    
  

  if default_reader_  is not None:
    default_reader_c = lib.cxios_is_defined_context_default_reader(context_hdl)
    default_reader_._c_value = ctypes.c_bool(default_reader_c)
    
  

  if default_using_server2_  is not None:
    default_using_server2_c = lib.cxios_is_defined_context_default_using_server2(context_hdl)
    default_using_server2_._c_value = ctypes.c_bool(default_using_server2_c)
    
  

  if default_writer_  is not None:
    default_writer_c = lib.cxios_is_defined_context_default_writer(context_hdl)
    default_writer_._c_value = ctypes.c_bool(default_writer_c)
    
  

  if output_dir_  is not None:
    output_dir_c = lib.cxios_is_defined_context_output_dir(context_hdl)
    output_dir_._c_value = ctypes.c_bool(output_dir_c)
    
  
  return 



