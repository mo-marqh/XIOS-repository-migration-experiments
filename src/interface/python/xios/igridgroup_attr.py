# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.igrid import  get_gridgroup_handle
from xios.ogridgroup_attr import GridGroup

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_gridgroup_comment.argtypes = [GridGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_gridgroup_comment.restypes = None

lib.cxios_set_gridgroup_comment.argtypes = [GridGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_gridgroup_comment.restypes = None

lib.cxios_is_defined_gridgroup_comment.argtypes = [GridGroup]
lib.cxios_is_defined_gridgroup_comment.restypes = ctypes.c_bool

lib.cxios_get_gridgroup_description.argtypes = [GridGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_gridgroup_description.restypes = None

lib.cxios_set_gridgroup_description.argtypes = [GridGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_gridgroup_description.restypes = None

lib.cxios_is_defined_gridgroup_description.argtypes = [GridGroup]
lib.cxios_is_defined_gridgroup_description.restypes = ctypes.c_bool

lib.cxios_get_gridgroup_group_ref.argtypes = [GridGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_gridgroup_group_ref.restypes = None

lib.cxios_set_gridgroup_group_ref.argtypes = [GridGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_gridgroup_group_ref.restypes = None

lib.cxios_is_defined_gridgroup_group_ref.argtypes = [GridGroup]
lib.cxios_is_defined_gridgroup_group_ref.restypes = ctypes.c_bool

lib.cxios_get_gridgroup_mask_0d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_gridgroup_mask_0d.restypes = None

lib.cxios_set_gridgroup_mask_0d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_gridgroup_mask_0d.restypes = None

lib.cxios_is_defined_gridgroup_mask_0d.argtypes = [GridGroup]
lib.cxios_is_defined_gridgroup_mask_0d.restypes = ctypes.c_bool

lib.cxios_get_gridgroup_mask_1d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_gridgroup_mask_1d.restypes = None

lib.cxios_set_gridgroup_mask_1d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_gridgroup_mask_1d.restypes = None

lib.cxios_is_defined_gridgroup_mask_1d.argtypes = [GridGroup]
lib.cxios_is_defined_gridgroup_mask_1d.restypes = ctypes.c_bool

lib.cxios_get_gridgroup_mask_2d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_gridgroup_mask_2d.restypes = None

lib.cxios_set_gridgroup_mask_2d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_gridgroup_mask_2d.restypes = None

lib.cxios_is_defined_gridgroup_mask_2d.argtypes = [GridGroup]
lib.cxios_is_defined_gridgroup_mask_2d.restypes = ctypes.c_bool

lib.cxios_get_gridgroup_mask_3d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_gridgroup_mask_3d.restypes = None

lib.cxios_set_gridgroup_mask_3d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_gridgroup_mask_3d.restypes = None

lib.cxios_is_defined_gridgroup_mask_3d.argtypes = [GridGroup]
lib.cxios_is_defined_gridgroup_mask_3d.restypes = ctypes.c_bool

lib.cxios_get_gridgroup_mask_4d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_gridgroup_mask_4d.restypes = None

lib.cxios_set_gridgroup_mask_4d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_gridgroup_mask_4d.restypes = None

lib.cxios_is_defined_gridgroup_mask_4d.argtypes = [GridGroup]
lib.cxios_is_defined_gridgroup_mask_4d.restypes = ctypes.c_bool

lib.cxios_get_gridgroup_mask_5d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_gridgroup_mask_5d.restypes = None

lib.cxios_set_gridgroup_mask_5d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_gridgroup_mask_5d.restypes = None

lib.cxios_is_defined_gridgroup_mask_5d.argtypes = [GridGroup]
lib.cxios_is_defined_gridgroup_mask_5d.restypes = ctypes.c_bool

lib.cxios_get_gridgroup_mask_6d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_gridgroup_mask_6d.restypes = None

lib.cxios_set_gridgroup_mask_6d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_gridgroup_mask_6d.restypes = None

lib.cxios_is_defined_gridgroup_mask_6d.argtypes = [GridGroup]
lib.cxios_is_defined_gridgroup_mask_6d.restypes = ctypes.c_bool

lib.cxios_get_gridgroup_mask_7d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_gridgroup_mask_7d.restypes = None

lib.cxios_set_gridgroup_mask_7d.argtypes = [GridGroup, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_gridgroup_mask_7d.restypes = None

lib.cxios_is_defined_gridgroup_mask_7d.argtypes = [GridGroup]
lib.cxios_is_defined_gridgroup_mask_7d.restypes = ctypes.c_bool

lib.cxios_get_gridgroup_name.argtypes = [GridGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_gridgroup_name.restypes = None

lib.cxios_set_gridgroup_name.argtypes = [GridGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_gridgroup_name.restypes = None

lib.cxios_is_defined_gridgroup_name.argtypes = [GridGroup]
lib.cxios_is_defined_gridgroup_name.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_gridgroup_attr(gridgroup_id : Union[String, str], comment : Optional[Union[str, String]] = None, description : Optional[Union[str, String]] = None,   
group_ref : Optional[Union[str, String]] = None, mask_0d : Optional[Union[np.ndarray, NpArrayBool]] = None,   
mask_1d : Optional[Union[np.ndarray, NpArrayBool]] = None, mask_2d : Optional[Union[np.ndarray, NpArrayBool]] = None,   
mask_3d : Optional[Union[np.ndarray, NpArrayBool]] = None, mask_4d : Optional[Union[np.ndarray, NpArrayBool]] = None,   
mask_5d : Optional[Union[np.ndarray, NpArrayBool]] = None, mask_6d : Optional[Union[np.ndarray, NpArrayBool]] = None,   
mask_7d : Optional[Union[np.ndarray, NpArrayBool]] = None, name : Optional[Union[str, String]] = None):

  
  gridgroup_hdl = GridGroup()
  

  get_gridgroup_handle(gridgroup_id, gridgroup_hdl)
  set_gridgroup_attr_hdl_(gridgroup_hdl, comment, description, group_ref, mask_0d, mask_1d, mask_2d, mask_3d, mask_4d,  
   mask_5d, mask_6d, mask_7d, name)
  return 



@typecheck
def set_gridgroup_attr_hdl(gridgroup_hdl : GridGroup, comment : Optional[Union[str, String]] = None, description : Optional[Union[str, String]] = None,   
group_ref : Optional[Union[str, String]] = None, mask_0d : Optional[Union[np.ndarray, NpArrayBool]] = None,   
mask_1d : Optional[Union[np.ndarray, NpArrayBool]] = None, mask_2d : Optional[Union[np.ndarray, NpArrayBool]] = None,   
mask_3d : Optional[Union[np.ndarray, NpArrayBool]] = None, mask_4d : Optional[Union[np.ndarray, NpArrayBool]] = None,   
mask_5d : Optional[Union[np.ndarray, NpArrayBool]] = None, mask_6d : Optional[Union[np.ndarray, NpArrayBool]] = None,   
mask_7d : Optional[Union[np.ndarray, NpArrayBool]] = None, name : Optional[Union[str, String]] = None):

  
  set_gridgroup_attr_hdl_(gridgroup_hdl, comment, description, group_ref, mask_0d, mask_1d, mask_2d, mask_3d, mask_4d,  
   mask_5d, mask_6d, mask_7d, name)
  return 



@typecheck
def set_gridgroup_attr_hdl_(gridgroup_hdl : GridGroup, comment_ : Optional[Union[str, String]] = None, description_ : Optional[Union[str, String]] = None,   
group_ref_ : Optional[Union[str, String]] = None, mask_0d_ : Optional[Union[np.ndarray, NpArrayBool]] = None,   
mask_1d_ : Optional[Union[np.ndarray, NpArrayBool]] = None, mask_2d_ : Optional[Union[np.ndarray, NpArrayBool]] = None,   
mask_3d_ : Optional[Union[np.ndarray, NpArrayBool]] = None, mask_4d_ : Optional[Union[np.ndarray, NpArrayBool]] = None,   
mask_5d_ : Optional[Union[np.ndarray, NpArrayBool]] = None, mask_6d_ : Optional[Union[np.ndarray, NpArrayBool]] = None,   
mask_7d_ : Optional[Union[np.ndarray, NpArrayBool]] = None, name_ : Optional[Union[str, String]] = None):

  
  

  if comment_ is not None:
  
    comment_= String(comment_)
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_set_gridgroup_comment(gridgroup_hdl, comment_c, len_comment_c)
    
  

  if description_ is not None:
  
    description_= String(description_)
    description_c = description_._c_value
    len_description_c = len(ctypes.string_at(description_c))
    lib.cxios_set_gridgroup_description(gridgroup_hdl, description_c, len_description_c)
    
  

  if group_ref_ is not None:
  
    group_ref_= String(group_ref_)
    group_ref_c = group_ref_._c_value
    len_group_ref_c = len(ctypes.string_at(group_ref_c))
    lib.cxios_set_gridgroup_group_ref(gridgroup_hdl, group_ref_c, len_group_ref_c)
    
  

  if mask_0d_ is not None:
    mask_0d_ = NpArrayBool(mask_0d_)
    mask_0d_c = mask_0d_._c_value
    if len(mask_0d_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(mask_0d_.shape)
      shape_c = shape(*mask_0d_.shape)
      lib.cxios_set_gridgroup_mask_0d(gridgroup_hdl, mask_0d_c, shape_c)
  

  if mask_1d_ is not None:
    mask_1d_ = NpArrayBool(mask_1d_)
    mask_1d_c = mask_1d_._c_value
    if len(mask_1d_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(mask_1d_.shape)
      shape_c = shape(*mask_1d_.shape)
      lib.cxios_set_gridgroup_mask_1d(gridgroup_hdl, mask_1d_c, shape_c)
  

  if mask_2d_ is not None:
    mask_2d_ = NpArrayBool(mask_2d_)
    mask_2d_c = mask_2d_._c_value
    if len(mask_2d_.shape) != 2 : 
      raise ValueError('The array dimension should be 2')
    else : 
      shape = ctypes.c_int * len(mask_2d_.shape)
      shape_c = shape(*mask_2d_.shape)
      lib.cxios_set_gridgroup_mask_2d(gridgroup_hdl, mask_2d_c, shape_c)
  

  if mask_3d_ is not None:
    mask_3d_ = NpArrayBool(mask_3d_)
    mask_3d_c = mask_3d_._c_value
    if len(mask_3d_.shape) != 3 : 
      raise ValueError('The array dimension should be 3')
    else : 
      shape = ctypes.c_int * len(mask_3d_.shape)
      shape_c = shape(*mask_3d_.shape)
      lib.cxios_set_gridgroup_mask_3d(gridgroup_hdl, mask_3d_c, shape_c)
  

  if mask_4d_ is not None:
    mask_4d_ = NpArrayBool(mask_4d_)
    mask_4d_c = mask_4d_._c_value
    if len(mask_4d_.shape) != 4 : 
      raise ValueError('The array dimension should be 4')
    else : 
      shape = ctypes.c_int * len(mask_4d_.shape)
      shape_c = shape(*mask_4d_.shape)
      lib.cxios_set_gridgroup_mask_4d(gridgroup_hdl, mask_4d_c, shape_c)
  

  if mask_5d_ is not None:
    mask_5d_ = NpArrayBool(mask_5d_)
    mask_5d_c = mask_5d_._c_value
    if len(mask_5d_.shape) != 5 : 
      raise ValueError('The array dimension should be 5')
    else : 
      shape = ctypes.c_int * len(mask_5d_.shape)
      shape_c = shape(*mask_5d_.shape)
      lib.cxios_set_gridgroup_mask_5d(gridgroup_hdl, mask_5d_c, shape_c)
  

  if mask_6d_ is not None:
    mask_6d_ = NpArrayBool(mask_6d_)
    mask_6d_c = mask_6d_._c_value
    if len(mask_6d_.shape) != 6 : 
      raise ValueError('The array dimension should be 6')
    else : 
      shape = ctypes.c_int * len(mask_6d_.shape)
      shape_c = shape(*mask_6d_.shape)
      lib.cxios_set_gridgroup_mask_6d(gridgroup_hdl, mask_6d_c, shape_c)
  

  if mask_7d_ is not None:
    mask_7d_ = NpArrayBool(mask_7d_)
    mask_7d_c = mask_7d_._c_value
    if len(mask_7d_.shape) != 7 : 
      raise ValueError('The array dimension should be 7')
    else : 
      shape = ctypes.c_int * len(mask_7d_.shape)
      shape_c = shape(*mask_7d_.shape)
      lib.cxios_set_gridgroup_mask_7d(gridgroup_hdl, mask_7d_c, shape_c)
  

  if name_ is not None:
  
    name_= String(name_)
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_set_gridgroup_name(gridgroup_hdl, name_c, len_name_c)
    
  
  return 



@typecheck
def get_gridgroup_attr(gridgroup_id : Union[String, str], comment : Optional[String] = None, description : Optional[String] = None,   
group_ref : Optional[String] = None, mask_0d : Optional[NpArrayBool] = None, mask_1d : Optional[NpArrayBool] = None,   
mask_2d : Optional[NpArrayBool] = None, mask_3d : Optional[NpArrayBool] = None, mask_4d : Optional[NpArrayBool] = None,   
mask_5d : Optional[NpArrayBool] = None, mask_6d : Optional[NpArrayBool] = None, mask_7d : Optional[NpArrayBool] = None,   
name : Optional[String] = None):

  
  gridgroup_hdl = GridGroup()
  

  get_gridgroup_handle(gridgroup_id, gridgroup_hdl)
  get_gridgroup_attr_hdl_(gridgroup_hdl, comment, description, group_ref, mask_0d, mask_1d, mask_2d, mask_3d, mask_4d,  
   mask_5d, mask_6d, mask_7d, name)
  return 



@typecheck
def get_gridgroup_attr_hdl(gridgroup_hdl : GridGroup, comment : Optional[String] = None, description : Optional[String] = None,   
group_ref : Optional[String] = None, mask_0d : Optional[NpArrayBool] = None, mask_1d : Optional[NpArrayBool] = None,   
mask_2d : Optional[NpArrayBool] = None, mask_3d : Optional[NpArrayBool] = None, mask_4d : Optional[NpArrayBool] = None,   
mask_5d : Optional[NpArrayBool] = None, mask_6d : Optional[NpArrayBool] = None, mask_7d : Optional[NpArrayBool] = None,   
name : Optional[String] = None):

  
  get_gridgroup_attr_hdl_(gridgroup_hdl, comment, description, group_ref, mask_0d, mask_1d, mask_2d, mask_3d, mask_4d,  
   mask_5d, mask_6d, mask_7d, name)
  return 



@typecheck
def get_gridgroup_attr_hdl_(gridgroup_hdl : GridGroup, comment_ : Optional[String] = None, description_ : Optional[String] = None,   
group_ref_ : Optional[String] = None, mask_0d_ : Optional[NpArrayBool] = None, mask_1d_ : Optional[NpArrayBool] = None,   
mask_2d_ : Optional[NpArrayBool] = None, mask_3d_ : Optional[NpArrayBool] = None, mask_4d_ : Optional[NpArrayBool] = None,   
mask_5d_ : Optional[NpArrayBool] = None, mask_6d_ : Optional[NpArrayBool] = None, mask_7d_ : Optional[NpArrayBool] = None,   
name_ : Optional[String] = None):

  
  

  if comment_ is not None:
  
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_get_gridgroup_comment(gridgroup_hdl, comment_c, len_comment_c)
    
  

  if description_ is not None:
  
    description_c = description_._c_value
    len_description_c = len(ctypes.string_at(description_c))
    lib.cxios_get_gridgroup_description(gridgroup_hdl, description_c, len_description_c)
    
  

  if group_ref_ is not None:
  
    group_ref_c = group_ref_._c_value
    len_group_ref_c = len(ctypes.string_at(group_ref_c))
    lib.cxios_get_gridgroup_group_ref(gridgroup_hdl, group_ref_c, len_group_ref_c)
    
  

  if mask_0d_ is not None:
    if mask_0d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_gridgroup_mask_0d_shape(gridgroup_hdl, shape_c)
      mask_0d_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_gridgroup_mask_0d(gridgroup_hdl, mask_0d_._c_value, shape_c)
    else: 
      mask_0d_c = mask_0d_._c_value
      if len(mask_0d_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(mask_0d_.shape)._c_value
        lib.cxios_get_gridgroup_mask_0d(gridgroup_hdl, mask_0d_c, shape_c)
  

  if mask_1d_ is not None:
    if mask_1d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_gridgroup_mask_1d_shape(gridgroup_hdl, shape_c)
      mask_1d_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_gridgroup_mask_1d(gridgroup_hdl, mask_1d_._c_value, shape_c)
    else: 
      mask_1d_c = mask_1d_._c_value
      if len(mask_1d_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(mask_1d_.shape)._c_value
        lib.cxios_get_gridgroup_mask_1d(gridgroup_hdl, mask_1d_c, shape_c)
  

  if mask_2d_ is not None:
    if mask_2d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 2)()
      lib.cxios_gridgroup_mask_2d_shape(gridgroup_hdl, shape_c)
      mask_2d_.arr = np.zeros((shape_c[0], shape_c[1]) , order = 'F')
      lib.cxios_get_gridgroup_mask_2d(gridgroup_hdl, mask_2d_._c_value, shape_c)
    else: 
      mask_2d_c = mask_2d_._c_value
      if len(mask_2d_.shape) != 2 : 
        raise ValueError('The array dimension should be 2')
      else : 
        shape_c = NpArrayInt(mask_2d_.shape)._c_value
        lib.cxios_get_gridgroup_mask_2d(gridgroup_hdl, mask_2d_c, shape_c)
  

  if mask_3d_ is not None:
    if mask_3d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 3)()
      lib.cxios_gridgroup_mask_3d_shape(gridgroup_hdl, shape_c)
      mask_3d_.arr = np.zeros((shape_c[0], shape_c[1], shape_c[2]) , order = 'F')
      lib.cxios_get_gridgroup_mask_3d(gridgroup_hdl, mask_3d_._c_value, shape_c)
    else: 
      mask_3d_c = mask_3d_._c_value
      if len(mask_3d_.shape) != 3 : 
        raise ValueError('The array dimension should be 3')
      else : 
        shape_c = NpArrayInt(mask_3d_.shape)._c_value
        lib.cxios_get_gridgroup_mask_3d(gridgroup_hdl, mask_3d_c, shape_c)
  

  if mask_4d_ is not None:
    if mask_4d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 4)()
      lib.cxios_gridgroup_mask_4d_shape(gridgroup_hdl, shape_c)
      mask_4d_.arr = np.zeros((shape_c[0], shape_c[1], shape_c[2], shape_c[3]) , order = 'F')
      lib.cxios_get_gridgroup_mask_4d(gridgroup_hdl, mask_4d_._c_value, shape_c)
    else: 
      mask_4d_c = mask_4d_._c_value
      if len(mask_4d_.shape) != 4 : 
        raise ValueError('The array dimension should be 4')
      else : 
        shape_c = NpArrayInt(mask_4d_.shape)._c_value
        lib.cxios_get_gridgroup_mask_4d(gridgroup_hdl, mask_4d_c, shape_c)
  

  if mask_5d_ is not None:
    if mask_5d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 5)()
      lib.cxios_gridgroup_mask_5d_shape(gridgroup_hdl, shape_c)
      mask_5d_.arr = np.zeros((shape_c[0], shape_c[1], shape_c[2], shape_c[3], shape_c[4]) , order = 'F')
      lib.cxios_get_gridgroup_mask_5d(gridgroup_hdl, mask_5d_._c_value, shape_c)
    else: 
      mask_5d_c = mask_5d_._c_value
      if len(mask_5d_.shape) != 5 : 
        raise ValueError('The array dimension should be 5')
      else : 
        shape_c = NpArrayInt(mask_5d_.shape)._c_value
        lib.cxios_get_gridgroup_mask_5d(gridgroup_hdl, mask_5d_c, shape_c)
  

  if mask_6d_ is not None:
    if mask_6d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 6)()
      lib.cxios_gridgroup_mask_6d_shape(gridgroup_hdl, shape_c)
      mask_6d_.arr = np.zeros((shape_c[0], shape_c[1], shape_c[2], shape_c[3], shape_c[4], shape_c[5]) , order = 'F')
      lib.cxios_get_gridgroup_mask_6d(gridgroup_hdl, mask_6d_._c_value, shape_c)
    else: 
      mask_6d_c = mask_6d_._c_value
      if len(mask_6d_.shape) != 6 : 
        raise ValueError('The array dimension should be 6')
      else : 
        shape_c = NpArrayInt(mask_6d_.shape)._c_value
        lib.cxios_get_gridgroup_mask_6d(gridgroup_hdl, mask_6d_c, shape_c)
  

  if mask_7d_ is not None:
    if mask_7d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 7)()
      lib.cxios_gridgroup_mask_7d_shape(gridgroup_hdl, shape_c)
      mask_7d_.arr = np.zeros((shape_c[0], shape_c[1], shape_c[2], shape_c[3], shape_c[4], shape_c[5], shape_c[6]) , order = 'F')
      lib.cxios_get_gridgroup_mask_7d(gridgroup_hdl, mask_7d_._c_value, shape_c)
    else: 
      mask_7d_c = mask_7d_._c_value
      if len(mask_7d_.shape) != 7 : 
        raise ValueError('The array dimension should be 7')
      else : 
        shape_c = NpArrayInt(mask_7d_.shape)._c_value
        lib.cxios_get_gridgroup_mask_7d(gridgroup_hdl, mask_7d_c, shape_c)
  

  if name_ is not None:
  
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_get_gridgroup_name(gridgroup_hdl, name_c, len_name_c)
    
  
  return 



@typecheck
def is_defined_gridgroup_attr(gridgroup_id : String, comment : Optional[Bool] = None, description : Optional[Bool] = None,   
group_ref : Optional[Bool] = None, mask_0d : Optional[Bool] = None, mask_1d : Optional[Bool] = None,   
mask_2d : Optional[Bool] = None, mask_3d : Optional[Bool] = None, mask_4d : Optional[Bool] = None,   
mask_5d : Optional[Bool] = None, mask_6d : Optional[Bool] = None, mask_7d : Optional[Bool] = None,   
name : Optional[Bool] = None):

  
  gridgroup_hdl = GridGroup()
  

  get_gridgroup_handle(gridgroup_id, gridgroup_hdl)
  is_defined_gridgroup_attr_hdl_(gridgroup_hdl, comment, description, group_ref, mask_0d, mask_1d, mask_2d, mask_3d, mask_4d,  
   mask_5d, mask_6d, mask_7d, name)
  return 



@typecheck
def is_defined_gridgroup_attr_hdl(gridgroup_hdl : GridGroup, comment : Optional[Bool] = None, description : Optional[Bool] = None,   
group_ref : Optional[Bool] = None, mask_0d : Optional[Bool] = None, mask_1d : Optional[Bool] = None,   
mask_2d : Optional[Bool] = None, mask_3d : Optional[Bool] = None, mask_4d : Optional[Bool] = None,   
mask_5d : Optional[Bool] = None, mask_6d : Optional[Bool] = None, mask_7d : Optional[Bool] = None,   
name : Optional[Bool] = None):

  
  is_defined_gridgroup_attr_hdl_(gridgroup_hdl, comment, description, group_ref, mask_0d, mask_1d, mask_2d, mask_3d, mask_4d,  
   mask_5d, mask_6d, mask_7d, name)
  return 



@typecheck
def is_defined_gridgroup_attr_hdl_(gridgroup_hdl : GridGroup, comment_ : Optional[Bool] = None, description_ : Optional[Bool] = None,   
group_ref_ : Optional[Bool] = None, mask_0d_ : Optional[Bool] = None, mask_1d_ : Optional[Bool] = None,   
mask_2d_ : Optional[Bool] = None, mask_3d_ : Optional[Bool] = None, mask_4d_ : Optional[Bool] = None,   
mask_5d_ : Optional[Bool] = None, mask_6d_ : Optional[Bool] = None, mask_7d_ : Optional[Bool] = None,   
name_ : Optional[Bool] = None):

  
  

  if comment_  is not None:
    comment_c = lib.cxios_is_defined_gridgroup_comment(gridgroup_hdl)
    comment_._c_value = ctypes.c_bool(comment_c)
    
  

  if description_  is not None:
    description_c = lib.cxios_is_defined_gridgroup_description(gridgroup_hdl)
    description_._c_value = ctypes.c_bool(description_c)
    
  

  if group_ref_  is not None:
    group_ref_c = lib.cxios_is_defined_gridgroup_group_ref(gridgroup_hdl)
    group_ref_._c_value = ctypes.c_bool(group_ref_c)
    
  

  if mask_0d_  is not None:
    mask_0d_c = lib.cxios_is_defined_gridgroup_mask_0d(gridgroup_hdl)
    mask_0d_._c_value = ctypes.c_bool(mask_0d_c)
    
  

  if mask_1d_  is not None:
    mask_1d_c = lib.cxios_is_defined_gridgroup_mask_1d(gridgroup_hdl)
    mask_1d_._c_value = ctypes.c_bool(mask_1d_c)
    
  

  if mask_2d_  is not None:
    mask_2d_c = lib.cxios_is_defined_gridgroup_mask_2d(gridgroup_hdl)
    mask_2d_._c_value = ctypes.c_bool(mask_2d_c)
    
  

  if mask_3d_  is not None:
    mask_3d_c = lib.cxios_is_defined_gridgroup_mask_3d(gridgroup_hdl)
    mask_3d_._c_value = ctypes.c_bool(mask_3d_c)
    
  

  if mask_4d_  is not None:
    mask_4d_c = lib.cxios_is_defined_gridgroup_mask_4d(gridgroup_hdl)
    mask_4d_._c_value = ctypes.c_bool(mask_4d_c)
    
  

  if mask_5d_  is not None:
    mask_5d_c = lib.cxios_is_defined_gridgroup_mask_5d(gridgroup_hdl)
    mask_5d_._c_value = ctypes.c_bool(mask_5d_c)
    
  

  if mask_6d_  is not None:
    mask_6d_c = lib.cxios_is_defined_gridgroup_mask_6d(gridgroup_hdl)
    mask_6d_._c_value = ctypes.c_bool(mask_6d_c)
    
  

  if mask_7d_  is not None:
    mask_7d_c = lib.cxios_is_defined_gridgroup_mask_7d(gridgroup_hdl)
    mask_7d_._c_value = ctypes.c_bool(mask_7d_c)
    
  

  if name_  is not None:
    name_c = lib.cxios_is_defined_gridgroup_name(gridgroup_hdl)
    name_._c_value = ctypes.c_bool(name_c)
    
  
  return 



