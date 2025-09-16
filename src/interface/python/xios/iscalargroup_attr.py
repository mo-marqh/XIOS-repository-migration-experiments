# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.iscalar import  get_scalargroup_handle
from xios.oscalargroup_attr import ScalarGroup

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_scalargroup_axis_type.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalargroup_axis_type.restypes = None

lib.cxios_set_scalargroup_axis_type.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalargroup_axis_type.restypes = None

lib.cxios_is_defined_scalargroup_axis_type.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_axis_type.restypes = ctypes.c_bool

lib.cxios_get_scalargroup_bounds.argtypes = [ScalarGroup, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_scalargroup_bounds.restypes = None

lib.cxios_set_scalargroup_bounds.argtypes = [ScalarGroup, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_scalargroup_bounds.restypes = None

lib.cxios_is_defined_scalargroup_bounds.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_bounds.restypes = ctypes.c_bool

lib.cxios_get_scalargroup_bounds_name.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalargroup_bounds_name.restypes = None

lib.cxios_set_scalargroup_bounds_name.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalargroup_bounds_name.restypes = None

lib.cxios_is_defined_scalargroup_bounds_name.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_bounds_name.restypes = ctypes.c_bool

lib.cxios_get_scalargroup_comment.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalargroup_comment.restypes = None

lib.cxios_set_scalargroup_comment.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalargroup_comment.restypes = None

lib.cxios_is_defined_scalargroup_comment.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_comment.restypes = ctypes.c_bool

lib.cxios_get_scalargroup_group_ref.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalargroup_group_ref.restypes = None

lib.cxios_set_scalargroup_group_ref.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalargroup_group_ref.restypes = None

lib.cxios_is_defined_scalargroup_group_ref.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_group_ref.restypes = ctypes.c_bool

lib.cxios_get_scalargroup_label.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalargroup_label.restypes = None

lib.cxios_set_scalargroup_label.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalargroup_label.restypes = None

lib.cxios_is_defined_scalargroup_label.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_label.restypes = ctypes.c_bool

lib.cxios_get_scalargroup_long_name.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalargroup_long_name.restypes = None

lib.cxios_set_scalargroup_long_name.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalargroup_long_name.restypes = None

lib.cxios_is_defined_scalargroup_long_name.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_long_name.restypes = ctypes.c_bool

lib.cxios_get_scalargroup_mask.argtypes = [ScalarGroup, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_scalargroup_mask.restypes = None

lib.cxios_set_scalargroup_mask.argtypes = [ScalarGroup, ctypes.c_bool]
lib.cxios_set_scalargroup_mask.restypes = None

lib.cxios_is_defined_scalargroup_mask.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_mask.restypes = ctypes.c_bool

lib.cxios_get_scalargroup_n.argtypes = [ScalarGroup, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_scalargroup_n.restypes = None

lib.cxios_set_scalargroup_n.argtypes = [ScalarGroup, ctypes.c_int]
lib.cxios_set_scalargroup_n.restypes = None

lib.cxios_is_defined_scalargroup_n.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_n.restypes = ctypes.c_bool

lib.cxios_get_scalargroup_name.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalargroup_name.restypes = None

lib.cxios_set_scalargroup_name.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalargroup_name.restypes = None

lib.cxios_is_defined_scalargroup_name.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_name.restypes = ctypes.c_bool

lib.cxios_get_scalargroup_positive.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalargroup_positive.restypes = None

lib.cxios_set_scalargroup_positive.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalargroup_positive.restypes = None

lib.cxios_is_defined_scalargroup_positive.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_positive.restypes = ctypes.c_bool

lib.cxios_get_scalargroup_prec.argtypes = [ScalarGroup, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_scalargroup_prec.restypes = None

lib.cxios_set_scalargroup_prec.argtypes = [ScalarGroup, ctypes.c_int]
lib.cxios_set_scalargroup_prec.restypes = None

lib.cxios_is_defined_scalargroup_prec.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_prec.restypes = ctypes.c_bool

lib.cxios_get_scalargroup_scalar_ref.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalargroup_scalar_ref.restypes = None

lib.cxios_set_scalargroup_scalar_ref.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalargroup_scalar_ref.restypes = None

lib.cxios_is_defined_scalargroup_scalar_ref.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_scalar_ref.restypes = ctypes.c_bool

lib.cxios_get_scalargroup_standard_name.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalargroup_standard_name.restypes = None

lib.cxios_set_scalargroup_standard_name.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalargroup_standard_name.restypes = None

lib.cxios_is_defined_scalargroup_standard_name.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_standard_name.restypes = ctypes.c_bool

lib.cxios_get_scalargroup_unit.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalargroup_unit.restypes = None

lib.cxios_set_scalargroup_unit.argtypes = [ScalarGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalargroup_unit.restypes = None

lib.cxios_is_defined_scalargroup_unit.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_unit.restypes = ctypes.c_bool

lib.cxios_get_scalargroup_value.argtypes = [ScalarGroup, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_scalargroup_value.restypes = None

lib.cxios_set_scalargroup_value.argtypes = [ScalarGroup, ctypes.c_double]
lib.cxios_set_scalargroup_value.restypes = None

lib.cxios_is_defined_scalargroup_value.argtypes = [ScalarGroup]
lib.cxios_is_defined_scalargroup_value.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_scalargroup_attr(scalargroup_id : Union[String, str], axis_type : Optional[Union[str, String]] = None, bounds : Optional[Union[np.ndarray, NpArray]] = None,   
bounds_name : Optional[Union[str, String]] = None, comment : Optional[Union[str, String]] = None,   
group_ref : Optional[Union[str, String]] = None, label : Optional[Union[str, String]] = None,   
long_name : Optional[Union[str, String]] = None, mask : Optional[Union[Bool, bool]] = None,   
n : Optional[Union[Int, int]] = None, name : Optional[Union[str, String]] = None, positive : Optional[Union[str, String]] = None,   
prec : Optional[Union[Int, int]] = None, scalar_ref : Optional[Union[str, String]] = None,   
standard_name : Optional[Union[str, String]] = None, unit : Optional[Union[str, String]] = None,   
value : Optional[Union[Double, float]] = None):

  
  scalargroup_hdl = ScalarGroup()
  

  get_scalargroup_handle(scalargroup_id, scalargroup_hdl)
  set_scalargroup_attr_hdl_(scalargroup_hdl, axis_type, bounds, bounds_name, comment, group_ref, label, long_name, mask,  
   n, name, positive, prec, scalar_ref, standard_name, unit, value)
  return 



@typecheck
def set_scalargroup_attr_hdl(scalargroup_hdl : ScalarGroup, axis_type : Optional[Union[str, String]] = None, bounds : Optional[Union[np.ndarray, NpArray]] = None,   
bounds_name : Optional[Union[str, String]] = None, comment : Optional[Union[str, String]] = None,   
group_ref : Optional[Union[str, String]] = None, label : Optional[Union[str, String]] = None,   
long_name : Optional[Union[str, String]] = None, mask : Optional[Union[Bool, bool]] = None,   
n : Optional[Union[Int, int]] = None, name : Optional[Union[str, String]] = None, positive : Optional[Union[str, String]] = None,   
prec : Optional[Union[Int, int]] = None, scalar_ref : Optional[Union[str, String]] = None,   
standard_name : Optional[Union[str, String]] = None, unit : Optional[Union[str, String]] = None,   
value : Optional[Union[Double, float]] = None):

  
  set_scalargroup_attr_hdl_(scalargroup_hdl, axis_type, bounds, bounds_name, comment, group_ref, label, long_name, mask,  
   n, name, positive, prec, scalar_ref, standard_name, unit, value)
  return 



@typecheck
def set_scalargroup_attr_hdl_(scalargroup_hdl : ScalarGroup, axis_type_ : Optional[Union[str, String]] = None, bounds_ : Optional[Union[np.ndarray, NpArray]] = None,   
bounds_name_ : Optional[Union[str, String]] = None, comment_ : Optional[Union[str, String]] = None,   
group_ref_ : Optional[Union[str, String]] = None, label_ : Optional[Union[str, String]] = None,   
long_name_ : Optional[Union[str, String]] = None, mask_ : Optional[Union[Bool, bool]] = None,   
n_ : Optional[Union[Int, int]] = None, name_ : Optional[Union[str, String]] = None, positive_ : Optional[Union[str, String]] = None,   
prec_ : Optional[Union[Int, int]] = None, scalar_ref_ : Optional[Union[str, String]] = None,   
standard_name_ : Optional[Union[str, String]] = None, unit_ : Optional[Union[str, String]] = None,   
value_ : Optional[Union[Double, float]] = None):

  
  

  if axis_type_ is not None:
  
    axis_type_= String(axis_type_)
    axis_type_c = axis_type_._c_value
    len_axis_type_c = len(ctypes.string_at(axis_type_c))
    lib.cxios_set_scalargroup_axis_type(scalargroup_hdl, axis_type_c, len_axis_type_c)
    
  

  if bounds_ is not None:
    bounds_ = NpArray(bounds_)
    bounds_c = bounds_._c_value
    if len(bounds_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(bounds_.shape)
      shape_c = shape(*bounds_.shape)
      lib.cxios_set_scalargroup_bounds(scalargroup_hdl, bounds_c, shape_c)
  

  if bounds_name_ is not None:
  
    bounds_name_= String(bounds_name_)
    bounds_name_c = bounds_name_._c_value
    len_bounds_name_c = len(ctypes.string_at(bounds_name_c))
    lib.cxios_set_scalargroup_bounds_name(scalargroup_hdl, bounds_name_c, len_bounds_name_c)
    
  

  if comment_ is not None:
  
    comment_= String(comment_)
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_set_scalargroup_comment(scalargroup_hdl, comment_c, len_comment_c)
    
  

  if group_ref_ is not None:
  
    group_ref_= String(group_ref_)
    group_ref_c = group_ref_._c_value
    len_group_ref_c = len(ctypes.string_at(group_ref_c))
    lib.cxios_set_scalargroup_group_ref(scalargroup_hdl, group_ref_c, len_group_ref_c)
    
  

  if label_ is not None:
  
    label_= String(label_)
    label_c = label_._c_value
    len_label_c = len(ctypes.string_at(label_c))
    lib.cxios_set_scalargroup_label(scalargroup_hdl, label_c, len_label_c)
    
  

  if long_name_ is not None:
  
    long_name_= String(long_name_)
    long_name_c = long_name_._c_value
    len_long_name_c = len(ctypes.string_at(long_name_c))
    lib.cxios_set_scalargroup_long_name(scalargroup_hdl, long_name_c, len_long_name_c)
    
  

  if mask_ is not None: 
  
    mask_ = Bool(mask_)
    mask_c = mask_._c_value
    lib.cxios_set_scalargroup_mask(scalargroup_hdl, mask_c)
    
  

  if n_ is not None: 
  
    n_ = Int(n_)
    n_c = n_._c_value
    lib.cxios_set_scalargroup_n(scalargroup_hdl, n_c)
    
  

  if name_ is not None:
  
    name_= String(name_)
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_set_scalargroup_name(scalargroup_hdl, name_c, len_name_c)
    
  

  if positive_ is not None:
  
    positive_= String(positive_)
    positive_c = positive_._c_value
    len_positive_c = len(ctypes.string_at(positive_c))
    lib.cxios_set_scalargroup_positive(scalargroup_hdl, positive_c, len_positive_c)
    
  

  if prec_ is not None: 
  
    prec_ = Int(prec_)
    prec_c = prec_._c_value
    lib.cxios_set_scalargroup_prec(scalargroup_hdl, prec_c)
    
  

  if scalar_ref_ is not None:
  
    scalar_ref_= String(scalar_ref_)
    scalar_ref_c = scalar_ref_._c_value
    len_scalar_ref_c = len(ctypes.string_at(scalar_ref_c))
    lib.cxios_set_scalargroup_scalar_ref(scalargroup_hdl, scalar_ref_c, len_scalar_ref_c)
    
  

  if standard_name_ is not None:
  
    standard_name_= String(standard_name_)
    standard_name_c = standard_name_._c_value
    len_standard_name_c = len(ctypes.string_at(standard_name_c))
    lib.cxios_set_scalargroup_standard_name(scalargroup_hdl, standard_name_c, len_standard_name_c)
    
  

  if unit_ is not None:
  
    unit_= String(unit_)
    unit_c = unit_._c_value
    len_unit_c = len(ctypes.string_at(unit_c))
    lib.cxios_set_scalargroup_unit(scalargroup_hdl, unit_c, len_unit_c)
    
  

  if value_ is not None: 
  
    value_ = Double(value_)
    value_c = value_._c_value
    lib.cxios_set_scalargroup_value(scalargroup_hdl, value_c)
    
  
  return 



@typecheck
def get_scalargroup_attr(scalargroup_id : Union[String, str], axis_type : Optional[String] = None, bounds : Optional[NpArray] = None,   
bounds_name : Optional[String] = None, comment : Optional[String] = None, group_ref : Optional[String] = None,   
label : Optional[String] = None, long_name : Optional[String] = None, mask : Optional[Bool] = None,   
n : Optional[Int] = None, name : Optional[String] = None, positive : Optional[String] = None,   
prec : Optional[Int] = None, scalar_ref : Optional[String] = None, standard_name : Optional[String] = None,   
unit : Optional[String] = None, value : Optional[Double] = None):

  
  scalargroup_hdl = ScalarGroup()
  

  get_scalargroup_handle(scalargroup_id, scalargroup_hdl)
  get_scalargroup_attr_hdl_(scalargroup_hdl, axis_type, bounds, bounds_name, comment, group_ref, label, long_name, mask,  
   n, name, positive, prec, scalar_ref, standard_name, unit, value)
  return 



@typecheck
def get_scalargroup_attr_hdl(scalargroup_hdl : ScalarGroup, axis_type : Optional[String] = None, bounds : Optional[NpArray] = None,   
bounds_name : Optional[String] = None, comment : Optional[String] = None, group_ref : Optional[String] = None,   
label : Optional[String] = None, long_name : Optional[String] = None, mask : Optional[Bool] = None,   
n : Optional[Int] = None, name : Optional[String] = None, positive : Optional[String] = None,   
prec : Optional[Int] = None, scalar_ref : Optional[String] = None, standard_name : Optional[String] = None,   
unit : Optional[String] = None, value : Optional[Double] = None):

  
  get_scalargroup_attr_hdl_(scalargroup_hdl, axis_type, bounds, bounds_name, comment, group_ref, label, long_name, mask,  
   n, name, positive, prec, scalar_ref, standard_name, unit, value)
  return 



@typecheck
def get_scalargroup_attr_hdl_(scalargroup_hdl : ScalarGroup, axis_type_ : Optional[String] = None, bounds_ : Optional[NpArray] = None,   
bounds_name_ : Optional[String] = None, comment_ : Optional[String] = None, group_ref_ : Optional[String] = None,   
label_ : Optional[String] = None, long_name_ : Optional[String] = None, mask_ : Optional[Bool] = None,   
n_ : Optional[Int] = None, name_ : Optional[String] = None, positive_ : Optional[String] = None,   
prec_ : Optional[Int] = None, scalar_ref_ : Optional[String] = None, standard_name_ : Optional[String] = None,   
unit_ : Optional[String] = None, value_ : Optional[Double] = None):

  
  

  if axis_type_ is not None:
  
    axis_type_c = axis_type_._c_value
    len_axis_type_c = len(ctypes.string_at(axis_type_c))
    lib.cxios_get_scalargroup_axis_type(scalargroup_hdl, axis_type_c, len_axis_type_c)
    
  

  if bounds_ is not None:
    if bounds_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_scalargroup_bounds_shape(scalargroup_hdl, shape_c)
      bounds_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_scalargroup_bounds(scalargroup_hdl, bounds_._c_value, shape_c)
    else: 
      bounds_c = bounds_._c_value
      if len(bounds_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(bounds_.shape)._c_value
        lib.cxios_get_scalargroup_bounds(scalargroup_hdl, bounds_c, shape_c)
  

  if bounds_name_ is not None:
  
    bounds_name_c = bounds_name_._c_value
    len_bounds_name_c = len(ctypes.string_at(bounds_name_c))
    lib.cxios_get_scalargroup_bounds_name(scalargroup_hdl, bounds_name_c, len_bounds_name_c)
    
  

  if comment_ is not None:
  
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_get_scalargroup_comment(scalargroup_hdl, comment_c, len_comment_c)
    
  

  if group_ref_ is not None:
  
    group_ref_c = group_ref_._c_value
    len_group_ref_c = len(ctypes.string_at(group_ref_c))
    lib.cxios_get_scalargroup_group_ref(scalargroup_hdl, group_ref_c, len_group_ref_c)
    
  

  if label_ is not None:
  
    label_c = label_._c_value
    len_label_c = len(ctypes.string_at(label_c))
    lib.cxios_get_scalargroup_label(scalargroup_hdl, label_c, len_label_c)
    
  

  if long_name_ is not None:
  
    long_name_c = long_name_._c_value
    len_long_name_c = len(ctypes.string_at(long_name_c))
    lib.cxios_get_scalargroup_long_name(scalargroup_hdl, long_name_c, len_long_name_c)
    
  

  if mask_ is not None: 
  
    mask_c = mask_._c_value
    lib.cxios_get_scalargroup_mask(scalargroup_hdl, mask_c)
    
  

  if n_ is not None: 
  
    n_c = n_._c_value
    lib.cxios_get_scalargroup_n(scalargroup_hdl, n_c)
    
  

  if name_ is not None:
  
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_get_scalargroup_name(scalargroup_hdl, name_c, len_name_c)
    
  

  if positive_ is not None:
  
    positive_c = positive_._c_value
    len_positive_c = len(ctypes.string_at(positive_c))
    lib.cxios_get_scalargroup_positive(scalargroup_hdl, positive_c, len_positive_c)
    
  

  if prec_ is not None: 
  
    prec_c = prec_._c_value
    lib.cxios_get_scalargroup_prec(scalargroup_hdl, prec_c)
    
  

  if scalar_ref_ is not None:
  
    scalar_ref_c = scalar_ref_._c_value
    len_scalar_ref_c = len(ctypes.string_at(scalar_ref_c))
    lib.cxios_get_scalargroup_scalar_ref(scalargroup_hdl, scalar_ref_c, len_scalar_ref_c)
    
  

  if standard_name_ is not None:
  
    standard_name_c = standard_name_._c_value
    len_standard_name_c = len(ctypes.string_at(standard_name_c))
    lib.cxios_get_scalargroup_standard_name(scalargroup_hdl, standard_name_c, len_standard_name_c)
    
  

  if unit_ is not None:
  
    unit_c = unit_._c_value
    len_unit_c = len(ctypes.string_at(unit_c))
    lib.cxios_get_scalargroup_unit(scalargroup_hdl, unit_c, len_unit_c)
    
  

  if value_ is not None: 
  
    value_c = value_._c_value
    lib.cxios_get_scalargroup_value(scalargroup_hdl, value_c)
    
  
  return 



@typecheck
def is_defined_scalargroup_attr(scalargroup_id : String, axis_type : Optional[Bool] = None, bounds : Optional[Bool] = None,   
bounds_name : Optional[Bool] = None, comment : Optional[Bool] = None, group_ref : Optional[Bool] = None,   
label : Optional[Bool] = None, long_name : Optional[Bool] = None, mask : Optional[Bool] = None,   
n : Optional[Bool] = None, name : Optional[Bool] = None, positive : Optional[Bool] = None,   
prec : Optional[Bool] = None, scalar_ref : Optional[Bool] = None, standard_name : Optional[Bool] = None,   
unit : Optional[Bool] = None, value : Optional[Bool] = None):

  
  scalargroup_hdl = ScalarGroup()
  

  get_scalargroup_handle(scalargroup_id, scalargroup_hdl)
  is_defined_scalargroup_attr_hdl_(scalargroup_hdl, axis_type, bounds, bounds_name, comment, group_ref, label, long_name, mask,  
   n, name, positive, prec, scalar_ref, standard_name, unit, value)
  return 



@typecheck
def is_defined_scalargroup_attr_hdl(scalargroup_hdl : ScalarGroup, axis_type : Optional[Bool] = None, bounds : Optional[Bool] = None,   
bounds_name : Optional[Bool] = None, comment : Optional[Bool] = None, group_ref : Optional[Bool] = None,   
label : Optional[Bool] = None, long_name : Optional[Bool] = None, mask : Optional[Bool] = None,   
n : Optional[Bool] = None, name : Optional[Bool] = None, positive : Optional[Bool] = None,   
prec : Optional[Bool] = None, scalar_ref : Optional[Bool] = None, standard_name : Optional[Bool] = None,   
unit : Optional[Bool] = None, value : Optional[Bool] = None):

  
  is_defined_scalargroup_attr_hdl_(scalargroup_hdl, axis_type, bounds, bounds_name, comment, group_ref, label, long_name, mask,  
   n, name, positive, prec, scalar_ref, standard_name, unit, value)
  return 



@typecheck
def is_defined_scalargroup_attr_hdl_(scalargroup_hdl : ScalarGroup, axis_type_ : Optional[Bool] = None, bounds_ : Optional[Bool] = None,   
bounds_name_ : Optional[Bool] = None, comment_ : Optional[Bool] = None, group_ref_ : Optional[Bool] = None,   
label_ : Optional[Bool] = None, long_name_ : Optional[Bool] = None, mask_ : Optional[Bool] = None,   
n_ : Optional[Bool] = None, name_ : Optional[Bool] = None, positive_ : Optional[Bool] = None,   
prec_ : Optional[Bool] = None, scalar_ref_ : Optional[Bool] = None, standard_name_ : Optional[Bool] = None,   
unit_ : Optional[Bool] = None, value_ : Optional[Bool] = None):

  
  

  if axis_type_  is not None:
    axis_type_c = lib.cxios_is_defined_scalargroup_axis_type(scalargroup_hdl)
    axis_type_._c_value = ctypes.c_bool(axis_type_c)
    
  

  if bounds_  is not None:
    bounds_c = lib.cxios_is_defined_scalargroup_bounds(scalargroup_hdl)
    bounds_._c_value = ctypes.c_bool(bounds_c)
    
  

  if bounds_name_  is not None:
    bounds_name_c = lib.cxios_is_defined_scalargroup_bounds_name(scalargroup_hdl)
    bounds_name_._c_value = ctypes.c_bool(bounds_name_c)
    
  

  if comment_  is not None:
    comment_c = lib.cxios_is_defined_scalargroup_comment(scalargroup_hdl)
    comment_._c_value = ctypes.c_bool(comment_c)
    
  

  if group_ref_  is not None:
    group_ref_c = lib.cxios_is_defined_scalargroup_group_ref(scalargroup_hdl)
    group_ref_._c_value = ctypes.c_bool(group_ref_c)
    
  

  if label_  is not None:
    label_c = lib.cxios_is_defined_scalargroup_label(scalargroup_hdl)
    label_._c_value = ctypes.c_bool(label_c)
    
  

  if long_name_  is not None:
    long_name_c = lib.cxios_is_defined_scalargroup_long_name(scalargroup_hdl)
    long_name_._c_value = ctypes.c_bool(long_name_c)
    
  

  if mask_  is not None:
    mask_c = lib.cxios_is_defined_scalargroup_mask(scalargroup_hdl)
    mask_._c_value = ctypes.c_bool(mask_c)
    
  

  if n_  is not None:
    n_c = lib.cxios_is_defined_scalargroup_n(scalargroup_hdl)
    n_._c_value = ctypes.c_bool(n_c)
    
  

  if name_  is not None:
    name_c = lib.cxios_is_defined_scalargroup_name(scalargroup_hdl)
    name_._c_value = ctypes.c_bool(name_c)
    
  

  if positive_  is not None:
    positive_c = lib.cxios_is_defined_scalargroup_positive(scalargroup_hdl)
    positive_._c_value = ctypes.c_bool(positive_c)
    
  

  if prec_  is not None:
    prec_c = lib.cxios_is_defined_scalargroup_prec(scalargroup_hdl)
    prec_._c_value = ctypes.c_bool(prec_c)
    
  

  if scalar_ref_  is not None:
    scalar_ref_c = lib.cxios_is_defined_scalargroup_scalar_ref(scalargroup_hdl)
    scalar_ref_._c_value = ctypes.c_bool(scalar_ref_c)
    
  

  if standard_name_  is not None:
    standard_name_c = lib.cxios_is_defined_scalargroup_standard_name(scalargroup_hdl)
    standard_name_._c_value = ctypes.c_bool(standard_name_c)
    
  

  if unit_  is not None:
    unit_c = lib.cxios_is_defined_scalargroup_unit(scalargroup_hdl)
    unit_._c_value = ctypes.c_bool(unit_c)
    
  

  if value_  is not None:
    value_c = lib.cxios_is_defined_scalargroup_value(scalargroup_hdl)
    value_._c_value = ctypes.c_bool(value_c)
    
  
  return 



