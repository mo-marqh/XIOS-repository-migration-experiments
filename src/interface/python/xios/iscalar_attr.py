# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.iscalar import  get_scalar_handle
from xios.oscalar_attr import Scalar

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_scalar_axis_type.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalar_axis_type.restypes = None

lib.cxios_set_scalar_axis_type.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalar_axis_type.restypes = None

lib.cxios_is_defined_scalar_axis_type.argtypes = [Scalar]
lib.cxios_is_defined_scalar_axis_type.restypes = ctypes.c_bool

lib.cxios_get_scalar_bounds.argtypes = [Scalar, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_scalar_bounds.restypes = None

lib.cxios_set_scalar_bounds.argtypes = [Scalar, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_scalar_bounds.restypes = None

lib.cxios_is_defined_scalar_bounds.argtypes = [Scalar]
lib.cxios_is_defined_scalar_bounds.restypes = ctypes.c_bool

lib.cxios_get_scalar_bounds_name.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalar_bounds_name.restypes = None

lib.cxios_set_scalar_bounds_name.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalar_bounds_name.restypes = None

lib.cxios_is_defined_scalar_bounds_name.argtypes = [Scalar]
lib.cxios_is_defined_scalar_bounds_name.restypes = ctypes.c_bool

lib.cxios_get_scalar_comment.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalar_comment.restypes = None

lib.cxios_set_scalar_comment.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalar_comment.restypes = None

lib.cxios_is_defined_scalar_comment.argtypes = [Scalar]
lib.cxios_is_defined_scalar_comment.restypes = ctypes.c_bool

lib.cxios_get_scalar_label.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalar_label.restypes = None

lib.cxios_set_scalar_label.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalar_label.restypes = None

lib.cxios_is_defined_scalar_label.argtypes = [Scalar]
lib.cxios_is_defined_scalar_label.restypes = ctypes.c_bool

lib.cxios_get_scalar_long_name.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalar_long_name.restypes = None

lib.cxios_set_scalar_long_name.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalar_long_name.restypes = None

lib.cxios_is_defined_scalar_long_name.argtypes = [Scalar]
lib.cxios_is_defined_scalar_long_name.restypes = ctypes.c_bool

lib.cxios_get_scalar_mask.argtypes = [Scalar, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_scalar_mask.restypes = None

lib.cxios_set_scalar_mask.argtypes = [Scalar, ctypes.c_bool]
lib.cxios_set_scalar_mask.restypes = None

lib.cxios_is_defined_scalar_mask.argtypes = [Scalar]
lib.cxios_is_defined_scalar_mask.restypes = ctypes.c_bool

lib.cxios_get_scalar_n.argtypes = [Scalar, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_scalar_n.restypes = None

lib.cxios_set_scalar_n.argtypes = [Scalar, ctypes.c_int]
lib.cxios_set_scalar_n.restypes = None

lib.cxios_is_defined_scalar_n.argtypes = [Scalar]
lib.cxios_is_defined_scalar_n.restypes = ctypes.c_bool

lib.cxios_get_scalar_name.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalar_name.restypes = None

lib.cxios_set_scalar_name.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalar_name.restypes = None

lib.cxios_is_defined_scalar_name.argtypes = [Scalar]
lib.cxios_is_defined_scalar_name.restypes = ctypes.c_bool

lib.cxios_get_scalar_positive.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalar_positive.restypes = None

lib.cxios_set_scalar_positive.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalar_positive.restypes = None

lib.cxios_is_defined_scalar_positive.argtypes = [Scalar]
lib.cxios_is_defined_scalar_positive.restypes = ctypes.c_bool

lib.cxios_get_scalar_prec.argtypes = [Scalar, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_scalar_prec.restypes = None

lib.cxios_set_scalar_prec.argtypes = [Scalar, ctypes.c_int]
lib.cxios_set_scalar_prec.restypes = None

lib.cxios_is_defined_scalar_prec.argtypes = [Scalar]
lib.cxios_is_defined_scalar_prec.restypes = ctypes.c_bool

lib.cxios_get_scalar_scalar_ref.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalar_scalar_ref.restypes = None

lib.cxios_set_scalar_scalar_ref.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalar_scalar_ref.restypes = None

lib.cxios_is_defined_scalar_scalar_ref.argtypes = [Scalar]
lib.cxios_is_defined_scalar_scalar_ref.restypes = ctypes.c_bool

lib.cxios_get_scalar_standard_name.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalar_standard_name.restypes = None

lib.cxios_set_scalar_standard_name.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalar_standard_name.restypes = None

lib.cxios_is_defined_scalar_standard_name.argtypes = [Scalar]
lib.cxios_is_defined_scalar_standard_name.restypes = ctypes.c_bool

lib.cxios_get_scalar_unit.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_scalar_unit.restypes = None

lib.cxios_set_scalar_unit.argtypes = [Scalar, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_scalar_unit.restypes = None

lib.cxios_is_defined_scalar_unit.argtypes = [Scalar]
lib.cxios_is_defined_scalar_unit.restypes = ctypes.c_bool

lib.cxios_get_scalar_value.argtypes = [Scalar, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_scalar_value.restypes = None

lib.cxios_set_scalar_value.argtypes = [Scalar, ctypes.c_double]
lib.cxios_set_scalar_value.restypes = None

lib.cxios_is_defined_scalar_value.argtypes = [Scalar]
lib.cxios_is_defined_scalar_value.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_scalar_attr(scalar_id : Union[String, str], axis_type : Optional[Union[str, String]] = None, bounds : Optional[Union[np.ndarray, NpArray]] = None,   
bounds_name : Optional[Union[str, String]] = None, comment : Optional[Union[str, String]] = None,   
label : Optional[Union[str, String]] = None, long_name : Optional[Union[str, String]] = None,   
mask : Optional[Union[Bool, bool]] = None, n : Optional[Union[Int, int]] = None, name : Optional[Union[str, String]] = None,   
positive : Optional[Union[str, String]] = None, prec : Optional[Union[Int, int]] = None, scalar_ref : Optional[Union[str, String]] = None,   
standard_name : Optional[Union[str, String]] = None, unit : Optional[Union[str, String]] = None,   
value : Optional[Union[Double, float]] = None):

  
  scalar_hdl = Scalar()
  

  get_scalar_handle(scalar_id, scalar_hdl)
  set_scalar_attr_hdl_(scalar_hdl, axis_type, bounds, bounds_name, comment, label, long_name, mask, n, name, positive,  
   prec, scalar_ref, standard_name, unit, value)
  return 



@typecheck
def set_scalar_attr_hdl(scalar_hdl : Scalar, axis_type : Optional[Union[str, String]] = None, bounds : Optional[Union[np.ndarray, NpArray]] = None,   
bounds_name : Optional[Union[str, String]] = None, comment : Optional[Union[str, String]] = None,   
label : Optional[Union[str, String]] = None, long_name : Optional[Union[str, String]] = None,   
mask : Optional[Union[Bool, bool]] = None, n : Optional[Union[Int, int]] = None, name : Optional[Union[str, String]] = None,   
positive : Optional[Union[str, String]] = None, prec : Optional[Union[Int, int]] = None, scalar_ref : Optional[Union[str, String]] = None,   
standard_name : Optional[Union[str, String]] = None, unit : Optional[Union[str, String]] = None,   
value : Optional[Union[Double, float]] = None):

  
  set_scalar_attr_hdl_(scalar_hdl, axis_type, bounds, bounds_name, comment, label, long_name, mask, n, name, positive,  
   prec, scalar_ref, standard_name, unit, value)
  return 



@typecheck
def set_scalar_attr_hdl_(scalar_hdl : Scalar, axis_type_ : Optional[Union[str, String]] = None, bounds_ : Optional[Union[np.ndarray, NpArray]] = None,   
bounds_name_ : Optional[Union[str, String]] = None, comment_ : Optional[Union[str, String]] = None,   
label_ : Optional[Union[str, String]] = None, long_name_ : Optional[Union[str, String]] = None,   
mask_ : Optional[Union[Bool, bool]] = None, n_ : Optional[Union[Int, int]] = None, name_ : Optional[Union[str, String]] = None,   
positive_ : Optional[Union[str, String]] = None, prec_ : Optional[Union[Int, int]] = None,   
scalar_ref_ : Optional[Union[str, String]] = None, standard_name_ : Optional[Union[str, String]] = None,   
unit_ : Optional[Union[str, String]] = None, value_ : Optional[Union[Double, float]] = None):

  
  

  if axis_type_ is not None:
  
    axis_type_= String(axis_type_)
    axis_type_c = axis_type_._c_value
    len_axis_type_c = len(ctypes.string_at(axis_type_c))
    lib.cxios_set_scalar_axis_type(scalar_hdl, axis_type_c, len_axis_type_c)
    
  

  if bounds_ is not None:
    bounds_ = NpArray(bounds_)
    bounds_c = bounds_._c_value
    if len(bounds_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(bounds_.shape)
      shape_c = shape(*bounds_.shape)
      lib.cxios_set_scalar_bounds(scalar_hdl, bounds_c, shape_c)
  

  if bounds_name_ is not None:
  
    bounds_name_= String(bounds_name_)
    bounds_name_c = bounds_name_._c_value
    len_bounds_name_c = len(ctypes.string_at(bounds_name_c))
    lib.cxios_set_scalar_bounds_name(scalar_hdl, bounds_name_c, len_bounds_name_c)
    
  

  if comment_ is not None:
  
    comment_= String(comment_)
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_set_scalar_comment(scalar_hdl, comment_c, len_comment_c)
    
  

  if label_ is not None:
  
    label_= String(label_)
    label_c = label_._c_value
    len_label_c = len(ctypes.string_at(label_c))
    lib.cxios_set_scalar_label(scalar_hdl, label_c, len_label_c)
    
  

  if long_name_ is not None:
  
    long_name_= String(long_name_)
    long_name_c = long_name_._c_value
    len_long_name_c = len(ctypes.string_at(long_name_c))
    lib.cxios_set_scalar_long_name(scalar_hdl, long_name_c, len_long_name_c)
    
  

  if mask_ is not None: 
  
    mask_ = Bool(mask_)
    mask_c = mask_._c_value
    lib.cxios_set_scalar_mask(scalar_hdl, mask_c)
    
  

  if n_ is not None: 
  
    n_ = Int(n_)
    n_c = n_._c_value
    lib.cxios_set_scalar_n(scalar_hdl, n_c)
    
  

  if name_ is not None:
  
    name_= String(name_)
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_set_scalar_name(scalar_hdl, name_c, len_name_c)
    
  

  if positive_ is not None:
  
    positive_= String(positive_)
    positive_c = positive_._c_value
    len_positive_c = len(ctypes.string_at(positive_c))
    lib.cxios_set_scalar_positive(scalar_hdl, positive_c, len_positive_c)
    
  

  if prec_ is not None: 
  
    prec_ = Int(prec_)
    prec_c = prec_._c_value
    lib.cxios_set_scalar_prec(scalar_hdl, prec_c)
    
  

  if scalar_ref_ is not None:
  
    scalar_ref_= String(scalar_ref_)
    scalar_ref_c = scalar_ref_._c_value
    len_scalar_ref_c = len(ctypes.string_at(scalar_ref_c))
    lib.cxios_set_scalar_scalar_ref(scalar_hdl, scalar_ref_c, len_scalar_ref_c)
    
  

  if standard_name_ is not None:
  
    standard_name_= String(standard_name_)
    standard_name_c = standard_name_._c_value
    len_standard_name_c = len(ctypes.string_at(standard_name_c))
    lib.cxios_set_scalar_standard_name(scalar_hdl, standard_name_c, len_standard_name_c)
    
  

  if unit_ is not None:
  
    unit_= String(unit_)
    unit_c = unit_._c_value
    len_unit_c = len(ctypes.string_at(unit_c))
    lib.cxios_set_scalar_unit(scalar_hdl, unit_c, len_unit_c)
    
  

  if value_ is not None: 
  
    value_ = Double(value_)
    value_c = value_._c_value
    lib.cxios_set_scalar_value(scalar_hdl, value_c)
    
  
  return 



@typecheck
def get_scalar_attr(scalar_id : Union[String, str], axis_type : Optional[String] = None, bounds : Optional[NpArray] = None,   
bounds_name : Optional[String] = None, comment : Optional[String] = None, label : Optional[String] = None,   
long_name : Optional[String] = None, mask : Optional[Bool] = None, n : Optional[Int] = None,   
name : Optional[String] = None, positive : Optional[String] = None, prec : Optional[Int] = None,   
scalar_ref : Optional[String] = None, standard_name : Optional[String] = None, unit : Optional[String] = None,   
value : Optional[Double] = None):

  
  scalar_hdl = Scalar()
  

  get_scalar_handle(scalar_id, scalar_hdl)
  get_scalar_attr_hdl_(scalar_hdl, axis_type, bounds, bounds_name, comment, label, long_name, mask, n, name, positive,  
   prec, scalar_ref, standard_name, unit, value)
  return 



@typecheck
def get_scalar_attr_hdl(scalar_hdl : Scalar, axis_type : Optional[String] = None, bounds : Optional[NpArray] = None,   
bounds_name : Optional[String] = None, comment : Optional[String] = None, label : Optional[String] = None,   
long_name : Optional[String] = None, mask : Optional[Bool] = None, n : Optional[Int] = None,   
name : Optional[String] = None, positive : Optional[String] = None, prec : Optional[Int] = None,   
scalar_ref : Optional[String] = None, standard_name : Optional[String] = None, unit : Optional[String] = None,   
value : Optional[Double] = None):

  
  get_scalar_attr_hdl_(scalar_hdl, axis_type, bounds, bounds_name, comment, label, long_name, mask, n, name, positive,  
   prec, scalar_ref, standard_name, unit, value)
  return 



@typecheck
def get_scalar_attr_hdl_(scalar_hdl : Scalar, axis_type_ : Optional[String] = None, bounds_ : Optional[NpArray] = None,   
bounds_name_ : Optional[String] = None, comment_ : Optional[String] = None, label_ : Optional[String] = None,   
long_name_ : Optional[String] = None, mask_ : Optional[Bool] = None, n_ : Optional[Int] = None,   
name_ : Optional[String] = None, positive_ : Optional[String] = None, prec_ : Optional[Int] = None,   
scalar_ref_ : Optional[String] = None, standard_name_ : Optional[String] = None, unit_ : Optional[String] = None,   
value_ : Optional[Double] = None):

  
  

  if axis_type_ is not None:
  
    axis_type_c = axis_type_._c_value
    len_axis_type_c = len(ctypes.string_at(axis_type_c))
    lib.cxios_get_scalar_axis_type(scalar_hdl, axis_type_c, len_axis_type_c)
    
  

  if bounds_ is not None:
    if bounds_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_scalar_bounds_shape(scalar_hdl, shape_c)
      bounds_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_scalar_bounds(scalar_hdl, bounds_._c_value, shape_c)
    else: 
      bounds_c = bounds_._c_value
      if len(bounds_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(bounds_.shape)._c_value
        lib.cxios_get_scalar_bounds(scalar_hdl, bounds_c, shape_c)
  

  if bounds_name_ is not None:
  
    bounds_name_c = bounds_name_._c_value
    len_bounds_name_c = len(ctypes.string_at(bounds_name_c))
    lib.cxios_get_scalar_bounds_name(scalar_hdl, bounds_name_c, len_bounds_name_c)
    
  

  if comment_ is not None:
  
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_get_scalar_comment(scalar_hdl, comment_c, len_comment_c)
    
  

  if label_ is not None:
  
    label_c = label_._c_value
    len_label_c = len(ctypes.string_at(label_c))
    lib.cxios_get_scalar_label(scalar_hdl, label_c, len_label_c)
    
  

  if long_name_ is not None:
  
    long_name_c = long_name_._c_value
    len_long_name_c = len(ctypes.string_at(long_name_c))
    lib.cxios_get_scalar_long_name(scalar_hdl, long_name_c, len_long_name_c)
    
  

  if mask_ is not None: 
  
    mask_c = mask_._c_value
    lib.cxios_get_scalar_mask(scalar_hdl, mask_c)
    
  

  if n_ is not None: 
  
    n_c = n_._c_value
    lib.cxios_get_scalar_n(scalar_hdl, n_c)
    
  

  if name_ is not None:
  
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_get_scalar_name(scalar_hdl, name_c, len_name_c)
    
  

  if positive_ is not None:
  
    positive_c = positive_._c_value
    len_positive_c = len(ctypes.string_at(positive_c))
    lib.cxios_get_scalar_positive(scalar_hdl, positive_c, len_positive_c)
    
  

  if prec_ is not None: 
  
    prec_c = prec_._c_value
    lib.cxios_get_scalar_prec(scalar_hdl, prec_c)
    
  

  if scalar_ref_ is not None:
  
    scalar_ref_c = scalar_ref_._c_value
    len_scalar_ref_c = len(ctypes.string_at(scalar_ref_c))
    lib.cxios_get_scalar_scalar_ref(scalar_hdl, scalar_ref_c, len_scalar_ref_c)
    
  

  if standard_name_ is not None:
  
    standard_name_c = standard_name_._c_value
    len_standard_name_c = len(ctypes.string_at(standard_name_c))
    lib.cxios_get_scalar_standard_name(scalar_hdl, standard_name_c, len_standard_name_c)
    
  

  if unit_ is not None:
  
    unit_c = unit_._c_value
    len_unit_c = len(ctypes.string_at(unit_c))
    lib.cxios_get_scalar_unit(scalar_hdl, unit_c, len_unit_c)
    
  

  if value_ is not None: 
  
    value_c = value_._c_value
    lib.cxios_get_scalar_value(scalar_hdl, value_c)
    
  
  return 



@typecheck
def is_defined_scalar_attr(scalar_id : String, axis_type : Optional[Bool] = None, bounds : Optional[Bool] = None, bounds_name : Optional[Bool] = None,   
comment : Optional[Bool] = None, label : Optional[Bool] = None, long_name : Optional[Bool] = None,   
mask : Optional[Bool] = None, n : Optional[Bool] = None, name : Optional[Bool] = None, positive : Optional[Bool] = None,   
prec : Optional[Bool] = None, scalar_ref : Optional[Bool] = None, standard_name : Optional[Bool] = None,   
unit : Optional[Bool] = None, value : Optional[Bool] = None):

  
  scalar_hdl = Scalar()
  

  get_scalar_handle(scalar_id, scalar_hdl)
  is_defined_scalar_attr_hdl_(scalar_hdl, axis_type, bounds, bounds_name, comment, label, long_name, mask, n, name, positive,  
   prec, scalar_ref, standard_name, unit, value)
  return 



@typecheck
def is_defined_scalar_attr_hdl(scalar_hdl : Scalar, axis_type : Optional[Bool] = None, bounds : Optional[Bool] = None, bounds_name : Optional[Bool] = None,   
comment : Optional[Bool] = None, label : Optional[Bool] = None, long_name : Optional[Bool] = None,   
mask : Optional[Bool] = None, n : Optional[Bool] = None, name : Optional[Bool] = None, positive : Optional[Bool] = None,   
prec : Optional[Bool] = None, scalar_ref : Optional[Bool] = None, standard_name : Optional[Bool] = None,   
unit : Optional[Bool] = None, value : Optional[Bool] = None):

  
  is_defined_scalar_attr_hdl_(scalar_hdl, axis_type, bounds, bounds_name, comment, label, long_name, mask, n, name, positive,  
   prec, scalar_ref, standard_name, unit, value)
  return 



@typecheck
def is_defined_scalar_attr_hdl_(scalar_hdl : Scalar, axis_type_ : Optional[Bool] = None, bounds_ : Optional[Bool] = None,   
bounds_name_ : Optional[Bool] = None, comment_ : Optional[Bool] = None, label_ : Optional[Bool] = None,   
long_name_ : Optional[Bool] = None, mask_ : Optional[Bool] = None, n_ : Optional[Bool] = None,   
name_ : Optional[Bool] = None, positive_ : Optional[Bool] = None, prec_ : Optional[Bool] = None,   
scalar_ref_ : Optional[Bool] = None, standard_name_ : Optional[Bool] = None, unit_ : Optional[Bool] = None,   
value_ : Optional[Bool] = None):

  
  

  if axis_type_  is not None:
    axis_type_c = lib.cxios_is_defined_scalar_axis_type(scalar_hdl)
    axis_type_._c_value = ctypes.c_bool(axis_type_c)
    
  

  if bounds_  is not None:
    bounds_c = lib.cxios_is_defined_scalar_bounds(scalar_hdl)
    bounds_._c_value = ctypes.c_bool(bounds_c)
    
  

  if bounds_name_  is not None:
    bounds_name_c = lib.cxios_is_defined_scalar_bounds_name(scalar_hdl)
    bounds_name_._c_value = ctypes.c_bool(bounds_name_c)
    
  

  if comment_  is not None:
    comment_c = lib.cxios_is_defined_scalar_comment(scalar_hdl)
    comment_._c_value = ctypes.c_bool(comment_c)
    
  

  if label_  is not None:
    label_c = lib.cxios_is_defined_scalar_label(scalar_hdl)
    label_._c_value = ctypes.c_bool(label_c)
    
  

  if long_name_  is not None:
    long_name_c = lib.cxios_is_defined_scalar_long_name(scalar_hdl)
    long_name_._c_value = ctypes.c_bool(long_name_c)
    
  

  if mask_  is not None:
    mask_c = lib.cxios_is_defined_scalar_mask(scalar_hdl)
    mask_._c_value = ctypes.c_bool(mask_c)
    
  

  if n_  is not None:
    n_c = lib.cxios_is_defined_scalar_n(scalar_hdl)
    n_._c_value = ctypes.c_bool(n_c)
    
  

  if name_  is not None:
    name_c = lib.cxios_is_defined_scalar_name(scalar_hdl)
    name_._c_value = ctypes.c_bool(name_c)
    
  

  if positive_  is not None:
    positive_c = lib.cxios_is_defined_scalar_positive(scalar_hdl)
    positive_._c_value = ctypes.c_bool(positive_c)
    
  

  if prec_  is not None:
    prec_c = lib.cxios_is_defined_scalar_prec(scalar_hdl)
    prec_._c_value = ctypes.c_bool(prec_c)
    
  

  if scalar_ref_  is not None:
    scalar_ref_c = lib.cxios_is_defined_scalar_scalar_ref(scalar_hdl)
    scalar_ref_._c_value = ctypes.c_bool(scalar_ref_c)
    
  

  if standard_name_  is not None:
    standard_name_c = lib.cxios_is_defined_scalar_standard_name(scalar_hdl)
    standard_name_._c_value = ctypes.c_bool(standard_name_c)
    
  

  if unit_  is not None:
    unit_c = lib.cxios_is_defined_scalar_unit(scalar_hdl)
    unit_._c_value = ctypes.c_bool(unit_c)
    
  

  if value_  is not None:
    value_c = lib.cxios_is_defined_scalar_value(scalar_hdl)
    value_._c_value = ctypes.c_bool(value_c)
    
  
  return 



