# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.iaxis import  get_axis_handle
from xios.oaxis_attr import Axis

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_axis_axis_ref.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_axis_axis_ref.restypes = None

lib.cxios_set_axis_axis_ref.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_axis_axis_ref.restypes = None

lib.cxios_is_defined_axis_axis_ref.argtypes = [Axis]
lib.cxios_is_defined_axis_axis_ref.restypes = ctypes.c_bool

lib.cxios_get_axis_axis_type.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_axis_axis_type.restypes = None

lib.cxios_set_axis_axis_type.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_axis_axis_type.restypes = None

lib.cxios_is_defined_axis_axis_type.argtypes = [Axis]
lib.cxios_is_defined_axis_axis_type.restypes = ctypes.c_bool

lib.cxios_get_axis_begin.argtypes = [Axis, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_axis_begin.restypes = None

lib.cxios_set_axis_begin.argtypes = [Axis, ctypes.c_int]
lib.cxios_set_axis_begin.restypes = None

lib.cxios_is_defined_axis_begin.argtypes = [Axis]
lib.cxios_is_defined_axis_begin.restypes = ctypes.c_bool

lib.cxios_get_axis_bounds.argtypes = [Axis, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_axis_bounds.restypes = None

lib.cxios_set_axis_bounds.argtypes = [Axis, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_axis_bounds.restypes = None

lib.cxios_is_defined_axis_bounds.argtypes = [Axis]
lib.cxios_is_defined_axis_bounds.restypes = ctypes.c_bool

lib.cxios_get_axis_bounds_name.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_axis_bounds_name.restypes = None

lib.cxios_set_axis_bounds_name.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_axis_bounds_name.restypes = None

lib.cxios_is_defined_axis_bounds_name.argtypes = [Axis]
lib.cxios_is_defined_axis_bounds_name.restypes = ctypes.c_bool

lib.cxios_get_axis_chunking_weight.argtypes = [Axis, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_axis_chunking_weight.restypes = None

lib.cxios_set_axis_chunking_weight.argtypes = [Axis, ctypes.c_double]
lib.cxios_set_axis_chunking_weight.restypes = None

lib.cxios_is_defined_axis_chunking_weight.argtypes = [Axis]
lib.cxios_is_defined_axis_chunking_weight.restypes = ctypes.c_bool

lib.cxios_get_axis_comment.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_axis_comment.restypes = None

lib.cxios_set_axis_comment.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_axis_comment.restypes = None

lib.cxios_is_defined_axis_comment.argtypes = [Axis]
lib.cxios_is_defined_axis_comment.restypes = ctypes.c_bool

lib.cxios_get_axis_convert_from_factor.argtypes = [Axis, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_axis_convert_from_factor.restypes = None

lib.cxios_set_axis_convert_from_factor.argtypes = [Axis, ctypes.c_double]
lib.cxios_set_axis_convert_from_factor.restypes = None

lib.cxios_is_defined_axis_convert_from_factor.argtypes = [Axis]
lib.cxios_is_defined_axis_convert_from_factor.restypes = ctypes.c_bool

lib.cxios_get_axis_data_begin.argtypes = [Axis, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_axis_data_begin.restypes = None

lib.cxios_set_axis_data_begin.argtypes = [Axis, ctypes.c_int]
lib.cxios_set_axis_data_begin.restypes = None

lib.cxios_is_defined_axis_data_begin.argtypes = [Axis]
lib.cxios_is_defined_axis_data_begin.restypes = ctypes.c_bool

lib.cxios_get_axis_data_index.argtypes = [Axis, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_axis_data_index.restypes = None

lib.cxios_set_axis_data_index.argtypes = [Axis, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_axis_data_index.restypes = None

lib.cxios_is_defined_axis_data_index.argtypes = [Axis]
lib.cxios_is_defined_axis_data_index.restypes = ctypes.c_bool

lib.cxios_get_axis_data_n.argtypes = [Axis, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_axis_data_n.restypes = None

lib.cxios_set_axis_data_n.argtypes = [Axis, ctypes.c_int]
lib.cxios_set_axis_data_n.restypes = None

lib.cxios_is_defined_axis_data_n.argtypes = [Axis]
lib.cxios_is_defined_axis_data_n.restypes = ctypes.c_bool

lib.cxios_get_axis_dim_name.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_axis_dim_name.restypes = None

lib.cxios_set_axis_dim_name.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_axis_dim_name.restypes = None

lib.cxios_is_defined_axis_dim_name.argtypes = [Axis]
lib.cxios_is_defined_axis_dim_name.restypes = ctypes.c_bool

lib.cxios_get_axis_formula.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_axis_formula.restypes = None

lib.cxios_set_axis_formula.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_axis_formula.restypes = None

lib.cxios_is_defined_axis_formula.argtypes = [Axis]
lib.cxios_is_defined_axis_formula.restypes = ctypes.c_bool

lib.cxios_get_axis_formula_bounds.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_axis_formula_bounds.restypes = None

lib.cxios_set_axis_formula_bounds.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_axis_formula_bounds.restypes = None

lib.cxios_is_defined_axis_formula_bounds.argtypes = [Axis]
lib.cxios_is_defined_axis_formula_bounds.restypes = ctypes.c_bool

lib.cxios_get_axis_formula_term.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_axis_formula_term.restypes = None

lib.cxios_set_axis_formula_term.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_axis_formula_term.restypes = None

lib.cxios_is_defined_axis_formula_term.argtypes = [Axis]
lib.cxios_is_defined_axis_formula_term.restypes = ctypes.c_bool

lib.cxios_get_axis_formula_term_bounds.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_axis_formula_term_bounds.restypes = None

lib.cxios_set_axis_formula_term_bounds.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_axis_formula_term_bounds.restypes = None

lib.cxios_is_defined_axis_formula_term_bounds.argtypes = [Axis]
lib.cxios_is_defined_axis_formula_term_bounds.restypes = ctypes.c_bool

lib.cxios_get_axis_index.argtypes = [Axis, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_axis_index.restypes = None

lib.cxios_set_axis_index.argtypes = [Axis, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_axis_index.restypes = None

lib.cxios_is_defined_axis_index.argtypes = [Axis]
lib.cxios_is_defined_axis_index.restypes = ctypes.c_bool

lib.cxios_get_axis_label.argtypes = [Axis, ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_axis_label.restypes = None

lib.cxios_set_axis_label.argtypes = [Axis, ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_axis_label.restypes = None

lib.cxios_is_defined_axis_label.argtypes = [Axis]
lib.cxios_is_defined_axis_label.restypes = ctypes.c_bool

lib.cxios_get_axis_long_name.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_axis_long_name.restypes = None

lib.cxios_set_axis_long_name.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_axis_long_name.restypes = None

lib.cxios_is_defined_axis_long_name.argtypes = [Axis]
lib.cxios_is_defined_axis_long_name.restypes = ctypes.c_bool

lib.cxios_get_axis_mask.argtypes = [Axis, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_axis_mask.restypes = None

lib.cxios_set_axis_mask.argtypes = [Axis, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_axis_mask.restypes = None

lib.cxios_is_defined_axis_mask.argtypes = [Axis]
lib.cxios_is_defined_axis_mask.restypes = ctypes.c_bool

lib.cxios_get_axis_n.argtypes = [Axis, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_axis_n.restypes = None

lib.cxios_set_axis_n.argtypes = [Axis, ctypes.c_int]
lib.cxios_set_axis_n.restypes = None

lib.cxios_is_defined_axis_n.argtypes = [Axis]
lib.cxios_is_defined_axis_n.restypes = ctypes.c_bool

lib.cxios_get_axis_n_distributed_partition.argtypes = [Axis, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_axis_n_distributed_partition.restypes = None

lib.cxios_set_axis_n_distributed_partition.argtypes = [Axis, ctypes.c_int]
lib.cxios_set_axis_n_distributed_partition.restypes = None

lib.cxios_is_defined_axis_n_distributed_partition.argtypes = [Axis]
lib.cxios_is_defined_axis_n_distributed_partition.restypes = ctypes.c_bool

lib.cxios_get_axis_n_glo.argtypes = [Axis, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_axis_n_glo.restypes = None

lib.cxios_set_axis_n_glo.argtypes = [Axis, ctypes.c_int]
lib.cxios_set_axis_n_glo.restypes = None

lib.cxios_is_defined_axis_n_glo.argtypes = [Axis]
lib.cxios_is_defined_axis_n_glo.restypes = ctypes.c_bool

lib.cxios_get_axis_name.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_axis_name.restypes = None

lib.cxios_set_axis_name.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_axis_name.restypes = None

lib.cxios_is_defined_axis_name.argtypes = [Axis]
lib.cxios_is_defined_axis_name.restypes = ctypes.c_bool

lib.cxios_get_axis_positive.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_axis_positive.restypes = None

lib.cxios_set_axis_positive.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_axis_positive.restypes = None

lib.cxios_is_defined_axis_positive.argtypes = [Axis]
lib.cxios_is_defined_axis_positive.restypes = ctypes.c_bool

lib.cxios_get_axis_prec.argtypes = [Axis, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_axis_prec.restypes = None

lib.cxios_set_axis_prec.argtypes = [Axis, ctypes.c_int]
lib.cxios_set_axis_prec.restypes = None

lib.cxios_is_defined_axis_prec.argtypes = [Axis]
lib.cxios_is_defined_axis_prec.restypes = ctypes.c_bool

lib.cxios_get_axis_standard_name.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_axis_standard_name.restypes = None

lib.cxios_set_axis_standard_name.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_axis_standard_name.restypes = None

lib.cxios_is_defined_axis_standard_name.argtypes = [Axis]
lib.cxios_is_defined_axis_standard_name.restypes = ctypes.c_bool

lib.cxios_get_axis_unit.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_axis_unit.restypes = None

lib.cxios_set_axis_unit.argtypes = [Axis, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_axis_unit.restypes = None

lib.cxios_is_defined_axis_unit.argtypes = [Axis]
lib.cxios_is_defined_axis_unit.restypes = ctypes.c_bool

lib.cxios_get_axis_value.argtypes = [Axis, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_axis_value.restypes = None

lib.cxios_set_axis_value.argtypes = [Axis, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_axis_value.restypes = None

lib.cxios_is_defined_axis_value.argtypes = [Axis]
lib.cxios_is_defined_axis_value.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_axis_attr(axis_id : Union[String, str], axis_ref : Optional[Union[str, String]] = None, axis_type : Optional[Union[str, String]] = None,   
begin : Optional[Union[Int, int]] = None, bounds : Optional[Union[np.ndarray, NpArray]] = None,   
bounds_name : Optional[Union[str, String]] = None, chunking_weight : Optional[Union[Double, float]] = None,   
comment : Optional[Union[str, String]] = None, convert_from_factor : Optional[Union[Double, float]] = None,   
data_begin : Optional[Union[Int, int]] = None, data_index : Optional[Union[np.ndarray, NpArrayInt]] = None,   
data_n : Optional[Union[Int, int]] = None, dim_name : Optional[Union[str, String]] = None,   
formula : Optional[Union[str, String]] = None, formula_bounds : Optional[Union[str, String]] = None,   
formula_term : Optional[Union[str, String]] = None, formula_term_bounds : Optional[Union[str, String]] = None,   
index : Optional[Union[np.ndarray, NpArrayInt]] = None, label : Optional[String] = None, long_name : Optional[Union[str, String]] = None,   
mask : Optional[Union[np.ndarray, NpArrayBool]] = None, n : Optional[Union[Int, int]] = None,   
n_distributed_partition : Optional[Union[Int, int]] = None, n_glo : Optional[Union[Int, int]] = None,   
name : Optional[Union[str, String]] = None, positive : Optional[Union[str, String]] = None,   
prec : Optional[Union[Int, int]] = None, standard_name : Optional[Union[str, String]] = None,   
unit : Optional[Union[str, String]] = None, value : Optional[Union[np.ndarray, NpArray]] = None):

  
  axis_hdl = Axis()
  

  get_axis_handle(axis_id, axis_hdl)
  set_axis_attr_hdl_(axis_hdl, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor,  
   data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds,  
   index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name,  
   unit, value)
  return 



@typecheck
def set_axis_attr_hdl(axis_hdl : Axis, axis_ref : Optional[Union[str, String]] = None, axis_type : Optional[Union[str, String]] = None,   
begin : Optional[Union[Int, int]] = None, bounds : Optional[Union[np.ndarray, NpArray]] = None,   
bounds_name : Optional[Union[str, String]] = None, chunking_weight : Optional[Union[Double, float]] = None,   
comment : Optional[Union[str, String]] = None, convert_from_factor : Optional[Union[Double, float]] = None,   
data_begin : Optional[Union[Int, int]] = None, data_index : Optional[Union[np.ndarray, NpArrayInt]] = None,   
data_n : Optional[Union[Int, int]] = None, dim_name : Optional[Union[str, String]] = None,   
formula : Optional[Union[str, String]] = None, formula_bounds : Optional[Union[str, String]] = None,   
formula_term : Optional[Union[str, String]] = None, formula_term_bounds : Optional[Union[str, String]] = None,   
index : Optional[Union[np.ndarray, NpArrayInt]] = None, label : Optional[String] = None, long_name : Optional[Union[str, String]] = None,   
mask : Optional[Union[np.ndarray, NpArrayBool]] = None, n : Optional[Union[Int, int]] = None,   
n_distributed_partition : Optional[Union[Int, int]] = None, n_glo : Optional[Union[Int, int]] = None,   
name : Optional[Union[str, String]] = None, positive : Optional[Union[str, String]] = None,   
prec : Optional[Union[Int, int]] = None, standard_name : Optional[Union[str, String]] = None,   
unit : Optional[Union[str, String]] = None, value : Optional[Union[np.ndarray, NpArray]] = None):

  
  set_axis_attr_hdl_(axis_hdl, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor,  
   data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds,  
   index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name,  
   unit, value)
  return 



@typecheck
def set_axis_attr_hdl_(axis_hdl : Axis, axis_ref_ : Optional[Union[str, String]] = None, axis_type_ : Optional[Union[str, String]] = None,   
begin_ : Optional[Union[Int, int]] = None, bounds_ : Optional[Union[np.ndarray, NpArray]] = None,   
bounds_name_ : Optional[Union[str, String]] = None, chunking_weight_ : Optional[Union[Double, float]] = None,   
comment_ : Optional[Union[str, String]] = None, convert_from_factor_ : Optional[Union[Double, float]] = None,   
data_begin_ : Optional[Union[Int, int]] = None, data_index_ : Optional[Union[np.ndarray, NpArrayInt]] = None,   
data_n_ : Optional[Union[Int, int]] = None, dim_name_ : Optional[Union[str, String]] = None,   
formula_ : Optional[Union[str, String]] = None, formula_bounds_ : Optional[Union[str, String]] = None,   
formula_term_ : Optional[Union[str, String]] = None, formula_term_bounds_ : Optional[Union[str, String]] = None,   
index_ : Optional[Union[np.ndarray, NpArrayInt]] = None, label_ : Optional[String] = None,   
long_name_ : Optional[Union[str, String]] = None, mask_ : Optional[Union[np.ndarray, NpArrayBool]] = None,   
n_ : Optional[Union[Int, int]] = None, n_distributed_partition_ : Optional[Union[Int, int]] = None,   
n_glo_ : Optional[Union[Int, int]] = None, name_ : Optional[Union[str, String]] = None, positive_ : Optional[Union[str, String]] = None,   
prec_ : Optional[Union[Int, int]] = None, standard_name_ : Optional[Union[str, String]] = None,   
unit_ : Optional[Union[str, String]] = None, value_ : Optional[Union[np.ndarray, NpArray]] = None):

  
  

  if axis_ref_ is not None:
  
    axis_ref_= String(axis_ref_)
    axis_ref_c = axis_ref_._c_value
    len_axis_ref_c = len(ctypes.string_at(axis_ref_c))
    lib.cxios_set_axis_axis_ref(axis_hdl, axis_ref_c, len_axis_ref_c)
    
  

  if axis_type_ is not None:
  
    axis_type_= String(axis_type_)
    axis_type_c = axis_type_._c_value
    len_axis_type_c = len(ctypes.string_at(axis_type_c))
    lib.cxios_set_axis_axis_type(axis_hdl, axis_type_c, len_axis_type_c)
    
  

  if begin_ is not None: 
  
    begin_ = Int(begin_)
    begin_c = begin_._c_value
    lib.cxios_set_axis_begin(axis_hdl, begin_c)
    
  

  if bounds_ is not None:
    bounds_ = NpArray(bounds_)
    bounds_c = bounds_._c_value
    if len(bounds_.shape) != 2 : 
      raise ValueError('The array dimension should be 2')
    else : 
      shape = ctypes.c_int * len(bounds_.shape)
      shape_c = shape(*bounds_.shape)
      lib.cxios_set_axis_bounds(axis_hdl, bounds_c, shape_c)
  

  if bounds_name_ is not None:
  
    bounds_name_= String(bounds_name_)
    bounds_name_c = bounds_name_._c_value
    len_bounds_name_c = len(ctypes.string_at(bounds_name_c))
    lib.cxios_set_axis_bounds_name(axis_hdl, bounds_name_c, len_bounds_name_c)
    
  

  if chunking_weight_ is not None: 
  
    chunking_weight_ = Double(chunking_weight_)
    chunking_weight_c = chunking_weight_._c_value
    lib.cxios_set_axis_chunking_weight(axis_hdl, chunking_weight_c)
    
  

  if comment_ is not None:
  
    comment_= String(comment_)
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_set_axis_comment(axis_hdl, comment_c, len_comment_c)
    
  

  if convert_from_factor_ is not None: 
  
    convert_from_factor_ = Double(convert_from_factor_)
    convert_from_factor_c = convert_from_factor_._c_value
    lib.cxios_set_axis_convert_from_factor(axis_hdl, convert_from_factor_c)
    
  

  if data_begin_ is not None: 
  
    data_begin_ = Int(data_begin_)
    data_begin_c = data_begin_._c_value
    lib.cxios_set_axis_data_begin(axis_hdl, data_begin_c)
    
  

  if data_index_ is not None:
    data_index_ = NpArrayInt(data_index_)
    data_index_c = data_index_._c_value
    if len(data_index_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(data_index_.shape)
      shape_c = shape(*data_index_.shape)
      lib.cxios_set_axis_data_index(axis_hdl, data_index_c, shape_c)
  

  if data_n_ is not None: 
  
    data_n_ = Int(data_n_)
    data_n_c = data_n_._c_value
    lib.cxios_set_axis_data_n(axis_hdl, data_n_c)
    
  

  if dim_name_ is not None:
  
    dim_name_= String(dim_name_)
    dim_name_c = dim_name_._c_value
    len_dim_name_c = len(ctypes.string_at(dim_name_c))
    lib.cxios_set_axis_dim_name(axis_hdl, dim_name_c, len_dim_name_c)
    
  

  if formula_ is not None:
  
    formula_= String(formula_)
    formula_c = formula_._c_value
    len_formula_c = len(ctypes.string_at(formula_c))
    lib.cxios_set_axis_formula(axis_hdl, formula_c, len_formula_c)
    
  

  if formula_bounds_ is not None:
  
    formula_bounds_= String(formula_bounds_)
    formula_bounds_c = formula_bounds_._c_value
    len_formula_bounds_c = len(ctypes.string_at(formula_bounds_c))
    lib.cxios_set_axis_formula_bounds(axis_hdl, formula_bounds_c, len_formula_bounds_c)
    
  

  if formula_term_ is not None:
  
    formula_term_= String(formula_term_)
    formula_term_c = formula_term_._c_value
    len_formula_term_c = len(ctypes.string_at(formula_term_c))
    lib.cxios_set_axis_formula_term(axis_hdl, formula_term_c, len_formula_term_c)
    
  

  if formula_term_bounds_ is not None:
  
    formula_term_bounds_= String(formula_term_bounds_)
    formula_term_bounds_c = formula_term_bounds_._c_value
    len_formula_term_bounds_c = len(ctypes.string_at(formula_term_bounds_c))
    lib.cxios_set_axis_formula_term_bounds(axis_hdl, formula_term_bounds_c, len_formula_term_bounds_c)
    
  

  if index_ is not None:
    index_ = NpArrayInt(index_)
    index_c = index_._c_value
    if len(index_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(index_.shape)
      shape_c = shape(*index_.shape)
      lib.cxios_set_axis_index(axis_hdl, index_c, shape_c)
  

  #CArray<StdString, T> !!
  if label_ is not None:
  
    label_c = label_._c_value
    lib.cxios_set_axis_label(axis_hdl, label_c)
    
  

  if long_name_ is not None:
  
    long_name_= String(long_name_)
    long_name_c = long_name_._c_value
    len_long_name_c = len(ctypes.string_at(long_name_c))
    lib.cxios_set_axis_long_name(axis_hdl, long_name_c, len_long_name_c)
    
  

  if mask_ is not None:
    mask_ = NpArrayBool(mask_)
    mask_c = mask_._c_value
    if len(mask_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(mask_.shape)
      shape_c = shape(*mask_.shape)
      lib.cxios_set_axis_mask(axis_hdl, mask_c, shape_c)
  

  if n_ is not None: 
  
    n_ = Int(n_)
    n_c = n_._c_value
    lib.cxios_set_axis_n(axis_hdl, n_c)
    
  

  if n_distributed_partition_ is not None: 
  
    n_distributed_partition_ = Int(n_distributed_partition_)
    n_distributed_partition_c = n_distributed_partition_._c_value
    lib.cxios_set_axis_n_distributed_partition(axis_hdl, n_distributed_partition_c)
    
  

  if n_glo_ is not None: 
  
    n_glo_ = Int(n_glo_)
    n_glo_c = n_glo_._c_value
    lib.cxios_set_axis_n_glo(axis_hdl, n_glo_c)
    
  

  if name_ is not None:
  
    name_= String(name_)
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_set_axis_name(axis_hdl, name_c, len_name_c)
    
  

  if positive_ is not None:
  
    positive_= String(positive_)
    positive_c = positive_._c_value
    len_positive_c = len(ctypes.string_at(positive_c))
    lib.cxios_set_axis_positive(axis_hdl, positive_c, len_positive_c)
    
  

  if prec_ is not None: 
  
    prec_ = Int(prec_)
    prec_c = prec_._c_value
    lib.cxios_set_axis_prec(axis_hdl, prec_c)
    
  

  if standard_name_ is not None:
  
    standard_name_= String(standard_name_)
    standard_name_c = standard_name_._c_value
    len_standard_name_c = len(ctypes.string_at(standard_name_c))
    lib.cxios_set_axis_standard_name(axis_hdl, standard_name_c, len_standard_name_c)
    
  

  if unit_ is not None:
  
    unit_= String(unit_)
    unit_c = unit_._c_value
    len_unit_c = len(ctypes.string_at(unit_c))
    lib.cxios_set_axis_unit(axis_hdl, unit_c, len_unit_c)
    
  

  if value_ is not None:
    value_ = NpArray(value_)
    value_c = value_._c_value
    if len(value_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(value_.shape)
      shape_c = shape(*value_.shape)
      lib.cxios_set_axis_value(axis_hdl, value_c, shape_c)
  
  return 



@typecheck
def get_axis_attr(axis_id : Union[String, str], axis_ref : Optional[String] = None, axis_type : Optional[String] = None,   
begin : Optional[Int] = None, bounds : Optional[NpArray] = None, bounds_name : Optional[String] = None,   
chunking_weight : Optional[Double] = None, comment : Optional[String] = None, convert_from_factor : Optional[Double] = None,   
data_begin : Optional[Int] = None, data_index : Optional[NpArrayInt] = None, data_n : Optional[Int] = None,   
dim_name : Optional[String] = None, formula : Optional[String] = None, formula_bounds : Optional[String] = None,   
formula_term : Optional[String] = None, formula_term_bounds : Optional[String] = None, index : Optional[NpArrayInt] = None,   
label : Optional[String] = None, long_name : Optional[String] = None, mask : Optional[NpArrayBool] = None,   
n : Optional[Int] = None, n_distributed_partition : Optional[Int] = None, n_glo : Optional[Int] = None,   
name : Optional[String] = None, positive : Optional[String] = None, prec : Optional[Int] = None,   
standard_name : Optional[String] = None, unit : Optional[String] = None, value : Optional[NpArray] = None):

  
  axis_hdl = Axis()
  

  get_axis_handle(axis_id, axis_hdl)
  get_axis_attr_hdl_(axis_hdl, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor,  
   data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds,  
   index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name,  
   unit, value)
  return 



@typecheck
def get_axis_attr_hdl(axis_hdl : Axis, axis_ref : Optional[String] = None, axis_type : Optional[String] = None,   
begin : Optional[Int] = None, bounds : Optional[NpArray] = None, bounds_name : Optional[String] = None,   
chunking_weight : Optional[Double] = None, comment : Optional[String] = None, convert_from_factor : Optional[Double] = None,   
data_begin : Optional[Int] = None, data_index : Optional[NpArrayInt] = None, data_n : Optional[Int] = None,   
dim_name : Optional[String] = None, formula : Optional[String] = None, formula_bounds : Optional[String] = None,   
formula_term : Optional[String] = None, formula_term_bounds : Optional[String] = None, index : Optional[NpArrayInt] = None,   
label : Optional[String] = None, long_name : Optional[String] = None, mask : Optional[NpArrayBool] = None,   
n : Optional[Int] = None, n_distributed_partition : Optional[Int] = None, n_glo : Optional[Int] = None,   
name : Optional[String] = None, positive : Optional[String] = None, prec : Optional[Int] = None,   
standard_name : Optional[String] = None, unit : Optional[String] = None, value : Optional[NpArray] = None):

  
  get_axis_attr_hdl_(axis_hdl, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor,  
   data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds,  
   index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name,  
   unit, value)
  return 



@typecheck
def get_axis_attr_hdl_(axis_hdl : Axis, axis_ref_ : Optional[String] = None, axis_type_ : Optional[String] = None,   
begin_ : Optional[Int] = None, bounds_ : Optional[NpArray] = None, bounds_name_ : Optional[String] = None,   
chunking_weight_ : Optional[Double] = None, comment_ : Optional[String] = None, convert_from_factor_ : Optional[Double] = None,   
data_begin_ : Optional[Int] = None, data_index_ : Optional[NpArrayInt] = None, data_n_ : Optional[Int] = None,   
dim_name_ : Optional[String] = None, formula_ : Optional[String] = None, formula_bounds_ : Optional[String] = None,   
formula_term_ : Optional[String] = None, formula_term_bounds_ : Optional[String] = None, index_ : Optional[NpArrayInt] = None,   
label_ : Optional[String] = None, long_name_ : Optional[String] = None, mask_ : Optional[NpArrayBool] = None,   
n_ : Optional[Int] = None, n_distributed_partition_ : Optional[Int] = None, n_glo_ : Optional[Int] = None,   
name_ : Optional[String] = None, positive_ : Optional[String] = None, prec_ : Optional[Int] = None,   
standard_name_ : Optional[String] = None, unit_ : Optional[String] = None, value_ : Optional[NpArray] = None):

  
  

  if axis_ref_ is not None:
  
    axis_ref_c = axis_ref_._c_value
    len_axis_ref_c = len(ctypes.string_at(axis_ref_c))
    lib.cxios_get_axis_axis_ref(axis_hdl, axis_ref_c, len_axis_ref_c)
    
  

  if axis_type_ is not None:
  
    axis_type_c = axis_type_._c_value
    len_axis_type_c = len(ctypes.string_at(axis_type_c))
    lib.cxios_get_axis_axis_type(axis_hdl, axis_type_c, len_axis_type_c)
    
  

  if begin_ is not None: 
  
    begin_c = begin_._c_value
    lib.cxios_get_axis_begin(axis_hdl, begin_c)
    
  

  if bounds_ is not None:
    if bounds_.shape_is_needed: 
      shape_c = (ctypes.c_int * 2)()
      lib.cxios_axis_bounds_shape(axis_hdl, shape_c)
      bounds_.arr = np.zeros((shape_c[0], shape_c[1]) , order = 'F')
      lib.cxios_get_axis_bounds(axis_hdl, bounds_._c_value, shape_c)
    else: 
      bounds_c = bounds_._c_value
      if len(bounds_.shape) != 2 : 
        raise ValueError('The array dimension should be 2')
      else : 
        shape_c = NpArrayInt(bounds_.shape)._c_value
        lib.cxios_get_axis_bounds(axis_hdl, bounds_c, shape_c)
  

  if bounds_name_ is not None:
  
    bounds_name_c = bounds_name_._c_value
    len_bounds_name_c = len(ctypes.string_at(bounds_name_c))
    lib.cxios_get_axis_bounds_name(axis_hdl, bounds_name_c, len_bounds_name_c)
    
  

  if chunking_weight_ is not None: 
  
    chunking_weight_c = chunking_weight_._c_value
    lib.cxios_get_axis_chunking_weight(axis_hdl, chunking_weight_c)
    
  

  if comment_ is not None:
  
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_get_axis_comment(axis_hdl, comment_c, len_comment_c)
    
  

  if convert_from_factor_ is not None: 
  
    convert_from_factor_c = convert_from_factor_._c_value
    lib.cxios_get_axis_convert_from_factor(axis_hdl, convert_from_factor_c)
    
  

  if data_begin_ is not None: 
  
    data_begin_c = data_begin_._c_value
    lib.cxios_get_axis_data_begin(axis_hdl, data_begin_c)
    
  

  if data_index_ is not None:
    if data_index_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_axis_data_index_shape(axis_hdl, shape_c)
      data_index_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_axis_data_index(axis_hdl, data_index_._c_value, shape_c)
    else: 
      data_index_c = data_index_._c_value
      if len(data_index_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(data_index_.shape)._c_value
        lib.cxios_get_axis_data_index(axis_hdl, data_index_c, shape_c)
  

  if data_n_ is not None: 
  
    data_n_c = data_n_._c_value
    lib.cxios_get_axis_data_n(axis_hdl, data_n_c)
    
  

  if dim_name_ is not None:
  
    dim_name_c = dim_name_._c_value
    len_dim_name_c = len(ctypes.string_at(dim_name_c))
    lib.cxios_get_axis_dim_name(axis_hdl, dim_name_c, len_dim_name_c)
    
  

  if formula_ is not None:
  
    formula_c = formula_._c_value
    len_formula_c = len(ctypes.string_at(formula_c))
    lib.cxios_get_axis_formula(axis_hdl, formula_c, len_formula_c)
    
  

  if formula_bounds_ is not None:
  
    formula_bounds_c = formula_bounds_._c_value
    len_formula_bounds_c = len(ctypes.string_at(formula_bounds_c))
    lib.cxios_get_axis_formula_bounds(axis_hdl, formula_bounds_c, len_formula_bounds_c)
    
  

  if formula_term_ is not None:
  
    formula_term_c = formula_term_._c_value
    len_formula_term_c = len(ctypes.string_at(formula_term_c))
    lib.cxios_get_axis_formula_term(axis_hdl, formula_term_c, len_formula_term_c)
    
  

  if formula_term_bounds_ is not None:
  
    formula_term_bounds_c = formula_term_bounds_._c_value
    len_formula_term_bounds_c = len(ctypes.string_at(formula_term_bounds_c))
    lib.cxios_get_axis_formula_term_bounds(axis_hdl, formula_term_bounds_c, len_formula_term_bounds_c)
    
  

  if index_ is not None:
    if index_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_axis_index_shape(axis_hdl, shape_c)
      index_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_axis_index(axis_hdl, index_._c_value, shape_c)
    else: 
      index_c = index_._c_value
      if len(index_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(index_.shape)._c_value
        lib.cxios_get_axis_index(axis_hdl, index_c, shape_c)
  

  if label_ is not None:
  
    label_c = label_._c_value
    lib.cxios_get_axis_label(axis_hdl, label_c)
    
  

  if long_name_ is not None:
  
    long_name_c = long_name_._c_value
    len_long_name_c = len(ctypes.string_at(long_name_c))
    lib.cxios_get_axis_long_name(axis_hdl, long_name_c, len_long_name_c)
    
  

  if mask_ is not None:
    if mask_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_axis_mask_shape(axis_hdl, shape_c)
      mask_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_axis_mask(axis_hdl, mask_._c_value, shape_c)
    else: 
      mask_c = mask_._c_value
      if len(mask_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(mask_.shape)._c_value
        lib.cxios_get_axis_mask(axis_hdl, mask_c, shape_c)
  

  if n_ is not None: 
  
    n_c = n_._c_value
    lib.cxios_get_axis_n(axis_hdl, n_c)
    
  

  if n_distributed_partition_ is not None: 
  
    n_distributed_partition_c = n_distributed_partition_._c_value
    lib.cxios_get_axis_n_distributed_partition(axis_hdl, n_distributed_partition_c)
    
  

  if n_glo_ is not None: 
  
    n_glo_c = n_glo_._c_value
    lib.cxios_get_axis_n_glo(axis_hdl, n_glo_c)
    
  

  if name_ is not None:
  
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_get_axis_name(axis_hdl, name_c, len_name_c)
    
  

  if positive_ is not None:
  
    positive_c = positive_._c_value
    len_positive_c = len(ctypes.string_at(positive_c))
    lib.cxios_get_axis_positive(axis_hdl, positive_c, len_positive_c)
    
  

  if prec_ is not None: 
  
    prec_c = prec_._c_value
    lib.cxios_get_axis_prec(axis_hdl, prec_c)
    
  

  if standard_name_ is not None:
  
    standard_name_c = standard_name_._c_value
    len_standard_name_c = len(ctypes.string_at(standard_name_c))
    lib.cxios_get_axis_standard_name(axis_hdl, standard_name_c, len_standard_name_c)
    
  

  if unit_ is not None:
  
    unit_c = unit_._c_value
    len_unit_c = len(ctypes.string_at(unit_c))
    lib.cxios_get_axis_unit(axis_hdl, unit_c, len_unit_c)
    
  

  if value_ is not None:
    if value_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_axis_value_shape(axis_hdl, shape_c)
      value_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_axis_value(axis_hdl, value_._c_value, shape_c)
    else: 
      value_c = value_._c_value
      if len(value_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(value_.shape)._c_value
        lib.cxios_get_axis_value(axis_hdl, value_c, shape_c)
  
  return 



@typecheck
def is_defined_axis_attr(axis_id : String, axis_ref : Optional[Bool] = None, axis_type : Optional[Bool] = None, begin : Optional[Bool] = None,   
bounds : Optional[Bool] = None, bounds_name : Optional[Bool] = None, chunking_weight : Optional[Bool] = None,   
comment : Optional[Bool] = None, convert_from_factor : Optional[Bool] = None, data_begin : Optional[Bool] = None,   
data_index : Optional[Bool] = None, data_n : Optional[Bool] = None, dim_name : Optional[Bool] = None,   
formula : Optional[Bool] = None, formula_bounds : Optional[Bool] = None, formula_term : Optional[Bool] = None,   
formula_term_bounds : Optional[Bool] = None, index : Optional[Bool] = None, label : Optional[Bool] = None,   
long_name : Optional[Bool] = None, mask : Optional[Bool] = None, n : Optional[Bool] = None,   
n_distributed_partition : Optional[Bool] = None, n_glo : Optional[Bool] = None, name : Optional[Bool] = None,   
positive : Optional[Bool] = None, prec : Optional[Bool] = None, standard_name : Optional[Bool] = None,   
unit : Optional[Bool] = None, value : Optional[Bool] = None):

  
  axis_hdl = Axis()
  

  get_axis_handle(axis_id, axis_hdl)
  is_defined_axis_attr_hdl_(axis_hdl, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor,  
   data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds,  
   index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name,  
   unit, value)
  return 



@typecheck
def is_defined_axis_attr_hdl(axis_hdl : Axis, axis_ref : Optional[Bool] = None, axis_type : Optional[Bool] = None, begin : Optional[Bool] = None,   
bounds : Optional[Bool] = None, bounds_name : Optional[Bool] = None, chunking_weight : Optional[Bool] = None,   
comment : Optional[Bool] = None, convert_from_factor : Optional[Bool] = None, data_begin : Optional[Bool] = None,   
data_index : Optional[Bool] = None, data_n : Optional[Bool] = None, dim_name : Optional[Bool] = None,   
formula : Optional[Bool] = None, formula_bounds : Optional[Bool] = None, formula_term : Optional[Bool] = None,   
formula_term_bounds : Optional[Bool] = None, index : Optional[Bool] = None, label : Optional[Bool] = None,   
long_name : Optional[Bool] = None, mask : Optional[Bool] = None, n : Optional[Bool] = None,   
n_distributed_partition : Optional[Bool] = None, n_glo : Optional[Bool] = None, name : Optional[Bool] = None,   
positive : Optional[Bool] = None, prec : Optional[Bool] = None, standard_name : Optional[Bool] = None,   
unit : Optional[Bool] = None, value : Optional[Bool] = None):

  
  is_defined_axis_attr_hdl_(axis_hdl, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor,  
   data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds,  
   index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name,  
   unit, value)
  return 



@typecheck
def is_defined_axis_attr_hdl_(axis_hdl : Axis, axis_ref_ : Optional[Bool] = None, axis_type_ : Optional[Bool] = None, begin_ : Optional[Bool] = None,   
bounds_ : Optional[Bool] = None, bounds_name_ : Optional[Bool] = None, chunking_weight_ : Optional[Bool] = None,   
comment_ : Optional[Bool] = None, convert_from_factor_ : Optional[Bool] = None, data_begin_ : Optional[Bool] = None,   
data_index_ : Optional[Bool] = None, data_n_ : Optional[Bool] = None, dim_name_ : Optional[Bool] = None,   
formula_ : Optional[Bool] = None, formula_bounds_ : Optional[Bool] = None, formula_term_ : Optional[Bool] = None,   
formula_term_bounds_ : Optional[Bool] = None, index_ : Optional[Bool] = None, label_ : Optional[Bool] = None,   
long_name_ : Optional[Bool] = None, mask_ : Optional[Bool] = None, n_ : Optional[Bool] = None,   
n_distributed_partition_ : Optional[Bool] = None, n_glo_ : Optional[Bool] = None, name_ : Optional[Bool] = None,   
positive_ : Optional[Bool] = None, prec_ : Optional[Bool] = None, standard_name_ : Optional[Bool] = None,   
unit_ : Optional[Bool] = None, value_ : Optional[Bool] = None):

  
  

  if axis_ref_  is not None:
    axis_ref_c = lib.cxios_is_defined_axis_axis_ref(axis_hdl)
    axis_ref_._c_value = ctypes.c_bool(axis_ref_c)
    
  

  if axis_type_  is not None:
    axis_type_c = lib.cxios_is_defined_axis_axis_type(axis_hdl)
    axis_type_._c_value = ctypes.c_bool(axis_type_c)
    
  

  if begin_  is not None:
    begin_c = lib.cxios_is_defined_axis_begin(axis_hdl)
    begin_._c_value = ctypes.c_bool(begin_c)
    
  

  if bounds_  is not None:
    bounds_c = lib.cxios_is_defined_axis_bounds(axis_hdl)
    bounds_._c_value = ctypes.c_bool(bounds_c)
    
  

  if bounds_name_  is not None:
    bounds_name_c = lib.cxios_is_defined_axis_bounds_name(axis_hdl)
    bounds_name_._c_value = ctypes.c_bool(bounds_name_c)
    
  

  if chunking_weight_  is not None:
    chunking_weight_c = lib.cxios_is_defined_axis_chunking_weight(axis_hdl)
    chunking_weight_._c_value = ctypes.c_bool(chunking_weight_c)
    
  

  if comment_  is not None:
    comment_c = lib.cxios_is_defined_axis_comment(axis_hdl)
    comment_._c_value = ctypes.c_bool(comment_c)
    
  

  if convert_from_factor_  is not None:
    convert_from_factor_c = lib.cxios_is_defined_axis_convert_from_factor(axis_hdl)
    convert_from_factor_._c_value = ctypes.c_bool(convert_from_factor_c)
    
  

  if data_begin_  is not None:
    data_begin_c = lib.cxios_is_defined_axis_data_begin(axis_hdl)
    data_begin_._c_value = ctypes.c_bool(data_begin_c)
    
  

  if data_index_  is not None:
    data_index_c = lib.cxios_is_defined_axis_data_index(axis_hdl)
    data_index_._c_value = ctypes.c_bool(data_index_c)
    
  

  if data_n_  is not None:
    data_n_c = lib.cxios_is_defined_axis_data_n(axis_hdl)
    data_n_._c_value = ctypes.c_bool(data_n_c)
    
  

  if dim_name_  is not None:
    dim_name_c = lib.cxios_is_defined_axis_dim_name(axis_hdl)
    dim_name_._c_value = ctypes.c_bool(dim_name_c)
    
  

  if formula_  is not None:
    formula_c = lib.cxios_is_defined_axis_formula(axis_hdl)
    formula_._c_value = ctypes.c_bool(formula_c)
    
  

  if formula_bounds_  is not None:
    formula_bounds_c = lib.cxios_is_defined_axis_formula_bounds(axis_hdl)
    formula_bounds_._c_value = ctypes.c_bool(formula_bounds_c)
    
  

  if formula_term_  is not None:
    formula_term_c = lib.cxios_is_defined_axis_formula_term(axis_hdl)
    formula_term_._c_value = ctypes.c_bool(formula_term_c)
    
  

  if formula_term_bounds_  is not None:
    formula_term_bounds_c = lib.cxios_is_defined_axis_formula_term_bounds(axis_hdl)
    formula_term_bounds_._c_value = ctypes.c_bool(formula_term_bounds_c)
    
  

  if index_  is not None:
    index_c = lib.cxios_is_defined_axis_index(axis_hdl)
    index_._c_value = ctypes.c_bool(index_c)
    
  

  if label_  is not None:
    label_c = lib.cxios_is_defined_axis_label(axis_hdl)
    label_._c_value = ctypes.c_bool(label_c)
    
  

  if long_name_  is not None:
    long_name_c = lib.cxios_is_defined_axis_long_name(axis_hdl)
    long_name_._c_value = ctypes.c_bool(long_name_c)
    
  

  if mask_  is not None:
    mask_c = lib.cxios_is_defined_axis_mask(axis_hdl)
    mask_._c_value = ctypes.c_bool(mask_c)
    
  

  if n_  is not None:
    n_c = lib.cxios_is_defined_axis_n(axis_hdl)
    n_._c_value = ctypes.c_bool(n_c)
    
  

  if n_distributed_partition_  is not None:
    n_distributed_partition_c = lib.cxios_is_defined_axis_n_distributed_partition(axis_hdl)
    n_distributed_partition_._c_value = ctypes.c_bool(n_distributed_partition_c)
    
  

  if n_glo_  is not None:
    n_glo_c = lib.cxios_is_defined_axis_n_glo(axis_hdl)
    n_glo_._c_value = ctypes.c_bool(n_glo_c)
    
  

  if name_  is not None:
    name_c = lib.cxios_is_defined_axis_name(axis_hdl)
    name_._c_value = ctypes.c_bool(name_c)
    
  

  if positive_  is not None:
    positive_c = lib.cxios_is_defined_axis_positive(axis_hdl)
    positive_._c_value = ctypes.c_bool(positive_c)
    
  

  if prec_  is not None:
    prec_c = lib.cxios_is_defined_axis_prec(axis_hdl)
    prec_._c_value = ctypes.c_bool(prec_c)
    
  

  if standard_name_  is not None:
    standard_name_c = lib.cxios_is_defined_axis_standard_name(axis_hdl)
    standard_name_._c_value = ctypes.c_bool(standard_name_c)
    
  

  if unit_  is not None:
    unit_c = lib.cxios_is_defined_axis_unit(axis_hdl)
    unit_._c_value = ctypes.c_bool(unit_c)
    
  

  if value_  is not None:
    value_c = lib.cxios_is_defined_axis_value(axis_hdl)
    value_._c_value = ctypes.c_bool(value_c)
    
  
  return 



