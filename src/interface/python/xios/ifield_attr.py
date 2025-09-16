# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.ifield import  get_field_handle
from xios.ofield import Field

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_field_add_offset.argtypes = [Field, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_field_add_offset.restypes = None

lib.cxios_set_field_add_offset.argtypes = [Field, ctypes.c_double]
lib.cxios_set_field_add_offset.restypes = None

lib.cxios_is_defined_field_add_offset.argtypes = [Field]
lib.cxios_is_defined_field_add_offset.restypes = ctypes.c_bool

lib.cxios_get_field_axis_ref.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_axis_ref.restypes = None

lib.cxios_set_field_axis_ref.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_axis_ref.restypes = None

lib.cxios_is_defined_field_axis_ref.argtypes = [Field]
lib.cxios_is_defined_field_axis_ref.restypes = ctypes.c_bool

lib.cxios_get_field_build_workflow_graph.argtypes = [Field, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_field_build_workflow_graph.restypes = None

lib.cxios_set_field_build_workflow_graph.argtypes = [Field, ctypes.c_bool]
lib.cxios_set_field_build_workflow_graph.restypes = None

lib.cxios_is_defined_field_build_workflow_graph.argtypes = [Field]
lib.cxios_is_defined_field_build_workflow_graph.restypes = ctypes.c_bool

lib.cxios_get_field_cell_methods.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_cell_methods.restypes = None

lib.cxios_set_field_cell_methods.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_cell_methods.restypes = None

lib.cxios_is_defined_field_cell_methods.argtypes = [Field]
lib.cxios_is_defined_field_cell_methods.restypes = ctypes.c_bool

lib.cxios_get_field_cell_methods_mode.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_cell_methods_mode.restypes = None

lib.cxios_set_field_cell_methods_mode.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_cell_methods_mode.restypes = None

lib.cxios_is_defined_field_cell_methods_mode.argtypes = [Field]
lib.cxios_is_defined_field_cell_methods_mode.restypes = ctypes.c_bool

lib.cxios_get_field_check_if_active.argtypes = [Field, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_field_check_if_active.restypes = None

lib.cxios_set_field_check_if_active.argtypes = [Field, ctypes.c_bool]
lib.cxios_set_field_check_if_active.restypes = None

lib.cxios_is_defined_field_check_if_active.argtypes = [Field]
lib.cxios_is_defined_field_check_if_active.restypes = ctypes.c_bool

lib.cxios_get_field_chunking_blocksize_target.argtypes = [Field, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_field_chunking_blocksize_target.restypes = None

lib.cxios_set_field_chunking_blocksize_target.argtypes = [Field, ctypes.c_double]
lib.cxios_set_field_chunking_blocksize_target.restypes = None

lib.cxios_is_defined_field_chunking_blocksize_target.argtypes = [Field]
lib.cxios_is_defined_field_chunking_blocksize_target.restypes = ctypes.c_bool

lib.cxios_get_field_comment.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_comment.restypes = None

lib.cxios_set_field_comment.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_comment.restypes = None

lib.cxios_is_defined_field_comment.argtypes = [Field]
lib.cxios_is_defined_field_comment.restypes = ctypes.c_bool

lib.cxios_get_field_compression_level.argtypes = [Field, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_field_compression_level.restypes = None

lib.cxios_set_field_compression_level.argtypes = [Field, ctypes.c_int]
lib.cxios_set_field_compression_level.restypes = None

lib.cxios_is_defined_field_compression_level.argtypes = [Field]
lib.cxios_is_defined_field_compression_level.restypes = ctypes.c_bool

lib.cxios_get_field_compression_params.argtypes = [Field, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_field_compression_params.restypes = None

lib.cxios_set_field_compression_params.argtypes = [Field, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_field_compression_params.restypes = None

lib.cxios_is_defined_field_compression_params.argtypes = [Field]
lib.cxios_is_defined_field_compression_params.restypes = ctypes.c_bool

lib.cxios_get_field_compression_type.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_compression_type.restypes = None

lib.cxios_set_field_compression_type.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_compression_type.restypes = None

lib.cxios_is_defined_field_compression_type.argtypes = [Field]
lib.cxios_is_defined_field_compression_type.restypes = ctypes.c_bool

lib.cxios_get_field_conversion_by_netcdf.argtypes = [Field, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_field_conversion_by_netcdf.restypes = None

lib.cxios_set_field_conversion_by_netcdf.argtypes = [Field, ctypes.c_bool]
lib.cxios_set_field_conversion_by_netcdf.restypes = None

lib.cxios_is_defined_field_conversion_by_netcdf.argtypes = [Field]
lib.cxios_is_defined_field_conversion_by_netcdf.restypes = ctypes.c_bool

lib.cxios_get_field_default_value.argtypes = [Field, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_field_default_value.restypes = None

lib.cxios_set_field_default_value.argtypes = [Field, ctypes.c_double]
lib.cxios_set_field_default_value.restypes = None

lib.cxios_is_defined_field_default_value.argtypes = [Field]
lib.cxios_is_defined_field_default_value.restypes = ctypes.c_bool

lib.cxios_get_field_detect_missing_value.argtypes = [Field, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_field_detect_missing_value.restypes = None

lib.cxios_set_field_detect_missing_value.argtypes = [Field, ctypes.c_bool]
lib.cxios_set_field_detect_missing_value.restypes = None

lib.cxios_is_defined_field_detect_missing_value.argtypes = [Field]
lib.cxios_is_defined_field_detect_missing_value.restypes = ctypes.c_bool

lib.cxios_get_field_domain_ref.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_domain_ref.restypes = None

lib.cxios_set_field_domain_ref.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_domain_ref.restypes = None

lib.cxios_is_defined_field_domain_ref.argtypes = [Field]
lib.cxios_is_defined_field_domain_ref.restypes = ctypes.c_bool

lib.cxios_get_field_enabled.argtypes = [Field, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_field_enabled.restypes = None

lib.cxios_set_field_enabled.argtypes = [Field, ctypes.c_bool]
lib.cxios_set_field_enabled.restypes = None

lib.cxios_is_defined_field_enabled.argtypes = [Field]
lib.cxios_is_defined_field_enabled.restypes = ctypes.c_bool

lib.cxios_get_field_expr.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_expr.restypes = None

lib.cxios_set_field_expr.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_expr.restypes = None

lib.cxios_is_defined_field_expr.argtypes = [Field]
lib.cxios_is_defined_field_expr.restypes = ctypes.c_bool

lib.cxios_get_field_field_ref.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_field_ref.restypes = None

lib.cxios_set_field_field_ref.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_field_ref.restypes = None

lib.cxios_is_defined_field_field_ref.argtypes = [Field]
lib.cxios_is_defined_field_field_ref.restypes = ctypes.c_bool

lib.cxios_get_field_freq_offset.argtypes = [Field, ctypes.POINTER(Duration)]
lib.cxios_get_field_freq_offset.restypes = None

lib.cxios_set_field_freq_offset.argtypes = [Field, Duration]
lib.cxios_set_field_freq_offset.restypes = None

lib.cxios_is_defined_field_freq_offset.argtypes = [Field]
lib.cxios_is_defined_field_freq_offset.restypes = ctypes.c_bool

lib.cxios_get_field_freq_op.argtypes = [Field, ctypes.POINTER(Duration)]
lib.cxios_get_field_freq_op.restypes = None

lib.cxios_set_field_freq_op.argtypes = [Field, Duration]
lib.cxios_set_field_freq_op.restypes = None

lib.cxios_is_defined_field_freq_op.argtypes = [Field]
lib.cxios_is_defined_field_freq_op.restypes = ctypes.c_bool

lib.cxios_get_field_grid_path.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_grid_path.restypes = None

lib.cxios_set_field_grid_path.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_grid_path.restypes = None

lib.cxios_is_defined_field_grid_path.argtypes = [Field]
lib.cxios_is_defined_field_grid_path.restypes = ctypes.c_bool

lib.cxios_get_field_grid_ref.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_grid_ref.restypes = None

lib.cxios_set_field_grid_ref.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_grid_ref.restypes = None

lib.cxios_is_defined_field_grid_ref.argtypes = [Field]
lib.cxios_is_defined_field_grid_ref.restypes = ctypes.c_bool

lib.cxios_get_field_indexed_output.argtypes = [Field, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_field_indexed_output.restypes = None

lib.cxios_set_field_indexed_output.argtypes = [Field, ctypes.c_bool]
lib.cxios_set_field_indexed_output.restypes = None

lib.cxios_is_defined_field_indexed_output.argtypes = [Field]
lib.cxios_is_defined_field_indexed_output.restypes = ctypes.c_bool

lib.cxios_get_field_level.argtypes = [Field, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_field_level.restypes = None

lib.cxios_set_field_level.argtypes = [Field, ctypes.c_int]
lib.cxios_set_field_level.restypes = None

lib.cxios_is_defined_field_level.argtypes = [Field]
lib.cxios_is_defined_field_level.restypes = ctypes.c_bool

lib.cxios_get_field_long_name.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_long_name.restypes = None

lib.cxios_set_field_long_name.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_long_name.restypes = None

lib.cxios_is_defined_field_long_name.argtypes = [Field]
lib.cxios_is_defined_field_long_name.restypes = ctypes.c_bool

lib.cxios_get_field_name.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_name.restypes = None

lib.cxios_set_field_name.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_name.restypes = None

lib.cxios_is_defined_field_name.argtypes = [Field]
lib.cxios_is_defined_field_name.restypes = ctypes.c_bool

lib.cxios_get_field_operation.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_operation.restypes = None

lib.cxios_set_field_operation.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_operation.restypes = None

lib.cxios_is_defined_field_operation.argtypes = [Field]
lib.cxios_is_defined_field_operation.restypes = ctypes.c_bool

lib.cxios_get_field_prec.argtypes = [Field, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_field_prec.restypes = None

lib.cxios_set_field_prec.argtypes = [Field, ctypes.c_int]
lib.cxios_set_field_prec.restypes = None

lib.cxios_is_defined_field_prec.argtypes = [Field]
lib.cxios_is_defined_field_prec.restypes = ctypes.c_bool

lib.cxios_get_field_read_access.argtypes = [Field, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_field_read_access.restypes = None

lib.cxios_set_field_read_access.argtypes = [Field, ctypes.c_bool]
lib.cxios_set_field_read_access.restypes = None

lib.cxios_is_defined_field_read_access.argtypes = [Field]
lib.cxios_is_defined_field_read_access.restypes = ctypes.c_bool

lib.cxios_get_field_scalar_ref.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_scalar_ref.restypes = None

lib.cxios_set_field_scalar_ref.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_scalar_ref.restypes = None

lib.cxios_is_defined_field_scalar_ref.argtypes = [Field]
lib.cxios_is_defined_field_scalar_ref.restypes = ctypes.c_bool

lib.cxios_get_field_scale_factor.argtypes = [Field, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_field_scale_factor.restypes = None

lib.cxios_set_field_scale_factor.argtypes = [Field, ctypes.c_double]
lib.cxios_set_field_scale_factor.restypes = None

lib.cxios_is_defined_field_scale_factor.argtypes = [Field]
lib.cxios_is_defined_field_scale_factor.restypes = ctypes.c_bool

lib.cxios_get_field_standard_name.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_standard_name.restypes = None

lib.cxios_set_field_standard_name.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_standard_name.restypes = None

lib.cxios_is_defined_field_standard_name.argtypes = [Field]
lib.cxios_is_defined_field_standard_name.restypes = ctypes.c_bool

lib.cxios_get_field_ts_enabled.argtypes = [Field, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_field_ts_enabled.restypes = None

lib.cxios_set_field_ts_enabled.argtypes = [Field, ctypes.c_bool]
lib.cxios_set_field_ts_enabled.restypes = None

lib.cxios_is_defined_field_ts_enabled.argtypes = [Field]
lib.cxios_is_defined_field_ts_enabled.restypes = ctypes.c_bool

lib.cxios_get_field_ts_split_freq.argtypes = [Field, ctypes.POINTER(Duration)]
lib.cxios_get_field_ts_split_freq.restypes = None

lib.cxios_set_field_ts_split_freq.argtypes = [Field, Duration]
lib.cxios_set_field_ts_split_freq.restypes = None

lib.cxios_is_defined_field_ts_split_freq.argtypes = [Field]
lib.cxios_is_defined_field_ts_split_freq.restypes = ctypes.c_bool

lib.cxios_get_field_unit.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_field_unit.restypes = None

lib.cxios_set_field_unit.argtypes = [Field, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_field_unit.restypes = None

lib.cxios_is_defined_field_unit.argtypes = [Field]
lib.cxios_is_defined_field_unit.restypes = ctypes.c_bool

lib.cxios_get_field_valid_max.argtypes = [Field, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_field_valid_max.restypes = None

lib.cxios_set_field_valid_max.argtypes = [Field, ctypes.c_double]
lib.cxios_set_field_valid_max.restypes = None

lib.cxios_is_defined_field_valid_max.argtypes = [Field]
lib.cxios_is_defined_field_valid_max.restypes = ctypes.c_bool

lib.cxios_get_field_valid_min.argtypes = [Field, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_field_valid_min.restypes = None

lib.cxios_set_field_valid_min.argtypes = [Field, ctypes.c_double]
lib.cxios_set_field_valid_min.restypes = None

lib.cxios_is_defined_field_valid_min.argtypes = [Field]
lib.cxios_is_defined_field_valid_min.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_field_attr(field_id : Union[String, str], add_offset : Optional[Union[Double, float]] = None, axis_ref : Optional[Union[str, String]] = None,   
build_workflow_graph : Optional[Union[Bool, bool]] = None, cell_methods : Optional[Union[str, String]] = None,   
cell_methods_mode : Optional[Union[str, String]] = None, check_if_active : Optional[Union[Bool, bool]] = None,   
chunking_blocksize_target : Optional[Union[Double, float]] = None, comment : Optional[Union[str, String]] = None,   
compression_level : Optional[Union[Int, int]] = None, compression_params : Optional[Union[np.ndarray, NpArray]] = None,   
compression_type : Optional[Union[str, String]] = None, conversion_by_netcdf : Optional[Union[Bool, bool]] = None,   
default_value : Optional[Union[Double, float]] = None, detect_missing_value : Optional[Union[Bool, bool]] = None,   
domain_ref : Optional[Union[str, String]] = None, enabled : Optional[Union[Bool, bool]] = None,   
expr : Optional[Union[str, String]] = None, field_ref : Optional[Union[str, String]] = None,   
freq_offset : Optional[Union[Duration, Duration]] = None, freq_op : Optional[Union[Duration, Duration]] = None,   
grid_path : Optional[Union[str, String]] = None, grid_ref : Optional[Union[str, String]] = None,   
indexed_output : Optional[Union[Bool, bool]] = None, level : Optional[Union[Int, int]] = None,   
long_name : Optional[Union[str, String]] = None, name : Optional[Union[str, String]] = None,   
operation : Optional[Union[str, String]] = None, prec : Optional[Union[Int, int]] = None, read_access : Optional[Union[Bool, bool]] = None,   
scalar_ref : Optional[Union[str, String]] = None, scale_factor : Optional[Union[Double, float]] = None,   
standard_name : Optional[Union[str, String]] = None, ts_enabled : Optional[Union[Bool, bool]] = None,   
ts_split_freq : Optional[Union[Duration, Duration]] = None, unit : Optional[Union[str, String]] = None,   
valid_max : Optional[Union[Double, float]] = None, valid_min : Optional[Union[Double, float]] = None):

  
  field_hdl = Field()
  

  get_field_handle(field_id, field_hdl)
  set_field_attr_hdl_(field_hdl, add_offset, axis_ref, build_workflow_graph, cell_methods, cell_methods_mode, check_if_active,  
   chunking_blocksize_target, comment, compression_level, compression_params, compression_type,  
   conversion_by_netcdf, default_value, detect_missing_value, domain_ref, enabled, expr, field_ref,  
   freq_offset, freq_op, grid_path, grid_ref, indexed_output, level, long_name, name, operation,  
   prec, read_access, scalar_ref, scale_factor, standard_name, ts_enabled, ts_split_freq, unit,  
   valid_max, valid_min)
  return 



@typecheck
def set_field_attr_hdl(field_hdl : Field, add_offset : Optional[Union[Double, float]] = None, axis_ref : Optional[Union[str, String]] = None,   
build_workflow_graph : Optional[Union[Bool, bool]] = None, cell_methods : Optional[Union[str, String]] = None,   
cell_methods_mode : Optional[Union[str, String]] = None, check_if_active : Optional[Union[Bool, bool]] = None,   
chunking_blocksize_target : Optional[Union[Double, float]] = None, comment : Optional[Union[str, String]] = None,   
compression_level : Optional[Union[Int, int]] = None, compression_params : Optional[Union[np.ndarray, NpArray]] = None,   
compression_type : Optional[Union[str, String]] = None, conversion_by_netcdf : Optional[Union[Bool, bool]] = None,   
default_value : Optional[Union[Double, float]] = None, detect_missing_value : Optional[Union[Bool, bool]] = None,   
domain_ref : Optional[Union[str, String]] = None, enabled : Optional[Union[Bool, bool]] = None,   
expr : Optional[Union[str, String]] = None, field_ref : Optional[Union[str, String]] = None,   
freq_offset : Optional[Union[Duration, Duration]] = None, freq_op : Optional[Union[Duration, Duration]] = None,   
grid_path : Optional[Union[str, String]] = None, grid_ref : Optional[Union[str, String]] = None,   
indexed_output : Optional[Union[Bool, bool]] = None, level : Optional[Union[Int, int]] = None,   
long_name : Optional[Union[str, String]] = None, name : Optional[Union[str, String]] = None,   
operation : Optional[Union[str, String]] = None, prec : Optional[Union[Int, int]] = None, read_access : Optional[Union[Bool, bool]] = None,   
scalar_ref : Optional[Union[str, String]] = None, scale_factor : Optional[Union[Double, float]] = None,   
standard_name : Optional[Union[str, String]] = None, ts_enabled : Optional[Union[Bool, bool]] = None,   
ts_split_freq : Optional[Union[Duration, Duration]] = None, unit : Optional[Union[str, String]] = None,   
valid_max : Optional[Union[Double, float]] = None, valid_min : Optional[Union[Double, float]] = None):

  
  set_field_attr_hdl_(field_hdl, add_offset, axis_ref, build_workflow_graph, cell_methods, cell_methods_mode, check_if_active,  
   chunking_blocksize_target, comment, compression_level, compression_params, compression_type,  
   conversion_by_netcdf, default_value, detect_missing_value, domain_ref, enabled, expr, field_ref,  
   freq_offset, freq_op, grid_path, grid_ref, indexed_output, level, long_name, name, operation,  
   prec, read_access, scalar_ref, scale_factor, standard_name, ts_enabled, ts_split_freq, unit,  
   valid_max, valid_min)
  return 



@typecheck
def set_field_attr_hdl_(field_hdl : Field, add_offset_ : Optional[Union[Double, float]] = None, axis_ref_ : Optional[Union[str, String]] = None,   
build_workflow_graph_ : Optional[Union[Bool, bool]] = None, cell_methods_ : Optional[Union[str, String]] = None,   
cell_methods_mode_ : Optional[Union[str, String]] = None, check_if_active_ : Optional[Union[Bool, bool]] = None,   
chunking_blocksize_target_ : Optional[Union[Double, float]] = None, comment_ : Optional[Union[str, String]] = None,   
compression_level_ : Optional[Union[Int, int]] = None, compression_params_ : Optional[Union[np.ndarray, NpArray]] = None,   
compression_type_ : Optional[Union[str, String]] = None, conversion_by_netcdf_ : Optional[Union[Bool, bool]] = None,   
default_value_ : Optional[Union[Double, float]] = None, detect_missing_value_ : Optional[Union[Bool, bool]] = None,   
domain_ref_ : Optional[Union[str, String]] = None, enabled_ : Optional[Union[Bool, bool]] = None,   
expr_ : Optional[Union[str, String]] = None, field_ref_ : Optional[Union[str, String]] = None,   
freq_offset_ : Optional[Union[Duration, Duration]] = None, freq_op_ : Optional[Union[Duration, Duration]] = None,   
grid_path_ : Optional[Union[str, String]] = None, grid_ref_ : Optional[Union[str, String]] = None,   
indexed_output_ : Optional[Union[Bool, bool]] = None, level_ : Optional[Union[Int, int]] = None,   
long_name_ : Optional[Union[str, String]] = None, name_ : Optional[Union[str, String]] = None,   
operation_ : Optional[Union[str, String]] = None, prec_ : Optional[Union[Int, int]] = None,   
read_access_ : Optional[Union[Bool, bool]] = None, scalar_ref_ : Optional[Union[str, String]] = None,   
scale_factor_ : Optional[Union[Double, float]] = None, standard_name_ : Optional[Union[str, String]] = None,   
ts_enabled_ : Optional[Union[Bool, bool]] = None, ts_split_freq_ : Optional[Union[Duration, Duration]] = None,   
unit_ : Optional[Union[str, String]] = None, valid_max_ : Optional[Union[Double, float]] = None,   
valid_min_ : Optional[Union[Double, float]] = None):

  
  

  if add_offset_ is not None: 
  
    add_offset_ = Double(add_offset_)
    add_offset_c = add_offset_._c_value
    lib.cxios_set_field_add_offset(field_hdl, add_offset_c)
    
  

  if axis_ref_ is not None:
  
    axis_ref_= String(axis_ref_)
    axis_ref_c = axis_ref_._c_value
    len_axis_ref_c = len(ctypes.string_at(axis_ref_c))
    lib.cxios_set_field_axis_ref(field_hdl, axis_ref_c, len_axis_ref_c)
    
  

  if build_workflow_graph_ is not None: 
  
    build_workflow_graph_ = Bool(build_workflow_graph_)
    build_workflow_graph_c = build_workflow_graph_._c_value
    lib.cxios_set_field_build_workflow_graph(field_hdl, build_workflow_graph_c)
    
  

  if cell_methods_ is not None:
  
    cell_methods_= String(cell_methods_)
    cell_methods_c = cell_methods_._c_value
    len_cell_methods_c = len(ctypes.string_at(cell_methods_c))
    lib.cxios_set_field_cell_methods(field_hdl, cell_methods_c, len_cell_methods_c)
    
  

  if cell_methods_mode_ is not None:
  
    cell_methods_mode_= String(cell_methods_mode_)
    cell_methods_mode_c = cell_methods_mode_._c_value
    len_cell_methods_mode_c = len(ctypes.string_at(cell_methods_mode_c))
    lib.cxios_set_field_cell_methods_mode(field_hdl, cell_methods_mode_c, len_cell_methods_mode_c)
    
  

  if check_if_active_ is not None: 
  
    check_if_active_ = Bool(check_if_active_)
    check_if_active_c = check_if_active_._c_value
    lib.cxios_set_field_check_if_active(field_hdl, check_if_active_c)
    
  

  if chunking_blocksize_target_ is not None: 
  
    chunking_blocksize_target_ = Double(chunking_blocksize_target_)
    chunking_blocksize_target_c = chunking_blocksize_target_._c_value
    lib.cxios_set_field_chunking_blocksize_target(field_hdl, chunking_blocksize_target_c)
    
  

  if comment_ is not None:
  
    comment_= String(comment_)
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_set_field_comment(field_hdl, comment_c, len_comment_c)
    
  

  if compression_level_ is not None: 
  
    compression_level_ = Int(compression_level_)
    compression_level_c = compression_level_._c_value
    lib.cxios_set_field_compression_level(field_hdl, compression_level_c)
    
  

  if compression_params_ is not None:
    compression_params_ = NpArray(compression_params_)
    compression_params_c = compression_params_._c_value
    if len(compression_params_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(compression_params_.shape)
      shape_c = shape(*compression_params_.shape)
      lib.cxios_set_field_compression_params(field_hdl, compression_params_c, shape_c)
  

  if compression_type_ is not None:
  
    compression_type_= String(compression_type_)
    compression_type_c = compression_type_._c_value
    len_compression_type_c = len(ctypes.string_at(compression_type_c))
    lib.cxios_set_field_compression_type(field_hdl, compression_type_c, len_compression_type_c)
    
  

  if conversion_by_netcdf_ is not None: 
  
    conversion_by_netcdf_ = Bool(conversion_by_netcdf_)
    conversion_by_netcdf_c = conversion_by_netcdf_._c_value
    lib.cxios_set_field_conversion_by_netcdf(field_hdl, conversion_by_netcdf_c)
    
  

  if default_value_ is not None: 
  
    default_value_ = Double(default_value_)
    default_value_c = default_value_._c_value
    lib.cxios_set_field_default_value(field_hdl, default_value_c)
    
  

  if detect_missing_value_ is not None: 
  
    detect_missing_value_ = Bool(detect_missing_value_)
    detect_missing_value_c = detect_missing_value_._c_value
    lib.cxios_set_field_detect_missing_value(field_hdl, detect_missing_value_c)
    
  

  if domain_ref_ is not None:
  
    domain_ref_= String(domain_ref_)
    domain_ref_c = domain_ref_._c_value
    len_domain_ref_c = len(ctypes.string_at(domain_ref_c))
    lib.cxios_set_field_domain_ref(field_hdl, domain_ref_c, len_domain_ref_c)
    
  

  if enabled_ is not None: 
  
    enabled_ = Bool(enabled_)
    enabled_c = enabled_._c_value
    lib.cxios_set_field_enabled(field_hdl, enabled_c)
    
  

  if expr_ is not None:
  
    expr_= String(expr_)
    expr_c = expr_._c_value
    len_expr_c = len(ctypes.string_at(expr_c))
    lib.cxios_set_field_expr(field_hdl, expr_c, len_expr_c)
    
  

  if field_ref_ is not None:
  
    field_ref_= String(field_ref_)
    field_ref_c = field_ref_._c_value
    len_field_ref_c = len(ctypes.string_at(field_ref_c))
    lib.cxios_set_field_field_ref(field_hdl, field_ref_c, len_field_ref_c)
    
  

  if freq_offset_ is not None: 
  
    freq_offset_c = freq_offset_
    lib.cxios_set_field_freq_offset(field_hdl, freq_offset_c)
    
  

  if freq_op_ is not None: 
  
    freq_op_c = freq_op_
    lib.cxios_set_field_freq_op(field_hdl, freq_op_c)
    
  

  if grid_path_ is not None:
  
    grid_path_= String(grid_path_)
    grid_path_c = grid_path_._c_value
    len_grid_path_c = len(ctypes.string_at(grid_path_c))
    lib.cxios_set_field_grid_path(field_hdl, grid_path_c, len_grid_path_c)
    
  

  if grid_ref_ is not None:
  
    grid_ref_= String(grid_ref_)
    grid_ref_c = grid_ref_._c_value
    len_grid_ref_c = len(ctypes.string_at(grid_ref_c))
    lib.cxios_set_field_grid_ref(field_hdl, grid_ref_c, len_grid_ref_c)
    
  

  if indexed_output_ is not None: 
  
    indexed_output_ = Bool(indexed_output_)
    indexed_output_c = indexed_output_._c_value
    lib.cxios_set_field_indexed_output(field_hdl, indexed_output_c)
    
  

  if level_ is not None: 
  
    level_ = Int(level_)
    level_c = level_._c_value
    lib.cxios_set_field_level(field_hdl, level_c)
    
  

  if long_name_ is not None:
  
    long_name_= String(long_name_)
    long_name_c = long_name_._c_value
    len_long_name_c = len(ctypes.string_at(long_name_c))
    lib.cxios_set_field_long_name(field_hdl, long_name_c, len_long_name_c)
    
  

  if name_ is not None:
  
    name_= String(name_)
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_set_field_name(field_hdl, name_c, len_name_c)
    
  

  if operation_ is not None:
  
    operation_= String(operation_)
    operation_c = operation_._c_value
    len_operation_c = len(ctypes.string_at(operation_c))
    lib.cxios_set_field_operation(field_hdl, operation_c, len_operation_c)
    
  

  if prec_ is not None: 
  
    prec_ = Int(prec_)
    prec_c = prec_._c_value
    lib.cxios_set_field_prec(field_hdl, prec_c)
    
  

  if read_access_ is not None: 
  
    read_access_ = Bool(read_access_)
    read_access_c = read_access_._c_value
    lib.cxios_set_field_read_access(field_hdl, read_access_c)
    
  

  if scalar_ref_ is not None:
  
    scalar_ref_= String(scalar_ref_)
    scalar_ref_c = scalar_ref_._c_value
    len_scalar_ref_c = len(ctypes.string_at(scalar_ref_c))
    lib.cxios_set_field_scalar_ref(field_hdl, scalar_ref_c, len_scalar_ref_c)
    
  

  if scale_factor_ is not None: 
  
    scale_factor_ = Double(scale_factor_)
    scale_factor_c = scale_factor_._c_value
    lib.cxios_set_field_scale_factor(field_hdl, scale_factor_c)
    
  

  if standard_name_ is not None:
  
    standard_name_= String(standard_name_)
    standard_name_c = standard_name_._c_value
    len_standard_name_c = len(ctypes.string_at(standard_name_c))
    lib.cxios_set_field_standard_name(field_hdl, standard_name_c, len_standard_name_c)
    
  

  if ts_enabled_ is not None: 
  
    ts_enabled_ = Bool(ts_enabled_)
    ts_enabled_c = ts_enabled_._c_value
    lib.cxios_set_field_ts_enabled(field_hdl, ts_enabled_c)
    
  

  if ts_split_freq_ is not None: 
  
    ts_split_freq_c = ts_split_freq_
    lib.cxios_set_field_ts_split_freq(field_hdl, ts_split_freq_c)
    
  

  if unit_ is not None:
  
    unit_= String(unit_)
    unit_c = unit_._c_value
    len_unit_c = len(ctypes.string_at(unit_c))
    lib.cxios_set_field_unit(field_hdl, unit_c, len_unit_c)
    
  

  if valid_max_ is not None: 
  
    valid_max_ = Double(valid_max_)
    valid_max_c = valid_max_._c_value
    lib.cxios_set_field_valid_max(field_hdl, valid_max_c)
    
  

  if valid_min_ is not None: 
  
    valid_min_ = Double(valid_min_)
    valid_min_c = valid_min_._c_value
    lib.cxios_set_field_valid_min(field_hdl, valid_min_c)
    
  
  return 



@typecheck
def get_field_attr(field_id : Union[String, str], add_offset : Optional[Double] = None, axis_ref : Optional[String] = None,   
build_workflow_graph : Optional[Bool] = None, cell_methods : Optional[String] = None, cell_methods_mode : Optional[String] = None,   
check_if_active : Optional[Bool] = None, chunking_blocksize_target : Optional[Double] = None,   
comment : Optional[String] = None, compression_level : Optional[Int] = None, compression_params : Optional[NpArray] = None,   
compression_type : Optional[String] = None, conversion_by_netcdf : Optional[Bool] = None, default_value : Optional[Double] = None,   
detect_missing_value : Optional[Bool] = None, domain_ref : Optional[String] = None, enabled : Optional[Bool] = None,   
expr : Optional[String] = None, field_ref : Optional[String] = None, freq_offset : Optional[Duration] = None,   
freq_op : Optional[Duration] = None, grid_path : Optional[String] = None, grid_ref : Optional[String] = None,   
indexed_output : Optional[Bool] = None, level : Optional[Int] = None, long_name : Optional[String] = None,   
name : Optional[String] = None, operation : Optional[String] = None, prec : Optional[Int] = None,   
read_access : Optional[Bool] = None, scalar_ref : Optional[String] = None, scale_factor : Optional[Double] = None,   
standard_name : Optional[String] = None, ts_enabled : Optional[Bool] = None, ts_split_freq : Optional[Duration] = None,   
unit : Optional[String] = None, valid_max : Optional[Double] = None, valid_min : Optional[Double] = None):

  
  field_hdl = Field()
  

  get_field_handle(field_id, field_hdl)
  get_field_attr_hdl_(field_hdl, add_offset, axis_ref, build_workflow_graph, cell_methods, cell_methods_mode, check_if_active,  
   chunking_blocksize_target, comment, compression_level, compression_params, compression_type,  
   conversion_by_netcdf, default_value, detect_missing_value, domain_ref, enabled, expr, field_ref,  
   freq_offset, freq_op, grid_path, grid_ref, indexed_output, level, long_name, name, operation,  
   prec, read_access, scalar_ref, scale_factor, standard_name, ts_enabled, ts_split_freq, unit,  
   valid_max, valid_min)
  return 



@typecheck
def get_field_attr_hdl(field_hdl : Field, add_offset : Optional[Double] = None, axis_ref : Optional[String] = None,   
build_workflow_graph : Optional[Bool] = None, cell_methods : Optional[String] = None, cell_methods_mode : Optional[String] = None,   
check_if_active : Optional[Bool] = None, chunking_blocksize_target : Optional[Double] = None,   
comment : Optional[String] = None, compression_level : Optional[Int] = None, compression_params : Optional[NpArray] = None,   
compression_type : Optional[String] = None, conversion_by_netcdf : Optional[Bool] = None, default_value : Optional[Double] = None,   
detect_missing_value : Optional[Bool] = None, domain_ref : Optional[String] = None, enabled : Optional[Bool] = None,   
expr : Optional[String] = None, field_ref : Optional[String] = None, freq_offset : Optional[Duration] = None,   
freq_op : Optional[Duration] = None, grid_path : Optional[String] = None, grid_ref : Optional[String] = None,   
indexed_output : Optional[Bool] = None, level : Optional[Int] = None, long_name : Optional[String] = None,   
name : Optional[String] = None, operation : Optional[String] = None, prec : Optional[Int] = None,   
read_access : Optional[Bool] = None, scalar_ref : Optional[String] = None, scale_factor : Optional[Double] = None,   
standard_name : Optional[String] = None, ts_enabled : Optional[Bool] = None, ts_split_freq : Optional[Duration] = None,   
unit : Optional[String] = None, valid_max : Optional[Double] = None, valid_min : Optional[Double] = None):

  
  get_field_attr_hdl_(field_hdl, add_offset, axis_ref, build_workflow_graph, cell_methods, cell_methods_mode, check_if_active,  
   chunking_blocksize_target, comment, compression_level, compression_params, compression_type,  
   conversion_by_netcdf, default_value, detect_missing_value, domain_ref, enabled, expr, field_ref,  
   freq_offset, freq_op, grid_path, grid_ref, indexed_output, level, long_name, name, operation,  
   prec, read_access, scalar_ref, scale_factor, standard_name, ts_enabled, ts_split_freq, unit,  
   valid_max, valid_min)
  return 



@typecheck
def get_field_attr_hdl_(field_hdl : Field, add_offset_ : Optional[Double] = None, axis_ref_ : Optional[String] = None,   
build_workflow_graph_ : Optional[Bool] = None, cell_methods_ : Optional[String] = None, cell_methods_mode_ : Optional[String] = None,   
check_if_active_ : Optional[Bool] = None, chunking_blocksize_target_ : Optional[Double] = None,   
comment_ : Optional[String] = None, compression_level_ : Optional[Int] = None, compression_params_ : Optional[NpArray] = None,   
compression_type_ : Optional[String] = None, conversion_by_netcdf_ : Optional[Bool] = None,   
default_value_ : Optional[Double] = None, detect_missing_value_ : Optional[Bool] = None, domain_ref_ : Optional[String] = None,   
enabled_ : Optional[Bool] = None, expr_ : Optional[String] = None, field_ref_ : Optional[String] = None,   
freq_offset_ : Optional[Duration] = None, freq_op_ : Optional[Duration] = None, grid_path_ : Optional[String] = None,   
grid_ref_ : Optional[String] = None, indexed_output_ : Optional[Bool] = None, level_ : Optional[Int] = None,   
long_name_ : Optional[String] = None, name_ : Optional[String] = None, operation_ : Optional[String] = None,   
prec_ : Optional[Int] = None, read_access_ : Optional[Bool] = None, scalar_ref_ : Optional[String] = None,   
scale_factor_ : Optional[Double] = None, standard_name_ : Optional[String] = None, ts_enabled_ : Optional[Bool] = None,   
ts_split_freq_ : Optional[Duration] = None, unit_ : Optional[String] = None, valid_max_ : Optional[Double] = None,   
valid_min_ : Optional[Double] = None):

  
  

  if add_offset_ is not None: 
  
    add_offset_c = add_offset_._c_value
    lib.cxios_get_field_add_offset(field_hdl, add_offset_c)
    
  

  if axis_ref_ is not None:
  
    axis_ref_c = axis_ref_._c_value
    len_axis_ref_c = len(ctypes.string_at(axis_ref_c))
    lib.cxios_get_field_axis_ref(field_hdl, axis_ref_c, len_axis_ref_c)
    
  

  if build_workflow_graph_ is not None: 
  
    build_workflow_graph_c = build_workflow_graph_._c_value
    lib.cxios_get_field_build_workflow_graph(field_hdl, build_workflow_graph_c)
    
  

  if cell_methods_ is not None:
  
    cell_methods_c = cell_methods_._c_value
    len_cell_methods_c = len(ctypes.string_at(cell_methods_c))
    lib.cxios_get_field_cell_methods(field_hdl, cell_methods_c, len_cell_methods_c)
    
  

  if cell_methods_mode_ is not None:
  
    cell_methods_mode_c = cell_methods_mode_._c_value
    len_cell_methods_mode_c = len(ctypes.string_at(cell_methods_mode_c))
    lib.cxios_get_field_cell_methods_mode(field_hdl, cell_methods_mode_c, len_cell_methods_mode_c)
    
  

  if check_if_active_ is not None: 
  
    check_if_active_c = check_if_active_._c_value
    lib.cxios_get_field_check_if_active(field_hdl, check_if_active_c)
    
  

  if chunking_blocksize_target_ is not None: 
  
    chunking_blocksize_target_c = chunking_blocksize_target_._c_value
    lib.cxios_get_field_chunking_blocksize_target(field_hdl, chunking_blocksize_target_c)
    
  

  if comment_ is not None:
  
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_get_field_comment(field_hdl, comment_c, len_comment_c)
    
  

  if compression_level_ is not None: 
  
    compression_level_c = compression_level_._c_value
    lib.cxios_get_field_compression_level(field_hdl, compression_level_c)
    
  

  if compression_params_ is not None:
    if compression_params_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_field_compression_params_shape(field_hdl, shape_c)
      compression_params_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_field_compression_params(field_hdl, compression_params_._c_value, shape_c)
    else: 
      compression_params_c = compression_params_._c_value
      if len(compression_params_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(compression_params_.shape)._c_value
        lib.cxios_get_field_compression_params(field_hdl, compression_params_c, shape_c)
  

  if compression_type_ is not None:
  
    compression_type_c = compression_type_._c_value
    len_compression_type_c = len(ctypes.string_at(compression_type_c))
    lib.cxios_get_field_compression_type(field_hdl, compression_type_c, len_compression_type_c)
    
  

  if conversion_by_netcdf_ is not None: 
  
    conversion_by_netcdf_c = conversion_by_netcdf_._c_value
    lib.cxios_get_field_conversion_by_netcdf(field_hdl, conversion_by_netcdf_c)
    
  

  if default_value_ is not None: 
  
    default_value_c = default_value_._c_value
    lib.cxios_get_field_default_value(field_hdl, default_value_c)
    
  

  if detect_missing_value_ is not None: 
  
    detect_missing_value_c = detect_missing_value_._c_value
    lib.cxios_get_field_detect_missing_value(field_hdl, detect_missing_value_c)
    
  

  if domain_ref_ is not None:
  
    domain_ref_c = domain_ref_._c_value
    len_domain_ref_c = len(ctypes.string_at(domain_ref_c))
    lib.cxios_get_field_domain_ref(field_hdl, domain_ref_c, len_domain_ref_c)
    
  

  if enabled_ is not None: 
  
    enabled_c = enabled_._c_value
    lib.cxios_get_field_enabled(field_hdl, enabled_c)
    
  

  if expr_ is not None:
  
    expr_c = expr_._c_value
    len_expr_c = len(ctypes.string_at(expr_c))
    lib.cxios_get_field_expr(field_hdl, expr_c, len_expr_c)
    
  

  if field_ref_ is not None:
  
    field_ref_c = field_ref_._c_value
    len_field_ref_c = len(ctypes.string_at(field_ref_c))
    lib.cxios_get_field_field_ref(field_hdl, field_ref_c, len_field_ref_c)
    
  

  if freq_offset_ is not None: 
  
    freq_offset_c = ctypes.pointer(freq_offset_)
    # DATE ?
    lib.cxios_get_field_freq_offset(field_hdl, freq_offset_c)
    
  

  if freq_op_ is not None: 
  
    freq_op_c = ctypes.pointer(freq_op_)
    # DATE ?
    lib.cxios_get_field_freq_op(field_hdl, freq_op_c)
    
  

  if grid_path_ is not None:
  
    grid_path_c = grid_path_._c_value
    len_grid_path_c = len(ctypes.string_at(grid_path_c))
    lib.cxios_get_field_grid_path(field_hdl, grid_path_c, len_grid_path_c)
    
  

  if grid_ref_ is not None:
  
    grid_ref_c = grid_ref_._c_value
    len_grid_ref_c = len(ctypes.string_at(grid_ref_c))
    lib.cxios_get_field_grid_ref(field_hdl, grid_ref_c, len_grid_ref_c)
    
  

  if indexed_output_ is not None: 
  
    indexed_output_c = indexed_output_._c_value
    lib.cxios_get_field_indexed_output(field_hdl, indexed_output_c)
    
  

  if level_ is not None: 
  
    level_c = level_._c_value
    lib.cxios_get_field_level(field_hdl, level_c)
    
  

  if long_name_ is not None:
  
    long_name_c = long_name_._c_value
    len_long_name_c = len(ctypes.string_at(long_name_c))
    lib.cxios_get_field_long_name(field_hdl, long_name_c, len_long_name_c)
    
  

  if name_ is not None:
  
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_get_field_name(field_hdl, name_c, len_name_c)
    
  

  if operation_ is not None:
  
    operation_c = operation_._c_value
    len_operation_c = len(ctypes.string_at(operation_c))
    lib.cxios_get_field_operation(field_hdl, operation_c, len_operation_c)
    
  

  if prec_ is not None: 
  
    prec_c = prec_._c_value
    lib.cxios_get_field_prec(field_hdl, prec_c)
    
  

  if read_access_ is not None: 
  
    read_access_c = read_access_._c_value
    lib.cxios_get_field_read_access(field_hdl, read_access_c)
    
  

  if scalar_ref_ is not None:
  
    scalar_ref_c = scalar_ref_._c_value
    len_scalar_ref_c = len(ctypes.string_at(scalar_ref_c))
    lib.cxios_get_field_scalar_ref(field_hdl, scalar_ref_c, len_scalar_ref_c)
    
  

  if scale_factor_ is not None: 
  
    scale_factor_c = scale_factor_._c_value
    lib.cxios_get_field_scale_factor(field_hdl, scale_factor_c)
    
  

  if standard_name_ is not None:
  
    standard_name_c = standard_name_._c_value
    len_standard_name_c = len(ctypes.string_at(standard_name_c))
    lib.cxios_get_field_standard_name(field_hdl, standard_name_c, len_standard_name_c)
    
  

  if ts_enabled_ is not None: 
  
    ts_enabled_c = ts_enabled_._c_value
    lib.cxios_get_field_ts_enabled(field_hdl, ts_enabled_c)
    
  

  if ts_split_freq_ is not None: 
  
    ts_split_freq_c = ctypes.pointer(ts_split_freq_)
    # DATE ?
    lib.cxios_get_field_ts_split_freq(field_hdl, ts_split_freq_c)
    
  

  if unit_ is not None:
  
    unit_c = unit_._c_value
    len_unit_c = len(ctypes.string_at(unit_c))
    lib.cxios_get_field_unit(field_hdl, unit_c, len_unit_c)
    
  

  if valid_max_ is not None: 
  
    valid_max_c = valid_max_._c_value
    lib.cxios_get_field_valid_max(field_hdl, valid_max_c)
    
  

  if valid_min_ is not None: 
  
    valid_min_c = valid_min_._c_value
    lib.cxios_get_field_valid_min(field_hdl, valid_min_c)
    
  
  return 



@typecheck
def is_defined_field_attr(field_id : String, add_offset : Optional[Bool] = None, axis_ref : Optional[Bool] = None, build_workflow_graph : Optional[Bool] = None,   
cell_methods : Optional[Bool] = None, cell_methods_mode : Optional[Bool] = None, check_if_active : Optional[Bool] = None,   
chunking_blocksize_target : Optional[Bool] = None, comment : Optional[Bool] = None, compression_level : Optional[Bool] = None,   
compression_params : Optional[Bool] = None, compression_type : Optional[Bool] = None, conversion_by_netcdf : Optional[Bool] = None,   
default_value : Optional[Bool] = None, detect_missing_value : Optional[Bool] = None, domain_ref : Optional[Bool] = None,   
enabled : Optional[Bool] = None, expr : Optional[Bool] = None, field_ref : Optional[Bool] = None,   
freq_offset : Optional[Bool] = None, freq_op : Optional[Bool] = None, grid_path : Optional[Bool] = None,   
grid_ref : Optional[Bool] = None, indexed_output : Optional[Bool] = None, level : Optional[Bool] = None,   
long_name : Optional[Bool] = None, name : Optional[Bool] = None, operation : Optional[Bool] = None,   
prec : Optional[Bool] = None, read_access : Optional[Bool] = None, scalar_ref : Optional[Bool] = None,   
scale_factor : Optional[Bool] = None, standard_name : Optional[Bool] = None, ts_enabled : Optional[Bool] = None,   
ts_split_freq : Optional[Bool] = None, unit : Optional[Bool] = None, valid_max : Optional[Bool] = None,   
valid_min : Optional[Bool] = None):

  
  field_hdl = Field()
  

  get_field_handle(field_id, field_hdl)
  is_defined_field_attr_hdl_(field_hdl, add_offset, axis_ref, build_workflow_graph, cell_methods, cell_methods_mode, check_if_active,  
   chunking_blocksize_target, comment, compression_level, compression_params, compression_type,  
   conversion_by_netcdf, default_value, detect_missing_value, domain_ref, enabled, expr, field_ref,  
   freq_offset, freq_op, grid_path, grid_ref, indexed_output, level, long_name, name, operation,  
   prec, read_access, scalar_ref, scale_factor, standard_name, ts_enabled, ts_split_freq, unit,  
   valid_max, valid_min)
  return 



@typecheck
def is_defined_field_attr_hdl(field_hdl : Field, add_offset : Optional[Bool] = None, axis_ref : Optional[Bool] = None, build_workflow_graph : Optional[Bool] = None,   
cell_methods : Optional[Bool] = None, cell_methods_mode : Optional[Bool] = None, check_if_active : Optional[Bool] = None,   
chunking_blocksize_target : Optional[Bool] = None, comment : Optional[Bool] = None, compression_level : Optional[Bool] = None,   
compression_params : Optional[Bool] = None, compression_type : Optional[Bool] = None, conversion_by_netcdf : Optional[Bool] = None,   
default_value : Optional[Bool] = None, detect_missing_value : Optional[Bool] = None, domain_ref : Optional[Bool] = None,   
enabled : Optional[Bool] = None, expr : Optional[Bool] = None, field_ref : Optional[Bool] = None,   
freq_offset : Optional[Bool] = None, freq_op : Optional[Bool] = None, grid_path : Optional[Bool] = None,   
grid_ref : Optional[Bool] = None, indexed_output : Optional[Bool] = None, level : Optional[Bool] = None,   
long_name : Optional[Bool] = None, name : Optional[Bool] = None, operation : Optional[Bool] = None,   
prec : Optional[Bool] = None, read_access : Optional[Bool] = None, scalar_ref : Optional[Bool] = None,   
scale_factor : Optional[Bool] = None, standard_name : Optional[Bool] = None, ts_enabled : Optional[Bool] = None,   
ts_split_freq : Optional[Bool] = None, unit : Optional[Bool] = None, valid_max : Optional[Bool] = None,   
valid_min : Optional[Bool] = None):

  
  is_defined_field_attr_hdl_(field_hdl, add_offset, axis_ref, build_workflow_graph, cell_methods, cell_methods_mode, check_if_active,  
   chunking_blocksize_target, comment, compression_level, compression_params, compression_type,  
   conversion_by_netcdf, default_value, detect_missing_value, domain_ref, enabled, expr, field_ref,  
   freq_offset, freq_op, grid_path, grid_ref, indexed_output, level, long_name, name, operation,  
   prec, read_access, scalar_ref, scale_factor, standard_name, ts_enabled, ts_split_freq, unit,  
   valid_max, valid_min)
  return 



@typecheck
def is_defined_field_attr_hdl_(field_hdl : Field, add_offset_ : Optional[Bool] = None, axis_ref_ : Optional[Bool] = None,   
build_workflow_graph_ : Optional[Bool] = None, cell_methods_ : Optional[Bool] = None, cell_methods_mode_ : Optional[Bool] = None,   
check_if_active_ : Optional[Bool] = None, chunking_blocksize_target_ : Optional[Bool] = None,   
comment_ : Optional[Bool] = None, compression_level_ : Optional[Bool] = None, compression_params_ : Optional[Bool] = None,   
compression_type_ : Optional[Bool] = None, conversion_by_netcdf_ : Optional[Bool] = None, default_value_ : Optional[Bool] = None,   
detect_missing_value_ : Optional[Bool] = None, domain_ref_ : Optional[Bool] = None, enabled_ : Optional[Bool] = None,   
expr_ : Optional[Bool] = None, field_ref_ : Optional[Bool] = None, freq_offset_ : Optional[Bool] = None,   
freq_op_ : Optional[Bool] = None, grid_path_ : Optional[Bool] = None, grid_ref_ : Optional[Bool] = None,   
indexed_output_ : Optional[Bool] = None, level_ : Optional[Bool] = None, long_name_ : Optional[Bool] = None,   
name_ : Optional[Bool] = None, operation_ : Optional[Bool] = None, prec_ : Optional[Bool] = None,   
read_access_ : Optional[Bool] = None, scalar_ref_ : Optional[Bool] = None, scale_factor_ : Optional[Bool] = None,   
standard_name_ : Optional[Bool] = None, ts_enabled_ : Optional[Bool] = None, ts_split_freq_ : Optional[Bool] = None,   
unit_ : Optional[Bool] = None, valid_max_ : Optional[Bool] = None, valid_min_ : Optional[Bool] = None):

  
  

  if add_offset_  is not None:
    add_offset_c = lib.cxios_is_defined_field_add_offset(field_hdl)
    add_offset_._c_value = ctypes.c_bool(add_offset_c)
    
  

  if axis_ref_  is not None:
    axis_ref_c = lib.cxios_is_defined_field_axis_ref(field_hdl)
    axis_ref_._c_value = ctypes.c_bool(axis_ref_c)
    
  

  if build_workflow_graph_  is not None:
    build_workflow_graph_c = lib.cxios_is_defined_field_build_workflow_graph(field_hdl)
    build_workflow_graph_._c_value = ctypes.c_bool(build_workflow_graph_c)
    
  

  if cell_methods_  is not None:
    cell_methods_c = lib.cxios_is_defined_field_cell_methods(field_hdl)
    cell_methods_._c_value = ctypes.c_bool(cell_methods_c)
    
  

  if cell_methods_mode_  is not None:
    cell_methods_mode_c = lib.cxios_is_defined_field_cell_methods_mode(field_hdl)
    cell_methods_mode_._c_value = ctypes.c_bool(cell_methods_mode_c)
    
  

  if check_if_active_  is not None:
    check_if_active_c = lib.cxios_is_defined_field_check_if_active(field_hdl)
    check_if_active_._c_value = ctypes.c_bool(check_if_active_c)
    
  

  if chunking_blocksize_target_  is not None:
    chunking_blocksize_target_c = lib.cxios_is_defined_field_chunking_blocksize_target(field_hdl)
    chunking_blocksize_target_._c_value = ctypes.c_bool(chunking_blocksize_target_c)
    
  

  if comment_  is not None:
    comment_c = lib.cxios_is_defined_field_comment(field_hdl)
    comment_._c_value = ctypes.c_bool(comment_c)
    
  

  if compression_level_  is not None:
    compression_level_c = lib.cxios_is_defined_field_compression_level(field_hdl)
    compression_level_._c_value = ctypes.c_bool(compression_level_c)
    
  

  if compression_params_  is not None:
    compression_params_c = lib.cxios_is_defined_field_compression_params(field_hdl)
    compression_params_._c_value = ctypes.c_bool(compression_params_c)
    
  

  if compression_type_  is not None:
    compression_type_c = lib.cxios_is_defined_field_compression_type(field_hdl)
    compression_type_._c_value = ctypes.c_bool(compression_type_c)
    
  

  if conversion_by_netcdf_  is not None:
    conversion_by_netcdf_c = lib.cxios_is_defined_field_conversion_by_netcdf(field_hdl)
    conversion_by_netcdf_._c_value = ctypes.c_bool(conversion_by_netcdf_c)
    
  

  if default_value_  is not None:
    default_value_c = lib.cxios_is_defined_field_default_value(field_hdl)
    default_value_._c_value = ctypes.c_bool(default_value_c)
    
  

  if detect_missing_value_  is not None:
    detect_missing_value_c = lib.cxios_is_defined_field_detect_missing_value(field_hdl)
    detect_missing_value_._c_value = ctypes.c_bool(detect_missing_value_c)
    
  

  if domain_ref_  is not None:
    domain_ref_c = lib.cxios_is_defined_field_domain_ref(field_hdl)
    domain_ref_._c_value = ctypes.c_bool(domain_ref_c)
    
  

  if enabled_  is not None:
    enabled_c = lib.cxios_is_defined_field_enabled(field_hdl)
    enabled_._c_value = ctypes.c_bool(enabled_c)
    
  

  if expr_  is not None:
    expr_c = lib.cxios_is_defined_field_expr(field_hdl)
    expr_._c_value = ctypes.c_bool(expr_c)
    
  

  if field_ref_  is not None:
    field_ref_c = lib.cxios_is_defined_field_field_ref(field_hdl)
    field_ref_._c_value = ctypes.c_bool(field_ref_c)
    
  

  if freq_offset_  is not None:
    freq_offset_c = lib.cxios_is_defined_field_freq_offset(field_hdl)
    freq_offset_._c_value = ctypes.c_bool(freq_offset_c)
    
  

  if freq_op_  is not None:
    freq_op_c = lib.cxios_is_defined_field_freq_op(field_hdl)
    freq_op_._c_value = ctypes.c_bool(freq_op_c)
    
  

  if grid_path_  is not None:
    grid_path_c = lib.cxios_is_defined_field_grid_path(field_hdl)
    grid_path_._c_value = ctypes.c_bool(grid_path_c)
    
  

  if grid_ref_  is not None:
    grid_ref_c = lib.cxios_is_defined_field_grid_ref(field_hdl)
    grid_ref_._c_value = ctypes.c_bool(grid_ref_c)
    
  

  if indexed_output_  is not None:
    indexed_output_c = lib.cxios_is_defined_field_indexed_output(field_hdl)
    indexed_output_._c_value = ctypes.c_bool(indexed_output_c)
    
  

  if level_  is not None:
    level_c = lib.cxios_is_defined_field_level(field_hdl)
    level_._c_value = ctypes.c_bool(level_c)
    
  

  if long_name_  is not None:
    long_name_c = lib.cxios_is_defined_field_long_name(field_hdl)
    long_name_._c_value = ctypes.c_bool(long_name_c)
    
  

  if name_  is not None:
    name_c = lib.cxios_is_defined_field_name(field_hdl)
    name_._c_value = ctypes.c_bool(name_c)
    
  

  if operation_  is not None:
    operation_c = lib.cxios_is_defined_field_operation(field_hdl)
    operation_._c_value = ctypes.c_bool(operation_c)
    
  

  if prec_  is not None:
    prec_c = lib.cxios_is_defined_field_prec(field_hdl)
    prec_._c_value = ctypes.c_bool(prec_c)
    
  

  if read_access_  is not None:
    read_access_c = lib.cxios_is_defined_field_read_access(field_hdl)
    read_access_._c_value = ctypes.c_bool(read_access_c)
    
  

  if scalar_ref_  is not None:
    scalar_ref_c = lib.cxios_is_defined_field_scalar_ref(field_hdl)
    scalar_ref_._c_value = ctypes.c_bool(scalar_ref_c)
    
  

  if scale_factor_  is not None:
    scale_factor_c = lib.cxios_is_defined_field_scale_factor(field_hdl)
    scale_factor_._c_value = ctypes.c_bool(scale_factor_c)
    
  

  if standard_name_  is not None:
    standard_name_c = lib.cxios_is_defined_field_standard_name(field_hdl)
    standard_name_._c_value = ctypes.c_bool(standard_name_c)
    
  

  if ts_enabled_  is not None:
    ts_enabled_c = lib.cxios_is_defined_field_ts_enabled(field_hdl)
    ts_enabled_._c_value = ctypes.c_bool(ts_enabled_c)
    
  

  if ts_split_freq_  is not None:
    ts_split_freq_c = lib.cxios_is_defined_field_ts_split_freq(field_hdl)
    ts_split_freq_._c_value = ctypes.c_bool(ts_split_freq_c)
    
  

  if unit_  is not None:
    unit_c = lib.cxios_is_defined_field_unit(field_hdl)
    unit_._c_value = ctypes.c_bool(unit_c)
    
  

  if valid_max_  is not None:
    valid_max_c = lib.cxios_is_defined_field_valid_max(field_hdl)
    valid_max_._c_value = ctypes.c_bool(valid_max_c)
    
  

  if valid_min_  is not None:
    valid_min_c = lib.cxios_is_defined_field_valid_min(field_hdl)
    valid_min_._c_value = ctypes.c_bool(valid_min_c)
    
  
  return 



