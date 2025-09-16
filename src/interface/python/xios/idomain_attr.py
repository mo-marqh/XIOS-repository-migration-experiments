# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.idomain import  get_domain_handle
from xios.odomain_attr import Domain

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_domain_area.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_area.restypes = None

lib.cxios_set_domain_area.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_area.restypes = None

lib.cxios_is_defined_domain_area.argtypes = [Domain]
lib.cxios_is_defined_domain_area.restypes = ctypes.c_bool

lib.cxios_get_domain_area_1d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_area_1d.restypes = None

lib.cxios_set_domain_area_1d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_area_1d.restypes = None

lib.cxios_is_defined_domain_area_1d.argtypes = [Domain]
lib.cxios_is_defined_domain_area_1d.restypes = ctypes.c_bool

lib.cxios_get_domain_area_2d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_area_2d.restypes = None

lib.cxios_set_domain_area_2d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_area_2d.restypes = None

lib.cxios_is_defined_domain_area_2d.argtypes = [Domain]
lib.cxios_is_defined_domain_area_2d.restypes = ctypes.c_bool

lib.cxios_get_domain_bounds_lat_1d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_bounds_lat_1d.restypes = None

lib.cxios_set_domain_bounds_lat_1d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_bounds_lat_1d.restypes = None

lib.cxios_is_defined_domain_bounds_lat_1d.argtypes = [Domain]
lib.cxios_is_defined_domain_bounds_lat_1d.restypes = ctypes.c_bool

lib.cxios_get_domain_bounds_lat_2d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_bounds_lat_2d.restypes = None

lib.cxios_set_domain_bounds_lat_2d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_bounds_lat_2d.restypes = None

lib.cxios_is_defined_domain_bounds_lat_2d.argtypes = [Domain]
lib.cxios_is_defined_domain_bounds_lat_2d.restypes = ctypes.c_bool

lib.cxios_get_domain_bounds_lat_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_domain_bounds_lat_name.restypes = None

lib.cxios_set_domain_bounds_lat_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_domain_bounds_lat_name.restypes = None

lib.cxios_is_defined_domain_bounds_lat_name.argtypes = [Domain]
lib.cxios_is_defined_domain_bounds_lat_name.restypes = ctypes.c_bool

lib.cxios_get_domain_bounds_lon_1d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_bounds_lon_1d.restypes = None

lib.cxios_set_domain_bounds_lon_1d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_bounds_lon_1d.restypes = None

lib.cxios_is_defined_domain_bounds_lon_1d.argtypes = [Domain]
lib.cxios_is_defined_domain_bounds_lon_1d.restypes = ctypes.c_bool

lib.cxios_get_domain_bounds_lon_2d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_bounds_lon_2d.restypes = None

lib.cxios_set_domain_bounds_lon_2d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_bounds_lon_2d.restypes = None

lib.cxios_is_defined_domain_bounds_lon_2d.argtypes = [Domain]
lib.cxios_is_defined_domain_bounds_lon_2d.restypes = ctypes.c_bool

lib.cxios_get_domain_bounds_lon_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_domain_bounds_lon_name.restypes = None

lib.cxios_set_domain_bounds_lon_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_domain_bounds_lon_name.restypes = None

lib.cxios_is_defined_domain_bounds_lon_name.argtypes = [Domain]
lib.cxios_is_defined_domain_bounds_lon_name.restypes = ctypes.c_bool

lib.cxios_get_domain_chunking_weight_i.argtypes = [Domain, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_domain_chunking_weight_i.restypes = None

lib.cxios_set_domain_chunking_weight_i.argtypes = [Domain, ctypes.c_double]
lib.cxios_set_domain_chunking_weight_i.restypes = None

lib.cxios_is_defined_domain_chunking_weight_i.argtypes = [Domain]
lib.cxios_is_defined_domain_chunking_weight_i.restypes = ctypes.c_bool

lib.cxios_get_domain_chunking_weight_j.argtypes = [Domain, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_domain_chunking_weight_j.restypes = None

lib.cxios_set_domain_chunking_weight_j.argtypes = [Domain, ctypes.c_double]
lib.cxios_set_domain_chunking_weight_j.restypes = None

lib.cxios_is_defined_domain_chunking_weight_j.argtypes = [Domain]
lib.cxios_is_defined_domain_chunking_weight_j.restypes = ctypes.c_bool

lib.cxios_get_domain_comment.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_domain_comment.restypes = None

lib.cxios_set_domain_comment.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_domain_comment.restypes = None

lib.cxios_is_defined_domain_comment.argtypes = [Domain]
lib.cxios_is_defined_domain_comment.restypes = ctypes.c_bool

lib.cxios_get_domain_data_dim.argtypes = [Domain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_data_dim.restypes = None

lib.cxios_set_domain_data_dim.argtypes = [Domain, ctypes.c_int]
lib.cxios_set_domain_data_dim.restypes = None

lib.cxios_is_defined_domain_data_dim.argtypes = [Domain]
lib.cxios_is_defined_domain_data_dim.restypes = ctypes.c_bool

lib.cxios_get_domain_data_i_index.argtypes = [Domain, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_data_i_index.restypes = None

lib.cxios_set_domain_data_i_index.argtypes = [Domain, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_data_i_index.restypes = None

lib.cxios_is_defined_domain_data_i_index.argtypes = [Domain]
lib.cxios_is_defined_domain_data_i_index.restypes = ctypes.c_bool

lib.cxios_get_domain_data_ibegin.argtypes = [Domain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_data_ibegin.restypes = None

lib.cxios_set_domain_data_ibegin.argtypes = [Domain, ctypes.c_int]
lib.cxios_set_domain_data_ibegin.restypes = None

lib.cxios_is_defined_domain_data_ibegin.argtypes = [Domain]
lib.cxios_is_defined_domain_data_ibegin.restypes = ctypes.c_bool

lib.cxios_get_domain_data_j_index.argtypes = [Domain, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_data_j_index.restypes = None

lib.cxios_set_domain_data_j_index.argtypes = [Domain, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_data_j_index.restypes = None

lib.cxios_is_defined_domain_data_j_index.argtypes = [Domain]
lib.cxios_is_defined_domain_data_j_index.restypes = ctypes.c_bool

lib.cxios_get_domain_data_jbegin.argtypes = [Domain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_data_jbegin.restypes = None

lib.cxios_set_domain_data_jbegin.argtypes = [Domain, ctypes.c_int]
lib.cxios_set_domain_data_jbegin.restypes = None

lib.cxios_is_defined_domain_data_jbegin.argtypes = [Domain]
lib.cxios_is_defined_domain_data_jbegin.restypes = ctypes.c_bool

lib.cxios_get_domain_data_ni.argtypes = [Domain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_data_ni.restypes = None

lib.cxios_set_domain_data_ni.argtypes = [Domain, ctypes.c_int]
lib.cxios_set_domain_data_ni.restypes = None

lib.cxios_is_defined_domain_data_ni.argtypes = [Domain]
lib.cxios_is_defined_domain_data_ni.restypes = ctypes.c_bool

lib.cxios_get_domain_data_nj.argtypes = [Domain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_data_nj.restypes = None

lib.cxios_set_domain_data_nj.argtypes = [Domain, ctypes.c_int]
lib.cxios_set_domain_data_nj.restypes = None

lib.cxios_is_defined_domain_data_nj.argtypes = [Domain]
lib.cxios_is_defined_domain_data_nj.restypes = ctypes.c_bool

lib.cxios_get_domain_dim_i_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_domain_dim_i_name.restypes = None

lib.cxios_set_domain_dim_i_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_domain_dim_i_name.restypes = None

lib.cxios_is_defined_domain_dim_i_name.argtypes = [Domain]
lib.cxios_is_defined_domain_dim_i_name.restypes = ctypes.c_bool

lib.cxios_get_domain_dim_j_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_domain_dim_j_name.restypes = None

lib.cxios_set_domain_dim_j_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_domain_dim_j_name.restypes = None

lib.cxios_is_defined_domain_dim_j_name.argtypes = [Domain]
lib.cxios_is_defined_domain_dim_j_name.restypes = ctypes.c_bool

lib.cxios_get_domain_domain_ref.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_domain_domain_ref.restypes = None

lib.cxios_set_domain_domain_ref.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_domain_domain_ref.restypes = None

lib.cxios_is_defined_domain_domain_ref.argtypes = [Domain]
lib.cxios_is_defined_domain_domain_ref.restypes = ctypes.c_bool

lib.cxios_get_domain_i_index.argtypes = [Domain, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_i_index.restypes = None

lib.cxios_set_domain_i_index.argtypes = [Domain, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_i_index.restypes = None

lib.cxios_is_defined_domain_i_index.argtypes = [Domain]
lib.cxios_is_defined_domain_i_index.restypes = ctypes.c_bool

lib.cxios_get_domain_ibegin.argtypes = [Domain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_ibegin.restypes = None

lib.cxios_set_domain_ibegin.argtypes = [Domain, ctypes.c_int]
lib.cxios_set_domain_ibegin.restypes = None

lib.cxios_is_defined_domain_ibegin.argtypes = [Domain]
lib.cxios_is_defined_domain_ibegin.restypes = ctypes.c_bool

lib.cxios_get_domain_j_index.argtypes = [Domain, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_j_index.restypes = None

lib.cxios_set_domain_j_index.argtypes = [Domain, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_j_index.restypes = None

lib.cxios_is_defined_domain_j_index.argtypes = [Domain]
lib.cxios_is_defined_domain_j_index.restypes = ctypes.c_bool

lib.cxios_get_domain_jbegin.argtypes = [Domain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_jbegin.restypes = None

lib.cxios_set_domain_jbegin.argtypes = [Domain, ctypes.c_int]
lib.cxios_set_domain_jbegin.restypes = None

lib.cxios_is_defined_domain_jbegin.argtypes = [Domain]
lib.cxios_is_defined_domain_jbegin.restypes = ctypes.c_bool

lib.cxios_get_domain_lat_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_domain_lat_name.restypes = None

lib.cxios_set_domain_lat_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_domain_lat_name.restypes = None

lib.cxios_is_defined_domain_lat_name.argtypes = [Domain]
lib.cxios_is_defined_domain_lat_name.restypes = ctypes.c_bool

lib.cxios_get_domain_latvalue_1d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_latvalue_1d.restypes = None

lib.cxios_set_domain_latvalue_1d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_latvalue_1d.restypes = None

lib.cxios_is_defined_domain_latvalue_1d.argtypes = [Domain]
lib.cxios_is_defined_domain_latvalue_1d.restypes = ctypes.c_bool

lib.cxios_get_domain_latvalue_2d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_latvalue_2d.restypes = None

lib.cxios_set_domain_latvalue_2d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_latvalue_2d.restypes = None

lib.cxios_is_defined_domain_latvalue_2d.argtypes = [Domain]
lib.cxios_is_defined_domain_latvalue_2d.restypes = ctypes.c_bool

lib.cxios_get_domain_lon_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_domain_lon_name.restypes = None

lib.cxios_set_domain_lon_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_domain_lon_name.restypes = None

lib.cxios_is_defined_domain_lon_name.argtypes = [Domain]
lib.cxios_is_defined_domain_lon_name.restypes = ctypes.c_bool

lib.cxios_get_domain_long_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_domain_long_name.restypes = None

lib.cxios_set_domain_long_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_domain_long_name.restypes = None

lib.cxios_is_defined_domain_long_name.argtypes = [Domain]
lib.cxios_is_defined_domain_long_name.restypes = ctypes.c_bool

lib.cxios_get_domain_lonvalue_1d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_lonvalue_1d.restypes = None

lib.cxios_set_domain_lonvalue_1d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_lonvalue_1d.restypes = None

lib.cxios_is_defined_domain_lonvalue_1d.argtypes = [Domain]
lib.cxios_is_defined_domain_lonvalue_1d.restypes = ctypes.c_bool

lib.cxios_get_domain_lonvalue_2d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_lonvalue_2d.restypes = None

lib.cxios_set_domain_lonvalue_2d.argtypes = [Domain, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_lonvalue_2d.restypes = None

lib.cxios_is_defined_domain_lonvalue_2d.argtypes = [Domain]
lib.cxios_is_defined_domain_lonvalue_2d.restypes = ctypes.c_bool

lib.cxios_get_domain_mask_1d.argtypes = [Domain, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_mask_1d.restypes = None

lib.cxios_set_domain_mask_1d.argtypes = [Domain, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_mask_1d.restypes = None

lib.cxios_is_defined_domain_mask_1d.argtypes = [Domain]
lib.cxios_is_defined_domain_mask_1d.restypes = ctypes.c_bool

lib.cxios_get_domain_mask_2d.argtypes = [Domain, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_mask_2d.restypes = None

lib.cxios_set_domain_mask_2d.argtypes = [Domain, ctypes.POINTER(ctypes.c_bool), ctypes.POINTER(ctypes.c_int)]
lib.cxios_set_domain_mask_2d.restypes = None

lib.cxios_is_defined_domain_mask_2d.argtypes = [Domain]
lib.cxios_is_defined_domain_mask_2d.restypes = ctypes.c_bool

lib.cxios_get_domain_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_domain_name.restypes = None

lib.cxios_set_domain_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_domain_name.restypes = None

lib.cxios_is_defined_domain_name.argtypes = [Domain]
lib.cxios_is_defined_domain_name.restypes = ctypes.c_bool

lib.cxios_get_domain_ni.argtypes = [Domain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_ni.restypes = None

lib.cxios_set_domain_ni.argtypes = [Domain, ctypes.c_int]
lib.cxios_set_domain_ni.restypes = None

lib.cxios_is_defined_domain_ni.argtypes = [Domain]
lib.cxios_is_defined_domain_ni.restypes = ctypes.c_bool

lib.cxios_get_domain_ni_glo.argtypes = [Domain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_ni_glo.restypes = None

lib.cxios_set_domain_ni_glo.argtypes = [Domain, ctypes.c_int]
lib.cxios_set_domain_ni_glo.restypes = None

lib.cxios_is_defined_domain_ni_glo.argtypes = [Domain]
lib.cxios_is_defined_domain_ni_glo.restypes = ctypes.c_bool

lib.cxios_get_domain_nj.argtypes = [Domain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_nj.restypes = None

lib.cxios_set_domain_nj.argtypes = [Domain, ctypes.c_int]
lib.cxios_set_domain_nj.restypes = None

lib.cxios_is_defined_domain_nj.argtypes = [Domain]
lib.cxios_is_defined_domain_nj.restypes = ctypes.c_bool

lib.cxios_get_domain_nj_glo.argtypes = [Domain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_nj_glo.restypes = None

lib.cxios_set_domain_nj_glo.argtypes = [Domain, ctypes.c_int]
lib.cxios_set_domain_nj_glo.restypes = None

lib.cxios_is_defined_domain_nj_glo.argtypes = [Domain]
lib.cxios_is_defined_domain_nj_glo.restypes = ctypes.c_bool

lib.cxios_get_domain_nvertex.argtypes = [Domain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_nvertex.restypes = None

lib.cxios_set_domain_nvertex.argtypes = [Domain, ctypes.c_int]
lib.cxios_set_domain_nvertex.restypes = None

lib.cxios_is_defined_domain_nvertex.argtypes = [Domain]
lib.cxios_is_defined_domain_nvertex.restypes = ctypes.c_bool

lib.cxios_get_domain_nvertex_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_domain_nvertex_name.restypes = None

lib.cxios_set_domain_nvertex_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_domain_nvertex_name.restypes = None

lib.cxios_is_defined_domain_nvertex_name.argtypes = [Domain]
lib.cxios_is_defined_domain_nvertex_name.restypes = ctypes.c_bool

lib.cxios_get_domain_prec.argtypes = [Domain, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_domain_prec.restypes = None

lib.cxios_set_domain_prec.argtypes = [Domain, ctypes.c_int]
lib.cxios_set_domain_prec.restypes = None

lib.cxios_is_defined_domain_prec.argtypes = [Domain]
lib.cxios_is_defined_domain_prec.restypes = ctypes.c_bool

lib.cxios_get_domain_radius.argtypes = [Domain, ctypes.POINTER(ctypes.c_double)]
lib.cxios_get_domain_radius.restypes = None

lib.cxios_set_domain_radius.argtypes = [Domain, ctypes.c_double]
lib.cxios_set_domain_radius.restypes = None

lib.cxios_is_defined_domain_radius.argtypes = [Domain]
lib.cxios_is_defined_domain_radius.restypes = ctypes.c_bool

lib.cxios_get_domain_standard_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_domain_standard_name.restypes = None

lib.cxios_set_domain_standard_name.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_domain_standard_name.restypes = None

lib.cxios_is_defined_domain_standard_name.argtypes = [Domain]
lib.cxios_is_defined_domain_standard_name.restypes = ctypes.c_bool

lib.cxios_get_domain_type.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_domain_type.restypes = None

lib.cxios_set_domain_type.argtypes = [Domain, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_domain_type.restypes = None

lib.cxios_is_defined_domain_type.argtypes = [Domain]
lib.cxios_is_defined_domain_type.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_domain_attr(domain_id : Union[String, str], area : Optional[Union[np.ndarray, NpArray]] = None, area_1d : Optional[Union[np.ndarray, NpArray]] = None,   
area_2d : Optional[Union[np.ndarray, NpArray]] = None, bounds_lat_1d : Optional[Union[np.ndarray, NpArray]] = None,   
bounds_lat_2d : Optional[Union[np.ndarray, NpArray]] = None, bounds_lat_name : Optional[Union[str, String]] = None,   
bounds_lon_1d : Optional[Union[np.ndarray, NpArray]] = None, bounds_lon_2d : Optional[Union[np.ndarray, NpArray]] = None,   
bounds_lon_name : Optional[Union[str, String]] = None, chunking_weight_i : Optional[Union[Double, float]] = None,   
chunking_weight_j : Optional[Union[Double, float]] = None, comment : Optional[Union[str, String]] = None,   
data_dim : Optional[Union[Int, int]] = None, data_i_index : Optional[Union[np.ndarray, NpArrayInt]] = None,   
data_ibegin : Optional[Union[Int, int]] = None, data_j_index : Optional[Union[np.ndarray, NpArrayInt]] = None,   
data_jbegin : Optional[Union[Int, int]] = None, data_ni : Optional[Union[Int, int]] = None,   
data_nj : Optional[Union[Int, int]] = None, dim_i_name : Optional[Union[str, String]] = None,   
dim_j_name : Optional[Union[str, String]] = None, domain_ref : Optional[Union[str, String]] = None,   
i_index : Optional[Union[np.ndarray, NpArrayInt]] = None, ibegin : Optional[Union[Int, int]] = None,   
j_index : Optional[Union[np.ndarray, NpArrayInt]] = None, jbegin : Optional[Union[Int, int]] = None,   
lat_name : Optional[Union[str, String]] = None, latvalue_1d : Optional[Union[np.ndarray, NpArray]] = None,   
latvalue_2d : Optional[Union[np.ndarray, NpArray]] = None, lon_name : Optional[Union[str, String]] = None,   
long_name : Optional[Union[str, String]] = None, lonvalue_1d : Optional[Union[np.ndarray, NpArray]] = None,   
lonvalue_2d : Optional[Union[np.ndarray, NpArray]] = None, mask_1d : Optional[Union[np.ndarray, NpArrayBool]] = None,   
mask_2d : Optional[Union[np.ndarray, NpArrayBool]] = None, name : Optional[Union[str, String]] = None,   
ni : Optional[Union[Int, int]] = None, ni_glo : Optional[Union[Int, int]] = None, nj : Optional[Union[Int, int]] = None,   
nj_glo : Optional[Union[Int, int]] = None, nvertex : Optional[Union[Int, int]] = None, nvertex_name : Optional[Union[str, String]] = None,   
prec : Optional[Union[Int, int]] = None, radius : Optional[Union[Double, float]] = None, standard_name : Optional[Union[str, String]] = None,   
type : Optional[Union[str, String]] = None):

  
  domain_hdl = Domain()
  

  get_domain_handle(domain_id, domain_hdl)
  set_domain_attr_hdl_(domain_hdl, area, area_1d, area_2d, bounds_lat_1d, bounds_lat_2d, bounds_lat_name, bounds_lon_1d,  
   bounds_lon_2d, bounds_lon_name, chunking_weight_i, chunking_weight_j, comment, data_dim, data_i_index,  
   data_ibegin, data_j_index, data_jbegin, data_ni, data_nj, dim_i_name, dim_j_name, domain_ref,  
   i_index, ibegin, j_index, jbegin, lat_name, latvalue_1d, latvalue_2d, lon_name, long_name,  
   lonvalue_1d, lonvalue_2d, mask_1d, mask_2d, name, ni, ni_glo, nj, nj_glo, nvertex, nvertex_name,  
   prec, radius, standard_name, type)
  return 



@typecheck
def set_domain_attr_hdl(domain_hdl : Domain, area : Optional[Union[np.ndarray, NpArray]] = None, area_1d : Optional[Union[np.ndarray, NpArray]] = None,   
area_2d : Optional[Union[np.ndarray, NpArray]] = None, bounds_lat_1d : Optional[Union[np.ndarray, NpArray]] = None,   
bounds_lat_2d : Optional[Union[np.ndarray, NpArray]] = None, bounds_lat_name : Optional[Union[str, String]] = None,   
bounds_lon_1d : Optional[Union[np.ndarray, NpArray]] = None, bounds_lon_2d : Optional[Union[np.ndarray, NpArray]] = None,   
bounds_lon_name : Optional[Union[str, String]] = None, chunking_weight_i : Optional[Union[Double, float]] = None,   
chunking_weight_j : Optional[Union[Double, float]] = None, comment : Optional[Union[str, String]] = None,   
data_dim : Optional[Union[Int, int]] = None, data_i_index : Optional[Union[np.ndarray, NpArrayInt]] = None,   
data_ibegin : Optional[Union[Int, int]] = None, data_j_index : Optional[Union[np.ndarray, NpArrayInt]] = None,   
data_jbegin : Optional[Union[Int, int]] = None, data_ni : Optional[Union[Int, int]] = None,   
data_nj : Optional[Union[Int, int]] = None, dim_i_name : Optional[Union[str, String]] = None,   
dim_j_name : Optional[Union[str, String]] = None, domain_ref : Optional[Union[str, String]] = None,   
i_index : Optional[Union[np.ndarray, NpArrayInt]] = None, ibegin : Optional[Union[Int, int]] = None,   
j_index : Optional[Union[np.ndarray, NpArrayInt]] = None, jbegin : Optional[Union[Int, int]] = None,   
lat_name : Optional[Union[str, String]] = None, latvalue_1d : Optional[Union[np.ndarray, NpArray]] = None,   
latvalue_2d : Optional[Union[np.ndarray, NpArray]] = None, lon_name : Optional[Union[str, String]] = None,   
long_name : Optional[Union[str, String]] = None, lonvalue_1d : Optional[Union[np.ndarray, NpArray]] = None,   
lonvalue_2d : Optional[Union[np.ndarray, NpArray]] = None, mask_1d : Optional[Union[np.ndarray, NpArrayBool]] = None,   
mask_2d : Optional[Union[np.ndarray, NpArrayBool]] = None, name : Optional[Union[str, String]] = None,   
ni : Optional[Union[Int, int]] = None, ni_glo : Optional[Union[Int, int]] = None, nj : Optional[Union[Int, int]] = None,   
nj_glo : Optional[Union[Int, int]] = None, nvertex : Optional[Union[Int, int]] = None, nvertex_name : Optional[Union[str, String]] = None,   
prec : Optional[Union[Int, int]] = None, radius : Optional[Union[Double, float]] = None, standard_name : Optional[Union[str, String]] = None,   
type : Optional[Union[str, String]] = None):

  
  set_domain_attr_hdl_(domain_hdl, area, area_1d, area_2d, bounds_lat_1d, bounds_lat_2d, bounds_lat_name, bounds_lon_1d,  
   bounds_lon_2d, bounds_lon_name, chunking_weight_i, chunking_weight_j, comment, data_dim, data_i_index,  
   data_ibegin, data_j_index, data_jbegin, data_ni, data_nj, dim_i_name, dim_j_name, domain_ref,  
   i_index, ibegin, j_index, jbegin, lat_name, latvalue_1d, latvalue_2d, lon_name, long_name,  
   lonvalue_1d, lonvalue_2d, mask_1d, mask_2d, name, ni, ni_glo, nj, nj_glo, nvertex, nvertex_name,  
   prec, radius, standard_name, type)
  return 



@typecheck
def set_domain_attr_hdl_(domain_hdl : Domain, area_ : Optional[Union[np.ndarray, NpArray]] = None, area_1d_ : Optional[Union[np.ndarray, NpArray]] = None,   
area_2d_ : Optional[Union[np.ndarray, NpArray]] = None, bounds_lat_1d_ : Optional[Union[np.ndarray, NpArray]] = None,   
bounds_lat_2d_ : Optional[Union[np.ndarray, NpArray]] = None, bounds_lat_name_ : Optional[Union[str, String]] = None,   
bounds_lon_1d_ : Optional[Union[np.ndarray, NpArray]] = None, bounds_lon_2d_ : Optional[Union[np.ndarray, NpArray]] = None,   
bounds_lon_name_ : Optional[Union[str, String]] = None, chunking_weight_i_ : Optional[Union[Double, float]] = None,   
chunking_weight_j_ : Optional[Union[Double, float]] = None, comment_ : Optional[Union[str, String]] = None,   
data_dim_ : Optional[Union[Int, int]] = None, data_i_index_ : Optional[Union[np.ndarray, NpArrayInt]] = None,   
data_ibegin_ : Optional[Union[Int, int]] = None, data_j_index_ : Optional[Union[np.ndarray, NpArrayInt]] = None,   
data_jbegin_ : Optional[Union[Int, int]] = None, data_ni_ : Optional[Union[Int, int]] = None,   
data_nj_ : Optional[Union[Int, int]] = None, dim_i_name_ : Optional[Union[str, String]] = None,   
dim_j_name_ : Optional[Union[str, String]] = None, domain_ref_ : Optional[Union[str, String]] = None,   
i_index_ : Optional[Union[np.ndarray, NpArrayInt]] = None, ibegin_ : Optional[Union[Int, int]] = None,   
j_index_ : Optional[Union[np.ndarray, NpArrayInt]] = None, jbegin_ : Optional[Union[Int, int]] = None,   
lat_name_ : Optional[Union[str, String]] = None, latvalue_1d_ : Optional[Union[np.ndarray, NpArray]] = None,   
latvalue_2d_ : Optional[Union[np.ndarray, NpArray]] = None, lon_name_ : Optional[Union[str, String]] = None,   
long_name_ : Optional[Union[str, String]] = None, lonvalue_1d_ : Optional[Union[np.ndarray, NpArray]] = None,   
lonvalue_2d_ : Optional[Union[np.ndarray, NpArray]] = None, mask_1d_ : Optional[Union[np.ndarray, NpArrayBool]] = None,   
mask_2d_ : Optional[Union[np.ndarray, NpArrayBool]] = None, name_ : Optional[Union[str, String]] = None,   
ni_ : Optional[Union[Int, int]] = None, ni_glo_ : Optional[Union[Int, int]] = None, nj_ : Optional[Union[Int, int]] = None,   
nj_glo_ : Optional[Union[Int, int]] = None, nvertex_ : Optional[Union[Int, int]] = None, nvertex_name_ : Optional[Union[str, String]] = None,   
prec_ : Optional[Union[Int, int]] = None, radius_ : Optional[Union[Double, float]] = None,   
standard_name_ : Optional[Union[str, String]] = None, type_ : Optional[Union[str, String]] = None):

  
  

  if area_ is not None:
    area_ = NpArray(area_)
    area_c = area_._c_value
    if len(area_.shape) != 2 : 
      raise ValueError('The array dimension should be 2')
    else : 
      shape = ctypes.c_int * len(area_.shape)
      shape_c = shape(*area_.shape)
      lib.cxios_set_domain_area(domain_hdl, area_c, shape_c)
  

  if area_1d_ is not None:
    area_1d_ = NpArray(area_1d_)
    area_1d_c = area_1d_._c_value
    if len(area_1d_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(area_1d_.shape)
      shape_c = shape(*area_1d_.shape)
      lib.cxios_set_domain_area_1d(domain_hdl, area_1d_c, shape_c)
  

  if area_2d_ is not None:
    area_2d_ = NpArray(area_2d_)
    area_2d_c = area_2d_._c_value
    if len(area_2d_.shape) != 2 : 
      raise ValueError('The array dimension should be 2')
    else : 
      shape = ctypes.c_int * len(area_2d_.shape)
      shape_c = shape(*area_2d_.shape)
      lib.cxios_set_domain_area_2d(domain_hdl, area_2d_c, shape_c)
  

  if bounds_lat_1d_ is not None:
    bounds_lat_1d_ = NpArray(bounds_lat_1d_)
    bounds_lat_1d_c = bounds_lat_1d_._c_value
    if len(bounds_lat_1d_.shape) != 2 : 
      raise ValueError('The array dimension should be 2')
    else : 
      shape = ctypes.c_int * len(bounds_lat_1d_.shape)
      shape_c = shape(*bounds_lat_1d_.shape)
      lib.cxios_set_domain_bounds_lat_1d(domain_hdl, bounds_lat_1d_c, shape_c)
  

  if bounds_lat_2d_ is not None:
    bounds_lat_2d_ = NpArray(bounds_lat_2d_)
    bounds_lat_2d_c = bounds_lat_2d_._c_value
    if len(bounds_lat_2d_.shape) != 3 : 
      raise ValueError('The array dimension should be 3')
    else : 
      shape = ctypes.c_int * len(bounds_lat_2d_.shape)
      shape_c = shape(*bounds_lat_2d_.shape)
      lib.cxios_set_domain_bounds_lat_2d(domain_hdl, bounds_lat_2d_c, shape_c)
  

  if bounds_lat_name_ is not None:
  
    bounds_lat_name_= String(bounds_lat_name_)
    bounds_lat_name_c = bounds_lat_name_._c_value
    len_bounds_lat_name_c = len(ctypes.string_at(bounds_lat_name_c))
    lib.cxios_set_domain_bounds_lat_name(domain_hdl, bounds_lat_name_c, len_bounds_lat_name_c)
    
  

  if bounds_lon_1d_ is not None:
    bounds_lon_1d_ = NpArray(bounds_lon_1d_)
    bounds_lon_1d_c = bounds_lon_1d_._c_value
    if len(bounds_lon_1d_.shape) != 2 : 
      raise ValueError('The array dimension should be 2')
    else : 
      shape = ctypes.c_int * len(bounds_lon_1d_.shape)
      shape_c = shape(*bounds_lon_1d_.shape)
      lib.cxios_set_domain_bounds_lon_1d(domain_hdl, bounds_lon_1d_c, shape_c)
  

  if bounds_lon_2d_ is not None:
    bounds_lon_2d_ = NpArray(bounds_lon_2d_)
    bounds_lon_2d_c = bounds_lon_2d_._c_value
    if len(bounds_lon_2d_.shape) != 3 : 
      raise ValueError('The array dimension should be 3')
    else : 
      shape = ctypes.c_int * len(bounds_lon_2d_.shape)
      shape_c = shape(*bounds_lon_2d_.shape)
      lib.cxios_set_domain_bounds_lon_2d(domain_hdl, bounds_lon_2d_c, shape_c)
  

  if bounds_lon_name_ is not None:
  
    bounds_lon_name_= String(bounds_lon_name_)
    bounds_lon_name_c = bounds_lon_name_._c_value
    len_bounds_lon_name_c = len(ctypes.string_at(bounds_lon_name_c))
    lib.cxios_set_domain_bounds_lon_name(domain_hdl, bounds_lon_name_c, len_bounds_lon_name_c)
    
  

  if chunking_weight_i_ is not None: 
  
    chunking_weight_i_ = Double(chunking_weight_i_)
    chunking_weight_i_c = chunking_weight_i_._c_value
    lib.cxios_set_domain_chunking_weight_i(domain_hdl, chunking_weight_i_c)
    
  

  if chunking_weight_j_ is not None: 
  
    chunking_weight_j_ = Double(chunking_weight_j_)
    chunking_weight_j_c = chunking_weight_j_._c_value
    lib.cxios_set_domain_chunking_weight_j(domain_hdl, chunking_weight_j_c)
    
  

  if comment_ is not None:
  
    comment_= String(comment_)
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_set_domain_comment(domain_hdl, comment_c, len_comment_c)
    
  

  if data_dim_ is not None: 
  
    data_dim_ = Int(data_dim_)
    data_dim_c = data_dim_._c_value
    lib.cxios_set_domain_data_dim(domain_hdl, data_dim_c)
    
  

  if data_i_index_ is not None:
    data_i_index_ = NpArrayInt(data_i_index_)
    data_i_index_c = data_i_index_._c_value
    if len(data_i_index_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(data_i_index_.shape)
      shape_c = shape(*data_i_index_.shape)
      lib.cxios_set_domain_data_i_index(domain_hdl, data_i_index_c, shape_c)
  

  if data_ibegin_ is not None: 
  
    data_ibegin_ = Int(data_ibegin_)
    data_ibegin_c = data_ibegin_._c_value
    lib.cxios_set_domain_data_ibegin(domain_hdl, data_ibegin_c)
    
  

  if data_j_index_ is not None:
    data_j_index_ = NpArrayInt(data_j_index_)
    data_j_index_c = data_j_index_._c_value
    if len(data_j_index_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(data_j_index_.shape)
      shape_c = shape(*data_j_index_.shape)
      lib.cxios_set_domain_data_j_index(domain_hdl, data_j_index_c, shape_c)
  

  if data_jbegin_ is not None: 
  
    data_jbegin_ = Int(data_jbegin_)
    data_jbegin_c = data_jbegin_._c_value
    lib.cxios_set_domain_data_jbegin(domain_hdl, data_jbegin_c)
    
  

  if data_ni_ is not None: 
  
    data_ni_ = Int(data_ni_)
    data_ni_c = data_ni_._c_value
    lib.cxios_set_domain_data_ni(domain_hdl, data_ni_c)
    
  

  if data_nj_ is not None: 
  
    data_nj_ = Int(data_nj_)
    data_nj_c = data_nj_._c_value
    lib.cxios_set_domain_data_nj(domain_hdl, data_nj_c)
    
  

  if dim_i_name_ is not None:
  
    dim_i_name_= String(dim_i_name_)
    dim_i_name_c = dim_i_name_._c_value
    len_dim_i_name_c = len(ctypes.string_at(dim_i_name_c))
    lib.cxios_set_domain_dim_i_name(domain_hdl, dim_i_name_c, len_dim_i_name_c)
    
  

  if dim_j_name_ is not None:
  
    dim_j_name_= String(dim_j_name_)
    dim_j_name_c = dim_j_name_._c_value
    len_dim_j_name_c = len(ctypes.string_at(dim_j_name_c))
    lib.cxios_set_domain_dim_j_name(domain_hdl, dim_j_name_c, len_dim_j_name_c)
    
  

  if domain_ref_ is not None:
  
    domain_ref_= String(domain_ref_)
    domain_ref_c = domain_ref_._c_value
    len_domain_ref_c = len(ctypes.string_at(domain_ref_c))
    lib.cxios_set_domain_domain_ref(domain_hdl, domain_ref_c, len_domain_ref_c)
    
  

  if i_index_ is not None:
    i_index_ = NpArrayInt(i_index_)
    i_index_c = i_index_._c_value
    if len(i_index_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(i_index_.shape)
      shape_c = shape(*i_index_.shape)
      lib.cxios_set_domain_i_index(domain_hdl, i_index_c, shape_c)
  

  if ibegin_ is not None: 
  
    ibegin_ = Int(ibegin_)
    ibegin_c = ibegin_._c_value
    lib.cxios_set_domain_ibegin(domain_hdl, ibegin_c)
    
  

  if j_index_ is not None:
    j_index_ = NpArrayInt(j_index_)
    j_index_c = j_index_._c_value
    if len(j_index_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(j_index_.shape)
      shape_c = shape(*j_index_.shape)
      lib.cxios_set_domain_j_index(domain_hdl, j_index_c, shape_c)
  

  if jbegin_ is not None: 
  
    jbegin_ = Int(jbegin_)
    jbegin_c = jbegin_._c_value
    lib.cxios_set_domain_jbegin(domain_hdl, jbegin_c)
    
  

  if lat_name_ is not None:
  
    lat_name_= String(lat_name_)
    lat_name_c = lat_name_._c_value
    len_lat_name_c = len(ctypes.string_at(lat_name_c))
    lib.cxios_set_domain_lat_name(domain_hdl, lat_name_c, len_lat_name_c)
    
  

  if latvalue_1d_ is not None:
    latvalue_1d_ = NpArray(latvalue_1d_)
    latvalue_1d_c = latvalue_1d_._c_value
    if len(latvalue_1d_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(latvalue_1d_.shape)
      shape_c = shape(*latvalue_1d_.shape)
      lib.cxios_set_domain_latvalue_1d(domain_hdl, latvalue_1d_c, shape_c)
  

  if latvalue_2d_ is not None:
    latvalue_2d_ = NpArray(latvalue_2d_)
    latvalue_2d_c = latvalue_2d_._c_value
    if len(latvalue_2d_.shape) != 2 : 
      raise ValueError('The array dimension should be 2')
    else : 
      shape = ctypes.c_int * len(latvalue_2d_.shape)
      shape_c = shape(*latvalue_2d_.shape)
      lib.cxios_set_domain_latvalue_2d(domain_hdl, latvalue_2d_c, shape_c)
  

  if lon_name_ is not None:
  
    lon_name_= String(lon_name_)
    lon_name_c = lon_name_._c_value
    len_lon_name_c = len(ctypes.string_at(lon_name_c))
    lib.cxios_set_domain_lon_name(domain_hdl, lon_name_c, len_lon_name_c)
    
  

  if long_name_ is not None:
  
    long_name_= String(long_name_)
    long_name_c = long_name_._c_value
    len_long_name_c = len(ctypes.string_at(long_name_c))
    lib.cxios_set_domain_long_name(domain_hdl, long_name_c, len_long_name_c)
    
  

  if lonvalue_1d_ is not None:
    lonvalue_1d_ = NpArray(lonvalue_1d_)
    lonvalue_1d_c = lonvalue_1d_._c_value
    if len(lonvalue_1d_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(lonvalue_1d_.shape)
      shape_c = shape(*lonvalue_1d_.shape)
      lib.cxios_set_domain_lonvalue_1d(domain_hdl, lonvalue_1d_c, shape_c)
  

  if lonvalue_2d_ is not None:
    lonvalue_2d_ = NpArray(lonvalue_2d_)
    lonvalue_2d_c = lonvalue_2d_._c_value
    if len(lonvalue_2d_.shape) != 2 : 
      raise ValueError('The array dimension should be 2')
    else : 
      shape = ctypes.c_int * len(lonvalue_2d_.shape)
      shape_c = shape(*lonvalue_2d_.shape)
      lib.cxios_set_domain_lonvalue_2d(domain_hdl, lonvalue_2d_c, shape_c)
  

  if mask_1d_ is not None:
    mask_1d_ = NpArrayBool(mask_1d_)
    mask_1d_c = mask_1d_._c_value
    if len(mask_1d_.shape) != 1 : 
      raise ValueError('The array dimension should be 1')
    else : 
      shape = ctypes.c_int * len(mask_1d_.shape)
      shape_c = shape(*mask_1d_.shape)
      lib.cxios_set_domain_mask_1d(domain_hdl, mask_1d_c, shape_c)
  

  if mask_2d_ is not None:
    mask_2d_ = NpArrayBool(mask_2d_)
    mask_2d_c = mask_2d_._c_value
    if len(mask_2d_.shape) != 2 : 
      raise ValueError('The array dimension should be 2')
    else : 
      shape = ctypes.c_int * len(mask_2d_.shape)
      shape_c = shape(*mask_2d_.shape)
      lib.cxios_set_domain_mask_2d(domain_hdl, mask_2d_c, shape_c)
  

  if name_ is not None:
  
    name_= String(name_)
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_set_domain_name(domain_hdl, name_c, len_name_c)
    
  

  if ni_ is not None: 
  
    ni_ = Int(ni_)
    ni_c = ni_._c_value
    lib.cxios_set_domain_ni(domain_hdl, ni_c)
    
  

  if ni_glo_ is not None: 
  
    ni_glo_ = Int(ni_glo_)
    ni_glo_c = ni_glo_._c_value
    lib.cxios_set_domain_ni_glo(domain_hdl, ni_glo_c)
    
  

  if nj_ is not None: 
  
    nj_ = Int(nj_)
    nj_c = nj_._c_value
    lib.cxios_set_domain_nj(domain_hdl, nj_c)
    
  

  if nj_glo_ is not None: 
  
    nj_glo_ = Int(nj_glo_)
    nj_glo_c = nj_glo_._c_value
    lib.cxios_set_domain_nj_glo(domain_hdl, nj_glo_c)
    
  

  if nvertex_ is not None: 
  
    nvertex_ = Int(nvertex_)
    nvertex_c = nvertex_._c_value
    lib.cxios_set_domain_nvertex(domain_hdl, nvertex_c)
    
  

  if nvertex_name_ is not None:
  
    nvertex_name_= String(nvertex_name_)
    nvertex_name_c = nvertex_name_._c_value
    len_nvertex_name_c = len(ctypes.string_at(nvertex_name_c))
    lib.cxios_set_domain_nvertex_name(domain_hdl, nvertex_name_c, len_nvertex_name_c)
    
  

  if prec_ is not None: 
  
    prec_ = Int(prec_)
    prec_c = prec_._c_value
    lib.cxios_set_domain_prec(domain_hdl, prec_c)
    
  

  if radius_ is not None: 
  
    radius_ = Double(radius_)
    radius_c = radius_._c_value
    lib.cxios_set_domain_radius(domain_hdl, radius_c)
    
  

  if standard_name_ is not None:
  
    standard_name_= String(standard_name_)
    standard_name_c = standard_name_._c_value
    len_standard_name_c = len(ctypes.string_at(standard_name_c))
    lib.cxios_set_domain_standard_name(domain_hdl, standard_name_c, len_standard_name_c)
    
  

  if type_ is not None:
  
    type_= String(type_)
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_set_domain_type(domain_hdl, type_c, len_type_c)
    
  
  return 



@typecheck
def get_domain_attr(domain_id : Union[String, str], area : Optional[NpArray] = None, area_1d : Optional[NpArray] = None,   
area_2d : Optional[NpArray] = None, bounds_lat_1d : Optional[NpArray] = None, bounds_lat_2d : Optional[NpArray] = None,   
bounds_lat_name : Optional[String] = None, bounds_lon_1d : Optional[NpArray] = None, bounds_lon_2d : Optional[NpArray] = None,   
bounds_lon_name : Optional[String] = None, chunking_weight_i : Optional[Double] = None, chunking_weight_j : Optional[Double] = None,   
comment : Optional[String] = None, data_dim : Optional[Int] = None, data_i_index : Optional[NpArrayInt] = None,   
data_ibegin : Optional[Int] = None, data_j_index : Optional[NpArrayInt] = None, data_jbegin : Optional[Int] = None,   
data_ni : Optional[Int] = None, data_nj : Optional[Int] = None, dim_i_name : Optional[String] = None,   
dim_j_name : Optional[String] = None, domain_ref : Optional[String] = None, i_index : Optional[NpArrayInt] = None,   
ibegin : Optional[Int] = None, j_index : Optional[NpArrayInt] = None, jbegin : Optional[Int] = None,   
lat_name : Optional[String] = None, latvalue_1d : Optional[NpArray] = None, latvalue_2d : Optional[NpArray] = None,   
lon_name : Optional[String] = None, long_name : Optional[String] = None, lonvalue_1d : Optional[NpArray] = None,   
lonvalue_2d : Optional[NpArray] = None, mask_1d : Optional[NpArrayBool] = None, mask_2d : Optional[NpArrayBool] = None,   
name : Optional[String] = None, ni : Optional[Int] = None, ni_glo : Optional[Int] = None, nj : Optional[Int] = None,   
nj_glo : Optional[Int] = None, nvertex : Optional[Int] = None, nvertex_name : Optional[String] = None,   
prec : Optional[Int] = None, radius : Optional[Double] = None, standard_name : Optional[String] = None,   
type : Optional[String] = None):

  
  domain_hdl = Domain()
  

  get_domain_handle(domain_id, domain_hdl)
  get_domain_attr_hdl_(domain_hdl, area, area_1d, area_2d, bounds_lat_1d, bounds_lat_2d, bounds_lat_name, bounds_lon_1d,  
   bounds_lon_2d, bounds_lon_name, chunking_weight_i, chunking_weight_j, comment, data_dim, data_i_index,  
   data_ibegin, data_j_index, data_jbegin, data_ni, data_nj, dim_i_name, dim_j_name, domain_ref,  
   i_index, ibegin, j_index, jbegin, lat_name, latvalue_1d, latvalue_2d, lon_name, long_name,  
   lonvalue_1d, lonvalue_2d, mask_1d, mask_2d, name, ni, ni_glo, nj, nj_glo, nvertex, nvertex_name,  
   prec, radius, standard_name, type)
  return 



@typecheck
def get_domain_attr_hdl(domain_hdl : Domain, area : Optional[NpArray] = None, area_1d : Optional[NpArray] = None,   
area_2d : Optional[NpArray] = None, bounds_lat_1d : Optional[NpArray] = None, bounds_lat_2d : Optional[NpArray] = None,   
bounds_lat_name : Optional[String] = None, bounds_lon_1d : Optional[NpArray] = None, bounds_lon_2d : Optional[NpArray] = None,   
bounds_lon_name : Optional[String] = None, chunking_weight_i : Optional[Double] = None, chunking_weight_j : Optional[Double] = None,   
comment : Optional[String] = None, data_dim : Optional[Int] = None, data_i_index : Optional[NpArrayInt] = None,   
data_ibegin : Optional[Int] = None, data_j_index : Optional[NpArrayInt] = None, data_jbegin : Optional[Int] = None,   
data_ni : Optional[Int] = None, data_nj : Optional[Int] = None, dim_i_name : Optional[String] = None,   
dim_j_name : Optional[String] = None, domain_ref : Optional[String] = None, i_index : Optional[NpArrayInt] = None,   
ibegin : Optional[Int] = None, j_index : Optional[NpArrayInt] = None, jbegin : Optional[Int] = None,   
lat_name : Optional[String] = None, latvalue_1d : Optional[NpArray] = None, latvalue_2d : Optional[NpArray] = None,   
lon_name : Optional[String] = None, long_name : Optional[String] = None, lonvalue_1d : Optional[NpArray] = None,   
lonvalue_2d : Optional[NpArray] = None, mask_1d : Optional[NpArrayBool] = None, mask_2d : Optional[NpArrayBool] = None,   
name : Optional[String] = None, ni : Optional[Int] = None, ni_glo : Optional[Int] = None, nj : Optional[Int] = None,   
nj_glo : Optional[Int] = None, nvertex : Optional[Int] = None, nvertex_name : Optional[String] = None,   
prec : Optional[Int] = None, radius : Optional[Double] = None, standard_name : Optional[String] = None,   
type : Optional[String] = None):

  
  get_domain_attr_hdl_(domain_hdl, area, area_1d, area_2d, bounds_lat_1d, bounds_lat_2d, bounds_lat_name, bounds_lon_1d,  
   bounds_lon_2d, bounds_lon_name, chunking_weight_i, chunking_weight_j, comment, data_dim, data_i_index,  
   data_ibegin, data_j_index, data_jbegin, data_ni, data_nj, dim_i_name, dim_j_name, domain_ref,  
   i_index, ibegin, j_index, jbegin, lat_name, latvalue_1d, latvalue_2d, lon_name, long_name,  
   lonvalue_1d, lonvalue_2d, mask_1d, mask_2d, name, ni, ni_glo, nj, nj_glo, nvertex, nvertex_name,  
   prec, radius, standard_name, type)
  return 



@typecheck
def get_domain_attr_hdl_(domain_hdl : Domain, area_ : Optional[NpArray] = None, area_1d_ : Optional[NpArray] = None,   
area_2d_ : Optional[NpArray] = None, bounds_lat_1d_ : Optional[NpArray] = None, bounds_lat_2d_ : Optional[NpArray] = None,   
bounds_lat_name_ : Optional[String] = None, bounds_lon_1d_ : Optional[NpArray] = None, bounds_lon_2d_ : Optional[NpArray] = None,   
bounds_lon_name_ : Optional[String] = None, chunking_weight_i_ : Optional[Double] = None, chunking_weight_j_ : Optional[Double] = None,   
comment_ : Optional[String] = None, data_dim_ : Optional[Int] = None, data_i_index_ : Optional[NpArrayInt] = None,   
data_ibegin_ : Optional[Int] = None, data_j_index_ : Optional[NpArrayInt] = None, data_jbegin_ : Optional[Int] = None,   
data_ni_ : Optional[Int] = None, data_nj_ : Optional[Int] = None, dim_i_name_ : Optional[String] = None,   
dim_j_name_ : Optional[String] = None, domain_ref_ : Optional[String] = None, i_index_ : Optional[NpArrayInt] = None,   
ibegin_ : Optional[Int] = None, j_index_ : Optional[NpArrayInt] = None, jbegin_ : Optional[Int] = None,   
lat_name_ : Optional[String] = None, latvalue_1d_ : Optional[NpArray] = None, latvalue_2d_ : Optional[NpArray] = None,   
lon_name_ : Optional[String] = None, long_name_ : Optional[String] = None, lonvalue_1d_ : Optional[NpArray] = None,   
lonvalue_2d_ : Optional[NpArray] = None, mask_1d_ : Optional[NpArrayBool] = None, mask_2d_ : Optional[NpArrayBool] = None,   
name_ : Optional[String] = None, ni_ : Optional[Int] = None, ni_glo_ : Optional[Int] = None,   
nj_ : Optional[Int] = None, nj_glo_ : Optional[Int] = None, nvertex_ : Optional[Int] = None,   
nvertex_name_ : Optional[String] = None, prec_ : Optional[Int] = None, radius_ : Optional[Double] = None,   
standard_name_ : Optional[String] = None, type_ : Optional[String] = None):

  
  

  if area_ is not None:
    if area_.shape_is_needed: 
      shape_c = (ctypes.c_int * 2)()
      lib.cxios_domain_area_shape(domain_hdl, shape_c)
      area_.arr = np.zeros((shape_c[0], shape_c[1]) , order = 'F')
      lib.cxios_get_domain_area(domain_hdl, area_._c_value, shape_c)
    else: 
      area_c = area_._c_value
      if len(area_.shape) != 2 : 
        raise ValueError('The array dimension should be 2')
      else : 
        shape_c = NpArrayInt(area_.shape)._c_value
        lib.cxios_get_domain_area(domain_hdl, area_c, shape_c)
  

  if area_1d_ is not None:
    if area_1d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_domain_area_1d_shape(domain_hdl, shape_c)
      area_1d_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_domain_area_1d(domain_hdl, area_1d_._c_value, shape_c)
    else: 
      area_1d_c = area_1d_._c_value
      if len(area_1d_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(area_1d_.shape)._c_value
        lib.cxios_get_domain_area_1d(domain_hdl, area_1d_c, shape_c)
  

  if area_2d_ is not None:
    if area_2d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 2)()
      lib.cxios_domain_area_2d_shape(domain_hdl, shape_c)
      area_2d_.arr = np.zeros((shape_c[0], shape_c[1]) , order = 'F')
      lib.cxios_get_domain_area_2d(domain_hdl, area_2d_._c_value, shape_c)
    else: 
      area_2d_c = area_2d_._c_value
      if len(area_2d_.shape) != 2 : 
        raise ValueError('The array dimension should be 2')
      else : 
        shape_c = NpArrayInt(area_2d_.shape)._c_value
        lib.cxios_get_domain_area_2d(domain_hdl, area_2d_c, shape_c)
  

  if bounds_lat_1d_ is not None:
    if bounds_lat_1d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 2)()
      lib.cxios_domain_bounds_lat_1d_shape(domain_hdl, shape_c)
      bounds_lat_1d_.arr = np.zeros((shape_c[0], shape_c[1]) , order = 'F')
      lib.cxios_get_domain_bounds_lat_1d(domain_hdl, bounds_lat_1d_._c_value, shape_c)
    else: 
      bounds_lat_1d_c = bounds_lat_1d_._c_value
      if len(bounds_lat_1d_.shape) != 2 : 
        raise ValueError('The array dimension should be 2')
      else : 
        shape_c = NpArrayInt(bounds_lat_1d_.shape)._c_value
        lib.cxios_get_domain_bounds_lat_1d(domain_hdl, bounds_lat_1d_c, shape_c)
  

  if bounds_lat_2d_ is not None:
    if bounds_lat_2d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 3)()
      lib.cxios_domain_bounds_lat_2d_shape(domain_hdl, shape_c)
      bounds_lat_2d_.arr = np.zeros((shape_c[0], shape_c[1], shape_c[2]) , order = 'F')
      lib.cxios_get_domain_bounds_lat_2d(domain_hdl, bounds_lat_2d_._c_value, shape_c)
    else: 
      bounds_lat_2d_c = bounds_lat_2d_._c_value
      if len(bounds_lat_2d_.shape) != 3 : 
        raise ValueError('The array dimension should be 3')
      else : 
        shape_c = NpArrayInt(bounds_lat_2d_.shape)._c_value
        lib.cxios_get_domain_bounds_lat_2d(domain_hdl, bounds_lat_2d_c, shape_c)
  

  if bounds_lat_name_ is not None:
  
    bounds_lat_name_c = bounds_lat_name_._c_value
    len_bounds_lat_name_c = len(ctypes.string_at(bounds_lat_name_c))
    lib.cxios_get_domain_bounds_lat_name(domain_hdl, bounds_lat_name_c, len_bounds_lat_name_c)
    
  

  if bounds_lon_1d_ is not None:
    if bounds_lon_1d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 2)()
      lib.cxios_domain_bounds_lon_1d_shape(domain_hdl, shape_c)
      bounds_lon_1d_.arr = np.zeros((shape_c[0], shape_c[1]) , order = 'F')
      lib.cxios_get_domain_bounds_lon_1d(domain_hdl, bounds_lon_1d_._c_value, shape_c)
    else: 
      bounds_lon_1d_c = bounds_lon_1d_._c_value
      if len(bounds_lon_1d_.shape) != 2 : 
        raise ValueError('The array dimension should be 2')
      else : 
        shape_c = NpArrayInt(bounds_lon_1d_.shape)._c_value
        lib.cxios_get_domain_bounds_lon_1d(domain_hdl, bounds_lon_1d_c, shape_c)
  

  if bounds_lon_2d_ is not None:
    if bounds_lon_2d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 3)()
      lib.cxios_domain_bounds_lon_2d_shape(domain_hdl, shape_c)
      bounds_lon_2d_.arr = np.zeros((shape_c[0], shape_c[1], shape_c[2]) , order = 'F')
      lib.cxios_get_domain_bounds_lon_2d(domain_hdl, bounds_lon_2d_._c_value, shape_c)
    else: 
      bounds_lon_2d_c = bounds_lon_2d_._c_value
      if len(bounds_lon_2d_.shape) != 3 : 
        raise ValueError('The array dimension should be 3')
      else : 
        shape_c = NpArrayInt(bounds_lon_2d_.shape)._c_value
        lib.cxios_get_domain_bounds_lon_2d(domain_hdl, bounds_lon_2d_c, shape_c)
  

  if bounds_lon_name_ is not None:
  
    bounds_lon_name_c = bounds_lon_name_._c_value
    len_bounds_lon_name_c = len(ctypes.string_at(bounds_lon_name_c))
    lib.cxios_get_domain_bounds_lon_name(domain_hdl, bounds_lon_name_c, len_bounds_lon_name_c)
    
  

  if chunking_weight_i_ is not None: 
  
    chunking_weight_i_c = chunking_weight_i_._c_value
    lib.cxios_get_domain_chunking_weight_i(domain_hdl, chunking_weight_i_c)
    
  

  if chunking_weight_j_ is not None: 
  
    chunking_weight_j_c = chunking_weight_j_._c_value
    lib.cxios_get_domain_chunking_weight_j(domain_hdl, chunking_weight_j_c)
    
  

  if comment_ is not None:
  
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_get_domain_comment(domain_hdl, comment_c, len_comment_c)
    
  

  if data_dim_ is not None: 
  
    data_dim_c = data_dim_._c_value
    lib.cxios_get_domain_data_dim(domain_hdl, data_dim_c)
    
  

  if data_i_index_ is not None:
    if data_i_index_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_domain_data_i_index_shape(domain_hdl, shape_c)
      data_i_index_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_domain_data_i_index(domain_hdl, data_i_index_._c_value, shape_c)
    else: 
      data_i_index_c = data_i_index_._c_value
      if len(data_i_index_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(data_i_index_.shape)._c_value
        lib.cxios_get_domain_data_i_index(domain_hdl, data_i_index_c, shape_c)
  

  if data_ibegin_ is not None: 
  
    data_ibegin_c = data_ibegin_._c_value
    lib.cxios_get_domain_data_ibegin(domain_hdl, data_ibegin_c)
    
  

  if data_j_index_ is not None:
    if data_j_index_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_domain_data_j_index_shape(domain_hdl, shape_c)
      data_j_index_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_domain_data_j_index(domain_hdl, data_j_index_._c_value, shape_c)
    else: 
      data_j_index_c = data_j_index_._c_value
      if len(data_j_index_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(data_j_index_.shape)._c_value
        lib.cxios_get_domain_data_j_index(domain_hdl, data_j_index_c, shape_c)
  

  if data_jbegin_ is not None: 
  
    data_jbegin_c = data_jbegin_._c_value
    lib.cxios_get_domain_data_jbegin(domain_hdl, data_jbegin_c)
    
  

  if data_ni_ is not None: 
  
    data_ni_c = data_ni_._c_value
    lib.cxios_get_domain_data_ni(domain_hdl, data_ni_c)
    
  

  if data_nj_ is not None: 
  
    data_nj_c = data_nj_._c_value
    lib.cxios_get_domain_data_nj(domain_hdl, data_nj_c)
    
  

  if dim_i_name_ is not None:
  
    dim_i_name_c = dim_i_name_._c_value
    len_dim_i_name_c = len(ctypes.string_at(dim_i_name_c))
    lib.cxios_get_domain_dim_i_name(domain_hdl, dim_i_name_c, len_dim_i_name_c)
    
  

  if dim_j_name_ is not None:
  
    dim_j_name_c = dim_j_name_._c_value
    len_dim_j_name_c = len(ctypes.string_at(dim_j_name_c))
    lib.cxios_get_domain_dim_j_name(domain_hdl, dim_j_name_c, len_dim_j_name_c)
    
  

  if domain_ref_ is not None:
  
    domain_ref_c = domain_ref_._c_value
    len_domain_ref_c = len(ctypes.string_at(domain_ref_c))
    lib.cxios_get_domain_domain_ref(domain_hdl, domain_ref_c, len_domain_ref_c)
    
  

  if i_index_ is not None:
    if i_index_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_domain_i_index_shape(domain_hdl, shape_c)
      i_index_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_domain_i_index(domain_hdl, i_index_._c_value, shape_c)
    else: 
      i_index_c = i_index_._c_value
      if len(i_index_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(i_index_.shape)._c_value
        lib.cxios_get_domain_i_index(domain_hdl, i_index_c, shape_c)
  

  if ibegin_ is not None: 
  
    ibegin_c = ibegin_._c_value
    lib.cxios_get_domain_ibegin(domain_hdl, ibegin_c)
    
  

  if j_index_ is not None:
    if j_index_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_domain_j_index_shape(domain_hdl, shape_c)
      j_index_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_domain_j_index(domain_hdl, j_index_._c_value, shape_c)
    else: 
      j_index_c = j_index_._c_value
      if len(j_index_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(j_index_.shape)._c_value
        lib.cxios_get_domain_j_index(domain_hdl, j_index_c, shape_c)
  

  if jbegin_ is not None: 
  
    jbegin_c = jbegin_._c_value
    lib.cxios_get_domain_jbegin(domain_hdl, jbegin_c)
    
  

  if lat_name_ is not None:
  
    lat_name_c = lat_name_._c_value
    len_lat_name_c = len(ctypes.string_at(lat_name_c))
    lib.cxios_get_domain_lat_name(domain_hdl, lat_name_c, len_lat_name_c)
    
  

  if latvalue_1d_ is not None:
    if latvalue_1d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_domain_latvalue_1d_shape(domain_hdl, shape_c)
      latvalue_1d_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_domain_latvalue_1d(domain_hdl, latvalue_1d_._c_value, shape_c)
    else: 
      latvalue_1d_c = latvalue_1d_._c_value
      if len(latvalue_1d_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(latvalue_1d_.shape)._c_value
        lib.cxios_get_domain_latvalue_1d(domain_hdl, latvalue_1d_c, shape_c)
  

  if latvalue_2d_ is not None:
    if latvalue_2d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 2)()
      lib.cxios_domain_latvalue_2d_shape(domain_hdl, shape_c)
      latvalue_2d_.arr = np.zeros((shape_c[0], shape_c[1]) , order = 'F')
      lib.cxios_get_domain_latvalue_2d(domain_hdl, latvalue_2d_._c_value, shape_c)
    else: 
      latvalue_2d_c = latvalue_2d_._c_value
      if len(latvalue_2d_.shape) != 2 : 
        raise ValueError('The array dimension should be 2')
      else : 
        shape_c = NpArrayInt(latvalue_2d_.shape)._c_value
        lib.cxios_get_domain_latvalue_2d(domain_hdl, latvalue_2d_c, shape_c)
  

  if lon_name_ is not None:
  
    lon_name_c = lon_name_._c_value
    len_lon_name_c = len(ctypes.string_at(lon_name_c))
    lib.cxios_get_domain_lon_name(domain_hdl, lon_name_c, len_lon_name_c)
    
  

  if long_name_ is not None:
  
    long_name_c = long_name_._c_value
    len_long_name_c = len(ctypes.string_at(long_name_c))
    lib.cxios_get_domain_long_name(domain_hdl, long_name_c, len_long_name_c)
    
  

  if lonvalue_1d_ is not None:
    if lonvalue_1d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_domain_lonvalue_1d_shape(domain_hdl, shape_c)
      lonvalue_1d_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_domain_lonvalue_1d(domain_hdl, lonvalue_1d_._c_value, shape_c)
    else: 
      lonvalue_1d_c = lonvalue_1d_._c_value
      if len(lonvalue_1d_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(lonvalue_1d_.shape)._c_value
        lib.cxios_get_domain_lonvalue_1d(domain_hdl, lonvalue_1d_c, shape_c)
  

  if lonvalue_2d_ is not None:
    if lonvalue_2d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 2)()
      lib.cxios_domain_lonvalue_2d_shape(domain_hdl, shape_c)
      lonvalue_2d_.arr = np.zeros((shape_c[0], shape_c[1]) , order = 'F')
      lib.cxios_get_domain_lonvalue_2d(domain_hdl, lonvalue_2d_._c_value, shape_c)
    else: 
      lonvalue_2d_c = lonvalue_2d_._c_value
      if len(lonvalue_2d_.shape) != 2 : 
        raise ValueError('The array dimension should be 2')
      else : 
        shape_c = NpArrayInt(lonvalue_2d_.shape)._c_value
        lib.cxios_get_domain_lonvalue_2d(domain_hdl, lonvalue_2d_c, shape_c)
  

  if mask_1d_ is not None:
    if mask_1d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 1)()
      lib.cxios_domain_mask_1d_shape(domain_hdl, shape_c)
      mask_1d_.arr = np.zeros(shape_c[0], order = 'F')
      lib.cxios_get_domain_mask_1d(domain_hdl, mask_1d_._c_value, shape_c)
    else: 
      mask_1d_c = mask_1d_._c_value
      if len(mask_1d_.shape) != 1 : 
        raise ValueError('The array dimension should be 1')
      else : 
        shape_c = NpArrayInt(mask_1d_.shape)._c_value
        lib.cxios_get_domain_mask_1d(domain_hdl, mask_1d_c, shape_c)
  

  if mask_2d_ is not None:
    if mask_2d_.shape_is_needed: 
      shape_c = (ctypes.c_int * 2)()
      lib.cxios_domain_mask_2d_shape(domain_hdl, shape_c)
      mask_2d_.arr = np.zeros((shape_c[0], shape_c[1]) , order = 'F')
      lib.cxios_get_domain_mask_2d(domain_hdl, mask_2d_._c_value, shape_c)
    else: 
      mask_2d_c = mask_2d_._c_value
      if len(mask_2d_.shape) != 2 : 
        raise ValueError('The array dimension should be 2')
      else : 
        shape_c = NpArrayInt(mask_2d_.shape)._c_value
        lib.cxios_get_domain_mask_2d(domain_hdl, mask_2d_c, shape_c)
  

  if name_ is not None:
  
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_get_domain_name(domain_hdl, name_c, len_name_c)
    
  

  if ni_ is not None: 
  
    ni_c = ni_._c_value
    lib.cxios_get_domain_ni(domain_hdl, ni_c)
    
  

  if ni_glo_ is not None: 
  
    ni_glo_c = ni_glo_._c_value
    lib.cxios_get_domain_ni_glo(domain_hdl, ni_glo_c)
    
  

  if nj_ is not None: 
  
    nj_c = nj_._c_value
    lib.cxios_get_domain_nj(domain_hdl, nj_c)
    
  

  if nj_glo_ is not None: 
  
    nj_glo_c = nj_glo_._c_value
    lib.cxios_get_domain_nj_glo(domain_hdl, nj_glo_c)
    
  

  if nvertex_ is not None: 
  
    nvertex_c = nvertex_._c_value
    lib.cxios_get_domain_nvertex(domain_hdl, nvertex_c)
    
  

  if nvertex_name_ is not None:
  
    nvertex_name_c = nvertex_name_._c_value
    len_nvertex_name_c = len(ctypes.string_at(nvertex_name_c))
    lib.cxios_get_domain_nvertex_name(domain_hdl, nvertex_name_c, len_nvertex_name_c)
    
  

  if prec_ is not None: 
  
    prec_c = prec_._c_value
    lib.cxios_get_domain_prec(domain_hdl, prec_c)
    
  

  if radius_ is not None: 
  
    radius_c = radius_._c_value
    lib.cxios_get_domain_radius(domain_hdl, radius_c)
    
  

  if standard_name_ is not None:
  
    standard_name_c = standard_name_._c_value
    len_standard_name_c = len(ctypes.string_at(standard_name_c))
    lib.cxios_get_domain_standard_name(domain_hdl, standard_name_c, len_standard_name_c)
    
  

  if type_ is not None:
  
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_get_domain_type(domain_hdl, type_c, len_type_c)
    
  
  return 



@typecheck
def is_defined_domain_attr(domain_id : String, area : Optional[Bool] = None, area_1d : Optional[Bool] = None, area_2d : Optional[Bool] = None,   
bounds_lat_1d : Optional[Bool] = None, bounds_lat_2d : Optional[Bool] = None, bounds_lat_name : Optional[Bool] = None,   
bounds_lon_1d : Optional[Bool] = None, bounds_lon_2d : Optional[Bool] = None, bounds_lon_name : Optional[Bool] = None,   
chunking_weight_i : Optional[Bool] = None, chunking_weight_j : Optional[Bool] = None, comment : Optional[Bool] = None,   
data_dim : Optional[Bool] = None, data_i_index : Optional[Bool] = None, data_ibegin : Optional[Bool] = None,   
data_j_index : Optional[Bool] = None, data_jbegin : Optional[Bool] = None, data_ni : Optional[Bool] = None,   
data_nj : Optional[Bool] = None, dim_i_name : Optional[Bool] = None, dim_j_name : Optional[Bool] = None,   
domain_ref : Optional[Bool] = None, i_index : Optional[Bool] = None, ibegin : Optional[Bool] = None,   
j_index : Optional[Bool] = None, jbegin : Optional[Bool] = None, lat_name : Optional[Bool] = None,   
latvalue_1d : Optional[Bool] = None, latvalue_2d : Optional[Bool] = None, lon_name : Optional[Bool] = None,   
long_name : Optional[Bool] = None, lonvalue_1d : Optional[Bool] = None, lonvalue_2d : Optional[Bool] = None,   
mask_1d : Optional[Bool] = None, mask_2d : Optional[Bool] = None, name : Optional[Bool] = None,   
ni : Optional[Bool] = None, ni_glo : Optional[Bool] = None, nj : Optional[Bool] = None, nj_glo : Optional[Bool] = None,   
nvertex : Optional[Bool] = None, nvertex_name : Optional[Bool] = None, prec : Optional[Bool] = None,   
radius : Optional[Bool] = None, standard_name : Optional[Bool] = None, type : Optional[Bool] = None):

  
  domain_hdl = Domain()
  

  get_domain_handle(domain_id, domain_hdl)
  is_defined_domain_attr_hdl_(domain_hdl, area, area_1d, area_2d, bounds_lat_1d, bounds_lat_2d, bounds_lat_name, bounds_lon_1d,  
   bounds_lon_2d, bounds_lon_name, chunking_weight_i, chunking_weight_j, comment, data_dim, data_i_index,  
   data_ibegin, data_j_index, data_jbegin, data_ni, data_nj, dim_i_name, dim_j_name, domain_ref,  
   i_index, ibegin, j_index, jbegin, lat_name, latvalue_1d, latvalue_2d, lon_name, long_name,  
   lonvalue_1d, lonvalue_2d, mask_1d, mask_2d, name, ni, ni_glo, nj, nj_glo, nvertex, nvertex_name,  
   prec, radius, standard_name, type)
  return 



@typecheck
def is_defined_domain_attr_hdl(domain_hdl : Domain, area : Optional[Bool] = None, area_1d : Optional[Bool] = None, area_2d : Optional[Bool] = None,   
bounds_lat_1d : Optional[Bool] = None, bounds_lat_2d : Optional[Bool] = None, bounds_lat_name : Optional[Bool] = None,   
bounds_lon_1d : Optional[Bool] = None, bounds_lon_2d : Optional[Bool] = None, bounds_lon_name : Optional[Bool] = None,   
chunking_weight_i : Optional[Bool] = None, chunking_weight_j : Optional[Bool] = None, comment : Optional[Bool] = None,   
data_dim : Optional[Bool] = None, data_i_index : Optional[Bool] = None, data_ibegin : Optional[Bool] = None,   
data_j_index : Optional[Bool] = None, data_jbegin : Optional[Bool] = None, data_ni : Optional[Bool] = None,   
data_nj : Optional[Bool] = None, dim_i_name : Optional[Bool] = None, dim_j_name : Optional[Bool] = None,   
domain_ref : Optional[Bool] = None, i_index : Optional[Bool] = None, ibegin : Optional[Bool] = None,   
j_index : Optional[Bool] = None, jbegin : Optional[Bool] = None, lat_name : Optional[Bool] = None,   
latvalue_1d : Optional[Bool] = None, latvalue_2d : Optional[Bool] = None, lon_name : Optional[Bool] = None,   
long_name : Optional[Bool] = None, lonvalue_1d : Optional[Bool] = None, lonvalue_2d : Optional[Bool] = None,   
mask_1d : Optional[Bool] = None, mask_2d : Optional[Bool] = None, name : Optional[Bool] = None,   
ni : Optional[Bool] = None, ni_glo : Optional[Bool] = None, nj : Optional[Bool] = None, nj_glo : Optional[Bool] = None,   
nvertex : Optional[Bool] = None, nvertex_name : Optional[Bool] = None, prec : Optional[Bool] = None,   
radius : Optional[Bool] = None, standard_name : Optional[Bool] = None, type : Optional[Bool] = None):

  
  is_defined_domain_attr_hdl_(domain_hdl, area, area_1d, area_2d, bounds_lat_1d, bounds_lat_2d, bounds_lat_name, bounds_lon_1d,  
   bounds_lon_2d, bounds_lon_name, chunking_weight_i, chunking_weight_j, comment, data_dim, data_i_index,  
   data_ibegin, data_j_index, data_jbegin, data_ni, data_nj, dim_i_name, dim_j_name, domain_ref,  
   i_index, ibegin, j_index, jbegin, lat_name, latvalue_1d, latvalue_2d, lon_name, long_name,  
   lonvalue_1d, lonvalue_2d, mask_1d, mask_2d, name, ni, ni_glo, nj, nj_glo, nvertex, nvertex_name,  
   prec, radius, standard_name, type)
  return 



@typecheck
def is_defined_domain_attr_hdl_(domain_hdl : Domain, area_ : Optional[Bool] = None, area_1d_ : Optional[Bool] = None, area_2d_ : Optional[Bool] = None,   
bounds_lat_1d_ : Optional[Bool] = None, bounds_lat_2d_ : Optional[Bool] = None, bounds_lat_name_ : Optional[Bool] = None,   
bounds_lon_1d_ : Optional[Bool] = None, bounds_lon_2d_ : Optional[Bool] = None, bounds_lon_name_ : Optional[Bool] = None,   
chunking_weight_i_ : Optional[Bool] = None, chunking_weight_j_ : Optional[Bool] = None, comment_ : Optional[Bool] = None,   
data_dim_ : Optional[Bool] = None, data_i_index_ : Optional[Bool] = None, data_ibegin_ : Optional[Bool] = None,   
data_j_index_ : Optional[Bool] = None, data_jbegin_ : Optional[Bool] = None, data_ni_ : Optional[Bool] = None,   
data_nj_ : Optional[Bool] = None, dim_i_name_ : Optional[Bool] = None, dim_j_name_ : Optional[Bool] = None,   
domain_ref_ : Optional[Bool] = None, i_index_ : Optional[Bool] = None, ibegin_ : Optional[Bool] = None,   
j_index_ : Optional[Bool] = None, jbegin_ : Optional[Bool] = None, lat_name_ : Optional[Bool] = None,   
latvalue_1d_ : Optional[Bool] = None, latvalue_2d_ : Optional[Bool] = None, lon_name_ : Optional[Bool] = None,   
long_name_ : Optional[Bool] = None, lonvalue_1d_ : Optional[Bool] = None, lonvalue_2d_ : Optional[Bool] = None,   
mask_1d_ : Optional[Bool] = None, mask_2d_ : Optional[Bool] = None, name_ : Optional[Bool] = None,   
ni_ : Optional[Bool] = None, ni_glo_ : Optional[Bool] = None, nj_ : Optional[Bool] = None,   
nj_glo_ : Optional[Bool] = None, nvertex_ : Optional[Bool] = None, nvertex_name_ : Optional[Bool] = None,   
prec_ : Optional[Bool] = None, radius_ : Optional[Bool] = None, standard_name_ : Optional[Bool] = None,   
type_ : Optional[Bool] = None):

  
  

  if area_  is not None:
    area_c = lib.cxios_is_defined_domain_area(domain_hdl)
    area_._c_value = ctypes.c_bool(area_c)
    
  

  if area_1d_  is not None:
    area_1d_c = lib.cxios_is_defined_domain_area_1d(domain_hdl)
    area_1d_._c_value = ctypes.c_bool(area_1d_c)
    
  

  if area_2d_  is not None:
    area_2d_c = lib.cxios_is_defined_domain_area_2d(domain_hdl)
    area_2d_._c_value = ctypes.c_bool(area_2d_c)
    
  

  if bounds_lat_1d_  is not None:
    bounds_lat_1d_c = lib.cxios_is_defined_domain_bounds_lat_1d(domain_hdl)
    bounds_lat_1d_._c_value = ctypes.c_bool(bounds_lat_1d_c)
    
  

  if bounds_lat_2d_  is not None:
    bounds_lat_2d_c = lib.cxios_is_defined_domain_bounds_lat_2d(domain_hdl)
    bounds_lat_2d_._c_value = ctypes.c_bool(bounds_lat_2d_c)
    
  

  if bounds_lat_name_  is not None:
    bounds_lat_name_c = lib.cxios_is_defined_domain_bounds_lat_name(domain_hdl)
    bounds_lat_name_._c_value = ctypes.c_bool(bounds_lat_name_c)
    
  

  if bounds_lon_1d_  is not None:
    bounds_lon_1d_c = lib.cxios_is_defined_domain_bounds_lon_1d(domain_hdl)
    bounds_lon_1d_._c_value = ctypes.c_bool(bounds_lon_1d_c)
    
  

  if bounds_lon_2d_  is not None:
    bounds_lon_2d_c = lib.cxios_is_defined_domain_bounds_lon_2d(domain_hdl)
    bounds_lon_2d_._c_value = ctypes.c_bool(bounds_lon_2d_c)
    
  

  if bounds_lon_name_  is not None:
    bounds_lon_name_c = lib.cxios_is_defined_domain_bounds_lon_name(domain_hdl)
    bounds_lon_name_._c_value = ctypes.c_bool(bounds_lon_name_c)
    
  

  if chunking_weight_i_  is not None:
    chunking_weight_i_c = lib.cxios_is_defined_domain_chunking_weight_i(domain_hdl)
    chunking_weight_i_._c_value = ctypes.c_bool(chunking_weight_i_c)
    
  

  if chunking_weight_j_  is not None:
    chunking_weight_j_c = lib.cxios_is_defined_domain_chunking_weight_j(domain_hdl)
    chunking_weight_j_._c_value = ctypes.c_bool(chunking_weight_j_c)
    
  

  if comment_  is not None:
    comment_c = lib.cxios_is_defined_domain_comment(domain_hdl)
    comment_._c_value = ctypes.c_bool(comment_c)
    
  

  if data_dim_  is not None:
    data_dim_c = lib.cxios_is_defined_domain_data_dim(domain_hdl)
    data_dim_._c_value = ctypes.c_bool(data_dim_c)
    
  

  if data_i_index_  is not None:
    data_i_index_c = lib.cxios_is_defined_domain_data_i_index(domain_hdl)
    data_i_index_._c_value = ctypes.c_bool(data_i_index_c)
    
  

  if data_ibegin_  is not None:
    data_ibegin_c = lib.cxios_is_defined_domain_data_ibegin(domain_hdl)
    data_ibegin_._c_value = ctypes.c_bool(data_ibegin_c)
    
  

  if data_j_index_  is not None:
    data_j_index_c = lib.cxios_is_defined_domain_data_j_index(domain_hdl)
    data_j_index_._c_value = ctypes.c_bool(data_j_index_c)
    
  

  if data_jbegin_  is not None:
    data_jbegin_c = lib.cxios_is_defined_domain_data_jbegin(domain_hdl)
    data_jbegin_._c_value = ctypes.c_bool(data_jbegin_c)
    
  

  if data_ni_  is not None:
    data_ni_c = lib.cxios_is_defined_domain_data_ni(domain_hdl)
    data_ni_._c_value = ctypes.c_bool(data_ni_c)
    
  

  if data_nj_  is not None:
    data_nj_c = lib.cxios_is_defined_domain_data_nj(domain_hdl)
    data_nj_._c_value = ctypes.c_bool(data_nj_c)
    
  

  if dim_i_name_  is not None:
    dim_i_name_c = lib.cxios_is_defined_domain_dim_i_name(domain_hdl)
    dim_i_name_._c_value = ctypes.c_bool(dim_i_name_c)
    
  

  if dim_j_name_  is not None:
    dim_j_name_c = lib.cxios_is_defined_domain_dim_j_name(domain_hdl)
    dim_j_name_._c_value = ctypes.c_bool(dim_j_name_c)
    
  

  if domain_ref_  is not None:
    domain_ref_c = lib.cxios_is_defined_domain_domain_ref(domain_hdl)
    domain_ref_._c_value = ctypes.c_bool(domain_ref_c)
    
  

  if i_index_  is not None:
    i_index_c = lib.cxios_is_defined_domain_i_index(domain_hdl)
    i_index_._c_value = ctypes.c_bool(i_index_c)
    
  

  if ibegin_  is not None:
    ibegin_c = lib.cxios_is_defined_domain_ibegin(domain_hdl)
    ibegin_._c_value = ctypes.c_bool(ibegin_c)
    
  

  if j_index_  is not None:
    j_index_c = lib.cxios_is_defined_domain_j_index(domain_hdl)
    j_index_._c_value = ctypes.c_bool(j_index_c)
    
  

  if jbegin_  is not None:
    jbegin_c = lib.cxios_is_defined_domain_jbegin(domain_hdl)
    jbegin_._c_value = ctypes.c_bool(jbegin_c)
    
  

  if lat_name_  is not None:
    lat_name_c = lib.cxios_is_defined_domain_lat_name(domain_hdl)
    lat_name_._c_value = ctypes.c_bool(lat_name_c)
    
  

  if latvalue_1d_  is not None:
    latvalue_1d_c = lib.cxios_is_defined_domain_latvalue_1d(domain_hdl)
    latvalue_1d_._c_value = ctypes.c_bool(latvalue_1d_c)
    
  

  if latvalue_2d_  is not None:
    latvalue_2d_c = lib.cxios_is_defined_domain_latvalue_2d(domain_hdl)
    latvalue_2d_._c_value = ctypes.c_bool(latvalue_2d_c)
    
  

  if lon_name_  is not None:
    lon_name_c = lib.cxios_is_defined_domain_lon_name(domain_hdl)
    lon_name_._c_value = ctypes.c_bool(lon_name_c)
    
  

  if long_name_  is not None:
    long_name_c = lib.cxios_is_defined_domain_long_name(domain_hdl)
    long_name_._c_value = ctypes.c_bool(long_name_c)
    
  

  if lonvalue_1d_  is not None:
    lonvalue_1d_c = lib.cxios_is_defined_domain_lonvalue_1d(domain_hdl)
    lonvalue_1d_._c_value = ctypes.c_bool(lonvalue_1d_c)
    
  

  if lonvalue_2d_  is not None:
    lonvalue_2d_c = lib.cxios_is_defined_domain_lonvalue_2d(domain_hdl)
    lonvalue_2d_._c_value = ctypes.c_bool(lonvalue_2d_c)
    
  

  if mask_1d_  is not None:
    mask_1d_c = lib.cxios_is_defined_domain_mask_1d(domain_hdl)
    mask_1d_._c_value = ctypes.c_bool(mask_1d_c)
    
  

  if mask_2d_  is not None:
    mask_2d_c = lib.cxios_is_defined_domain_mask_2d(domain_hdl)
    mask_2d_._c_value = ctypes.c_bool(mask_2d_c)
    
  

  if name_  is not None:
    name_c = lib.cxios_is_defined_domain_name(domain_hdl)
    name_._c_value = ctypes.c_bool(name_c)
    
  

  if ni_  is not None:
    ni_c = lib.cxios_is_defined_domain_ni(domain_hdl)
    ni_._c_value = ctypes.c_bool(ni_c)
    
  

  if ni_glo_  is not None:
    ni_glo_c = lib.cxios_is_defined_domain_ni_glo(domain_hdl)
    ni_glo_._c_value = ctypes.c_bool(ni_glo_c)
    
  

  if nj_  is not None:
    nj_c = lib.cxios_is_defined_domain_nj(domain_hdl)
    nj_._c_value = ctypes.c_bool(nj_c)
    
  

  if nj_glo_  is not None:
    nj_glo_c = lib.cxios_is_defined_domain_nj_glo(domain_hdl)
    nj_glo_._c_value = ctypes.c_bool(nj_glo_c)
    
  

  if nvertex_  is not None:
    nvertex_c = lib.cxios_is_defined_domain_nvertex(domain_hdl)
    nvertex_._c_value = ctypes.c_bool(nvertex_c)
    
  

  if nvertex_name_  is not None:
    nvertex_name_c = lib.cxios_is_defined_domain_nvertex_name(domain_hdl)
    nvertex_name_._c_value = ctypes.c_bool(nvertex_name_c)
    
  

  if prec_  is not None:
    prec_c = lib.cxios_is_defined_domain_prec(domain_hdl)
    prec_._c_value = ctypes.c_bool(prec_c)
    
  

  if radius_  is not None:
    radius_c = lib.cxios_is_defined_domain_radius(domain_hdl)
    radius_._c_value = ctypes.c_bool(radius_c)
    
  

  if standard_name_  is not None:
    standard_name_c = lib.cxios_is_defined_domain_standard_name(domain_hdl)
    standard_name_._c_value = ctypes.c_bool(standard_name_c)
    
  

  if type_  is not None:
    type_c = lib.cxios_is_defined_domain_type(domain_hdl)
    type_._c_value = ctypes.c_bool(type_c)
    
  
  return 



