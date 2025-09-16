# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.ifile import  get_filegroup_handle
from xios.ofilegroup_attr import FileGroup

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



lib.cxios_get_filegroup_append.argtypes = [FileGroup, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_filegroup_append.restypes = None

lib.cxios_set_filegroup_append.argtypes = [FileGroup, ctypes.c_bool]
lib.cxios_set_filegroup_append.restypes = None

lib.cxios_is_defined_filegroup_append.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_append.restypes = ctypes.c_bool

lib.cxios_get_filegroup_comment.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_comment.restypes = None

lib.cxios_set_filegroup_comment.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_comment.restypes = None

lib.cxios_is_defined_filegroup_comment.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_comment.restypes = ctypes.c_bool

lib.cxios_get_filegroup_compression_level.argtypes = [FileGroup, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_filegroup_compression_level.restypes = None

lib.cxios_set_filegroup_compression_level.argtypes = [FileGroup, ctypes.c_int]
lib.cxios_set_filegroup_compression_level.restypes = None

lib.cxios_is_defined_filegroup_compression_level.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_compression_level.restypes = ctypes.c_bool

lib.cxios_get_filegroup_convention.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_convention.restypes = None

lib.cxios_set_filegroup_convention.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_convention.restypes = None

lib.cxios_is_defined_filegroup_convention.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_convention.restypes = ctypes.c_bool

lib.cxios_get_filegroup_convention_str.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_convention_str.restypes = None

lib.cxios_set_filegroup_convention_str.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_convention_str.restypes = None

lib.cxios_is_defined_filegroup_convention_str.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_convention_str.restypes = ctypes.c_bool

lib.cxios_get_filegroup_cyclic.argtypes = [FileGroup, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_filegroup_cyclic.restypes = None

lib.cxios_set_filegroup_cyclic.argtypes = [FileGroup, ctypes.c_bool]
lib.cxios_set_filegroup_cyclic.restypes = None

lib.cxios_is_defined_filegroup_cyclic.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_cyclic.restypes = ctypes.c_bool

lib.cxios_get_filegroup_description.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_description.restypes = None

lib.cxios_set_filegroup_description.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_description.restypes = None

lib.cxios_is_defined_filegroup_description.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_description.restypes = ctypes.c_bool

lib.cxios_get_filegroup_enabled.argtypes = [FileGroup, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_filegroup_enabled.restypes = None

lib.cxios_set_filegroup_enabled.argtypes = [FileGroup, ctypes.c_bool]
lib.cxios_set_filegroup_enabled.restypes = None

lib.cxios_is_defined_filegroup_enabled.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_enabled.restypes = ctypes.c_bool

lib.cxios_get_filegroup_format.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_format.restypes = None

lib.cxios_set_filegroup_format.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_format.restypes = None

lib.cxios_is_defined_filegroup_format.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_format.restypes = ctypes.c_bool

lib.cxios_get_filegroup_gatherer.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_gatherer.restypes = None

lib.cxios_set_filegroup_gatherer.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_gatherer.restypes = None

lib.cxios_is_defined_filegroup_gatherer.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_gatherer.restypes = ctypes.c_bool

lib.cxios_get_filegroup_group_ref.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_group_ref.restypes = None

lib.cxios_set_filegroup_group_ref.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_group_ref.restypes = None

lib.cxios_is_defined_filegroup_group_ref.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_group_ref.restypes = ctypes.c_bool

lib.cxios_get_filegroup_min_digits.argtypes = [FileGroup, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_filegroup_min_digits.restypes = None

lib.cxios_set_filegroup_min_digits.argtypes = [FileGroup, ctypes.c_int]
lib.cxios_set_filegroup_min_digits.restypes = None

lib.cxios_is_defined_filegroup_min_digits.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_min_digits.restypes = ctypes.c_bool

lib.cxios_get_filegroup_mode.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_mode.restypes = None

lib.cxios_set_filegroup_mode.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_mode.restypes = None

lib.cxios_is_defined_filegroup_mode.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_mode.restypes = ctypes.c_bool

lib.cxios_get_filegroup_name.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_name.restypes = None

lib.cxios_set_filegroup_name.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_name.restypes = None

lib.cxios_is_defined_filegroup_name.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_name.restypes = ctypes.c_bool

lib.cxios_get_filegroup_name_suffix.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_name_suffix.restypes = None

lib.cxios_set_filegroup_name_suffix.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_name_suffix.restypes = None

lib.cxios_is_defined_filegroup_name_suffix.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_name_suffix.restypes = ctypes.c_bool

lib.cxios_get_filegroup_output_freq.argtypes = [FileGroup, ctypes.POINTER(Duration)]
lib.cxios_get_filegroup_output_freq.restypes = None

lib.cxios_set_filegroup_output_freq.argtypes = [FileGroup, Duration]
lib.cxios_set_filegroup_output_freq.restypes = None

lib.cxios_is_defined_filegroup_output_freq.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_output_freq.restypes = ctypes.c_bool

lib.cxios_get_filegroup_output_level.argtypes = [FileGroup, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_filegroup_output_level.restypes = None

lib.cxios_set_filegroup_output_level.argtypes = [FileGroup, ctypes.c_int]
lib.cxios_set_filegroup_output_level.restypes = None

lib.cxios_is_defined_filegroup_output_level.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_output_level.restypes = ctypes.c_bool

lib.cxios_get_filegroup_par_access.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_par_access.restypes = None

lib.cxios_set_filegroup_par_access.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_par_access.restypes = None

lib.cxios_is_defined_filegroup_par_access.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_par_access.restypes = ctypes.c_bool

lib.cxios_get_filegroup_pool_gatherer.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_pool_gatherer.restypes = None

lib.cxios_set_filegroup_pool_gatherer.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_pool_gatherer.restypes = None

lib.cxios_is_defined_filegroup_pool_gatherer.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_pool_gatherer.restypes = ctypes.c_bool

lib.cxios_get_filegroup_pool_reader.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_pool_reader.restypes = None

lib.cxios_set_filegroup_pool_reader.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_pool_reader.restypes = None

lib.cxios_is_defined_filegroup_pool_reader.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_pool_reader.restypes = ctypes.c_bool

lib.cxios_get_filegroup_pool_writer.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_pool_writer.restypes = None

lib.cxios_set_filegroup_pool_writer.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_pool_writer.restypes = None

lib.cxios_is_defined_filegroup_pool_writer.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_pool_writer.restypes = ctypes.c_bool

lib.cxios_get_filegroup_read_metadata_par.argtypes = [FileGroup, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_filegroup_read_metadata_par.restypes = None

lib.cxios_set_filegroup_read_metadata_par.argtypes = [FileGroup, ctypes.c_bool]
lib.cxios_set_filegroup_read_metadata_par.restypes = None

lib.cxios_is_defined_filegroup_read_metadata_par.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_read_metadata_par.restypes = ctypes.c_bool

lib.cxios_get_filegroup_reader.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_reader.restypes = None

lib.cxios_set_filegroup_reader.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_reader.restypes = None

lib.cxios_is_defined_filegroup_reader.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_reader.restypes = ctypes.c_bool

lib.cxios_get_filegroup_record_offset.argtypes = [FileGroup, ctypes.POINTER(ctypes.c_int)]
lib.cxios_get_filegroup_record_offset.restypes = None

lib.cxios_set_filegroup_record_offset.argtypes = [FileGroup, ctypes.c_int]
lib.cxios_set_filegroup_record_offset.restypes = None

lib.cxios_is_defined_filegroup_record_offset.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_record_offset.restypes = ctypes.c_bool

lib.cxios_get_filegroup_split_end_offset.argtypes = [FileGroup, ctypes.POINTER(Duration)]
lib.cxios_get_filegroup_split_end_offset.restypes = None

lib.cxios_set_filegroup_split_end_offset.argtypes = [FileGroup, Duration]
lib.cxios_set_filegroup_split_end_offset.restypes = None

lib.cxios_is_defined_filegroup_split_end_offset.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_split_end_offset.restypes = ctypes.c_bool

lib.cxios_get_filegroup_split_freq.argtypes = [FileGroup, ctypes.POINTER(Duration)]
lib.cxios_get_filegroup_split_freq.restypes = None

lib.cxios_set_filegroup_split_freq.argtypes = [FileGroup, Duration]
lib.cxios_set_filegroup_split_freq.restypes = None

lib.cxios_is_defined_filegroup_split_freq.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_split_freq.restypes = ctypes.c_bool

lib.cxios_get_filegroup_split_freq_format.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_split_freq_format.restypes = None

lib.cxios_set_filegroup_split_freq_format.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_split_freq_format.restypes = None

lib.cxios_is_defined_filegroup_split_freq_format.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_split_freq_format.restypes = ctypes.c_bool

lib.cxios_get_filegroup_split_last_date.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_split_last_date.restypes = None

lib.cxios_set_filegroup_split_last_date.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_split_last_date.restypes = None

lib.cxios_is_defined_filegroup_split_last_date.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_split_last_date.restypes = ctypes.c_bool

lib.cxios_get_filegroup_split_start_offset.argtypes = [FileGroup, ctypes.POINTER(Duration)]
lib.cxios_get_filegroup_split_start_offset.restypes = None

lib.cxios_set_filegroup_split_start_offset.argtypes = [FileGroup, Duration]
lib.cxios_set_filegroup_split_start_offset.restypes = None

lib.cxios_is_defined_filegroup_split_start_offset.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_split_start_offset.restypes = ctypes.c_bool

lib.cxios_get_filegroup_sync_freq.argtypes = [FileGroup, ctypes.POINTER(Duration)]
lib.cxios_get_filegroup_sync_freq.restypes = None

lib.cxios_set_filegroup_sync_freq.argtypes = [FileGroup, Duration]
lib.cxios_set_filegroup_sync_freq.restypes = None

lib.cxios_is_defined_filegroup_sync_freq.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_sync_freq.restypes = ctypes.c_bool

lib.cxios_get_filegroup_time_counter.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_time_counter.restypes = None

lib.cxios_set_filegroup_time_counter.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_time_counter.restypes = None

lib.cxios_is_defined_filegroup_time_counter.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_time_counter.restypes = ctypes.c_bool

lib.cxios_get_filegroup_time_counter_name.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_time_counter_name.restypes = None

lib.cxios_set_filegroup_time_counter_name.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_time_counter_name.restypes = None

lib.cxios_is_defined_filegroup_time_counter_name.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_time_counter_name.restypes = ctypes.c_bool

lib.cxios_get_filegroup_time_stamp_format.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_time_stamp_format.restypes = None

lib.cxios_set_filegroup_time_stamp_format.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_time_stamp_format.restypes = None

lib.cxios_is_defined_filegroup_time_stamp_format.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_time_stamp_format.restypes = ctypes.c_bool

lib.cxios_get_filegroup_time_stamp_name.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_time_stamp_name.restypes = None

lib.cxios_set_filegroup_time_stamp_name.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_time_stamp_name.restypes = None

lib.cxios_is_defined_filegroup_time_stamp_name.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_time_stamp_name.restypes = ctypes.c_bool

lib.cxios_get_filegroup_time_units.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_time_units.restypes = None

lib.cxios_set_filegroup_time_units.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_time_units.restypes = None

lib.cxios_is_defined_filegroup_time_units.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_time_units.restypes = ctypes.c_bool

lib.cxios_get_filegroup_timeseries.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_timeseries.restypes = None

lib.cxios_set_filegroup_timeseries.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_timeseries.restypes = None

lib.cxios_is_defined_filegroup_timeseries.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_timeseries.restypes = ctypes.c_bool

lib.cxios_get_filegroup_ts_prefix.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_ts_prefix.restypes = None

lib.cxios_set_filegroup_ts_prefix.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_ts_prefix.restypes = None

lib.cxios_is_defined_filegroup_ts_prefix.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_ts_prefix.restypes = ctypes.c_bool

lib.cxios_get_filegroup_type.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_type.restypes = None

lib.cxios_set_filegroup_type.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_type.restypes = None

lib.cxios_is_defined_filegroup_type.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_type.restypes = ctypes.c_bool

lib.cxios_get_filegroup_using_server2.argtypes = [FileGroup, ctypes.POINTER(ctypes.c_bool)]
lib.cxios_get_filegroup_using_server2.restypes = None

lib.cxios_set_filegroup_using_server2.argtypes = [FileGroup, ctypes.c_bool]
lib.cxios_set_filegroup_using_server2.restypes = None

lib.cxios_is_defined_filegroup_using_server2.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_using_server2.restypes = ctypes.c_bool

lib.cxios_get_filegroup_uuid_format.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_uuid_format.restypes = None

lib.cxios_set_filegroup_uuid_format.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_uuid_format.restypes = None

lib.cxios_is_defined_filegroup_uuid_format.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_uuid_format.restypes = ctypes.c_bool

lib.cxios_get_filegroup_uuid_name.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_uuid_name.restypes = None

lib.cxios_set_filegroup_uuid_name.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_uuid_name.restypes = None

lib.cxios_is_defined_filegroup_uuid_name.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_uuid_name.restypes = ctypes.c_bool

lib.cxios_get_filegroup_writer.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_get_filegroup_writer.restypes = None

lib.cxios_set_filegroup_writer.argtypes = [FileGroup, ctypes.c_char_p, ctypes.c_int]
lib.cxios_set_filegroup_writer.restypes = None

lib.cxios_is_defined_filegroup_writer.argtypes = [FileGroup]
lib.cxios_is_defined_filegroup_writer.restypes = ctypes.c_bool

#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_filegroup_attr(filegroup_id : Union[String, str], append : Optional[Union[Bool, bool]] = None, comment : Optional[Union[str, String]] = None,   
compression_level : Optional[Union[Int, int]] = None, convention : Optional[Union[str, String]] = None,   
convention_str : Optional[Union[str, String]] = None, cyclic : Optional[Union[Bool, bool]] = None,   
description : Optional[Union[str, String]] = None, enabled : Optional[Union[Bool, bool]] = None,   
format : Optional[Union[str, String]] = None, gatherer : Optional[Union[str, String]] = None,   
group_ref : Optional[Union[str, String]] = None, min_digits : Optional[Union[Int, int]] = None,   
mode : Optional[Union[str, String]] = None, name : Optional[Union[str, String]] = None, name_suffix : Optional[Union[str, String]] = None,   
output_freq : Optional[Union[Duration, Duration]] = None, output_level : Optional[Union[Int, int]] = None,   
par_access : Optional[Union[str, String]] = None, pool_gatherer : Optional[Union[str, String]] = None,   
pool_reader : Optional[Union[str, String]] = None, pool_writer : Optional[Union[str, String]] = None,   
read_metadata_par : Optional[Union[Bool, bool]] = None, reader : Optional[Union[str, String]] = None,   
record_offset : Optional[Union[Int, int]] = None, split_end_offset : Optional[Union[Duration, Duration]] = None,   
split_freq : Optional[Union[Duration, Duration]] = None, split_freq_format : Optional[Union[str, String]] = None,   
split_last_date : Optional[Union[str, String]] = None, split_start_offset : Optional[Union[Duration, Duration]] = None,   
sync_freq : Optional[Union[Duration, Duration]] = None, time_counter : Optional[Union[str, String]] = None,   
time_counter_name : Optional[Union[str, String]] = None, time_stamp_format : Optional[Union[str, String]] = None,   
time_stamp_name : Optional[Union[str, String]] = None, time_units : Optional[Union[str, String]] = None,   
timeseries : Optional[Union[str, String]] = None, ts_prefix : Optional[Union[str, String]] = None,   
type : Optional[Union[str, String]] = None, using_server2 : Optional[Union[Bool, bool]] = None,   
uuid_format : Optional[Union[str, String]] = None, uuid_name : Optional[Union[str, String]] = None,   
writer : Optional[Union[str, String]] = None):

  
  filegroup_hdl = FileGroup()
  

  get_filegroup_handle(filegroup_id, filegroup_hdl)
  set_filegroup_attr_hdl_(filegroup_hdl, append, comment, compression_level, convention, convention_str, cyclic, description,  
   enabled, format, gatherer, group_ref, min_digits, mode, name, name_suffix, output_freq, output_level,  
   par_access, pool_gatherer, pool_reader, pool_writer, read_metadata_par, reader, record_offset,  
   split_end_offset, split_freq, split_freq_format, split_last_date, split_start_offset, sync_freq,  
   time_counter, time_counter_name, time_stamp_format, time_stamp_name, time_units, timeseries,  
   ts_prefix, type, using_server2, uuid_format, uuid_name, writer)
  return 



@typecheck
def set_filegroup_attr_hdl(filegroup_hdl : FileGroup, append : Optional[Union[Bool, bool]] = None, comment : Optional[Union[str, String]] = None,   
compression_level : Optional[Union[Int, int]] = None, convention : Optional[Union[str, String]] = None,   
convention_str : Optional[Union[str, String]] = None, cyclic : Optional[Union[Bool, bool]] = None,   
description : Optional[Union[str, String]] = None, enabled : Optional[Union[Bool, bool]] = None,   
format : Optional[Union[str, String]] = None, gatherer : Optional[Union[str, String]] = None,   
group_ref : Optional[Union[str, String]] = None, min_digits : Optional[Union[Int, int]] = None,   
mode : Optional[Union[str, String]] = None, name : Optional[Union[str, String]] = None, name_suffix : Optional[Union[str, String]] = None,   
output_freq : Optional[Union[Duration, Duration]] = None, output_level : Optional[Union[Int, int]] = None,   
par_access : Optional[Union[str, String]] = None, pool_gatherer : Optional[Union[str, String]] = None,   
pool_reader : Optional[Union[str, String]] = None, pool_writer : Optional[Union[str, String]] = None,   
read_metadata_par : Optional[Union[Bool, bool]] = None, reader : Optional[Union[str, String]] = None,   
record_offset : Optional[Union[Int, int]] = None, split_end_offset : Optional[Union[Duration, Duration]] = None,   
split_freq : Optional[Union[Duration, Duration]] = None, split_freq_format : Optional[Union[str, String]] = None,   
split_last_date : Optional[Union[str, String]] = None, split_start_offset : Optional[Union[Duration, Duration]] = None,   
sync_freq : Optional[Union[Duration, Duration]] = None, time_counter : Optional[Union[str, String]] = None,   
time_counter_name : Optional[Union[str, String]] = None, time_stamp_format : Optional[Union[str, String]] = None,   
time_stamp_name : Optional[Union[str, String]] = None, time_units : Optional[Union[str, String]] = None,   
timeseries : Optional[Union[str, String]] = None, ts_prefix : Optional[Union[str, String]] = None,   
type : Optional[Union[str, String]] = None, using_server2 : Optional[Union[Bool, bool]] = None,   
uuid_format : Optional[Union[str, String]] = None, uuid_name : Optional[Union[str, String]] = None,   
writer : Optional[Union[str, String]] = None):

  
  set_filegroup_attr_hdl_(filegroup_hdl, append, comment, compression_level, convention, convention_str, cyclic, description,  
   enabled, format, gatherer, group_ref, min_digits, mode, name, name_suffix, output_freq, output_level,  
   par_access, pool_gatherer, pool_reader, pool_writer, read_metadata_par, reader, record_offset,  
   split_end_offset, split_freq, split_freq_format, split_last_date, split_start_offset, sync_freq,  
   time_counter, time_counter_name, time_stamp_format, time_stamp_name, time_units, timeseries,  
   ts_prefix, type, using_server2, uuid_format, uuid_name, writer)
  return 



@typecheck
def set_filegroup_attr_hdl_(filegroup_hdl : FileGroup, append_ : Optional[Union[Bool, bool]] = None, comment_ : Optional[Union[str, String]] = None,   
compression_level_ : Optional[Union[Int, int]] = None, convention_ : Optional[Union[str, String]] = None,   
convention_str_ : Optional[Union[str, String]] = None, cyclic_ : Optional[Union[Bool, bool]] = None,   
description_ : Optional[Union[str, String]] = None, enabled_ : Optional[Union[Bool, bool]] = None,   
format_ : Optional[Union[str, String]] = None, gatherer_ : Optional[Union[str, String]] = None,   
group_ref_ : Optional[Union[str, String]] = None, min_digits_ : Optional[Union[Int, int]] = None,   
mode_ : Optional[Union[str, String]] = None, name_ : Optional[Union[str, String]] = None, name_suffix_ : Optional[Union[str, String]] = None,   
output_freq_ : Optional[Union[Duration, Duration]] = None, output_level_ : Optional[Union[Int, int]] = None,   
par_access_ : Optional[Union[str, String]] = None, pool_gatherer_ : Optional[Union[str, String]] = None,   
pool_reader_ : Optional[Union[str, String]] = None, pool_writer_ : Optional[Union[str, String]] = None,   
read_metadata_par_ : Optional[Union[Bool, bool]] = None, reader_ : Optional[Union[str, String]] = None,   
record_offset_ : Optional[Union[Int, int]] = None, split_end_offset_ : Optional[Union[Duration, Duration]] = None,   
split_freq_ : Optional[Union[Duration, Duration]] = None, split_freq_format_ : Optional[Union[str, String]] = None,   
split_last_date_ : Optional[Union[str, String]] = None, split_start_offset_ : Optional[Union[Duration, Duration]] = None,   
sync_freq_ : Optional[Union[Duration, Duration]] = None, time_counter_ : Optional[Union[str, String]] = None,   
time_counter_name_ : Optional[Union[str, String]] = None, time_stamp_format_ : Optional[Union[str, String]] = None,   
time_stamp_name_ : Optional[Union[str, String]] = None, time_units_ : Optional[Union[str, String]] = None,   
timeseries_ : Optional[Union[str, String]] = None, ts_prefix_ : Optional[Union[str, String]] = None,   
type_ : Optional[Union[str, String]] = None, using_server2_ : Optional[Union[Bool, bool]] = None,   
uuid_format_ : Optional[Union[str, String]] = None, uuid_name_ : Optional[Union[str, String]] = None,   
writer_ : Optional[Union[str, String]] = None):

  
  

  if append_ is not None: 
  
    append_ = Bool(append_)
    append_c = append_._c_value
    lib.cxios_set_filegroup_append(filegroup_hdl, append_c)
    
  

  if comment_ is not None:
  
    comment_= String(comment_)
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_set_filegroup_comment(filegroup_hdl, comment_c, len_comment_c)
    
  

  if compression_level_ is not None: 
  
    compression_level_ = Int(compression_level_)
    compression_level_c = compression_level_._c_value
    lib.cxios_set_filegroup_compression_level(filegroup_hdl, compression_level_c)
    
  

  if convention_ is not None:
  
    convention_= String(convention_)
    convention_c = convention_._c_value
    len_convention_c = len(ctypes.string_at(convention_c))
    lib.cxios_set_filegroup_convention(filegroup_hdl, convention_c, len_convention_c)
    
  

  if convention_str_ is not None:
  
    convention_str_= String(convention_str_)
    convention_str_c = convention_str_._c_value
    len_convention_str_c = len(ctypes.string_at(convention_str_c))
    lib.cxios_set_filegroup_convention_str(filegroup_hdl, convention_str_c, len_convention_str_c)
    
  

  if cyclic_ is not None: 
  
    cyclic_ = Bool(cyclic_)
    cyclic_c = cyclic_._c_value
    lib.cxios_set_filegroup_cyclic(filegroup_hdl, cyclic_c)
    
  

  if description_ is not None:
  
    description_= String(description_)
    description_c = description_._c_value
    len_description_c = len(ctypes.string_at(description_c))
    lib.cxios_set_filegroup_description(filegroup_hdl, description_c, len_description_c)
    
  

  if enabled_ is not None: 
  
    enabled_ = Bool(enabled_)
    enabled_c = enabled_._c_value
    lib.cxios_set_filegroup_enabled(filegroup_hdl, enabled_c)
    
  

  if format_ is not None:
  
    format_= String(format_)
    format_c = format_._c_value
    len_format_c = len(ctypes.string_at(format_c))
    lib.cxios_set_filegroup_format(filegroup_hdl, format_c, len_format_c)
    
  

  if gatherer_ is not None:
  
    gatherer_= String(gatherer_)
    gatherer_c = gatherer_._c_value
    len_gatherer_c = len(ctypes.string_at(gatherer_c))
    lib.cxios_set_filegroup_gatherer(filegroup_hdl, gatherer_c, len_gatherer_c)
    
  

  if group_ref_ is not None:
  
    group_ref_= String(group_ref_)
    group_ref_c = group_ref_._c_value
    len_group_ref_c = len(ctypes.string_at(group_ref_c))
    lib.cxios_set_filegroup_group_ref(filegroup_hdl, group_ref_c, len_group_ref_c)
    
  

  if min_digits_ is not None: 
  
    min_digits_ = Int(min_digits_)
    min_digits_c = min_digits_._c_value
    lib.cxios_set_filegroup_min_digits(filegroup_hdl, min_digits_c)
    
  

  if mode_ is not None:
  
    mode_= String(mode_)
    mode_c = mode_._c_value
    len_mode_c = len(ctypes.string_at(mode_c))
    lib.cxios_set_filegroup_mode(filegroup_hdl, mode_c, len_mode_c)
    
  

  if name_ is not None:
  
    name_= String(name_)
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_set_filegroup_name(filegroup_hdl, name_c, len_name_c)
    
  

  if name_suffix_ is not None:
  
    name_suffix_= String(name_suffix_)
    name_suffix_c = name_suffix_._c_value
    len_name_suffix_c = len(ctypes.string_at(name_suffix_c))
    lib.cxios_set_filegroup_name_suffix(filegroup_hdl, name_suffix_c, len_name_suffix_c)
    
  

  if output_freq_ is not None: 
  
    output_freq_c = output_freq_
    lib.cxios_set_filegroup_output_freq(filegroup_hdl, output_freq_c)
    
  

  if output_level_ is not None: 
  
    output_level_ = Int(output_level_)
    output_level_c = output_level_._c_value
    lib.cxios_set_filegroup_output_level(filegroup_hdl, output_level_c)
    
  

  if par_access_ is not None:
  
    par_access_= String(par_access_)
    par_access_c = par_access_._c_value
    len_par_access_c = len(ctypes.string_at(par_access_c))
    lib.cxios_set_filegroup_par_access(filegroup_hdl, par_access_c, len_par_access_c)
    
  

  if pool_gatherer_ is not None:
  
    pool_gatherer_= String(pool_gatherer_)
    pool_gatherer_c = pool_gatherer_._c_value
    len_pool_gatherer_c = len(ctypes.string_at(pool_gatherer_c))
    lib.cxios_set_filegroup_pool_gatherer(filegroup_hdl, pool_gatherer_c, len_pool_gatherer_c)
    
  

  if pool_reader_ is not None:
  
    pool_reader_= String(pool_reader_)
    pool_reader_c = pool_reader_._c_value
    len_pool_reader_c = len(ctypes.string_at(pool_reader_c))
    lib.cxios_set_filegroup_pool_reader(filegroup_hdl, pool_reader_c, len_pool_reader_c)
    
  

  if pool_writer_ is not None:
  
    pool_writer_= String(pool_writer_)
    pool_writer_c = pool_writer_._c_value
    len_pool_writer_c = len(ctypes.string_at(pool_writer_c))
    lib.cxios_set_filegroup_pool_writer(filegroup_hdl, pool_writer_c, len_pool_writer_c)
    
  

  if read_metadata_par_ is not None: 
  
    read_metadata_par_ = Bool(read_metadata_par_)
    read_metadata_par_c = read_metadata_par_._c_value
    lib.cxios_set_filegroup_read_metadata_par(filegroup_hdl, read_metadata_par_c)
    
  

  if reader_ is not None:
  
    reader_= String(reader_)
    reader_c = reader_._c_value
    len_reader_c = len(ctypes.string_at(reader_c))
    lib.cxios_set_filegroup_reader(filegroup_hdl, reader_c, len_reader_c)
    
  

  if record_offset_ is not None: 
  
    record_offset_ = Int(record_offset_)
    record_offset_c = record_offset_._c_value
    lib.cxios_set_filegroup_record_offset(filegroup_hdl, record_offset_c)
    
  

  if split_end_offset_ is not None: 
  
    split_end_offset_c = split_end_offset_
    lib.cxios_set_filegroup_split_end_offset(filegroup_hdl, split_end_offset_c)
    
  

  if split_freq_ is not None: 
  
    split_freq_c = split_freq_
    lib.cxios_set_filegroup_split_freq(filegroup_hdl, split_freq_c)
    
  

  if split_freq_format_ is not None:
  
    split_freq_format_= String(split_freq_format_)
    split_freq_format_c = split_freq_format_._c_value
    len_split_freq_format_c = len(ctypes.string_at(split_freq_format_c))
    lib.cxios_set_filegroup_split_freq_format(filegroup_hdl, split_freq_format_c, len_split_freq_format_c)
    
  

  if split_last_date_ is not None:
  
    split_last_date_= String(split_last_date_)
    split_last_date_c = split_last_date_._c_value
    len_split_last_date_c = len(ctypes.string_at(split_last_date_c))
    lib.cxios_set_filegroup_split_last_date(filegroup_hdl, split_last_date_c, len_split_last_date_c)
    
  

  if split_start_offset_ is not None: 
  
    split_start_offset_c = split_start_offset_
    lib.cxios_set_filegroup_split_start_offset(filegroup_hdl, split_start_offset_c)
    
  

  if sync_freq_ is not None: 
  
    sync_freq_c = sync_freq_
    lib.cxios_set_filegroup_sync_freq(filegroup_hdl, sync_freq_c)
    
  

  if time_counter_ is not None:
  
    time_counter_= String(time_counter_)
    time_counter_c = time_counter_._c_value
    len_time_counter_c = len(ctypes.string_at(time_counter_c))
    lib.cxios_set_filegroup_time_counter(filegroup_hdl, time_counter_c, len_time_counter_c)
    
  

  if time_counter_name_ is not None:
  
    time_counter_name_= String(time_counter_name_)
    time_counter_name_c = time_counter_name_._c_value
    len_time_counter_name_c = len(ctypes.string_at(time_counter_name_c))
    lib.cxios_set_filegroup_time_counter_name(filegroup_hdl, time_counter_name_c, len_time_counter_name_c)
    
  

  if time_stamp_format_ is not None:
  
    time_stamp_format_= String(time_stamp_format_)
    time_stamp_format_c = time_stamp_format_._c_value
    len_time_stamp_format_c = len(ctypes.string_at(time_stamp_format_c))
    lib.cxios_set_filegroup_time_stamp_format(filegroup_hdl, time_stamp_format_c, len_time_stamp_format_c)
    
  

  if time_stamp_name_ is not None:
  
    time_stamp_name_= String(time_stamp_name_)
    time_stamp_name_c = time_stamp_name_._c_value
    len_time_stamp_name_c = len(ctypes.string_at(time_stamp_name_c))
    lib.cxios_set_filegroup_time_stamp_name(filegroup_hdl, time_stamp_name_c, len_time_stamp_name_c)
    
  

  if time_units_ is not None:
  
    time_units_= String(time_units_)
    time_units_c = time_units_._c_value
    len_time_units_c = len(ctypes.string_at(time_units_c))
    lib.cxios_set_filegroup_time_units(filegroup_hdl, time_units_c, len_time_units_c)
    
  

  if timeseries_ is not None:
  
    timeseries_= String(timeseries_)
    timeseries_c = timeseries_._c_value
    len_timeseries_c = len(ctypes.string_at(timeseries_c))
    lib.cxios_set_filegroup_timeseries(filegroup_hdl, timeseries_c, len_timeseries_c)
    
  

  if ts_prefix_ is not None:
  
    ts_prefix_= String(ts_prefix_)
    ts_prefix_c = ts_prefix_._c_value
    len_ts_prefix_c = len(ctypes.string_at(ts_prefix_c))
    lib.cxios_set_filegroup_ts_prefix(filegroup_hdl, ts_prefix_c, len_ts_prefix_c)
    
  

  if type_ is not None:
  
    type_= String(type_)
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_set_filegroup_type(filegroup_hdl, type_c, len_type_c)
    
  

  if using_server2_ is not None: 
  
    using_server2_ = Bool(using_server2_)
    using_server2_c = using_server2_._c_value
    lib.cxios_set_filegroup_using_server2(filegroup_hdl, using_server2_c)
    
  

  if uuid_format_ is not None:
  
    uuid_format_= String(uuid_format_)
    uuid_format_c = uuid_format_._c_value
    len_uuid_format_c = len(ctypes.string_at(uuid_format_c))
    lib.cxios_set_filegroup_uuid_format(filegroup_hdl, uuid_format_c, len_uuid_format_c)
    
  

  if uuid_name_ is not None:
  
    uuid_name_= String(uuid_name_)
    uuid_name_c = uuid_name_._c_value
    len_uuid_name_c = len(ctypes.string_at(uuid_name_c))
    lib.cxios_set_filegroup_uuid_name(filegroup_hdl, uuid_name_c, len_uuid_name_c)
    
  

  if writer_ is not None:
  
    writer_= String(writer_)
    writer_c = writer_._c_value
    len_writer_c = len(ctypes.string_at(writer_c))
    lib.cxios_set_filegroup_writer(filegroup_hdl, writer_c, len_writer_c)
    
  
  return 



@typecheck
def get_filegroup_attr(filegroup_id : Union[String, str], append : Optional[Bool] = None, comment : Optional[String] = None,   
compression_level : Optional[Int] = None, convention : Optional[String] = None, convention_str : Optional[String] = None,   
cyclic : Optional[Bool] = None, description : Optional[String] = None, enabled : Optional[Bool] = None,   
format : Optional[String] = None, gatherer : Optional[String] = None, group_ref : Optional[String] = None,   
min_digits : Optional[Int] = None, mode : Optional[String] = None, name : Optional[String] = None,   
name_suffix : Optional[String] = None, output_freq : Optional[Duration] = None, output_level : Optional[Int] = None,   
par_access : Optional[String] = None, pool_gatherer : Optional[String] = None, pool_reader : Optional[String] = None,   
pool_writer : Optional[String] = None, read_metadata_par : Optional[Bool] = None, reader : Optional[String] = None,   
record_offset : Optional[Int] = None, split_end_offset : Optional[Duration] = None, split_freq : Optional[Duration] = None,   
split_freq_format : Optional[String] = None, split_last_date : Optional[String] = None, split_start_offset : Optional[Duration] = None,   
sync_freq : Optional[Duration] = None, time_counter : Optional[String] = None, time_counter_name : Optional[String] = None,   
time_stamp_format : Optional[String] = None, time_stamp_name : Optional[String] = None, time_units : Optional[String] = None,   
timeseries : Optional[String] = None, ts_prefix : Optional[String] = None, type : Optional[String] = None,   
using_server2 : Optional[Bool] = None, uuid_format : Optional[String] = None, uuid_name : Optional[String] = None,   
writer : Optional[String] = None):

  
  filegroup_hdl = FileGroup()
  

  get_filegroup_handle(filegroup_id, filegroup_hdl)
  get_filegroup_attr_hdl_(filegroup_hdl, append, comment, compression_level, convention, convention_str, cyclic, description,  
   enabled, format, gatherer, group_ref, min_digits, mode, name, name_suffix, output_freq, output_level,  
   par_access, pool_gatherer, pool_reader, pool_writer, read_metadata_par, reader, record_offset,  
   split_end_offset, split_freq, split_freq_format, split_last_date, split_start_offset, sync_freq,  
   time_counter, time_counter_name, time_stamp_format, time_stamp_name, time_units, timeseries,  
   ts_prefix, type, using_server2, uuid_format, uuid_name, writer)
  return 



@typecheck
def get_filegroup_attr_hdl(filegroup_hdl : FileGroup, append : Optional[Bool] = None, comment : Optional[String] = None,   
compression_level : Optional[Int] = None, convention : Optional[String] = None, convention_str : Optional[String] = None,   
cyclic : Optional[Bool] = None, description : Optional[String] = None, enabled : Optional[Bool] = None,   
format : Optional[String] = None, gatherer : Optional[String] = None, group_ref : Optional[String] = None,   
min_digits : Optional[Int] = None, mode : Optional[String] = None, name : Optional[String] = None,   
name_suffix : Optional[String] = None, output_freq : Optional[Duration] = None, output_level : Optional[Int] = None,   
par_access : Optional[String] = None, pool_gatherer : Optional[String] = None, pool_reader : Optional[String] = None,   
pool_writer : Optional[String] = None, read_metadata_par : Optional[Bool] = None, reader : Optional[String] = None,   
record_offset : Optional[Int] = None, split_end_offset : Optional[Duration] = None, split_freq : Optional[Duration] = None,   
split_freq_format : Optional[String] = None, split_last_date : Optional[String] = None, split_start_offset : Optional[Duration] = None,   
sync_freq : Optional[Duration] = None, time_counter : Optional[String] = None, time_counter_name : Optional[String] = None,   
time_stamp_format : Optional[String] = None, time_stamp_name : Optional[String] = None, time_units : Optional[String] = None,   
timeseries : Optional[String] = None, ts_prefix : Optional[String] = None, type : Optional[String] = None,   
using_server2 : Optional[Bool] = None, uuid_format : Optional[String] = None, uuid_name : Optional[String] = None,   
writer : Optional[String] = None):

  
  get_filegroup_attr_hdl_(filegroup_hdl, append, comment, compression_level, convention, convention_str, cyclic, description,  
   enabled, format, gatherer, group_ref, min_digits, mode, name, name_suffix, output_freq, output_level,  
   par_access, pool_gatherer, pool_reader, pool_writer, read_metadata_par, reader, record_offset,  
   split_end_offset, split_freq, split_freq_format, split_last_date, split_start_offset, sync_freq,  
   time_counter, time_counter_name, time_stamp_format, time_stamp_name, time_units, timeseries,  
   ts_prefix, type, using_server2, uuid_format, uuid_name, writer)
  return 



@typecheck
def get_filegroup_attr_hdl_(filegroup_hdl : FileGroup, append_ : Optional[Bool] = None, comment_ : Optional[String] = None,   
compression_level_ : Optional[Int] = None, convention_ : Optional[String] = None, convention_str_ : Optional[String] = None,   
cyclic_ : Optional[Bool] = None, description_ : Optional[String] = None, enabled_ : Optional[Bool] = None,   
format_ : Optional[String] = None, gatherer_ : Optional[String] = None, group_ref_ : Optional[String] = None,   
min_digits_ : Optional[Int] = None, mode_ : Optional[String] = None, name_ : Optional[String] = None,   
name_suffix_ : Optional[String] = None, output_freq_ : Optional[Duration] = None, output_level_ : Optional[Int] = None,   
par_access_ : Optional[String] = None, pool_gatherer_ : Optional[String] = None, pool_reader_ : Optional[String] = None,   
pool_writer_ : Optional[String] = None, read_metadata_par_ : Optional[Bool] = None, reader_ : Optional[String] = None,   
record_offset_ : Optional[Int] = None, split_end_offset_ : Optional[Duration] = None, split_freq_ : Optional[Duration] = None,   
split_freq_format_ : Optional[String] = None, split_last_date_ : Optional[String] = None, split_start_offset_ : Optional[Duration] = None,   
sync_freq_ : Optional[Duration] = None, time_counter_ : Optional[String] = None, time_counter_name_ : Optional[String] = None,   
time_stamp_format_ : Optional[String] = None, time_stamp_name_ : Optional[String] = None, time_units_ : Optional[String] = None,   
timeseries_ : Optional[String] = None, ts_prefix_ : Optional[String] = None, type_ : Optional[String] = None,   
using_server2_ : Optional[Bool] = None, uuid_format_ : Optional[String] = None, uuid_name_ : Optional[String] = None,   
writer_ : Optional[String] = None):

  
  

  if append_ is not None: 
  
    append_c = append_._c_value
    lib.cxios_get_filegroup_append(filegroup_hdl, append_c)
    
  

  if comment_ is not None:
  
    comment_c = comment_._c_value
    len_comment_c = len(ctypes.string_at(comment_c))
    lib.cxios_get_filegroup_comment(filegroup_hdl, comment_c, len_comment_c)
    
  

  if compression_level_ is not None: 
  
    compression_level_c = compression_level_._c_value
    lib.cxios_get_filegroup_compression_level(filegroup_hdl, compression_level_c)
    
  

  if convention_ is not None:
  
    convention_c = convention_._c_value
    len_convention_c = len(ctypes.string_at(convention_c))
    lib.cxios_get_filegroup_convention(filegroup_hdl, convention_c, len_convention_c)
    
  

  if convention_str_ is not None:
  
    convention_str_c = convention_str_._c_value
    len_convention_str_c = len(ctypes.string_at(convention_str_c))
    lib.cxios_get_filegroup_convention_str(filegroup_hdl, convention_str_c, len_convention_str_c)
    
  

  if cyclic_ is not None: 
  
    cyclic_c = cyclic_._c_value
    lib.cxios_get_filegroup_cyclic(filegroup_hdl, cyclic_c)
    
  

  if description_ is not None:
  
    description_c = description_._c_value
    len_description_c = len(ctypes.string_at(description_c))
    lib.cxios_get_filegroup_description(filegroup_hdl, description_c, len_description_c)
    
  

  if enabled_ is not None: 
  
    enabled_c = enabled_._c_value
    lib.cxios_get_filegroup_enabled(filegroup_hdl, enabled_c)
    
  

  if format_ is not None:
  
    format_c = format_._c_value
    len_format_c = len(ctypes.string_at(format_c))
    lib.cxios_get_filegroup_format(filegroup_hdl, format_c, len_format_c)
    
  

  if gatherer_ is not None:
  
    gatherer_c = gatherer_._c_value
    len_gatherer_c = len(ctypes.string_at(gatherer_c))
    lib.cxios_get_filegroup_gatherer(filegroup_hdl, gatherer_c, len_gatherer_c)
    
  

  if group_ref_ is not None:
  
    group_ref_c = group_ref_._c_value
    len_group_ref_c = len(ctypes.string_at(group_ref_c))
    lib.cxios_get_filegroup_group_ref(filegroup_hdl, group_ref_c, len_group_ref_c)
    
  

  if min_digits_ is not None: 
  
    min_digits_c = min_digits_._c_value
    lib.cxios_get_filegroup_min_digits(filegroup_hdl, min_digits_c)
    
  

  if mode_ is not None:
  
    mode_c = mode_._c_value
    len_mode_c = len(ctypes.string_at(mode_c))
    lib.cxios_get_filegroup_mode(filegroup_hdl, mode_c, len_mode_c)
    
  

  if name_ is not None:
  
    name_c = name_._c_value
    len_name_c = len(ctypes.string_at(name_c))
    lib.cxios_get_filegroup_name(filegroup_hdl, name_c, len_name_c)
    
  

  if name_suffix_ is not None:
  
    name_suffix_c = name_suffix_._c_value
    len_name_suffix_c = len(ctypes.string_at(name_suffix_c))
    lib.cxios_get_filegroup_name_suffix(filegroup_hdl, name_suffix_c, len_name_suffix_c)
    
  

  if output_freq_ is not None: 
  
    output_freq_c = ctypes.pointer(output_freq_)
    # DATE ?
    lib.cxios_get_filegroup_output_freq(filegroup_hdl, output_freq_c)
    
  

  if output_level_ is not None: 
  
    output_level_c = output_level_._c_value
    lib.cxios_get_filegroup_output_level(filegroup_hdl, output_level_c)
    
  

  if par_access_ is not None:
  
    par_access_c = par_access_._c_value
    len_par_access_c = len(ctypes.string_at(par_access_c))
    lib.cxios_get_filegroup_par_access(filegroup_hdl, par_access_c, len_par_access_c)
    
  

  if pool_gatherer_ is not None:
  
    pool_gatherer_c = pool_gatherer_._c_value
    len_pool_gatherer_c = len(ctypes.string_at(pool_gatherer_c))
    lib.cxios_get_filegroup_pool_gatherer(filegroup_hdl, pool_gatherer_c, len_pool_gatherer_c)
    
  

  if pool_reader_ is not None:
  
    pool_reader_c = pool_reader_._c_value
    len_pool_reader_c = len(ctypes.string_at(pool_reader_c))
    lib.cxios_get_filegroup_pool_reader(filegroup_hdl, pool_reader_c, len_pool_reader_c)
    
  

  if pool_writer_ is not None:
  
    pool_writer_c = pool_writer_._c_value
    len_pool_writer_c = len(ctypes.string_at(pool_writer_c))
    lib.cxios_get_filegroup_pool_writer(filegroup_hdl, pool_writer_c, len_pool_writer_c)
    
  

  if read_metadata_par_ is not None: 
  
    read_metadata_par_c = read_metadata_par_._c_value
    lib.cxios_get_filegroup_read_metadata_par(filegroup_hdl, read_metadata_par_c)
    
  

  if reader_ is not None:
  
    reader_c = reader_._c_value
    len_reader_c = len(ctypes.string_at(reader_c))
    lib.cxios_get_filegroup_reader(filegroup_hdl, reader_c, len_reader_c)
    
  

  if record_offset_ is not None: 
  
    record_offset_c = record_offset_._c_value
    lib.cxios_get_filegroup_record_offset(filegroup_hdl, record_offset_c)
    
  

  if split_end_offset_ is not None: 
  
    split_end_offset_c = ctypes.pointer(split_end_offset_)
    # DATE ?
    lib.cxios_get_filegroup_split_end_offset(filegroup_hdl, split_end_offset_c)
    
  

  if split_freq_ is not None: 
  
    split_freq_c = ctypes.pointer(split_freq_)
    # DATE ?
    lib.cxios_get_filegroup_split_freq(filegroup_hdl, split_freq_c)
    
  

  if split_freq_format_ is not None:
  
    split_freq_format_c = split_freq_format_._c_value
    len_split_freq_format_c = len(ctypes.string_at(split_freq_format_c))
    lib.cxios_get_filegroup_split_freq_format(filegroup_hdl, split_freq_format_c, len_split_freq_format_c)
    
  

  if split_last_date_ is not None:
  
    split_last_date_c = split_last_date_._c_value
    len_split_last_date_c = len(ctypes.string_at(split_last_date_c))
    lib.cxios_get_filegroup_split_last_date(filegroup_hdl, split_last_date_c, len_split_last_date_c)
    
  

  if split_start_offset_ is not None: 
  
    split_start_offset_c = ctypes.pointer(split_start_offset_)
    # DATE ?
    lib.cxios_get_filegroup_split_start_offset(filegroup_hdl, split_start_offset_c)
    
  

  if sync_freq_ is not None: 
  
    sync_freq_c = ctypes.pointer(sync_freq_)
    # DATE ?
    lib.cxios_get_filegroup_sync_freq(filegroup_hdl, sync_freq_c)
    
  

  if time_counter_ is not None:
  
    time_counter_c = time_counter_._c_value
    len_time_counter_c = len(ctypes.string_at(time_counter_c))
    lib.cxios_get_filegroup_time_counter(filegroup_hdl, time_counter_c, len_time_counter_c)
    
  

  if time_counter_name_ is not None:
  
    time_counter_name_c = time_counter_name_._c_value
    len_time_counter_name_c = len(ctypes.string_at(time_counter_name_c))
    lib.cxios_get_filegroup_time_counter_name(filegroup_hdl, time_counter_name_c, len_time_counter_name_c)
    
  

  if time_stamp_format_ is not None:
  
    time_stamp_format_c = time_stamp_format_._c_value
    len_time_stamp_format_c = len(ctypes.string_at(time_stamp_format_c))
    lib.cxios_get_filegroup_time_stamp_format(filegroup_hdl, time_stamp_format_c, len_time_stamp_format_c)
    
  

  if time_stamp_name_ is not None:
  
    time_stamp_name_c = time_stamp_name_._c_value
    len_time_stamp_name_c = len(ctypes.string_at(time_stamp_name_c))
    lib.cxios_get_filegroup_time_stamp_name(filegroup_hdl, time_stamp_name_c, len_time_stamp_name_c)
    
  

  if time_units_ is not None:
  
    time_units_c = time_units_._c_value
    len_time_units_c = len(ctypes.string_at(time_units_c))
    lib.cxios_get_filegroup_time_units(filegroup_hdl, time_units_c, len_time_units_c)
    
  

  if timeseries_ is not None:
  
    timeseries_c = timeseries_._c_value
    len_timeseries_c = len(ctypes.string_at(timeseries_c))
    lib.cxios_get_filegroup_timeseries(filegroup_hdl, timeseries_c, len_timeseries_c)
    
  

  if ts_prefix_ is not None:
  
    ts_prefix_c = ts_prefix_._c_value
    len_ts_prefix_c = len(ctypes.string_at(ts_prefix_c))
    lib.cxios_get_filegroup_ts_prefix(filegroup_hdl, ts_prefix_c, len_ts_prefix_c)
    
  

  if type_ is not None:
  
    type_c = type_._c_value
    len_type_c = len(ctypes.string_at(type_c))
    lib.cxios_get_filegroup_type(filegroup_hdl, type_c, len_type_c)
    
  

  if using_server2_ is not None: 
  
    using_server2_c = using_server2_._c_value
    lib.cxios_get_filegroup_using_server2(filegroup_hdl, using_server2_c)
    
  

  if uuid_format_ is not None:
  
    uuid_format_c = uuid_format_._c_value
    len_uuid_format_c = len(ctypes.string_at(uuid_format_c))
    lib.cxios_get_filegroup_uuid_format(filegroup_hdl, uuid_format_c, len_uuid_format_c)
    
  

  if uuid_name_ is not None:
  
    uuid_name_c = uuid_name_._c_value
    len_uuid_name_c = len(ctypes.string_at(uuid_name_c))
    lib.cxios_get_filegroup_uuid_name(filegroup_hdl, uuid_name_c, len_uuid_name_c)
    
  

  if writer_ is not None:
  
    writer_c = writer_._c_value
    len_writer_c = len(ctypes.string_at(writer_c))
    lib.cxios_get_filegroup_writer(filegroup_hdl, writer_c, len_writer_c)
    
  
  return 



@typecheck
def is_defined_filegroup_attr(filegroup_id : String, append : Optional[Bool] = None, comment : Optional[Bool] = None, compression_level : Optional[Bool] = None,   
convention : Optional[Bool] = None, convention_str : Optional[Bool] = None, cyclic : Optional[Bool] = None,   
description : Optional[Bool] = None, enabled : Optional[Bool] = None, format : Optional[Bool] = None,   
gatherer : Optional[Bool] = None, group_ref : Optional[Bool] = None, min_digits : Optional[Bool] = None,   
mode : Optional[Bool] = None, name : Optional[Bool] = None, name_suffix : Optional[Bool] = None,   
output_freq : Optional[Bool] = None, output_level : Optional[Bool] = None, par_access : Optional[Bool] = None,   
pool_gatherer : Optional[Bool] = None, pool_reader : Optional[Bool] = None, pool_writer : Optional[Bool] = None,   
read_metadata_par : Optional[Bool] = None, reader : Optional[Bool] = None, record_offset : Optional[Bool] = None,   
split_end_offset : Optional[Bool] = None, split_freq : Optional[Bool] = None, split_freq_format : Optional[Bool] = None,   
split_last_date : Optional[Bool] = None, split_start_offset : Optional[Bool] = None, sync_freq : Optional[Bool] = None,   
time_counter : Optional[Bool] = None, time_counter_name : Optional[Bool] = None, time_stamp_format : Optional[Bool] = None,   
time_stamp_name : Optional[Bool] = None, time_units : Optional[Bool] = None, timeseries : Optional[Bool] = None,   
ts_prefix : Optional[Bool] = None, type : Optional[Bool] = None, using_server2 : Optional[Bool] = None,   
uuid_format : Optional[Bool] = None, uuid_name : Optional[Bool] = None, writer : Optional[Bool] = None):

  
  filegroup_hdl = FileGroup()
  

  get_filegroup_handle(filegroup_id, filegroup_hdl)
  is_defined_filegroup_attr_hdl_(filegroup_hdl, append, comment, compression_level, convention, convention_str, cyclic, description,  
   enabled, format, gatherer, group_ref, min_digits, mode, name, name_suffix, output_freq, output_level,  
   par_access, pool_gatherer, pool_reader, pool_writer, read_metadata_par, reader, record_offset,  
   split_end_offset, split_freq, split_freq_format, split_last_date, split_start_offset, sync_freq,  
   time_counter, time_counter_name, time_stamp_format, time_stamp_name, time_units, timeseries,  
   ts_prefix, type, using_server2, uuid_format, uuid_name, writer)
  return 



@typecheck
def is_defined_filegroup_attr_hdl(filegroup_hdl : FileGroup, append : Optional[Bool] = None, comment : Optional[Bool] = None,   
compression_level : Optional[Bool] = None, convention : Optional[Bool] = None, convention_str : Optional[Bool] = None,   
cyclic : Optional[Bool] = None, description : Optional[Bool] = None, enabled : Optional[Bool] = None,   
format : Optional[Bool] = None, gatherer : Optional[Bool] = None, group_ref : Optional[Bool] = None,   
min_digits : Optional[Bool] = None, mode : Optional[Bool] = None, name : Optional[Bool] = None,   
name_suffix : Optional[Bool] = None, output_freq : Optional[Bool] = None, output_level : Optional[Bool] = None,   
par_access : Optional[Bool] = None, pool_gatherer : Optional[Bool] = None, pool_reader : Optional[Bool] = None,   
pool_writer : Optional[Bool] = None, read_metadata_par : Optional[Bool] = None, reader : Optional[Bool] = None,   
record_offset : Optional[Bool] = None, split_end_offset : Optional[Bool] = None, split_freq : Optional[Bool] = None,   
split_freq_format : Optional[Bool] = None, split_last_date : Optional[Bool] = None, split_start_offset : Optional[Bool] = None,   
sync_freq : Optional[Bool] = None, time_counter : Optional[Bool] = None, time_counter_name : Optional[Bool] = None,   
time_stamp_format : Optional[Bool] = None, time_stamp_name : Optional[Bool] = None, time_units : Optional[Bool] = None,   
timeseries : Optional[Bool] = None, ts_prefix : Optional[Bool] = None, type : Optional[Bool] = None,   
using_server2 : Optional[Bool] = None, uuid_format : Optional[Bool] = None, uuid_name : Optional[Bool] = None,   
writer : Optional[Bool] = None):

  
  is_defined_filegroup_attr_hdl_(filegroup_hdl, append, comment, compression_level, convention, convention_str, cyclic, description,  
   enabled, format, gatherer, group_ref, min_digits, mode, name, name_suffix, output_freq, output_level,  
   par_access, pool_gatherer, pool_reader, pool_writer, read_metadata_par, reader, record_offset,  
   split_end_offset, split_freq, split_freq_format, split_last_date, split_start_offset, sync_freq,  
   time_counter, time_counter_name, time_stamp_format, time_stamp_name, time_units, timeseries,  
   ts_prefix, type, using_server2, uuid_format, uuid_name, writer)
  return 



@typecheck
def is_defined_filegroup_attr_hdl_(filegroup_hdl : FileGroup, append_ : Optional[Bool] = None, comment_ : Optional[Bool] = None,   
compression_level_ : Optional[Bool] = None, convention_ : Optional[Bool] = None, convention_str_ : Optional[Bool] = None,   
cyclic_ : Optional[Bool] = None, description_ : Optional[Bool] = None, enabled_ : Optional[Bool] = None,   
format_ : Optional[Bool] = None, gatherer_ : Optional[Bool] = None, group_ref_ : Optional[Bool] = None,   
min_digits_ : Optional[Bool] = None, mode_ : Optional[Bool] = None, name_ : Optional[Bool] = None,   
name_suffix_ : Optional[Bool] = None, output_freq_ : Optional[Bool] = None, output_level_ : Optional[Bool] = None,   
par_access_ : Optional[Bool] = None, pool_gatherer_ : Optional[Bool] = None, pool_reader_ : Optional[Bool] = None,   
pool_writer_ : Optional[Bool] = None, read_metadata_par_ : Optional[Bool] = None, reader_ : Optional[Bool] = None,   
record_offset_ : Optional[Bool] = None, split_end_offset_ : Optional[Bool] = None, split_freq_ : Optional[Bool] = None,   
split_freq_format_ : Optional[Bool] = None, split_last_date_ : Optional[Bool] = None, split_start_offset_ : Optional[Bool] = None,   
sync_freq_ : Optional[Bool] = None, time_counter_ : Optional[Bool] = None, time_counter_name_ : Optional[Bool] = None,   
time_stamp_format_ : Optional[Bool] = None, time_stamp_name_ : Optional[Bool] = None, time_units_ : Optional[Bool] = None,   
timeseries_ : Optional[Bool] = None, ts_prefix_ : Optional[Bool] = None, type_ : Optional[Bool] = None,   
using_server2_ : Optional[Bool] = None, uuid_format_ : Optional[Bool] = None, uuid_name_ : Optional[Bool] = None,   
writer_ : Optional[Bool] = None):

  
  

  if append_  is not None:
    append_c = lib.cxios_is_defined_filegroup_append(filegroup_hdl)
    append_._c_value = ctypes.c_bool(append_c)
    
  

  if comment_  is not None:
    comment_c = lib.cxios_is_defined_filegroup_comment(filegroup_hdl)
    comment_._c_value = ctypes.c_bool(comment_c)
    
  

  if compression_level_  is not None:
    compression_level_c = lib.cxios_is_defined_filegroup_compression_level(filegroup_hdl)
    compression_level_._c_value = ctypes.c_bool(compression_level_c)
    
  

  if convention_  is not None:
    convention_c = lib.cxios_is_defined_filegroup_convention(filegroup_hdl)
    convention_._c_value = ctypes.c_bool(convention_c)
    
  

  if convention_str_  is not None:
    convention_str_c = lib.cxios_is_defined_filegroup_convention_str(filegroup_hdl)
    convention_str_._c_value = ctypes.c_bool(convention_str_c)
    
  

  if cyclic_  is not None:
    cyclic_c = lib.cxios_is_defined_filegroup_cyclic(filegroup_hdl)
    cyclic_._c_value = ctypes.c_bool(cyclic_c)
    
  

  if description_  is not None:
    description_c = lib.cxios_is_defined_filegroup_description(filegroup_hdl)
    description_._c_value = ctypes.c_bool(description_c)
    
  

  if enabled_  is not None:
    enabled_c = lib.cxios_is_defined_filegroup_enabled(filegroup_hdl)
    enabled_._c_value = ctypes.c_bool(enabled_c)
    
  

  if format_  is not None:
    format_c = lib.cxios_is_defined_filegroup_format(filegroup_hdl)
    format_._c_value = ctypes.c_bool(format_c)
    
  

  if gatherer_  is not None:
    gatherer_c = lib.cxios_is_defined_filegroup_gatherer(filegroup_hdl)
    gatherer_._c_value = ctypes.c_bool(gatherer_c)
    
  

  if group_ref_  is not None:
    group_ref_c = lib.cxios_is_defined_filegroup_group_ref(filegroup_hdl)
    group_ref_._c_value = ctypes.c_bool(group_ref_c)
    
  

  if min_digits_  is not None:
    min_digits_c = lib.cxios_is_defined_filegroup_min_digits(filegroup_hdl)
    min_digits_._c_value = ctypes.c_bool(min_digits_c)
    
  

  if mode_  is not None:
    mode_c = lib.cxios_is_defined_filegroup_mode(filegroup_hdl)
    mode_._c_value = ctypes.c_bool(mode_c)
    
  

  if name_  is not None:
    name_c = lib.cxios_is_defined_filegroup_name(filegroup_hdl)
    name_._c_value = ctypes.c_bool(name_c)
    
  

  if name_suffix_  is not None:
    name_suffix_c = lib.cxios_is_defined_filegroup_name_suffix(filegroup_hdl)
    name_suffix_._c_value = ctypes.c_bool(name_suffix_c)
    
  

  if output_freq_  is not None:
    output_freq_c = lib.cxios_is_defined_filegroup_output_freq(filegroup_hdl)
    output_freq_._c_value = ctypes.c_bool(output_freq_c)
    
  

  if output_level_  is not None:
    output_level_c = lib.cxios_is_defined_filegroup_output_level(filegroup_hdl)
    output_level_._c_value = ctypes.c_bool(output_level_c)
    
  

  if par_access_  is not None:
    par_access_c = lib.cxios_is_defined_filegroup_par_access(filegroup_hdl)
    par_access_._c_value = ctypes.c_bool(par_access_c)
    
  

  if pool_gatherer_  is not None:
    pool_gatherer_c = lib.cxios_is_defined_filegroup_pool_gatherer(filegroup_hdl)
    pool_gatherer_._c_value = ctypes.c_bool(pool_gatherer_c)
    
  

  if pool_reader_  is not None:
    pool_reader_c = lib.cxios_is_defined_filegroup_pool_reader(filegroup_hdl)
    pool_reader_._c_value = ctypes.c_bool(pool_reader_c)
    
  

  if pool_writer_  is not None:
    pool_writer_c = lib.cxios_is_defined_filegroup_pool_writer(filegroup_hdl)
    pool_writer_._c_value = ctypes.c_bool(pool_writer_c)
    
  

  if read_metadata_par_  is not None:
    read_metadata_par_c = lib.cxios_is_defined_filegroup_read_metadata_par(filegroup_hdl)
    read_metadata_par_._c_value = ctypes.c_bool(read_metadata_par_c)
    
  

  if reader_  is not None:
    reader_c = lib.cxios_is_defined_filegroup_reader(filegroup_hdl)
    reader_._c_value = ctypes.c_bool(reader_c)
    
  

  if record_offset_  is not None:
    record_offset_c = lib.cxios_is_defined_filegroup_record_offset(filegroup_hdl)
    record_offset_._c_value = ctypes.c_bool(record_offset_c)
    
  

  if split_end_offset_  is not None:
    split_end_offset_c = lib.cxios_is_defined_filegroup_split_end_offset(filegroup_hdl)
    split_end_offset_._c_value = ctypes.c_bool(split_end_offset_c)
    
  

  if split_freq_  is not None:
    split_freq_c = lib.cxios_is_defined_filegroup_split_freq(filegroup_hdl)
    split_freq_._c_value = ctypes.c_bool(split_freq_c)
    
  

  if split_freq_format_  is not None:
    split_freq_format_c = lib.cxios_is_defined_filegroup_split_freq_format(filegroup_hdl)
    split_freq_format_._c_value = ctypes.c_bool(split_freq_format_c)
    
  

  if split_last_date_  is not None:
    split_last_date_c = lib.cxios_is_defined_filegroup_split_last_date(filegroup_hdl)
    split_last_date_._c_value = ctypes.c_bool(split_last_date_c)
    
  

  if split_start_offset_  is not None:
    split_start_offset_c = lib.cxios_is_defined_filegroup_split_start_offset(filegroup_hdl)
    split_start_offset_._c_value = ctypes.c_bool(split_start_offset_c)
    
  

  if sync_freq_  is not None:
    sync_freq_c = lib.cxios_is_defined_filegroup_sync_freq(filegroup_hdl)
    sync_freq_._c_value = ctypes.c_bool(sync_freq_c)
    
  

  if time_counter_  is not None:
    time_counter_c = lib.cxios_is_defined_filegroup_time_counter(filegroup_hdl)
    time_counter_._c_value = ctypes.c_bool(time_counter_c)
    
  

  if time_counter_name_  is not None:
    time_counter_name_c = lib.cxios_is_defined_filegroup_time_counter_name(filegroup_hdl)
    time_counter_name_._c_value = ctypes.c_bool(time_counter_name_c)
    
  

  if time_stamp_format_  is not None:
    time_stamp_format_c = lib.cxios_is_defined_filegroup_time_stamp_format(filegroup_hdl)
    time_stamp_format_._c_value = ctypes.c_bool(time_stamp_format_c)
    
  

  if time_stamp_name_  is not None:
    time_stamp_name_c = lib.cxios_is_defined_filegroup_time_stamp_name(filegroup_hdl)
    time_stamp_name_._c_value = ctypes.c_bool(time_stamp_name_c)
    
  

  if time_units_  is not None:
    time_units_c = lib.cxios_is_defined_filegroup_time_units(filegroup_hdl)
    time_units_._c_value = ctypes.c_bool(time_units_c)
    
  

  if timeseries_  is not None:
    timeseries_c = lib.cxios_is_defined_filegroup_timeseries(filegroup_hdl)
    timeseries_._c_value = ctypes.c_bool(timeseries_c)
    
  

  if ts_prefix_  is not None:
    ts_prefix_c = lib.cxios_is_defined_filegroup_ts_prefix(filegroup_hdl)
    ts_prefix_._c_value = ctypes.c_bool(ts_prefix_c)
    
  

  if type_  is not None:
    type_c = lib.cxios_is_defined_filegroup_type(filegroup_hdl)
    type_._c_value = ctypes.c_bool(type_c)
    
  

  if using_server2_  is not None:
    using_server2_c = lib.cxios_is_defined_filegroup_using_server2(filegroup_hdl)
    using_server2_._c_value = ctypes.c_bool(using_server2_c)
    
  

  if uuid_format_  is not None:
    uuid_format_c = lib.cxios_is_defined_filegroup_uuid_format(filegroup_hdl)
    uuid_format_._c_value = ctypes.c_bool(uuid_format_c)
    
  

  if uuid_name_  is not None:
    uuid_name_c = lib.cxios_is_defined_filegroup_uuid_name(filegroup_hdl)
    uuid_name_._c_value = ctypes.c_bool(uuid_name_c)
    
  

  if writer_  is not None:
    writer_c = lib.cxios_is_defined_filegroup_writer(filegroup_hdl)
    writer_._c_value = ctypes.c_bool(writer_c)
    
  
  return 



