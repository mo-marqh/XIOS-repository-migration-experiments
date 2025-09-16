# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.obj import Object, swipswap_ctx
from xios.icontext import get_current_context_id, set_current_context
from xios.idate import Date
from xios.iduration import Duration


class File(Object): 
  _object_type = 'file'
  FIELD_META = {
  'file_id': {'type': String},
  'append': {'type': Bool},
  'comment': {'type': String},
  'compression_level': {'type': Int},
  'convention': {'type': String},
  'convention_str': {'type': String},
  'cyclic': {'type': Bool},
  'description': {'type': String},
  'enabled': {'type': Bool},
  'format': {'type': String},
  'gatherer': {'type': String},
  'min_digits': {'type': Int},
  'mode': {'type': String},
  'name': {'type': String},
  'name_suffix': {'type': String},
  'output_freq': {'type': Duration},
  'output_level': {'type': Int},
  'par_access': {'type': String},
  'pool_gatherer': {'type': String},
  'pool_reader': {'type': String},
  'pool_writer': {'type': String},
  'read_metadata_par': {'type': Bool},
  'reader': {'type': String},
  'record_offset': {'type': Int},
  'split_end_offset': {'type': Duration},
  'split_freq': {'type': Duration},
  'split_freq_format': {'type': String},
  'split_last_date': {'type': String},
  'split_start_offset': {'type': Duration},
  'sync_freq': {'type': Duration},
  'time_counter': {'type': String},
  'time_counter_name': {'type': String},
  'time_stamp_format': {'type': String},
  'time_stamp_name': {'type': String},
  'time_units': {'type': String},
  'timeseries': {'type': String},
  'ts_prefix': {'type': String},
  'type': {'type': String},
  'using_server2': {'type': Bool},
  'uuid_format': {'type': String},
  'uuid_name': {'type': String},
  'writer': {'type': String},
  }
  
  def __init__(self, arg1 = None, arg2 : str = None, **kwargs):
    self._context_id = String()
    get_current_context_id(self._context_id)
    super().__init__(arg1, arg2)
    for key, value in kwargs.items():
      if key in self.FIELD_META:
        setattr(self, key, value)
      else: 
        raise AttributeError(f"Unknown attribute '{key}' for class '{self.__class__.__name__}'")
  
  @property
  def context_id(self):
    return self._context_id
  
  def set_attr(self, **kwargs):
    current_context_id = swipswap_ctx(self._context_id)
    for key, value in kwargs.items():
      if key in self.FIELD_META:
        setattr(self, key, value)
      else:
        raise AttributeError(f'Unknown attribute {key} for class {self.__class__.__name__}')
    swipswap_ctx(current_context_id)
  
  
File._generate_child_methods()

