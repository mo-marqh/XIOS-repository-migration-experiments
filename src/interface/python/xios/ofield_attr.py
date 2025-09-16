# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.obj import Object, swipswap_ctx
from xios.icontext import get_current_context_id, set_current_context
from xios.idate import Date
from xios.iduration import Duration


class Field_(Object): 
  _object_type = 'field'
  FIELD_META = {
  'field_id': {'type': String},
  'add_offset': {'type': Double},
  'axis_ref': {'type': String},
  'build_workflow_graph': {'type': Bool},
  'cell_methods': {'type': String},
  'cell_methods_mode': {'type': String},
  'check_if_active': {'type': Bool},
  'chunking_blocksize_target': {'type': Double},
  'comment': {'type': String},
  'compression_level': {'type': Int},
  'compression_params': {'type': NpArray, 'ndim': 1},
  'compression_type': {'type': String},
  'conversion_by_netcdf': {'type': Bool},
  'default_value': {'type': Double},
  'detect_missing_value': {'type': Bool},
  'domain_ref': {'type': String},
  'enabled': {'type': Bool},
  'expr': {'type': String},
  'field_ref': {'type': String},
  'freq_offset': {'type': Duration},
  'freq_op': {'type': Duration},
  'grid_path': {'type': String},
  'grid_ref': {'type': String},
  'indexed_output': {'type': Bool},
  'level': {'type': Int},
  'long_name': {'type': String},
  'name': {'type': String},
  'operation': {'type': String},
  'prec': {'type': Int},
  'read_access': {'type': Bool},
  'scalar_ref': {'type': String},
  'scale_factor': {'type': Double},
  'standard_name': {'type': String},
  'ts_enabled': {'type': Bool},
  'ts_split_freq': {'type': Duration},
  'unit': {'type': String},
  'valid_max': {'type': Double},
  'valid_min': {'type': Double},
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
  
  
Field_._generate_child_methods()

