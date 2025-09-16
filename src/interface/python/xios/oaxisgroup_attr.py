# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.obj import Object, swipswap_ctx
from xios.icontext import get_current_context_id, set_current_context
from xios.idate import Date
from xios.iduration import Duration


class AxisGroup(Object): 
  _object_type = 'axisgroup'
  FIELD_META = {
  'axisgroup_id': {'type': String},
  'axis_ref': {'type': String},
  'axis_type': {'type': String},
  'begin': {'type': Int},
  'bounds': {'type': NpArray, 'ndim': 2},
  'bounds_name': {'type': String},
  'chunking_weight': {'type': Double},
  'comment': {'type': String},
  'convert_from_factor': {'type': Double},
  'data_begin': {'type': Int},
  'data_index': {'type': NpArrayInt, 'ndim': 1},
  'data_n': {'type': Int},
  'dim_name': {'type': String},
  'formula': {'type': String},
  'formula_bounds': {'type': String},
  'formula_term': {'type': String},
  'formula_term_bounds': {'type': String},
  'group_ref': {'type': String},
  'index': {'type': NpArrayInt, 'ndim': 1},
  'label': {'type': String},
  'long_name': {'type': String},
  'mask': {'type': NpArrayBool, 'ndim': 1},
  'n': {'type': Int},
  'n_distributed_partition': {'type': Int},
  'n_glo': {'type': Int},
  'name': {'type': String},
  'positive': {'type': String},
  'prec': {'type': Int},
  'standard_name': {'type': String},
  'unit': {'type': String},
  'value': {'type': NpArray, 'ndim': 1},
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
  
  
AxisGroup._generate_child_methods()

