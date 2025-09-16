# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.obj import Object, swipswap_ctx
from xios.icontext import get_current_context_id, set_current_context
from xios.idate import Date
from xios.iduration import Duration


class ScalarGroup(Object): 
  _object_type = 'scalargroup'
  FIELD_META = {
  'scalargroup_id': {'type': String},
  'axis_type': {'type': String},
  'bounds': {'type': NpArray, 'ndim': 1},
  'bounds_name': {'type': String},
  'comment': {'type': String},
  'group_ref': {'type': String},
  'label': {'type': String},
  'long_name': {'type': String},
  'mask': {'type': Bool},
  'n': {'type': Int},
  'name': {'type': String},
  'positive': {'type': String},
  'prec': {'type': Int},
  'scalar_ref': {'type': String},
  'standard_name': {'type': String},
  'unit': {'type': String},
  'value': {'type': Double},
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
  
  
ScalarGroup._generate_child_methods()

