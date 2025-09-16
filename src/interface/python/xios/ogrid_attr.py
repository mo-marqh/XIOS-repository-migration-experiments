# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.obj import Object, swipswap_ctx
from xios.icontext import get_current_context_id, set_current_context
from xios.idate import Date
from xios.iduration import Duration


class Grid(Object): 
  _object_type = 'grid'
  FIELD_META = {
  'grid_id': {'type': String},
  'comment': {'type': String},
  'description': {'type': String},
  'mask_0d': {'type': NpArrayBool, 'ndim': 1},
  'mask_1d': {'type': NpArrayBool, 'ndim': 1},
  'mask_2d': {'type': NpArrayBool, 'ndim': 2},
  'mask_3d': {'type': NpArrayBool, 'ndim': 3},
  'mask_4d': {'type': NpArrayBool, 'ndim': 4},
  'mask_5d': {'type': NpArrayBool, 'ndim': 5},
  'mask_6d': {'type': NpArrayBool, 'ndim': 6},
  'mask_7d': {'type': NpArrayBool, 'ndim': 7},
  'name': {'type': String},
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
  
  
Grid._generate_child_methods()

