# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.obj import Object, swipswap_ctx
from xios.icontext import get_current_context_id, set_current_context
from xios.idate import Date
from xios.iduration import Duration


class Context_(Object): 
  _object_type = 'context'
  FIELD_META = {
  'context_id': {'type': String},
  'attached_mode': {'type': Bool},
  'default_gatherer': {'type': String},
  'default_pool': {'type': String},
  'default_pool_gatherer': {'type': String},
  'default_pool_reader': {'type': String},
  'default_pool_writer': {'type': String},
  'default_reader': {'type': String},
  'default_using_server2': {'type': Bool},
  'default_writer': {'type': String},
  'output_dir': {'type': String},
  }
  
  def __init__(self, arg1 = None, arg2 : str = None, **kwargs):
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
  
  
Context_._generate_child_methods()

