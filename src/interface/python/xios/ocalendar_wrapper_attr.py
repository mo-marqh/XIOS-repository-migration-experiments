# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.obj import Object, swipswap_ctx
from xios.icontext import get_current_context_id, set_current_context
from xios.idate import Date
from xios.iduration import Duration


class CalendarWrapper(Object): 
  _object_type = 'calendar_wrapper'
  FIELD_META = {
  'calendar_wrapper_id': {'type': String},
  'comment': {'type': String},
  'day_length': {'type': Int},
  'leap_year_drift': {'type': Double},
  'leap_year_drift_offset': {'type': Double},
  'leap_year_month': {'type': Int},
  'month_lengths': {'type': NpArrayInt, 'ndim': 1},
  'start_date': {'type': String},
  'time_origin': {'type': String},
  'timestep': {'type': Duration},
  'type': {'type': String},
  'year_length': {'type': Int},
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
  
  
CalendarWrapper._generate_child_methods()

