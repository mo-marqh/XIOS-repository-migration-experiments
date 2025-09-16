# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.obj import Object, swipswap_ctx
from xios.icontext import get_current_context_id, set_current_context
from xios.idate import Date
from xios.iduration import Duration


class DomainGroup(Object): 
  _object_type = 'domaingroup'
  FIELD_META = {
  'domaingroup_id': {'type': String},
  'area': {'type': NpArray, 'ndim': 2},
  'area_1d': {'type': NpArray, 'ndim': 1},
  'area_2d': {'type': NpArray, 'ndim': 2},
  'bounds_lat_1d': {'type': NpArray, 'ndim': 2},
  'bounds_lat_2d': {'type': NpArray, 'ndim': 3},
  'bounds_lat_name': {'type': String},
  'bounds_lon_1d': {'type': NpArray, 'ndim': 2},
  'bounds_lon_2d': {'type': NpArray, 'ndim': 3},
  'bounds_lon_name': {'type': String},
  'chunking_weight_i': {'type': Double},
  'chunking_weight_j': {'type': Double},
  'comment': {'type': String},
  'data_dim': {'type': Int},
  'data_i_index': {'type': NpArrayInt, 'ndim': 1},
  'data_ibegin': {'type': Int},
  'data_j_index': {'type': NpArrayInt, 'ndim': 1},
  'data_jbegin': {'type': Int},
  'data_ni': {'type': Int},
  'data_nj': {'type': Int},
  'dim_i_name': {'type': String},
  'dim_j_name': {'type': String},
  'domain_ref': {'type': String},
  'group_ref': {'type': String},
  'i_index': {'type': NpArrayInt, 'ndim': 1},
  'ibegin': {'type': Int},
  'j_index': {'type': NpArrayInt, 'ndim': 1},
  'jbegin': {'type': Int},
  'lat_name': {'type': String},
  'latvalue_1d': {'type': NpArray, 'ndim': 1},
  'latvalue_2d': {'type': NpArray, 'ndim': 2},
  'lon_name': {'type': String},
  'long_name': {'type': String},
  'lonvalue_1d': {'type': NpArray, 'ndim': 1},
  'lonvalue_2d': {'type': NpArray, 'ndim': 2},
  'mask_1d': {'type': NpArrayBool, 'ndim': 1},
  'mask_2d': {'type': NpArrayBool, 'ndim': 2},
  'name': {'type': String},
  'ni': {'type': Int},
  'ni_glo': {'type': Int},
  'nj': {'type': Int},
  'nj_glo': {'type': Int},
  'nvertex': {'type': Int},
  'nvertex_name': {'type': String},
  'prec': {'type': Int},
  'radius': {'type': Double},
  'standard_name': {'type': String},
  'type': {'type': String},
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
  
  
DomainGroup._generate_child_methods()

