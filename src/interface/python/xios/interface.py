from xios.config import String, typecheck
from typing import Union, Optional
from xios.config import CObject
import importlib



@typecheck
def get_handle(idt : Union[String, str], ret : CObject):
    obj_type = str(ret.__class__.__name__).lower()
    mod = importlib.import_module(f'xios.i{obj_type}')
    func_name = f'get_{obj_type}_handle'
    func = getattr(mod, func_name)
    func(idt, ret)
    
@typecheck
def set_attr(obj_hdl : CObject, **kwargs):
    obj_type = str(obj_hdl.__class__.__name__).lower()
    mod = importlib.import_module(f'xios.i{obj_type}_attr')
    func_name = f'set_{obj_type}_attr_hdl'
    func = getattr(mod, func_name)
    func(obj_hdl, **kwargs)
    
@typecheck
def get_attr(obj_hdl : CObject, **kwargs):
    obj_type = str(obj_hdl.__class__.__name__).lower()
    mod = importlib.import_module(f'xios.i{obj_type}_attr')
    func_name = f'get_{obj_type}_attr_hdl'
    func = getattr(mod, func_name)
    func(obj_hdl, **kwargs)
    
@typecheck
def is_defined_attr(obj_hdl : CObject, **kwargs):
    obj_type = str(obj_hdl.__class__.__name__).lower()
    mod = importlib.import_module(f'xios.i{obj_type}_attr')
    func_name = f'is_defined_{obj_type}_attr_hdl'
    func = getattr(mod, func_name)
    func(obj_hdl, **kwargs)

@typecheck
def add_child(parent_hdl : CObject, child_hdl : CObject, child_id : Optional[Union[str, String]] = None):
    parent_type = str(parent_hdl.__class__.__name__).lower()
    child_type = str(child_hdl.__class__.__name__).lower()
    if child_type not in child_hdl.CHILD_META[parent_type]:
        raise TypeError(f"{child_type} cannot be added as a child to {parent_type}, please check the corresponding table : {child_hdl.CHILD_META}")
    mod = importlib.import_module(f'xios.ixml_tree')
    if parent_type[-5::] == 'group':
        func_name = f'add_{child_type}'
    else:
        func_name = f'add_{child_type}to{parent_type}'
    func = getattr(mod, func_name)
    func(parent_hdl, child_hdl, child_id)

