from ctypes import Structure, c_void_p, string_at, pointer
import functools
from xios.config import lib, typecheck, String, CObject, Bool
from typing import Union
import importlib
from xios.icontext import get_current_context_id, set_current_context_id, set_current_context
import numpy as np



def swipswap_ctx(tmp_context_id):
    current_context_id = String()
    get_current_context_id(current_context_id)
    set_current_context_id(String(tmp_context_id))
    return current_context_id


def current_context_id():
    current_context_id = String()
    get_current_context_id(current_context_id)
    return current_context_id.value



#####################################################################################################
##                              Object-oriented super class                                        ##
#####################################################################################################


class Object(CObject):
    FIELD_META = {}

    CHILD_META = {
    'scalargroup': ['scalar', 'scalargroup'],
    'axisgroup': ['axis', 'axisgroup'],
    'domaingroup': ['domain', 'domaingroup'],
    'file': ['fieldgroup', 'field', 'variablegroup', 'variable'],
    'filegroup': ['file', 'filegroup'],
    'gridgroup': ['grid', 'gridgroup'],
    'fieldgroup': ['scalar', 'field', 'fieldgroup'],
    'grid': ['scalar', 'axis', 'domain'],
    'field': ['variablegroup', 'variable'],
    'fieldgroup': ['field', 'fieldgroup'],
    'domain': [
    'zoom_domain', 'extract_domain', 'interpolate_domain', 'generate_rectilinear_domain',
    'compute_connectivity_domain', 'expand_domain', 'redistribute_domain'
    ],
    'axis': [
    'zoom_axis', 'interpolate_axis', 'inverse_axis', 'reduce_domain_to_axis',
    'extract_domain_to_axis', 'redistribute_axis'
    ],
    'scalar': [
    'reduce_axis_to_scalar', 'extract_axis_to_scalar', 'reduce_domain_to_scalar',
    'redistribute_scalar'
    ]
}


    def __init__(self, arg1 = None, arg2 : str = None):
        if arg1 is None and arg2 is None : return
        self._module_name_attr =  f"xios.i{self._object_type.lower()}_attr"
        s = self._manage_group(self._object_type)
        if self._object_type.lower() in ['context', 'variable', 'field']:
            self._omodule_name =  f"xios.o{self._object_type.lower()}"
        else:
            self._omodule_name =  f"xios.o{self._object_type.lower()}_attr"
        self._module_name  = f'xios.i{s.lower()}'
        class_ins    = self._resolve_func(f"{self._oclass_name}", self._omodule_name)


        if isinstance(arg1, str) and arg2 is None :
            # case where we used the id (the object is already defined thanks to the iodef.xml)
            object_id = arg1
            is_valid_func =  self._resolve_func(f"is_valid_{self._object_type}", self._module_name)
            if not is_valid_func(object_id) :
                raise ValueError(f"The id \'{object_id}\' does not correspond to any {self._object_type}")
            self._object_id = String(object_id)
            get_hdl_func = self._resolve_func(f"get_{self._object_type}_handle", self._module_name)
            get_hdl_func(self._object_id, self)
            self._object_hdl = self
            self._id = String(object_id)
        elif isinstance(arg1, Object):
            parent       = arg1
            tree_module  = f"xios.ixml_tree"
            s =  str(type(parent._object_hdl))
            start = next((i for i, c in enumerate(s) if c.isupper()), None)
            if start is not None:
                result = s[start:-2].lower()
            s = self._manage_group(result, True)
            if s : s = 'to' + s
            self._parent_type = s
            parent_hdl   = parent._object_hdl
            child_hdl    = class_ins()
            self._is_id_defined = arg2 is not None
            if self._is_id_defined : self._id = arg2
            add_func = self._resolve_func(f"add_{self._object_type.replace('_', '')}{self._parent_type}", tree_module)
            add_func(parent_hdl, child_hdl, arg2)
            self._object_hdl = child_hdl


        else:
            raise TypeError("Wrong type argumetn passed to the constructor !")


    def _get_classname(self, s):
        return ''.join(word.title() for word in s.split('_'))
    
    def _manage_group(self, s, parent = False):
        suff = s[-5::]
        if suff == 'group':
            s = self._get_classname(s[0:-5])
            self._class_name = s+'Group'
            self._oclass_name = s+'Group'
            if parent : s = ""
        else :
            self._class_name   = self._get_classname(s)
            self._oclass_name  = self._class_name
            # if (self._class_name == 'Context' or self._class_name == 'Field'):
            #      self._oclass_name = self._class_name + '_'
        
        return s



    def _resolve_func(self, func_name: str, module: str):
        # prefix = f"xios.i{self._object_type.lower()}

        mod = importlib.import_module(module)

        try:
            return getattr(mod, func_name)
        except AttributeError:
            raise RuntimeError(f"Function '{func_name}' not found in module '{mod.__name__}'")

    def __getattr__(self, name: str):

        cls_attr = getattr(type(self), name, None)
        if callable(cls_attr):
            return functools.partial(cls_attr, self)

        if name.startswith("_") or name not in self.FIELD_META:
            raise AttributeError(f"'{type(self).__name__}' object has no attribute '{name}'")


        is_defined_func = self._resolve_func(f"is_defined_{self._object_type}_attr_hdl", self._module_name_attr)
        is_defined = Bool(False)
        is_defined_func(self._object_hdl, **{name: is_defined})
        if not is_defined.value:
            raise AttributeError(f"Attribute '{name}' is not defined for the {self._object_type} '{self._object_id}'")

        meta = self.FIELD_META[name]
        WrappedType = meta["type"]
        wrapper = WrappedType() if "ndim" in meta else WrappedType()

        getter_func = self._resolve_func(f"get_{self._object_type}_attr_hdl", self._module_name_attr)
        current_context_id = swipswap_ctx(self._context_id)
        getter_func(self._object_hdl, **{name: wrapper})
        swipswap_ctx(current_context_id)
        return wrapper.value


    def __setattr__(self, name: str, value):
        if name.startswith("_"):
            object.__setattr__(self, name, value)
            return

        cls_attr = getattr(type(self), name, None)
        if isinstance(cls_attr, property) and cls_attr.fset is not None:
            cls_attr.fset(self, value)
            return


        if name not in self.FIELD_META:
            raise AttributeError(f"No such attribute: {name}")

        meta = self.FIELD_META[name]
        WrappedType = meta["type"]

        if isinstance(value, np.ndarray) and "ndim" in meta:
            if value.ndim != meta["ndim"]:
                raise ValueError(f"{name} must be a {meta['ndim']}D NumPy array")

        wrapped_value = WrappedType(value)
        setter_func = self._resolve_func(f"set_{self._object_type}_attr_hdl", self._module_name_attr)
        current_context_id = swipswap_ctx(self._context_id)
        setter_func(self._object_hdl, **{name: wrapped_value})
        swipswap_ctx(current_context_id)

    @classmethod
    def _generate_child_methods(cls):
        for parent, child_types in Object.CHILD_META.items():
            for child in child_types:
                def make_method(parent=parent, child=child):
                    def method(self, idt=None):
                        class_name = cls._get_classname(cls, child)
                        child_class = cls._resolve_func(cls, class_name, f"xios.o{child}_attr")                   
                        child_obj = child_class(self, idt)
                        return child_obj
                    return method

                setattr(cls, f"add_{child}", make_method(parent, child))

    
    def __str__(self):
        s = ""
        s += f"{self._class_name} '{self.id}' "
        s += f"in context '{self._context_id}'\n"
        for k in self.FIELD_META:
            if k != f'{self._object_type}_id':
                value = getattr(self, k, None)
                if value is not None:
                    s += f"  {k} = {value}\n"
        return s




    @property
    def id(self):
        return self._id

