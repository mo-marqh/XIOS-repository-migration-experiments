from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure, pointer
from typing import Union
from xios.config  import lib, typecheck, String, Int, Bool
from xios.odomain_attr import Domain
from xios.oaxis_attr import Axis
from xios.oscalar_attr import Scalar
from xios.ofield import Field
from xios.ofieldgroup_attr import FieldGroup


#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_field_handle_create.argtypes = [POINTER(Field), c_char_p, c_int]
lib.cxios_field_handle_create.restype = None

lib.cxios_fieldgroup_handle_create.argtypes = [POINTER(FieldGroup), c_char_p, c_int]
lib.cxios_fieldgroup_handle_create.restype = None

lib.cxios_field_get_domain_handle.argtypes = [POINTER(Domain), Field, c_int]
lib.cxios_field_get_domain_handle.restype = None

lib.cxios_field_get_axis_handle.argtypes = [POINTER(Axis), Field, c_int]
lib.cxios_field_get_axis_handle.restype = None

lib.cxios_field_get_scalar_handle.argtypes = [POINTER(Scalar), Field, c_int]
lib.cxios_field_get_scalar_handle.restype = None

lib.cxios_field_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_field_valid_id.restype = None

lib.cxios_fieldgroup_valid_id.argtypes = [POINTER(FieldGroup), c_char_p, c_int]
lib.cxios_fieldgroup_valid_id.restype = None

lib.cxios_field_is_active.argtypes = [POINTER(Field), c_bool, POINTER(c_bool)]
lib.cxios_field_is_active.restype = None

#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def get_field_handle(idt: Union[str, String], ret: Field):
    idt = String(idt)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    lib.cxios_field_handle_create(byref(ret), idt_c, len_idt_c)


@typecheck
def get_fieldgroup_handle(idt: Union[str, String], ret: FieldGroup):
    if not isinstance(ret, FieldGroup):
        raise TypeError("ret should be an instance of xios.FieldGroup")

    idt = String(idt)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    lib.cxios_fieldgroup_handle_create(byref(ret), idt_c, len_idt_c)

@typecheck
def field_get_domain(field: Union[str, String, Field], ret: Domain, idx: int = 0):
    if isinstance(field, str) or isinstance(field, String):
        field_id_get_domain_handle(field, ret, idx)
    elif isinstance(field, Field):
        field_get_domain_handle(field, ret, idx)

@typecheck
def field_get_domain_handle(field_hdl: Field, ret: Domain, idx: int = 0):
    if not isinstance(ret, Domain):
        raise TypeError("ret should be an instance of xios.Domain")

    idx_c = Int(idx)._c_value
    lib.cxios_field_get_domain_handle(pointer(ret), field_hdl, idx_c)


@typecheck
def field_id_get_domain_handle(field_id: Union[str, String], ret: Domain, idx: int = 0):
    field_id = String(field_id)
    idx = Int(idx)
    field_hdl = Field()

    get_field_handle(field_id, field_hdl)
    field_get_domain_handle(field_hdl, ret, idx.value)

@typecheck
def field_get_axis(field: Union[str, String, Field], ret: Axis, idx: int = 0):
    if isinstance(field, str) or isinstance(field, String):
        field_id_get_axis_handle(field, ret, idx)
    elif isinstance(field, Field):
        field_get_axis_handle(field, ret, idx)

@typecheck
def field_get_axis_handle(field_hdl: Field, ret: Axis, idx: int = 0):
    idx_c = Int(idx)._c_value
    lib.cxios_field_get_axis_handle(byref(ret.daddr), byref(field_hdl.daddr), idx_c)


@typecheck
def field_id_get_axis_handle(field_id: Union[str, String], ret: Axis, idx: int = 0):
    field_id = String(field_id)
    idx = Int(idx)
    field_hdl = Field()

    get_field_handle(field_id, field_hdl)
    field_get_axis_handle(field_hdl, ret, idx.value)

@typecheck
def field_get_scalar(field: Union[str, String, Field], ret: Scalar, idx: int = 0):
    if isinstance(field, str) or isinstance(field, String):
        field_id_get_scalar_handle(field, ret, idx)
    elif isinstance(field, Field):
        field_get_scalar_handle(field, ret, idx)

@typecheck
def field_get_scalar_handle(field_hdl: Field, ret: Scalar, idx: int = 0):
    idx_c = Int(idx)._c_value
    lib.cxios_field_get_scalar_handle(byref(ret.daddr), byref(field_hdl.daddr), idx_c)


@typecheck
def field_id_get_scalar_handle(field_id: Union[str, String], ret: Scalar, idx: int = 0):
    field_id = String(field_id)
    idx = Int(idx)
    field_hdl = Field()

    get_field_handle(field_id, field_hdl)
    field_get_scalar_handle(field_hdl, ret, idx.value)


@typecheck
def is_valid_field(idt: Union[str, String]) -> bool:
    idt = String(idt)
    is_val = Bool(False)

    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)

    lib.cxios_field_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)


@typecheck
def is_valid_fieldgroup(idt: Union[str, String]) -> bool:
    idt = String(idt)
    is_val = Bool(False)

    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)

    lib.cxios_fieldgroup_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)

@typecheck
def field_is_active(field: Union[str, String, Field], at_current_timestep_arg : bool = False):
    if isinstance(field, str) or isinstance(field, String):
        return field_is_active_id(field, at_current_timestep_arg)
    elif isinstance(field, Field):
        return field_is_active_hdl(field, at_current_timestep_arg)


@typecheck
def field_is_active_hdl(field_hdl: Field, at_current_timestep_arg: bool = False) -> bool:
    is_act = Bool(False)
    is_act_c = pointer(is_act._c_value)

    lib.cxios_field_is_active(byref(field_hdl), at_current_timestep_arg, is_act_c)
    return bool(is_act_c.contents)


@typecheck
def field_is_active_id(field_id: Union[str, String], at_current_timestep_arg: bool = False) -> bool:
    field_id = String(field_id)
    field_hdl = Field()

    get_field_handle(field_id, field_hdl)
    return field_is_active_hdl(field_hdl, at_current_timestep_arg)
