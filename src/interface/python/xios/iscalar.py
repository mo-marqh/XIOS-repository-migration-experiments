from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure, pointer
from xios.config import lib, typecheck, String, Bool
from typing import Union
from xios.oscalar_attr import Scalar
from xios.oscalargroup_attr import ScalarGroup


#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_scalar_handle_create.argtypes = [POINTER(Scalar), c_char_p, c_int]
lib.cxios_scalar_handle_create.restype = None

lib.cxios_scalargroup_handle_create.argtypes = [POINTER(ScalarGroup), c_char_p, c_int]
lib.cxios_scalargroup_handle_create.restype = None

lib.cxios_scalar_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_scalar_valid_id.restype = None

lib.cxios_scalargroup_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_scalargroup_valid_id.restype = None

#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def get_scalar_handle(idt: Union[str, String], ret: Scalar):
    idt = String(idt)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    lib.cxios_scalar_handle_create(byref(ret), idt_c, len_idt_c)

@typecheck
def get_scalargroup_handle(idt: Union[str, String], ret: ScalarGroup):
    idt = String(idt)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    lib.cxios_scalargroup_handle_create(byref(ret), idt_c, len_idt_c)

@typecheck
def is_valid_scalar(idt: Union[str, String]) -> bool:
    idt = String(idt)
    is_val = Bool(False)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)
    lib.cxios_scalar_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)

@typecheck
def is_valid_scalargroup(idt: Union[str, String]) -> bool:
    idt = String(idt)
    is_val = Bool(False)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)
    lib.cxios_scalargroup_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)
