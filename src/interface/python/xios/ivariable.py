from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure, pointer
from xios.config import lib, typecheck, String, Bool, CObject as Variable
from typing import Union
from xios.ovariable import Variable
from xios.ovariablegroup_attr import VariableGroup


#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_variable_handle_create.argtypes = [POINTER(Variable), c_char_p, c_int]
lib.cxios_variable_handle_create.restype = None

lib.cxios_variablegroup_handle_create.argtypes = [POINTER(VariableGroup), c_char_p, c_int]
lib.cxios_variablegroup_handle_create.restype = None

lib.cxios_variable_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_variable_valid_id.restype = c_bool

lib.cxios_variablegroup_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_variablegroup_valid_id.restype = c_bool

#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def get_variable_handle(idt: Union[str, String], ret: Variable):
    idt = String(idt)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    lib.cxios_variable_handle_create(byref(ret), idt_c, len_idt_c)

@typecheck
def get_variablegroup_handle(idt: Union[str, String], ret: VariableGroup):
    idt = String(idt)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    lib.cxios_variablegroup_handle_create(byref(ret), idt_c, len_idt_c)

@typecheck
def is_valid_variable(idt: Union[str, String]) -> bool:
    idt = String(idt)
    is_val = Bool(False)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)
    lib.cxios_variable_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)

@typecheck
def is_valid_variablegroup(idt: Union[str, String]) -> bool:
    idt = String(idt)
    is_val = Bool(False)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)
    lib.cxios_variablegroup_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)
