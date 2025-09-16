from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure, pointer
from xios.config import lib, typecheck, String, Bool
from typing import Union
from xios.ogrid_attr import Grid
from xios.ogridgroup_attr import GridGroup


#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_grid_handle_create.argtypes = [POINTER(Grid), c_char_p, c_int]
lib.cxios_grid_handle_create.restype = None

lib.cxios_gridgroup_handle_create.argtypes = [POINTER(GridGroup), c_char_p, c_int]
lib.cxios_gridgroup_handle_create.restype = None

lib.cxios_grid_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_grid_valid_id.restype = c_bool

lib.cxios_gridgroup_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_gridgroup_valid_id.restype = c_bool

#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def get_grid_handle(idt: Union[str, String], ret: Grid):
    idt = String(idt)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    lib.cxios_grid_handle_create(byref(ret), idt_c, len_idt_c)

@typecheck
def get_gridgroup_handle(idt: Union[str, String], ret: GridGroup):
    idt = String(idt)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    lib.cxios_gridgroup_handle_create(byref(ret), idt_c, len_idt_c)

@typecheck
def is_valid_grid(idt: Union[str, String]) -> bool:
    idt = String(idt)
    is_val = Bool(False)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)
    lib.cxios_grid_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)

@typecheck
def is_valid_gridgroup(idt: Union[str, String]) -> bool:
    idt = String(idt)
    is_val = Bool(False)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)
    lib.cxios_gridgroup_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)
