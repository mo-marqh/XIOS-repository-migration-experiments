from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure, pointer
import os
from xios.config import lib, typecheck, String, Bool
from xios.oaxis_attr import Axis
from xios.oaxisgroup_attr import AxisGroup
from typing import Union


#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_axis_handle_create.argtypes = [POINTER(Axis), c_char_p, c_int]
lib.cxios_axis_handle_create.restype = None

lib.cxios_axisgroup_handle_create.argtypes = [POINTER(AxisGroup), c_char_p, c_int]
lib.cxios_axisgroup_handle_create.restype = None

lib.cxios_axis_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_axis_valid_id.restype = c_bool

lib.cxios_axisgroup_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_axisgroup_valid_id.restype = c_bool

#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def get_axis_handle(idt: Union[str, String], ret: Axis):
    """
    Gets the axis handle associated with the given axis id.

    Parameters:
        idt (Union[str, String]): The axis identifier as a string or String object.
        ret (Axis): The Axis object to be filled with the handle (passed by reference).

    Returns:
        None: The function modifies `ret` in place.
    """
    idt = String(idt)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    lib.cxios_axis_handle_create(byref(ret), idt_c, len_idt_c)

@typecheck
def get_axisgroup_handle(idt: Union[str, String], ret: AxisGroup):
    """
    Gets the axisgroup handle associated with the given axisgroup id.

    Parameters:
        idt (Union[str, String]): The axisgroup identifier as a string or String object.
        ret (Axis): The AxisGroup object to be filled with the handle (passed by reference).

    Returns:
        None: The function modifies `ret` in place.
    """
    idt = String(idt)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    lib.cxios_axisgroup_handle_create(byref(ret), idt_c, len_idt_c)

@typecheck
def is_valid_axis(idt: Union[str, String]) -> bool:
    """
    Check that the axis is valid.

    Parameters:
        idt (Union[str, String]): The axis identifier as a string or String object.

    Returns:
        bool: True if the axis is valid, False otherwise.
    """
    idt = String(idt)
    is_val = Bool(False)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)
    lib.cxios_axis_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)

@typecheck
def is_valid_axisgroup(idt: Union[str, String]) -> bool:
    """
    Check that the axisgroup is valid.

    Parameters:
        idt (Union[str, String]): The axisgroup identifier as a string or String object.

    Returns:
        bool: True if the axisgroup is valid, False otherwise.
    """
    idt = String(idt)
    is_val = Bool(False)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)
    lib.cxios_axisgroup_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)
