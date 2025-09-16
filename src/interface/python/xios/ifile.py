from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure, pointer
from xios.config import lib, String, typecheck
from xios.ofile_attr import File
from xios.ofilegroup_attr import FileGroup
from typing import Union



#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_file_handle_create.argtypes = [POINTER(File), c_char_p, c_int]
lib.cxios_file_handle_create.restype = None

lib.cxios_filegroup_handle_create.argtypes = [POINTER(File), c_char_p, c_int]
lib.cxios_filegroup_handle_create.restype = None

lib.cxios_file_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_file_valid_id.restype = None

lib.cxios_filegroup_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_filegroup_valid_id.restype = None

#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def get_file_handle(idt: Union[str, String], ret: File) -> None:
    idt = String(idt)

    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))

    lib.cxios_file_handle_create(byref(ret), idt_c, len_idt_c)

@typecheck
def get_filegroup_handle(idt: Union[str, String], ret: FileGroup) -> None:
    idt = String(idt)

    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))

    lib.cxios_filegroup_handle_create(byref(ret), idt_c, len_idt_c)

@typecheck
def is_valid_file(idt: Union[str, String]) -> bool:
    idt = String(idt)
    is_val = Bool(False)

    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)

    lib.cxios_file_valid_id(is_val_c, idt_c, len_idt_c)

    return bool(is_val_c.contents)

@typecheck
def is_valid_filegroup(idt: Union[str, String]) -> bool:
    idt = String(idt)
    is_val = Bool(False)

    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)

    lib.cxios_filegroup_valid_id(is_val_c, idt_c, len_idt_c)

    return bool(is_val_c.contents)
