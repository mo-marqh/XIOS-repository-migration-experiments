from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure, pointer
from typing import Union
from xios.config import lib, typecheck, String, Bool
from xios.oexpand_domain_attr import ExpandDomain


#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_expand_domain_handle_create.argtypes = [POINTER(ExpandDomain), c_char_p, c_int]
lib.cxios_expand_domain_handle_create.restype = None

lib.cxios_expand_domain_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_expand_domain_valid_id.restype = c_bool

#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def get_expand_domain_handle(idt : Union[String, str], ret : ExpandDomain):
    idt = String(idt)

    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))

    lib.cxios_expand_domain_handle_create(pointer(ret), idt_c, len_idt_c)

@typecheck
def is_valid_expand_domain(idt: Union[str, String]) -> bool:
    idt = String(idt)
    is_val = Bool(False)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)
    lib.cxios_expand_domain_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)