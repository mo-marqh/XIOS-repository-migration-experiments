from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure, pointer
from typing import Union
from xios.config import lib, typecheck, String, Bool
from xios.oreduce_domain_to_scalar_attr import ReduceDomainToScalar


#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_reduce_domain_to_scalar_handle_create.argtypes = [POINTER(ReduceDomainToScalar), c_char_p, c_int]
lib.cxios_reduce_domain_to_scalar_handle_create.restype = None

lib.cxios_reduce_domain_to_scalar_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_reduce_domain_to_scalar_valid_id.restype = c_bool

#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def get_reduce_domain_to_scalar_handle(idt : Union[String, str], ret : ReduceDomainToScalar):
    idt = String(idt)

    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))

    lib.cxios_reduce_domain_to_scalar_handle_create(pointer(ret), idt_c, len_idt_c)

@typecheck
def is_valid_reduce_domain_to_scalar(idt: Union[str, String]) -> bool:
    idt = String(idt)
    is_val = Bool(False)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)
    lib.cxios_reduce_domain_to_scalar_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)