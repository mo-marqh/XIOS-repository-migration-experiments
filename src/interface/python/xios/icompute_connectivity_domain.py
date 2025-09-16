from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure, pointer
from typing import Union
from xios.config import lib, typecheck, String, Bool
from xios.ocompute_connectivity_domain_attr import ComputeConnectivityDomain

#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_compute_connectivity_domain_handle_create.argtypes = [POINTER(ComputeConnectivityDomain), c_char_p, c_int]
lib.cxios_compute_connectivity_domain_handle_create.restype = None

lib.cxios_compute_connectivity_domain_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_compute_connectivity_domain_valid_id.restype = c_bool

#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def get_compute_connectivity_domain_handle(idt : Union[String, str], ret : ComputeConnectivityDomain):
    """
    Gets the compute_connectivity_domain handle associated with the given compute_connectivity_domain id.

    Parameters:
        idt (Union[str, String]): The compute_connectivity_domain identifier as a string or String object.
        ret (Axis): The ComputeConnectivityDomain object to be filled with the handle (passed by reference).

    Returns:
        None: The function modifies `ret` in place.
    """
    idt = String(idt)

    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))

    lib.cxios_compute_connectivity_domain_handle_create(pointer(ret), idt_c, len_idt_c)

@typecheck
def is_valid_compute_connectivity_domain(idt: Union[str, String]) -> bool:
    """
    Check that the compute_connnectivity_domain is valid.

    Parameters:
        idt (Union[str, String]): The compute_connectivity_domain identifier as a string or String object.

    Returns:
        bool: True if the compute_connectivity_domain is valid, False otherwise.
    """
    idt = String(idt)
    is_val = Bool(False)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)
    lib.cxios_compute_connectivity_domain_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)