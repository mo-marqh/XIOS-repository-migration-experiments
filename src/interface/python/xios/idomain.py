from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure, pointer
import os
from xios import config as xios
from xios.config import lib, typecheck, String
from typing import Union
from xios.odomain_attr import Domain
from xios.odomaingroup_attr import DomainGroup


#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_domain_handle_create.argtypes = [POINTER(Domain), c_char_p, c_int]
lib.cxios_domain_handle_create.restype = None

lib.cxios_domaingroup_handle_create.argtypes = [POINTER(DomainGroup), c_char_p, c_int]
lib.cxios_domaingroup_handle_create.restype = None

lib.cxios_domain_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_domain_valid_id.restype = None

lib.cxios_domaingroup_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_domaingroup_valid_id.restype = None

#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def get_domain_handle(idt: Union[str, String], ret: Domain):
    idt = xios.String(idt)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    lib.cxios_domain_handle_create(byref(ret), idt_c, len_idt_c)

@typecheck
def get_domaingroup_handle(idt: Union[str, String], ret: DomainGroup):
    idt = xios.String(idt)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    lib.cxios_domaingroup_handle_create(byref(ret), idt_c, len_idt_c)

@typecheck
def is_valid_domain(idt: Union[str, String]) -> bool:
    idt = xios.String(idt)
    is_val = xios.Bool(False)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)
    lib.cxios_domain_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)

@typecheck
def is_valid_domaingroup(idt: Union[str, String]) -> bool:
    idt = xios.String(idt)
    is_val = xios.Bool(False)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)
    lib.cxios_domaingroup_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)
