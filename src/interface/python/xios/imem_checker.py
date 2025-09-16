from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure, pointer
import os
from xios import config as xios
from xios.config import lib, typecheck, String, Bool
from typing import Union


#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_mem_checker_get.argtypes = []
lib.cxios_mem_checker_get.restype = c_bool

lib.cxios_mem_checker_log.argtypes = [c_char_p, c_int, c_bool]
lib.cxios_mem_checker_log.restype = None


#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def mem_checker_get():
    mem_ = Bool()
    mem = mem_._c_value
    lib.cxios_mem_checker_get(pointer(mem))
    return bool(mem)

@typecheck
def mem_checker_log(mem_id : Union[String, str], finalize : Union[bool, Bool] = Bool(False)):
    mem_id = xios.String(mem_id)
    finalize = Bool(finalize)
    mem_id_c = mem_id._c_value
    len_mem_id_c = len(string_at(mem_id_c))
    finalize_c = finalize._c_value

    cxios_mem_checker_log(mem_id_c, len_mem_id_c, finalize)