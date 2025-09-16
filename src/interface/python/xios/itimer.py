from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure, pointer
from typing import Union
from xios.config import lib, typecheck, String, Bool


#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_timer_resume.argtypes = [c_char_p, c_int, c_bool]
lib.cxios_timer_resume.restype = None

lib.cxios_timer_suspend.argtypes = [c_char_p, c_int, c_bool]
lib.cxios_timer_suspend.restype = None

lib.cxios_timer_reset.argtypes = [c_char_p, c_int]
lib.cxios_timer_reset.restype = None

lib.cxios_timer_get_cumulated_time.argtypes = [c_char_p, c_int, POINTER(c_double)]
lib.cxios_timer_get_cumulated_time.restype = None

lib.cxios_timer_get_time.argtypes = [POINTER(c_double)]
lib.cxios_timer_get_time.restype = None


#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def timer_resume(timer_id : Union[String, str], trace : Union[Bool, bool] = Bool(True)):
    timer_id = String(timer_id)
    trace = Bool(trace)

    timer_id_c = timer_id._c_value
    len_timer_id_c = len(string_at(timer_id_c))
    trace_c = trace._c_value

    lib.cxios_timer_resume(timer_id_c, len_timer_id_c, trace)
    
@typecheck
def timer_suspend(timer_id : Union[String, str], trace : Union[Bool, bool] = Bool(True)):
    timer_id = String(timer_id)
    trace = Bool(trace)

    timer_id_c = timer_id._c_value
    len_timer_id_c = len(string_at(timer_id_c))
    trace_c = trace._c_value

    lib.cxios_timer_suspend(timer_id_c, len_timer_id_c, trace)
    
@typecheck
def timer_reset(timer_id : Union[String, str]):
    timer_id = String(timer_id)

    timer_id_c = timer_id._c_value
    len_timer_id_c = len(string_at(timer_id_c))

    lib.cxios_timer_reset(timer_id_c, len_timer_id_c)

@typecheck
def timer_get_time(timer_id : Union[String, str] = ''):
    time = c_double(.0)

    if timer_id != String(''):
        timer_id = String(timer_id)

        timer_id_c = timer_id._c_value
        len_timer_id_c = len(string_at(timer_id_c))
        cxios_timer_get_cumulated_time(timer_id, len_timer_id_c, pointer(time))
    else:
        cxios_timer_get_time(pointer(time))
        
    return time