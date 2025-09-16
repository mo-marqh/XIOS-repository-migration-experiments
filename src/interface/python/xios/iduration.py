from ctypes import c_char_p, c_int, c_bool, c_double, string_at, Structure
from xios.config import lib, typecheck
from xios import config as xios
from typing import Union
from typing import get_type_hints, get_args

#_____________________________________________________________________________________________#
#                                         XIOS_TYPES                                          #
#_____________________________________________________________________________________________#

class Duration(Structure):
    _fields_ = [
        ("year", c_double),
        ("month", c_double),
        ("day", c_double),
        ("hour", c_double),
        ("minute", c_double),
        ("second", c_double),
        ("timestep", c_double),
    ]

    def __init__(
        self,
        duration: Union[None, 'Duration'] = None,
        year: float = 0.0,
        month: float = 0.0,
        day: float = 0.0,
        hour: float = 0.0,
        minute: float = 0.0,
        second: float = 0.0,
        timestep: float = 0.0,
    ):
        super().__init__()

        if duration is None:
            self.year     = xios.Double(year)._c_value
            self.month    = xios.Double(month)._c_value
            self.day      = xios.Double(day)._c_value
            self.hour     = xios.Double(hour)._c_value
            self.minute   = xios.Double(minute)._c_value
            self.second   = xios.Double(second)._c_value
            self.timestep = xios.Double(timestep)._c_value

        elif isinstance(duration, Duration):
            self.year     = duration.year
            self.month    = duration.month
            self.day      = duration.day
            self.hour     = duration.hour
            self.minute   = duration.minute
            self.second   = duration.second
            self.timestep = duration.timestep

        else:
            raise TypeError("duration arg should be an instance of xios.Duration or set to None!")
    


    def __repr__(self):
        parts = []
        if int(self.year):   parts.append(f"{int(self.year)}y")
        if int(self.month):  parts.append(f"{int(self.month)}mo")
        if int(self.day):    parts.append(f"{int(self.day)}d")
        if int(self.hour):   parts.append(f"{int(self.hour)}h")
        if int(self.minute): parts.append(f"{int(self.minute)}m")
        if int(self.second): parts.append(f"{int(self.second)}s")
        return " ".join(parts) if parts else "0 s"

    def __add__(self, duration): return duration_add(self, duration)
    def __neg__(self): return duration_neg(self)
    def __sub__(self, duration): return duration_sub(self, duration)
    def __mul__(self, scalar): return duration_mult(scalar, self)
    __rmul__ = __mul__
    def __eq__(self, duration): return duration_eq(self, duration)
    def __ne__(self, duration): return duration_neq(self, duration)


#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_duration_add.restype = Duration
lib.cxios_duration_sub.restype = Duration
lib.cxios_duration_mult.restype = Duration
lib.cxios_duration_neg.restype = Duration
lib.cxios_duration_convert_from_string.restype = Duration

lib.cxios_duration_add.argtypes = [Duration, Duration]
lib.cxios_duration_sub.argtypes = [Duration, Duration]
lib.cxios_duration_mult.argtypes = [c_double, Duration]
lib.cxios_duration_neg.argtypes = [Duration]
lib.cxios_duration_convert_from_string.argtypes = [c_char_p, c_int]
lib.cxios_duration_convert_to_string.argtypes = [Duration, c_char_p, c_int]


#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def duration_convert_to_string(dur: Duration, out_str: xios.String):
    dur = Duration(dur)
    out_str.value = ' ' * 256
    out_str_c = out_str._c_value
    len_out_str_c = len(string_at(out_str_c))
    lib.cxios_duration_convert_to_string(dur, out_str_c, len_out_str_c)

@typecheck
def duration_convert_from_string(in_str: Union[str, xios.String]) -> Duration:
    in_str = xios.String(in_str)
    in_str_c = in_str._c_value
    len_in_str_c = len(string_at(in_str_c))
    return Duration(lib.cxios_duration_convert_from_string(in_str_c, len_in_str_c))

@typecheck
def duration_add(dur1: Duration, dur2: Duration) -> Duration:
    dur1 = Duration(dur1)
    dur2 = Duration(dur2)
    return Duration(lib.cxios_duration_add(dur1, dur2))

@typecheck
def duration_sub(dur1: Duration, dur2: Duration) -> Duration:
    dur1 = Duration(dur1)
    dur2 = Duration(dur2)
    return Duration(lib.cxios_duration_sub(dur1, dur2))

@typecheck
def duration_mult(val: Union[int, float], dur: Duration) -> Duration:
    dur = Duration(dur)
    return Duration(lib.cxios_duration_mult(float(val), dur))

@typecheck
def duration_neg(dur: Duration) -> Duration:
    dur = Duration(dur)
    return Duration(lib.cxios_duration_neg(dur))

@typecheck
def duration_eq(dur1: Duration, dur2: Duration) -> bool:
    dur1 = Duration(dur1)
    dur2 = Duration(dur2)
    res_tmp = lib.cxios_duration_eq(dur1, dur2)
    return bool(res_tmp)

@typecheck
def duration_neq(dur1: Duration, dur2: Duration) -> bool:
    dur1 = Duration(dur1)
    dur2 = Duration(dur2)
    res_tmp = lib.cxios_duration_neq(dur1, dur2)
    return bool(res_tmp)
