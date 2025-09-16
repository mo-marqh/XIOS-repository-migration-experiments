from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure
from xios.config import lib, String, typecheck, Int
from xios.iduration import Duration

#_____________________________________________________________________________________________#
#                                         XIOS_TYPES                                          #
#_____________________________________________________________________________________________#

class Date(Structure):
    """
    Represents a date.

    Attributes:
        year   (int): Year component of the date.
        month  (int): Month component of the date.
        day    (int): Day componenet of the date.
        hour   (int): Hour component of the date.
        minute (int): Minute component of the date.
        second (int): Second componenet of the date.

    Methods:
        to_string(): Returns the date in a string format.
    """
    _fields_ = [
        ("year", c_int),
        ("month", c_int),
        ("day", c_int),
        ("hour", c_int),
        ("minute", c_int),
        ("second", c_int)
    ]

    def __init__(self, date=None, year=0, month=0, day=0, hour=0, minute=0, second=0, timestep=0):
        super().__init__()

        if date is None:
            self.year     = Int(year)._c_value
            self.month    = Int(month)._c_value
            self.day      = Int(day)._c_value
            self.hour     = Int(hour)._c_value
            self.minute   = Int(minute)._c_value
            self.second   = Int(second)._c_value

        elif isinstance(date, Date):
            self.year     = date.year
            self.month    = date.month
            self.day      = date.day
            self.hour     = date.hour
            self.minute   = date.minute
            self.second   = date.second
        
        else:
            raise TypeError("date arg should be an instance of xios.Date or set to None!")

    def to_string(self):
        date_string = String()
        date_convert_to_string(self, date_string)
        return date_string.value
        
    def toString(self):
        date_string = String()
        date_convert_to_string(self, date_string)
        return date_string

    def __str__(self):
        return self.to_string()
        

    
    def __add__(self, date):
        return date_add_duration(self, date)

    def __sub__(self, d):
        if isinstance(d, Date):
            return date_sub_date(self, d)
        elif isinstance(d, Duration):
            return date_sub_duration(self, d)
        else:
            raise TypeError("You can subtract from a Date only Date or Duration!")
        
    def __eq__(self, date):
        return date_eq(self, date)
        
    def __ne__(self, date):
        return date_neq(self, date)
    
    def __lt__(self, date):
        return date_lt(self, date)
        
    def __le__(self, date):
        return date_le(self, date)
        
    def __gt__(self, date):
        return date_gt(self, date)
        
    def __ge__(self, date):
        return date_ge(self, date)

#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_date_add_duration.restype = Date
lib.cxios_date_sub_duration.restype = Date
lib.cxios_date_sub.restype = Duration
lib.cxios_date_neq.restype = c_bool
lib.cxios_date_lt.restype = c_bool
lib.cxios_date_le.restype = c_bool
lib.cxios_date_gt.restype = c_bool
lib.cxios_date_ge.restype = c_bool
lib.cxios_date_convert_from_string.restype = Date
lib.cxios_date_convert_to_seconds.restype = Date

lib.cxios_date_add_duration.argtypes = [Date, Duration]
lib.cxios_date_sub_duration.argtypes = [Date, Duration]
lib.cxios_date_sub.argtypes = [Date, Date]
lib.cxios_date_neq.argtypes = [Date, Date]
lib.cxios_date_lt.argtypes = [Date, Date]
lib.cxios_date_gt.argtypes = [Date, Date]
lib.cxios_date_le.argtypes = [Date, Date]
lib.cxios_date_ge.argtypes = [Date, Date]
lib.cxios_date_convert_from_string.argtypes = [c_char_p, c_int]
lib.cxios_date_convert_to_string.argtypes = [Date, c_char_p, c_int]
lib.cxios_date_convert_to_seconds.argtypes = [Date]

#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def date_convert_to_second(date: Date) -> Date:
    return lib.cxios_date_convert_to_seconds(Date(date))

@typecheck
def date_convert_to_string(date: Date, out_str: String) -> None:
    if len(out_str.value) < 20: out_str.value = ' ' * 256
    out_str_c = out_str._c_value
    len_out_str_c = len(string_at(out_str_c))
    lib.cxios_date_convert_to_string(Date(date), out_str_c, len_out_str_c)

@typecheck
def date_convert_from_string(in_str: str) -> Date:
    in_str = String(in_str)
    in_str_c = in_str._c_value
    len_in_str_c = len(string_at(in_str_c))
    return Date(lib.cxios_date_convert_from_string(in_str_c, len_in_str_c))

@typecheck
def date_add_duration(date: Date, dur: Duration) -> Date:
    return Date(lib.cxios_date_add_duration(Date(date), Duration(dur)))

@typecheck
def date_sub_duration(date: Date, dur: Duration) -> Date:
    return Date(lib.cxios_date_sub_duration(Date(date), Duration(dur)))

@typecheck
def date_sub_date(date1: Date, date2: Date) -> Duration:
    return Duration(lib.cxios_date_sub(Date(date1), Date(date2)))

@typecheck
def date_eq(date1: Date, date2: Date) -> bool:
    return bool(lib.cxios_date_eq(Date(date1), Date(date2)))

@typecheck
def date_neq(date1: Date, date2: Date) -> bool:
    return bool(lib.cxios_date_neq(Date(date1), Date(date2)))

@typecheck
def date_lt(date1: Date, date2: Date) -> bool:
    return bool(lib.cxios_date_lt(Date(date1), Date(date2)))

@typecheck
def date_le(date1: Date, date2: Date) -> bool:
    return bool(lib.cxios_date_le(Date(date1), Date(date2)))

@typecheck
def date_gt(date1: Date, date2: Date) -> bool:
    return bool(lib.cxios_date_gt(Date(date1), Date(date2)))

@typecheck
def date_ge(date1: Date, date2: Date) -> bool:
    return bool(lib.cxios_date_ge(Date(date1), Date(date2)))

@typecheck
def date_assign_duration(date: Date, dur: Duration) -> None:
    dur = Duration(dur)
    date.year   = dur.year
    date.month  = dur.month + 1
    date.day    = dur.day + 1
    date.hour   = dur.hour
    date.minute = dur.minute

@typecheck
def date_get_second_of_year(date: Date) -> int:
    return int(lib.cxios_date_get_second_of_year(date))

@typecheck
def date_get_day_of_year(date: Date) -> int:
    return int(lib.cxios_date_get_day_of_year(date))

@typecheck
def date_get_fraction_of_year(date: Date) -> float:
    return float(lib.cxios_date_get_fraction_of_year(date))

@typecheck
def date_get_second_of_day(date: Date) -> int:
    return int(lib.cxios_date_get_second_of_day(date))

@typecheck
def date_get_fraction_of_day(date: Date) -> float:
    return float(lib.cxios_date_get_fraction_of_day(date))
