from xios.config import String, Int, Double, NpArrayInt, Bool
from xios.icalendar_wrapper import CalendarWrapper, get_default_calendar_wrapper_handle
from xios.icalendar_wrapper_attr import set_calendar_wrapper_attr_hdl, get_calendar_wrapper_attr_hdl, is_defined_calendar_wrapper_attr_hdl
from xios.icalendar import get_timestep, set_timestep, get_start_date, set_start_date, set_start_date_dur, get_current_date
from xios.icalendar import get_time_origin, set_time_origin, set_time_origin_dur, get_calendar_type, update_calendar
from xios.iduration import Duration
from xios.idate import Date
from xios.obj import swipswap_ctx, current_context_id

class Calendar:

    def __init__(self):
        self._context_id = current_context_id()
        self._calendar_wrapper_hdl = CalendarWrapper()
        get_default_calendar_wrapper_handle(self._calendar_wrapper_hdl)

    def set_attr(self, type = None, timestep = None, start_date = None, time_origin = None,
                 day_length = None, month_lengths = None, year_length = None,
                 leap_year_month = None, leap_year_drift = None,
                 leap_year_drift_offset = None, comment = None):

        if type is not None :                   self.type                   = (type)
        if timestep is not None :               self.timestep               = (timestep)
        if start_date is not None :             self.start_date             = (start_date) 
        if time_origin is not None :            self.time_origin            = (time_origin) 
        if day_length is not None :             self.day_length             = (day_length) 
        if month_lengths is not None :          self.month_lengths          = (month_lengths) 
        if year_length is not None :            self.year_length            = (year_length) 
        if leap_year_month is not None :        self.leap_year_month        = (leap_year_month) 
        if leap_year_drift is not None :        self.leap_year_drift        = (leap_year_drift) 
        if leap_year_drift_offset is not None : self.leap_year_drift_offset = (leap_year_drift_offset) 
        if comment is not None :                self.comment                = (comment) 

    @property
    def day_length(self):
        is_day_length = Bool()
        is_defined_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, day_length = is_day_length)
        if not is_day_length.value:
            raise RuntimeError(f"The attribute day length has not yet been set for the calendar of the context {self._context_id} !")
        current_context_id = swipswap_ctx(self._context_id)
        day_length = Int()
        get_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, day_length = day_length)
        swipswap_ctx(current_context_id)
        return day_length

    @day_length.setter
    def day_length(self, value):
        current_context_id = swipswap_ctx(self._context_id)
        value = Int(value)
        set_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, day_length = value)
        swipswap_ctx(current_context_id)

    @property
    def month_lengths(self):
        is_month_lengths = Bool()
        is_defined_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, month_lengths = is_month_lengths)
        if not is_month_lengths.value:
            raise RuntimeError(f"The attribute month lengths has not yet been set for the calendar of the context {self._context_id} !")
        current_context_id = swipswap_ctx(self._context_id)
        month_lengths = Int()
        get_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, month_lengths = month_lengths)
        swipswap_ctx(current_context_id)
        return month_lengths

    @month_lengths.setter
    def month_lengths(self, value):
        current_context_id = swipswap_ctx(self._context_id)
        value = Int(value)
        set_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, month_lengths = value)
        swipswap_ctx(current_context_id)

    @property
    def year_length(self):
        is_year_length = Bool()
        is_defined_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, year_length = is_year_length)
        if not is_year_length.value:
            raise RuntimeError(f"The attribute year length has not yet been set for the calendar of the context {self._context_id} !")
        current_context_id = swipswap_ctx(self._context_id)
        year_length = Int()
        get_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, year_length = year_length)
        swipswap_ctx(current_context_id)
        return year_length

    @year_length.setter
    def year_length(self, value):
        current_context_id = swipswap_ctx(self._context_id)
        value = Int(value)
        set_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, year_length = value)
        swipswap_ctx(current_context_id)

    @property
    def leap_year_drift(self):
        is_leap_year_drift = Bool()
        is_defined_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, leap_year_drift = is_leap_year_drift)
        if not is_leap_year_drift.value:
            raise RuntimeError(f"The attribute leap year drift has not yet been set for the calendar of the context {self._context_id} !")
        current_context_id = swipswap_ctx(self._context_id)
        leap_year_drift = Int()
        get_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, leap_year_drift = leap_year_drift)
        swipswap_ctx(current_context_id)
        return leap_year_drift

    @leap_year_drift.setter
    def leap_year_drift(self, value):
        current_context_id = swipswap_ctx(self._context_id)
        value = Int(value)
        set_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, leap_year_drift = value)
        swipswap_ctx(current_context_id)

    @property
    def leap_year_drift_offset(self):
        is_leap_year_drift_offset = Bool()
        is_defined_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, leap_year_drift_offset = is_leap_year_drift_offset)
        if not is_leap_year_drift_offset.value:
            raise RuntimeError(f"The attribute leap year drift offset has not yet been set for the calendar of the context {self._context_id} !")
        current_context_id = swipswap_ctx(self._context_id)
        leap_year_drift_offset = Int()
        get_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, leap_year_drift_offset = leap_year_drift_offset)
        swipswap_ctx(current_context_id)
        return leap_year_drift_offset

    @leap_year_drift_offset.setter
    def leap_year_drift_offset(self, value):
        current_context_id = swipswap_ctx(self._context_id)
        value = Int(value)
        set_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, leap_year_drift_offset = value)
        swipswap_ctx(current_context_id)

    @property
    def leap_year_month(self):
        is_leap_year_month = Bool()
        is_defined_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, leap_year_month = is_leap_year_month)
        if not is_leap_year_month.value:
            raise RuntimeError(f"The attribute leap year month has not yet been set for the calendar of the context {self._context_id} !")
        current_context_id = swipswap_ctx(self._context_id)
        leap_year_month = Int()
        get_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, leap_year_month = leap_year_month)
        swipswap_ctx(current_context_id)
        return leap_year_month

    @leap_year_month.setter
    def leap_year_month(self, value):
        current_context_id = swipswap_ctx(self._context_id)
        value = Int(value)
        set_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, leap_year_month = value)
        swipswap_ctx(current_context_id)

    @property
    def comment(self):
        is_comment = Bool()
        is_defined_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, comment = is_comment)
        if not is_comment.value:
            raise RuntimeError(f"The attribute comment has not yet been set for the calendar of the context {self._context_id} !")
        current_context_id = swipswap_ctx(self._context_id)
        comment = String()
        get_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, comment = comment)
        swipswap_ctx(current_context_id)
        return comment

    @comment.setter
    def comment(self, value):
        current_context_id = swipswap_ctx(self._context_id)
        value = String(value)
        set_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, comment = value)
        swipswap_ctx(current_context_id)

    @property
    def type(self):
        is_type = Bool()
        is_defined_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, type = is_type)
        if not is_type.value:
            raise RuntimeError(f"The attribute type has not yet been set for the calendar of the context {self._context_id} !")
        current_context_id = swipswap_ctx(self._context_id)
        type_ = String()
        get_calendar_type(type_)
        swipswap_ctx(current_context_id)
        return type_

    @type.setter
    def type(self, value):
        current_context_id = swipswap_ctx(self._context_id)
        value = String(value)
        set_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, type = value)
        swipswap_ctx(current_context_id)

    @property
    def timestep(self):
        is_timestep = Bool()
        is_defined_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, timestep = is_timestep)
        if not is_timestep.value:
            raise RuntimeError(f"The attribute timestep has not yet been set for the calendar of the context {self._context_id} !")
        current_context_id = swipswap_ctx(self._context_id)
        timestep = Duration()
        get_timestep(timestep)
        swipswap_ctx(current_context_id)
        return timestep

    @timestep.setter
    def timestep(self, value):
        current_context_id = swipswap_ctx(self._context_id)
        set_timestep(value)
        swipswap_ctx(current_context_id)

    @property
    def start_date(self):
        is_start_date = Bool()
        is_defined_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, start_date = is_start_date)
        if not is_start_date.value:
            raise RuntimeError(f"The attribute start date has not yet been set for the calendar of the context {self._context_id} !")
        current_context_id = swipswap_ctx(self._context_id)
        start_date = Date()
        get_start_date(start_date)
        swipswap_ctx(current_context_id)
        return start_date

    @start_date.setter
    def start_date(self, value):
        current_context_id = swipswap_ctx(self._context_id)
        if isinstance(value, Date):
            set_start_date(value)
        elif isinstance(value, Duration):
            set_start_date_dur(value)
        else:
            raise TypeError("start date should be an instance of xios.Duration or xios.Date")
        swipswap_ctx(current_context_id)

    @property
    def time_origin(self):
        is_time_origin = Bool()
        is_defined_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, time_origin = is_time_origin)
        if not is_time_origin.value:
            raise RuntimeError(f"The attribute time origin has not yet been set for the calendar of the context {self._context_id} !")
        current_context_id = swipswap_ctx(self._context_id)
        time_origin = Date()
        get_time_origin(time_origin)
        swipswap_ctx(current_context_id)
        return time_origin

    @time_origin.setter
    def time_origin(self, value):
        current_context_id = swipswap_ctx(self._context_id)
        if isinstance(value, Date):
            set_time_origin(value)
        elif isinstance(value, Duration):
            set_time_origin_dur(value)
        else:
            raise TypeError("time_origin should be an instance of xios.Duration or xios.Date")
        swipswap_ctx(current_context_id)

    def update(self, step):
        current_context_id = swipswap_ctx(self._context_id)
        update_calendar(step)
        swipswap_ctx(current_context_id)

    @property
    def current_date(self):
        is_current_date = Bool()
        is_defined_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, current_date = is_current_date)
        if not is_current_date.value:
            raise RuntimeError(f"The attribute current date has not yet been set for the calendar of the context {self._context_id} !")
        current_context_id = swipswap_ctx(self._context_id)
        current_date = Date()
        get_current_date(current_date)
        swipswap_ctx(current_context_id)
        return current_date

    def __str__(self):
        is_comment                = Bool() 
        is_day_length             = Bool() 
        is_leap_year_drift        = Bool() 
        is_leap_year_drift_offset = Bool() 
        is_leap_year_month        = Bool() 
        is_month_lengths          = Bool() 
        is_start_date             = Bool() 
        is_time_origin            = Bool() 
        is_timestep               = Bool() 
        is_type                   = Bool() 
        is_year_length            = Bool()

        is_defined_calendar_wrapper_attr_hdl(self._calendar_wrapper_hdl, is_comment, is_day_length,   
        is_leap_year_drift, is_leap_year_drift_offset, is_leap_year_month, is_month_lengths, is_start_date, 
        is_time_origin, is_timestep, is_type, is_year_length)

        s = "\n"
        s += f"\tCalendar of the context  '{self._context_id}'\n"

        if is_type.value:                   s += f"\ttype                   = {self.type}\n"
        if is_timestep.value:               s += f"\ttimestep               = {self.timestep}\n"
        if is_start_date.value:             s += f"\tstart_date             = {self.start_date}\n"
        if is_time_origin.value:            s += f"\ttime_origin            = {self.time_origin}\n"
        if is_day_length.value:             s += f"\tday_length             = {self.day_length}\n"
        if is_month_lengths.value:          s += f"\tmonth_lengths          = {self.month_lengths}\n"
        if is_year_length.value:            s += f"\tyear_length            = {self.year_length}\n"
        if is_leap_year_month.value:        s += f"\tleap_year_month        = {self.leap_year_month}\n"
        if is_leap_year_drift.value:        s += f"\tleap_year_drift        = {self.leap_year_drift}\n"
        if is_leap_year_drift_offset.value: s += f"\tleap_year_drift_offset = {self.leap_year_drift_offset}\n"
        if is_comment.value:                s += f"\tcomment                = {self.comment}\n"

        return s
