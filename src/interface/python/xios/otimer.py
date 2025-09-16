from xios.itimer import timer_resume, timer_suspend, timer_reset, timer_get_time


class Timer():

    def __init__(id):
        self._id = id
    
    def resume(self, trace = True):
        timer_resume(self._id, trace)

    def suspend(self, trace = True):
        timer_suspend(self._id, trace)
    
    def reset(self):
        timer_reset(self._id)

    @property
    def time(self):
        return timer_get_time(self._id)
    
    @property
    def id(self):
        return self._id
 
