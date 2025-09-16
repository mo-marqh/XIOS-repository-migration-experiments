from xios.ovariable_attr import Variable_
from xios.idata import getVar
from xios.obj import swipswap_ctx

class Variable(Variable_):

    def __init__(self, arg1 = None, arg2 : str = None):
        super().__init__(arg1, arg2)

    @property
    def get(self, var_id, data):
        current_context_id = swipswap_ctx(self._context_id)
        if getVar(var_id, data):
            return data
        swipswap_ctx(current_context_id)

    def receive(self, data):
        current_context_id = swipswap_ctx(self._context_id)
        data = NpArray(data)
        receive_field(self._id, data)
        swipswap_ctx(current_context_id)
        return data.arr