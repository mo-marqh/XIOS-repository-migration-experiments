from xios.ofield_attr import Field_
from xios.idata import send_field, receive_field
from xios.config import NpArray
from xios.obj import swipswap_ctx

class Field(Field_):

    def __init__(self, arg1 = None, arg2 : str = None):
        super().__init__(arg1, arg2)

    def send(self, data):
        current_context_id = swipswap_ctx(self._context_id)
        send_field(self._id, data)
        swipswap_ctx(current_context_id)

    def receive(self, data):
        current_context_id = swipswap_ctx(self._context_id)
        data = NpArray(data)
        receive_field(self._id, data)
        swipswap_ctx(current_context_id)
        return data.arr