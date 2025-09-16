from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure, pointer
from typing import Union
from xios.config import lib, String, typecheck, Bool
from xios.config import CObject as Context

#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_context_handle_create.argtypes = [POINTER(Context), c_char_p, c_int]
lib.cxios_context_handle_create.restype = None

lib.cxios_context_get_current.argtypes = [POINTER(Context)]
lib.cxios_context_get_current.restype = None

lib.cxios_context_get_id.argtypes = [Context, c_char_p, c_int]
lib.cxios_context_get_id.restype = None

lib.cxios_context_set_current.argtypes = [Context, c_bool]
lib.cxios_context_set_current.restype = None

lib.cxios_context_valid_id.argtypes = [POINTER(c_bool), c_char_p, c_int]
lib.cxios_context_valid_id.restype = None

#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def set_current_context(context: Union[str, String, Context]):
    """
    Set the current context with the given context.

    Parameters:
        context (Union[str, String, Context]): The context identifier as a string, a String or a Context object.

    Returns:
        None
    """
    if isinstance(context, String):
        set_current_context_id(context)
    elif isinstance(context, str):
        context = String(context)
        set_current_context_id(context)
    elif isinstance(context, Context):
        set_current_context_hdl(context)
    else:
        raise EnvironmentError("context should be either set by id(String) or handle (Context)!")

@typecheck
def get_current_context(context: Union[String, Context]):
    """
    Get the current context.

    Parameters:
        context (Union[str, String, Context]): The context identifier as a string, a String or a Context object.

    Returns:
        None: The function modifies `context` in place.
    """
    if isinstance(context, String):
        get_current_context_id(context)
    elif isinstance(context, Context):
        get_current_context_handle(context)
    else:
        raise EnvironmentError("context should be either set by id(String) or handle (Context)!")

@typecheck
def get_context_handle(idt: Union[str, String], ret: Context):
    """
    Get the context handle associated with the given context id.

    Parameters:
        idt (Union[str, String]): The context identifier as a string or a String object.
        ret (Context): The Context object to be filled with the handle (passed by reference).


    Returns:
        None: The function modifies `ret` in place.
    """
    idt = String(idt)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    lib.cxios_context_handle_create(pointer(ret), idt_c, len_idt_c)

@typecheck
def get_current_context_handle(context: Context):
    """
    Get the current context handle.

    Parameters:
        context (Context): The Context object to be filled with the handle (passed by reference).


    Returns:
        None: The function modifies `context` in place.
    """
    lib.cxios_context_get_current(pointer(context))

@typecheck
def get_current_context_id(idt: String):
    """
    Get the current context id.

    Parameters:
        idt (Union[str, String]): The Context id to be filled with the id (passed by reference).


    Returns:
        None: The function modifies `idt` in place.
    """
    idt = String(idt)
    context = Context()
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    lib.cxios_context_get_current(pointer(context))
    lib.cxios_context_get_id(context, idt_c, len_idt_c)

@typecheck
def set_current_context_hdl(context: Context, withswap: bool = False):
    """
    Set the current context with the given context handle.

    Parameters:
        context (Union[Context]): The context identifier as a Context object.
        withswap Optional[bool]: True to swap, False otherwise

    Returns:
        None
    """
    withswap = Bool(withswap)
    withswap_c = withswap._c_value
    lib.cxios_context_set_current(context, withswap_c)

@typecheck
def set_current_context_id(idt: Union[str, String]):
    """
    Set the current context with the given context id.

    Parameters:
        idt (Union[str, String]): The context identifier as a string or a String object.

    Returns:
        None
    """
    context = Context()
    get_context_handle(idt, context)
    set_current_context_hdl(context)

@typecheck
def is_valid_context(idt: Union[str, String]) -> bool:
    """
    Check that the given  context is a valid context.

    Parameters:
        idt (Union[str, String]): The context identifier as a string or a String  object.

    Returns:
        Bool: True if the context is valid, False otherwise
    """
    idt = String(idt)
    is_val = Bool(False)
    idt_c = idt._c_value
    len_idt_c = len(string_at(idt_c))
    is_val_c = pointer(is_val._c_value)
    lib.cxios_context_valid_id(is_val_c, idt_c, len_idt_c)
    return bool(is_val_c.contents)
