from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, pointer
import os
from mpi4py import MPI
from xios.config import lib, String, MPIComm, typecheck, NpArray, MPIComm, Bool, Double, Int
from xios.obj import CObject as Field
from typing import Union, Optional
import numpy as np

#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#

lib.cxios_init_server.argtypes = []
lib.cxios_init_server.restype = None

lib.cxios_init_client.argtypes = [c_char_p, c_int, POINTER(c_int), POINTER(c_int)]
lib.cxios_init_client.restype = None

lib.cxios_context_initialize.argtypes = [c_char_p, c_int, POINTER(c_int)]
lib.cxios_context_initialize.restype = None

lib.cxios_context_is_initialized.argtypes = [c_char_p, c_int, POINTER(c_bool)]
lib.cxios_context_is_initialized.restype = None

lib.cxios_finalize.argtypes = []
lib.cxios_finalize.restype = None

lib.cxios_oasis_enddef.argtypes = []
lib.cxios_oasis_enddef.restype = None

lib.cxios_context_close_definition.argtypes = []
lib.cxios_context_close_definition.restype = None

lib.cxios_solve_inheritance.argtypes = []
lib.cxios_solve_inheritance.restype = None

lib.cxios_get_variable_data_k8.argtypes = [c_char_p, c_int, POINTER(c_double), POINTER(c_bool)]
lib.cxios_get_variable_data_k8.restype = c_bool

lib.cxios_get_variable_data_k4.argtypes = [c_char_p, c_int, POINTER(c_double), POINTER(c_bool)]
lib.cxios_get_variable_data_k4.restype = c_bool

lib.cxios_get_variable_data_int.argtypes = [c_char_p, c_int, POINTER(c_int), POINTER(c_bool)]
lib.cxios_get_variable_data_int.restype = c_bool

lib.cxios_get_variable_data_logic.argtypes = [c_char_p, c_int, POINTER(c_bool), POINTER(c_bool)]
lib.cxios_get_variable_data_logic.restype = c_bool

lib.cxios_set_variable_data_k8.argtypes = [c_char_p, c_int, POINTER(c_double), POINTER(c_bool)]
lib.cxios_set_variable_data_k8.restype = c_bool

lib.cxios_set_variable_data_k4.argtypes = [c_char_p, c_int, POINTER(c_double), POINTER(c_bool)]
lib.cxios_set_variable_data_k4.restype = c_bool

lib.cxios_set_variable_data_int.argtypes = [c_char_p, c_int, POINTER(c_int), POINTER(c_bool)]
lib.cxios_set_variable_data_int.restype = c_bool

lib.cxios_set_variable_data_logic.argtypes = [c_char_p, c_int, POINTER(c_bool), POINTER(c_bool)]
lib.cxios_set_variable_data_logic.restype = c_bool

lib.cxios_set_variable_data_char.argtypes = [c_char_p, c_int, c_char_p, c_int, POINTER(c_bool)]
lib.cxios_set_variable_data_char.restype = c_bool






write_funcs = {}
for act in ['write', 'read']:
    for inp in ['_hdl', '']:
        for dim in range(8):
            func_name = f'cxios_{act}_data_k8{dim}{inp}'
            try:
                func = getattr(lib, func_name)
                if inp == '_hdl':
                    func.argtypes = [POINTER(Field), POINTER(c_double)] + dim*[c_int]
                elif inp == '':
                    func.argtypes = [c_char_p, c_int, POINTER(c_double)] + dim*[c_int] 
                else:
                    raise ValueError(f"Unexpected inp: {inp}")
                func.restype = None
                write_funcs[dim, inp, act] = func
            except AttributeError:
                raise RuntimeError(f"Function {func_name} not found in library")

#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#
@typecheck
def init_server():
    """
    Initialize the server.
    """
    lib.cxios_init_server()

@typecheck
def initialize(client_id: Union[String, str], local_comm: MPIComm = MPIComm(), return_comm: MPIComm = MPIComm()):
    """
    Initialize the client with local_comm or return_comm.

    Parameters:
        client_id (Union[str, String]): The client identifiant
        local_comm (MPIComm): The local communicator
        return_comm (MPIComm): The global communicator 
    """

    client_id = String(client_id)

    p_return_comm = return_comm._c_value
    p_local_comm = local_comm._c_value
    client_id_c = client_id._c_value
    len_client_id_c = len(string_at(client_id_c))

    lib.cxios_init_client(client_id_c, len_client_id_c, p_local_comm, p_return_comm)

    local_comm._c_value = p_local_comm
    return_comm._c_value = p_return_comm





@typecheck
def context_initialize(context_id : Union[String, str], comm : Union[MPIComm, MPI.Comm]):
    """
    Initialize the context with the context id and the communicator.

    Parameters:
        context_id (Union[str, String]): The context id
        comm (MPIComm): The communicator for this context
    """
    comm = MPIComm(comm)
    context_id = String(context_id)

    MPICommc = comm._c_value
    context_id_c = context_id._c_value
    len_context_id_c = len(string_at(context_id_c))

    lib.cxios_context_initialize(context_id_c, len_context_id_c, MPICommc)

@typecheck
def context_is_initialized(context_id : Union[String, str]):
    """
    Check if the context given with context_id is initialized.

    Parameters:
        context_id (Union[str, String]): The context identifiant
    """

    context_id = String(context_id)
    is_init = Bool(False)

    context_id_c = context_id._c_value
    len_context_id_c = len(string_at(context_id_c))
    is_init_c = pointer(is_init._c_value)

    lib.cxios_context_is_initialized(context_id_c, len_context_id_c, is_init_c)

    return bool(is_init_c.contents)

@typecheck
def finalize():
    """
    Finalize xios.
    """
    lib.cxios_finalize()

@typecheck
def oasis_enddef():
    lib.cxios_oasis_enddef()

@typecheck
def close_context_definition():
    """
    Close the context definition of the current context.
    """
    lib.cxios_context_close_definition()

@typecheck
def context_finalize():
    """
    Finalize the current context.
    """
    lib.cxios_context_finalize()

@typecheck
def solve_inheritance():
    lib.cxios_solve_inheritance()

#send field
@typecheck
def send_field(field : Union[str, Field, String], data : Union[np.ndarray, NpArray]):
    """
    Send the field with the given handle or id and the given data.

    Parameters:
        field (Union[str, String, Field]): The field as a Field, a String or a str
        data (Union[np.ndarray, NPArray]): The data as a np.ndarray or a NPArray
    """
    if isinstance(field, Field):
        send_field_hdl(field, data)
    elif isinstance(field, str) or isinstance(field, String):
        send_field_id(field, data)
    else:
        raise TypeError("field should be either a str, either a xios.String or a xios.Field")

@typecheck
def send_field_id(field_id : Union[str, String], data : Union[NpArray, np.ndarray]):
    """
    Send the field with the given id and the given data.

    Parameters:
        field (Union[str, String]): The field as a String or a str
        data (Union[np.ndarray, NPArray]): The data as a np.ndarray or a NPArray
    """

    data = NpArray(data)
    field_id = String(field_id)

    field_id_c = field_id._c_value
    len_field_id_c = len(string_at(field_id_c))

    dim = data.ndim
    inp = ''
    act = 'write'
    if (dim, inp, act) not in write_funcs:
        raise ValueError(f"No C function available for dimension {dim}")

    func = write_funcs[dim, inp, act]
    data_ptr = data._c_value
    shape = [c_int(e) for e in data.shape]

    func(field_id_c, len_field_id_c, data_ptr, *shape)

@typecheck
def send_field_hdl(field_hdl : Field, data : Union[NpArray, np.ndarray]):
    """
    Send the field with the given handle and the given data.

    Parameters:
        field (Union[Field]): The field as a Field
        data (Union[np.ndarray, NPArray]): The data as a np.ndarray or a NPArray
    """

    data = NpArray(data)
    dim = data.ndim
    inp = '_hdl'
    act = 'write'
    if (dim, inp, act) not in write_funcs:
        raise ValueError(f"No C function available for dimension {dim}")

    func = write_funcs[dim, inp, act]
    data_ptr = data._c_value
    shape = [c_int(e) for e in data.shape]

    func(byref(field_hdl), data_ptr, *shape)
    
#receive field
@typecheck
def receive_field(field : Union[str, Field, String], data : Union[np.ndarray, NpArray]):
    """
    Receive the field with the given handle or id.

    Parameters:
        field (Union[str, String, Field]): The field as a Field, a String or a str
        data (Union[np.ndarray, NPArray]): The data to be filled(passed by reference).

    Returns:
        None: The function modify 'data' in place
    """
    if isinstance(field, Field):
        receive_field_hdl(field, data)
    elif isinstance(field, str) or isinstance(field, String):
        receive_field_id(field, data)
    else:
        raise TypeError("field should be either a str, either a xios.String or a xios.Field")

@typecheck
def receive_field_id(field_id : Union[str, String], data : Union[NpArray, np.ndarray]):
    """
    Receive the field with the given id.

    Parameters:
        field (Union[str, String, Field]): The field as a String or a str
        data (Union[np.ndarray, NPArray]): The data to be filled(passed by reference).

    Returns:
        None: The function modify 'data' in place
    """

    data = NpArray(data)
    field_id = String(field_id)

    field_id_c = field_id._c_value
    len_field_id_c = len(string_at(field_id_c))

    dim = data.ndim
    inp = ''
    act = 'read'
    if (dim, inp, act) not in write_funcs:
        raise ValueError(f"No C function available for dimension {dim}")

    func = write_funcs[dim, inp, act]
    data_ptr = data._c_value
    shape = [c_int(e) for e in data.shape]

    func(field_id_c, len_field_id_c, data_ptr, *shape)

@typecheck
def send_field_hdl(field_hdl : Field, data : Union[NpArray, np.ndarray]):
    """
    Receive the field with the given handle.

    Parameters:
        field (Union[str, String, Field]): The field as a Field.
        data (Union[np.ndarray, NPArray]): The data to be filled(passed by reference).

    Returns:
        None: The function modify 'data' in place.
    """

    data = NpArray(data)
    dim = data.ndim
    inp = '_hdl'
    act = 'read'
    if (dim, inp, act) not in write_funcs:
        raise ValueError(f"No C function available for dimension {dim}")

    func = write_funcs[dim, inp, act]
    data_ptr = data._c_value
    shape = [c_int(e) for e in data.shape]

    func(byref(field_hdl), data_ptr, *shape)
    
@typecheck
def getVar(varId : Union[str, String], data: Union[Double, Int, Bool]):
    if isinstance(data, Double):
        return getVar_k8(varId, data)
    elif isinstance(data, Double):
        return getVar_k4(varId, data)
    elif isinstance(data, Int):
        return getVar_int(varId, data)
    elif isinstance(data, Bool):
        return getVar_logic(varId, data)

@typecheck
def getVar_k8(varId : Union[str, String], data : Double):
    varId = String(varId)
    varId_c = varId._c_value
    len_varId_c = len(string_at(varId_c))
    val = c_bool(False)
    lib.cxios_get_variable_data_k8(varId_c, len_varId_c, data._c_value, pointer(val))
    return bool(val)
    
@typecheck
def getVar_k4(varId : Union[str, String], data : Double):
    varId = String(varId)
    varId_c = varId._c_value
    len_varId_c = len(string_at(varId_c))
    val = c_bool(False)
    lib.cxios_get_variable_data_k4(varId_c, len_varId_c, data._c_value, pointer(val))
    return bool(val)
    
@typecheck
def getVar_int(varId : Union[str, String], data : Int):
    varId = String(varId)
    varId_c = varId._c_value
    len_varId_c = len(string_at(varId_c))
    val = c_bool(False)
    data_c = pointer(data._c_value)
    print(data.value)
    lib.cxios_get_variable_data_int(varId_c, len_varId_c, data_c, pointer(val))
    print(data.value)
    print(data_c.contents)
    print(val)
    return bool(val)

@typecheck
def getVar_logic(varId : Union[str, String], data : Bool):
    varId = String(varId)
    varId_c = varId._c_value
    len_varId_c = len(string_at(varId_c))
    val = c_bool(False)
    lib.cxios_get_variable_data_logic(varId_c, len_varId_c, data._c_value, pointer(val))
    return val

@typecheck
def setVar(varId : Union[str, String], data: Union[Double, float, Int, int, Bool, bool, String, str]):
    if isinstance(data, Double) or isinstance(data, float):
        return setVar_k8(varId, data)
    elif isinstance(data, Double) or isinstance(data, float):
        return setVar_k4(varId, data)
    elif isinstance(data, Int) or isinstance(data, int):
        return setVar_int(varId, data)
    elif isinstance(data, Bool) or isinstance(data, bool):
        return setVar_logic(varId, data)
    elif isinstance(data, String) or isinstance(data, str):
        return setVar_char(varId, data)

@typecheck
def setVar_k8(varId : Union[str, String], data : Union[Double, float]):
    data = Double(data)
    varId = String(varId)
    varId_c = varId._c_value
    len_varId_c = len(string_at(varId_c))
    val = c_bool(False)
    lib.cxios_set_variable_data_k8(varId_c, len_varId_c, data._c_value, pointer(val))
    return bool(val)
    
@typecheck
def setVar_k4(varId : Union[str, String], data : Union[Double, float]):
    data = Double(data)
    varId = String(varId)
    varId_c = varId._c_value
    len_varId_c = len(string_at(varId_c))
    val = c_bool(False)
    lib.cxios_set_variable_data_k4(varId_c, len_varId_c, data._c_value, pointer(val))
    return bool(val)
    
@typecheck
def setVar_int(varId : Union[str, String], data : Union[Int, int]):
    data = Int(data)
    varId = String(varId)
    varId_c = varId._c_value
    len_varId_c = len(string_at(varId_c))
    val = c_bool(False)
    lib.cxios_set_variable_data_int(varId_c, len_varId_c, data._c_value, pointer(val))
    return bool(val)
    
@typecheck
def setVar_logic(varId : Union[str, String], data : Union[Bool, bool]):
    data = Bool(data)
    varId = String(varId)
    varId_c = varId._c_value
    len_varId_c = len(string_at(varId_c))
    val = c_bool(False)
    lib.cxios_set_variable_data_logic(varId_c, len_varId_c, data._c_value, pointer(val))
    return bool(val)
    
@typecheck
def setVar_char(varId : Union[str, String], data : Union[String, str]):
    data = String(data)
    varId = String(varId)
    varId_c = varId._c_value
    len_varId_c = len(string_at(varId_c))
    val = c_bool(False)
    lib.cxios_set_variable_data_char(varId_c, len_varId_c, data._c_value,  len(string_at(data._c_value)), pointer(val))
    return bool(val)
    