from ctypes import CDLL, c_int, c_float, c_char, c_void_p, c_char_p, c_bool, pointer
from ctypes import create_string_buffer, POINTER, cast, string_at, Structure, c_double
import functools
import inspect
from typing import get_type_hints, get_args, Union
import os
import importlib
from mpi4py import MPI
import numpy as np

################################  LOADING xios.so ###################################

xios_dir = os.environ.get('XIOS_DIR')
if not xios_dir:
    raise EnvironmentError("XIOS_DIR is not defined. Please set it before running this script.")

lib_path = os.path.join(xios_dir, 'lib', 'libxios.so')

if not os.path.isfile(lib_path):
    raise FileNotFoundError(f"libxios.so not found at {lib_path}. Please check your installation.")

try:
    lib = CDLL(lib_path)
    # print(f"Library {lib_path} successfully loaded.")
except OSError as e:
    raise RuntimeError(f"Failed to load the library from {lib_path}: {e}")



############################################################################################
############################## XIOS PYTHON TYPES WRAPPER  #################################
# making class to overcome the 'non way to pass by reference an immutable object in python'#
############################################################################################

def typecheck(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        sig = inspect.signature(func)
        bound_args = sig.bind(*args, **kwargs)
        bound_args.apply_defaults()
        hints = get_type_hints(func)

        for name, value in bound_args.arguments.items():
            if name in hints:
                expected = hints[name]
                if hasattr(expected, '__origin__') and expected.__origin__ is Union:
                    # Unpack union types (e.g., Union[int, str])
                    if not any((typ is type(None) and value is None) or isinstance(value, typ)for typ in get_args(expected)):
                        raise TypeError(
                            f"Argument '{name}' must be of type {expected}, got {type(value)}"
                        )
                elif not isinstance(value, expected):
                    raise TypeError(
                        f"Argument '{name}' must be of type {expected}, got {type(value)}"
                    )

        return func(*args, **kwargs)

    return wrapper


class Bool:
    def __init__(self, value=False):
        if isinstance(value, bool):
            self._c_value = c_bool(value)
        else:
            raise ValueError("Bool only accpets bool")

    @property
    def value(self):
        return bool(self._c_value.value)

    def __repr__(self):
        return f"Bool({self.value})" 

    def __str__(self):
        return self.value
        
class Int:
    def __init__(self, value=0):
        if isinstance(value, int):
            self._c_value = (c_int(value))
        elif isinstance(value, Int):
            self._c_value = value._c_value
        else:
            raise ValueError("xios.Int only accpets int or xios.Int")

    @property
    def value(self):
        return int(self._c_value.value)

    def __str__(self):
        return str(self.value) 

    def __int__(self):
        return self.value

    def __add__(self, other):
        return int(self) + int(other)

    def __sub__(self, other):
        return int(self) - int(other)

    def __mul__(self, other):
        return int(self) * int(other)

    def __truediv__(self, other):
        return int(self) / int(other)

    def __floordiv__(self, other):
        return int(self) // int(other)

    def __radd__(self, other):
        return int(other) + int(self)

    def __rsub__(self, other):
        return int(other) - int(self)

    def __rmul__(self, other):
        return int(other) * int(self)

    def __rtruediv__(self, other):
        return int(other) / int(self)

    def __rfloordiv__(self, other):
        return int(other) // int(self)

class Double:
    def __init__(self, value=0):
        if isinstance(value, float):
            self._c_value = (c_double(value))
        elif isinstance(value, int):
            self._c_value = c_double(float(value))
        else:
            raise ValueError("xios.Double only accpets float or int")

    @property
    def value(self):
        return float(self._c_value.contents)


    def __repr__(self):
        return f"Double({self.value})" 

    def __str__(self):
        return self.value 

    def __float__(self):
        return self.value

    def __add__(self, other):
        return float(self) + float(other)

    def __sub__(self, other):
        return float(self) - float(other)

    def __mul__(self, other):
        return float(self) * float(other)

    def __truediv__(self, other):
        return float(self) / float(other)

    def __floordiv__(self, other):
        return float(self) // float(other)

    def __radd__(self, other):
        return float(other) + float(self)

    def __rsub__(self, other):
        return float(other) - float(self)

    def __rmul__(self, other):
        return float(other) * float(self)

    def __rtruediv__(self, other):
        return float(other) / float(self)

    def __rfloordiv__(self, other):
        return float(other) // float(self)

class String:
    def __init__(self, value=" "*128):
        if isinstance(value, String):
            self._c_value = value._c_value
        elif isinstance(value, str):
            self._c_value = create_string_buffer(value.encode('utf-8'))
        elif isinstance(value, (c_char_p, bytes)):
            self._c_value = create_string_buffer(value if isinstance(value, bytes) else value.value)
        else:
            raise TypeError("Unsupported type for String")
    
    @property
    def value(self):
        return string_at(self._c_value).decode('utf-8').rstrip()

    @value.setter
    def value(self, new_value):
        self._c_value = create_string_buffer(new_value.encode('utf-8'))


    def __repr__(self):
        return self.value

    def __str__(self):
        return self.value



class NpArray:
    _dtype = np.float64
    _ctype = c_double
    def __init__(self, arr=None, dim = None):
        self.shape_is_needed = False
        if arr is None:
            if dim is None :
                dim = (1,)
                self.shape_is_needed = True
            arr = np.zeros(dim, dtype=self._dtype, order = 'F')
        if isinstance(arr, np.ndarray):
            if arr.dtype != self._dtype:
                arr = arr.astype(self._dtype)  

            if not arr.flags['F_CONTIGUOUS']:
                arr = np.asfortranarray(arr) 

            self._arr = arr 
            self._c_value = arr.ctypes.data_as(POINTER(self._ctype))

        elif isinstance(arr, NpArray):
            self._arr = arr._arr 
            self._c_value = arr._c_value
            
        elif isinstance(arr, POINTER(self._ctype)):
            if dim is None : raise ValueError("You must provide dimensions if you create an array from ctypes !")
            self._arr = np.ctypeslib.as_array(arr, shape = dim)
            self._c_value = arr
        
        elif isinstance(arr, tuple):
            self._arr = np.array(arr, order = 'F')
            self._c_value = self._arr.ctypes.data_as(POINTER(self._ctype))

        else:
            raise TypeError(f"NpArray only accepts np.ndarray, NpArray or POINTER{self._ctype}")

    @property
    def arr(self):
        return self._arr

    @arr.setter
    def arr(self, new_arr):
        if not isinstance(new_arr, np.ndarray):
            raise TypeError("Assigned value must be a numpy.ndarray")
        if new_arr.dtype != self._dtype:
            new_arr = new_arr.astype(self._dtype)
        if not new_arr.flags['F_CONTIGUOUS']:
            new_arr = np.asfortranarray(new_arr)

        self._arr = new_arr
        self._c_value = new_arr.ctypes.data_as(POINTER(self._ctype))



    @property
    def ptr(self):
        return self._c_value

    @property
    def size(self):
        return self._arr.size

    @property
    def ndim(self):
        return self._arr.ndim

    @property
    def shape(self):
        return self._arr.shape

    @property
    def value(self):
        return self._arr

    def __str__(self):
        return np.array2string(self.value) 
        
    def __array__(self):
        return self._arr



class NpArrayInt(NpArray):
    _dtype = np.int32
    _ctype = c_int

             
class NpArrayBool(NpArray):
   _dtype = np.bool_
   _ctype = c_bool

class MPIComm:
    def __init__(self, comm = MPI.COMM_NULL):
        if isinstance(comm, MPI.Comm):
            self._c_value = pointer(c_int(comm.py2f()))
        elif isinstance(comm,  POINTER(c_int)):
            self._c_value = comm
        elif isinstance(comm, MPIComm):
            self._c_value = comm._c_value
        else:
            raise TypeError("MPIComm only accepts MPI.Comm, xios.MPIComm or pointer(c_int) type")

    @property
    def value(self):
        return MPI.Comm.f2py(self._c_value.contents.value)

    @value.setter
    def value(self, new_value):
        if isinstance(comm, MPI.Comm):
            self._c_value.value = MPI.Comm.f2py(new_value.contents)
        elif isinstance(comm,  POINTER(c_int)):
            self._c_value = new_value
        else:
            raise TypeError("MPIComm only accepts MPI.Intracomm type")
    
    @property
    def rank(self):
        return self.value.Get_rank()

    @property
    def size(self):
        return self.value.Get_size()

    


class CObject(Structure):
    _fields_ = [("daddr", c_void_p)]