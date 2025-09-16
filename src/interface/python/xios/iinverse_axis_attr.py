# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.iinverse_axis import  get_inverse_axis_handle
from xios.oinverse_axis_attr import InverseAxis

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_inverse_axis_attr(inverse_axis_id : Union[String, str]):

  
  inverse_axis_hdl = InverseAxis()
  

  get_inverse_axis_handle(inverse_axis_id, inverse_axis_hdl)
  set_inverse_axis_attr_hdl_(inverse_axis_hdl)
  return 



@typecheck
def set_inverse_axis_attr_hdl(inverse_axis_hdl : InverseAxis):

  
  set_inverse_axis_attr_hdl_(inverse_axis_hdl)
  return 



@typecheck
def set_inverse_axis_attr_hdl_(inverse_axis_hdl : InverseAxis):

  
  
  return 



@typecheck
def get_inverse_axis_attr(inverse_axis_id : Union[String, str]):

  
  inverse_axis_hdl = InverseAxis()
  

  get_inverse_axis_handle(inverse_axis_id, inverse_axis_hdl)
  get_inverse_axis_attr_hdl_(inverse_axis_hdl)
  return 



@typecheck
def get_inverse_axis_attr_hdl(inverse_axis_hdl : InverseAxis):

  
  get_inverse_axis_attr_hdl_(inverse_axis_hdl)
  return 



@typecheck
def get_inverse_axis_attr_hdl_(inverse_axis_hdl : InverseAxis):

  
  
  return 



@typecheck
def is_defined_inverse_axis_attr(inverse_axis_id : String):

  
  inverse_axis_hdl = InverseAxis()
  

  get_inverse_axis_handle(inverse_axis_id, inverse_axis_hdl)
  is_defined_inverse_axis_attr_hdl_(inverse_axis_hdl)
  return 



@typecheck
def is_defined_inverse_axis_attr_hdl(inverse_axis_hdl : InverseAxis):

  
  is_defined_inverse_axis_attr_hdl_(inverse_axis_hdl)
  return 



@typecheck
def is_defined_inverse_axis_attr_hdl_(inverse_axis_hdl : InverseAxis):

  
  
  return 



