# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.iduplicate_scalar_to_axis import  get_duplicate_scalar_to_axis_handle
from xios.oduplicate_scalar_to_axis_attr import DuplicateScalarToAxis

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_duplicate_scalar_to_axis_attr(duplicate_scalar_to_axis_id : Union[String, str]):

  
  duplicate_scalar_to_axis_hdl = DuplicateScalarToAxis()
  

  get_duplicate_scalar_to_axis_handle(duplicate_scalar_to_axis_id, duplicate_scalar_to_axis_hdl)
  set_duplicate_scalar_to_axis_attr_hdl_(duplicate_scalar_to_axis_hdl)
  return 



@typecheck
def set_duplicate_scalar_to_axis_attr_hdl(duplicate_scalar_to_axis_hdl : DuplicateScalarToAxis):

  
  set_duplicate_scalar_to_axis_attr_hdl_(duplicate_scalar_to_axis_hdl)
  return 



@typecheck
def set_duplicate_scalar_to_axis_attr_hdl_(duplicate_scalar_to_axis_hdl : DuplicateScalarToAxis):

  
  
  return 



@typecheck
def get_duplicate_scalar_to_axis_attr(duplicate_scalar_to_axis_id : Union[String, str]):

  
  duplicate_scalar_to_axis_hdl = DuplicateScalarToAxis()
  

  get_duplicate_scalar_to_axis_handle(duplicate_scalar_to_axis_id, duplicate_scalar_to_axis_hdl)
  get_duplicate_scalar_to_axis_attr_hdl_(duplicate_scalar_to_axis_hdl)
  return 



@typecheck
def get_duplicate_scalar_to_axis_attr_hdl(duplicate_scalar_to_axis_hdl : DuplicateScalarToAxis):

  
  get_duplicate_scalar_to_axis_attr_hdl_(duplicate_scalar_to_axis_hdl)
  return 



@typecheck
def get_duplicate_scalar_to_axis_attr_hdl_(duplicate_scalar_to_axis_hdl : DuplicateScalarToAxis):

  
  
  return 



@typecheck
def is_defined_duplicate_scalar_to_axis_attr(duplicate_scalar_to_axis_id : String):

  
  duplicate_scalar_to_axis_hdl = DuplicateScalarToAxis()
  

  get_duplicate_scalar_to_axis_handle(duplicate_scalar_to_axis_id, duplicate_scalar_to_axis_hdl)
  is_defined_duplicate_scalar_to_axis_attr_hdl_(duplicate_scalar_to_axis_hdl)
  return 



@typecheck
def is_defined_duplicate_scalar_to_axis_attr_hdl(duplicate_scalar_to_axis_hdl : DuplicateScalarToAxis):

  
  is_defined_duplicate_scalar_to_axis_attr_hdl_(duplicate_scalar_to_axis_hdl)
  return 



@typecheck
def is_defined_duplicate_scalar_to_axis_attr_hdl_(duplicate_scalar_to_axis_hdl : DuplicateScalarToAxis):

  
  
  return 



