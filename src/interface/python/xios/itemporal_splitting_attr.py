# * ************************************************************************** *
# *               Interface auto generated - do not modify                     *
# * ************************************************************************** *

import ctypes
import numpy as np
from typing import Union, Optional
from xios.config import lib, typecheck, String, Bool, Int, NpArrayInt, NpArrayBool, NpArray, Double
from xios.iduration import Duration
from xios.idate import Date
from xios.itemporal_splitting import  get_temporal_splitting_handle
from xios.otemporal_splitting_attr import TemporalSplitting

#_____________________________________________________________________________________________#
#                                    #C function definitions                                  #
#_____________________________________________________________________________________________#



#_____________________________________________________________________________________________#
#                                   #C interface functions                                    #
#_____________________________________________________________________________________________#

@typecheck
def set_temporal_splitting_attr(temporal_splitting_id : Union[String, str]):

  
  temporal_splitting_hdl = TemporalSplitting()
  

  get_temporal_splitting_handle(temporal_splitting_id, temporal_splitting_hdl)
  set_temporal_splitting_attr_hdl_(temporal_splitting_hdl)
  return 



@typecheck
def set_temporal_splitting_attr_hdl(temporal_splitting_hdl : TemporalSplitting):

  
  set_temporal_splitting_attr_hdl_(temporal_splitting_hdl)
  return 



@typecheck
def set_temporal_splitting_attr_hdl_(temporal_splitting_hdl : TemporalSplitting):

  
  
  return 



@typecheck
def get_temporal_splitting_attr(temporal_splitting_id : Union[String, str]):

  
  temporal_splitting_hdl = TemporalSplitting()
  

  get_temporal_splitting_handle(temporal_splitting_id, temporal_splitting_hdl)
  get_temporal_splitting_attr_hdl_(temporal_splitting_hdl)
  return 



@typecheck
def get_temporal_splitting_attr_hdl(temporal_splitting_hdl : TemporalSplitting):

  
  get_temporal_splitting_attr_hdl_(temporal_splitting_hdl)
  return 



@typecheck
def get_temporal_splitting_attr_hdl_(temporal_splitting_hdl : TemporalSplitting):

  
  
  return 



@typecheck
def is_defined_temporal_splitting_attr(temporal_splitting_id : String):

  
  temporal_splitting_hdl = TemporalSplitting()
  

  get_temporal_splitting_handle(temporal_splitting_id, temporal_splitting_hdl)
  is_defined_temporal_splitting_attr_hdl_(temporal_splitting_hdl)
  return 



@typecheck
def is_defined_temporal_splitting_attr_hdl(temporal_splitting_hdl : TemporalSplitting):

  
  is_defined_temporal_splitting_attr_hdl_(temporal_splitting_hdl)
  return 



@typecheck
def is_defined_temporal_splitting_attr_hdl_(temporal_splitting_hdl : TemporalSplitting):

  
  
  return 



