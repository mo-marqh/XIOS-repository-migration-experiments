from ctypes import c_void_p, c_char_p, c_int, c_bool, POINTER, c_double, string_at, byref, Structure, pointer
from typing import Union, Optional
from xios.config import lib, typecheck, String
from xios.oscalar_attr import Scalar
from xios.oscalargroup_attr import  ScalarGroup
from xios.oaxis_attr import Axis
from xios.oaxisgroup_attr import AxisGroup
from xios.odomain_attr import Domain
from xios.odomaingroup_attr import  DomainGroup
from xios.ofile_attr import File
from xios.ofilegroup_attr import FileGroup
from xios.ofield import Field
from xios.ofieldgroup_attr import FieldGroup
from xios.ovariable import Variable
from xios.ovariablegroup_attr import VariableGroup
from xios.ogrid_attr import Grid
from xios.ogridgroup_attr import GridGroup
from xios.ozoom_domain_attr import ZoomDomain
from xios.oextract_domain_attr import ExtractDomain
from xios.ointerpolate_domain_attr import InterpolateDomain
from xios.ogenerate_rectilinear_domain_attr import GenerateRectilinearDomain
from xios.ocompute_connectivity_domain_attr import ComputeConnectivityDomain
from xios.oexpand_domain_attr import ExpandDomain
from xios.oredistribute_domain_attr import RedistributeDomain
from xios.ozoom_axis_attr import ZoomAxis
from xios.ointerpolate_axis_attr import InterpolateAxis
from xios.oinverse_axis_attr import InverseAxis
from xios.oreduce_domain_to_axis_attr import ReduceDomainToAxis
from xios.oextract_domain_to_axis_attr import ExtractDomainToAxis
from xios.oredistribute_axis_attr import RedistributeAxis
from xios.oreduce_axis_to_scalar_attr import ReduceAxisToScalar
from xios.oextract_axis_to_scalar_attr import ExtractAxisToScalar
from xios.oreduce_domain_to_scalar_attr import ReduceDomainToScalar
from xios.oredistribute_scalar_attr import RedistributeScalar





#_____________________________________________________________________________________________#
#                                    #C functions definitions                                 #
#_____________________________________________________________________________________________#


lib.cxios_xml_tree_add_scalar.argtypes = [ScalarGroup, POINTER(Scalar), c_char_p, c_int]
lib.cxios_xml_tree_add_scalar.restype = None

lib.cxios_xml_tree_add_scalargroup.argtypes = [ScalarGroup, POINTER(ScalarGroup), c_char_p, c_int]
lib.cxios_xml_tree_add_scalargroup.restype = None

lib.cxios_xml_tree_add_axis.argtypes = [AxisGroup, POINTER(Axis), c_char_p, c_int]
lib.cxios_xml_tree_add_axis.restype = None

lib.cxios_xml_tree_add_axisgroup.argtypes = [AxisGroup, POINTER(AxisGroup), c_char_p, c_int]
lib.cxios_xml_tree_add_axisgroup.restype = None

lib.cxios_xml_tree_add_domain.argtypes = [DomainGroup, POINTER(Domain), c_char_p, c_int]
lib.cxios_xml_tree_add_domain.restype = None

lib.cxios_xml_tree_add_domaingroup.argtypes = [DomainGroup, POINTER(DomainGroup), c_char_p, c_int]
lib.cxios_xml_tree_add_domaingroup.restype = None

lib.cxios_xml_tree_add_fieldgrouptofile.argtypes = [File, POINTER(FieldGroup), c_char_p, c_int]
lib.cxios_xml_tree_add_fieldgrouptofile.restype = None

lib.cxios_xml_tree_add_fieldtofile.argtypes = [File, POINTER(Field), c_char_p, c_int]
lib.cxios_xml_tree_add_fieldtofile.restype = None

lib.cxios_xml_tree_add_variablegrouptofile.argtypes = [File, POINTER(VariableGroup), c_char_p, c_int]
lib.cxios_xml_tree_add_variablegrouptofile.restype = None

lib.cxios_xml_tree_add_variabletofile.argtypes = [File, POINTER(Variable), c_char_p, c_int]
lib.cxios_xml_tree_add_variabletofile.restype = None

lib.cxios_xml_tree_add_variablegrouptofield.argtypes = [Field, POINTER(VariableGroup), c_char_p, c_int]
lib.cxios_xml_tree_add_variablegrouptofield.restype = None

lib.cxios_xml_tree_add_variabletofield.argtypes = [Field, POINTER(Variable), c_char_p, c_int]
lib.cxios_xml_tree_add_variabletofield.restype = None

lib.cxios_xml_tree_add_file.argtypes = [FileGroup, POINTER(File), c_char_p, c_int]
lib.cxios_xml_tree_add_file.restype = None

lib.cxios_xml_tree_add_filegroup.argtypes = [FileGroup, POINTER(FileGroup), c_char_p, c_int]
lib.cxios_xml_tree_add_filegroup.restype = None

lib.cxios_xml_tree_add_grid.argtypes = [GridGroup, POINTER(Grid), c_char_p, c_int]
lib.cxios_xml_tree_add_grid.restype = None

lib.cxios_xml_tree_add_gridgroup.argtypes = [GridGroup, POINTER(GridGroup), c_char_p, c_int]
lib.cxios_xml_tree_add_gridgroup.restype = None

lib.cxios_xml_tree_add_field.argtypes = [FieldGroup, POINTER(Field), c_char_p, c_int]
lib.cxios_xml_tree_add_field.restype = None

lib.cxios_xml_tree_add_fieldgroup.argtypes = [FieldGroup, POINTER(FieldGroup), c_char_p, c_int]
lib.cxios_xml_tree_add_fieldgroup.restype = None

lib.cxios_xml_tree_add_scalartogrid.argtypes = [Grid, POINTER(Scalar), c_char_p, c_int]
lib.cxios_xml_tree_add_scalartogrid.restype = None

lib.cxios_xml_tree_add_axistogrid.argtypes = [Grid, POINTER(Axis), c_char_p, c_int]
lib.cxios_xml_tree_add_axistogrid.restype = None

lib.cxios_xml_tree_add_domaintogrid.argtypes = [Grid, POINTER(Domain), c_char_p, c_int]
lib.cxios_xml_tree_add_domaintogrid.restype = None

# --- Domain Transformations ---

lib.cxios_xml_tree_add_zoomdomaintodomain.argtypes = [Domain, POINTER(ZoomDomain), c_char_p, c_int]
lib.cxios_xml_tree_add_zoomdomaintodomain.restype = None

lib.cxios_xml_tree_add_extractdomaintodomain.argtypes = [Domain, POINTER(ExtractDomain), c_char_p, c_int]
lib.cxios_xml_tree_add_extractdomaintodomain.restype = None

lib.cxios_xml_tree_add_interpolatedomaintodomain.argtypes = [Domain, POINTER(InterpolateDomain), c_char_p, c_int]
lib.cxios_xml_tree_add_interpolatedomaintodomain.restype = None

lib.cxios_xml_tree_add_generatedomaintodomain.argtypes = [Domain, POINTER(GenerateRectilinearDomain), c_char_p, c_int]
lib.cxios_xml_tree_add_generatedomaintodomain.restype = None

lib.cxios_xml_tree_add_computeconnectivitydomaintodomain.argtypes = [Domain, POINTER(ComputeConnectivityDomain), c_char_p, c_int]
lib.cxios_xml_tree_add_computeconnectivitydomaintodomain.restype = None

lib.cxios_xml_tree_add_expanddomaintodomain.argtypes = [Domain, POINTER(ExpandDomain), c_char_p, c_int]
lib.cxios_xml_tree_add_expanddomaintodomain.restype = None

lib.cxios_xml_tree_add_redistributedomain.argtypes = [Domain, POINTER(RedistributeDomain), c_char_p, c_int]
lib.cxios_xml_tree_add_redistributedomain.restype = None

# --- Axis Transformations ---

lib.cxios_xml_tree_add_zoomaxistoaxis.argtypes = [Axis, POINTER(ZoomAxis), c_char_p, c_int]
lib.cxios_xml_tree_add_zoomaxistoaxis.restype = None

# lib.cxios_xml_tree_add_extractaxistoaxis.argtypes = [Axis, POINTER(ExtractAxis), c_char_p, c_int]
# lib.cxios_xml_tree_add_extractaxistoaxis.restype = None

lib.cxios_xml_tree_add_interpolateaxistoaxis.argtypes = [Axis, POINTER(InterpolateAxis), c_char_p, c_int]
lib.cxios_xml_tree_add_interpolateaxistoaxis.restype = None

lib.cxios_xml_tree_add_inverseaxistoaxis.argtypes = [Axis, POINTER(InverseAxis), c_char_p, c_int]
lib.cxios_xml_tree_add_inverseaxistoaxis.restype = None

lib.cxios_xml_tree_add_reducedomaintoaxistoaxis.argtypes = [Axis, POINTER(ReduceDomainToAxis), c_char_p, c_int]
lib.cxios_xml_tree_add_reducedomaintoaxistoaxis.restype = None

lib.cxios_xml_tree_add_extractdomaintoaxistoaxis.argtypes = [Axis, POINTER(ExtractDomainToAxis), c_char_p, c_int]
lib.cxios_xml_tree_add_extractdomaintoaxistoaxis.restype = None

lib.cxios_xml_tree_add_redistributeaxis.argtypes = [Axis, POINTER(RedistributeAxis), c_char_p, c_int]
lib.cxios_xml_tree_add_redistributeaxis.restype = None

# --- Scalar Transformations ---

lib.cxios_xml_tree_add_reduceaxistoscalartoscalar.argtypes = [Scalar, POINTER(ReduceAxisToScalar), c_char_p, c_int]
lib.cxios_xml_tree_add_reduceaxistoscalartoscalar.restype = None

lib.cxios_xml_tree_add_extractaxistoscalartoscalar.argtypes = [Scalar, POINTER(ExtractAxisToScalar), c_char_p, c_int]
lib.cxios_xml_tree_add_extractaxistoscalartoscalar.restype = None

lib.cxios_xml_tree_add_reducedomaintoscalartoscalar.argtypes = [Scalar, POINTER(ReduceDomainToScalar), c_char_p, c_int]
lib.cxios_xml_tree_add_reducedomaintoscalartoscalar.restype = None

lib.cxios_xml_tree_add_redistributescalar.argtypes = [Scalar, POINTER(RedistributeScalar), c_char_p, c_int]
lib.cxios_xml_tree_add_redistributescalar.restype = None


#__________________________________________________________________________________________________#
#                                      INTERFACE FUNCTIONS                                         #
#__________________________________________________________________________________________________#

@typecheck
def add_scalar(parent_hdl: ScalarGroup, child_hdl: Scalar, child_id: Optional[Union[str, String]] = None):
    if child_id is None:
        lib.cxios_xml_tree_add_scalar(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_scalar(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)


@typecheck
def add_axis(parent_hdl : AxisGroup, child_hdl : Axis, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_axis(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_axis(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_file(parent_hdl : FileGroup, child_hdl : File, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_file(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_file(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_grid(parent_hdl : GridGroup, child_hdl : Grid, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_grid(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_grid(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_domain(parent_hdl : DomainGroup, child_hdl : Domain, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_domain(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_domain(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)


@typecheck
def add_fieldtofile(parent_hdl : File, child_hdl : Field, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_fieldtofile(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_fieldtofile(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_variabletofile(parent_hdl : File, child_hdl : Variable, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_variabletofile(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_variabletofile(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_variabletofield(parent_hdl : Field, child_hdl : Variable, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_variabletofield(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_variabletofield(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)


@typecheck
def add_axisgroup(parent_hdl : AxisGroup, child_hdl : AxisGroup, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_axisgroup(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_axisgroup(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_scalargroup(parent_hdl : ScalarGroup, child_hdl : ScalarGroup, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_scalargroup(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_scalargroup(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_filegroup(parent_hdl : FileGroup, child_hdl : FileGroup, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_filegroup(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_filegroup(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_gridgroup(parent_hdl : GridGroup, child_hdl : GridGroup, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_gridgroup(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_gridgroup(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_domaingroup(parent_hdl : DomainGroup, child_hdl : DomainGroup, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_domaingroup(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_domaingroup(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_fieldgrouptofile(parent_hdl : File, child_hdl : FieldGroup, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_fieldgrouptofile(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_fieldgrouptofile(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_variablegrouptofile(parent_hdl : File, child_hdl : VariableGroup, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_variablegrouptofile(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_variablegrouptofile(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_variablegrouptofield(parent_hdl : Field, child_hdl : VariableGroup, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_variablegrouptofield(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_variablegrouptofield(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_scalartogrid(parent_hdl : Grid, child_hdl : Scalar, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_scalartogrid(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_scalartogrid(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_axistogrid(parent_hdl : Grid, child_hdl : Axis, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_axistogrid(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_axistogrid(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_domaintogrid(parent_hdl : Grid, child_hdl : Domain, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_domaintogrid(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_domaintogrid(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)


@typecheck
def add_zoomdomaintodomain(parent_hdl : Domain, child_hdl : ZoomDomain, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_zoomdomaintodomain(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_zoomdomaintodomain(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_extractdomaintodomain(parent_hdl : Domain, child_hdl : ExtractDomain, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_extractdomaintodomain(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_extractdomaintodomain(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_interpolatedomaintodomain(parent_hdl : Domain, child_hdl : InterpolateDomain, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_interpolatedomaintodomain(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_interpolatedomaintodomain(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_generaterectilineardomaintodomain(parent_hdl : Domain, child_hdl : GenerateRectilinearDomain, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_generatedomaintodomain(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_generatedomaintodomain(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_computeconnectivitydomaintodomain(parent_hdl : Domain, child_hdl : ComputeConnectivityDomain, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_computeconnectivitydomaintodomain(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_computeconnectivitydomaintodomain(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_expanddomaintodomain(parent_hdl : Domain, child_hdl : ExpandDomain, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_expanddomaintodomain(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_expanddomaintodomain(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_redistributedomain(parent_hdl : Domain, child_hdl : RedistributeDomain, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_redistributedomain(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_redistributedomain(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_zoomaxistoaxis(parent_hdl : Axis, child_hdl : ZoomAxis, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_zoomaxistoaxis(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_zoomaxistoaxis(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

# @typecheck
# def add_extractaxistoaxis(parent_hdl : Axis, child_hdl : ExtractAxis, child_id : Optional[Union[str, String]] = None):

#     if child_id == None:
#         lib.cxios_xml_tree_add_extractaxistoaxis(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
#     else:
#         child_id = String(child_id)
#         child_id_c = child_id._c_value
#         len_child_id_c = len(string_at(child_id_c))
#         lib.cxios_xml_tree_add_extractaxistoaxis(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_interpolateaxistoaxis(parent_hdl : Axis, child_hdl : InterpolateAxis, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_interpolateaxistoaxis(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_interpolateaxistoaxis(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_inverseaxistoaxis(parent_hdl : Axis, child_hdl : InverseAxis, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_inverseaxistoaxis(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_inverseaxistoaxis(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_reducedomaintoaxistoaxis(parent_hdl : Axis, child_hdl : ReduceDomainToAxis, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_reducedomaintoaxistoaxis(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_reducedomaintoaxistoaxis(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_extractdomaintoaxistoaxis(parent_hdl : Axis, child_hdl : ExtractDomainToAxis, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_extractdomaintoaxistoaxis(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_extractdomaintoaxistoaxis(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_redistributeaxis(parent_hdl : Axis, child_hdl : RedistributeAxis, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_redistributeaxis(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_redistributeaxis(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_reduceaxistoscalartoscalar(parent_hdl : Scalar, child_hdl : ReduceAxisToScalar, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_reduceaxistoscalartoscalar(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_reduceaxistoscalartoscalar(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_extractaxistoscalartoscalar(parent_hdl : Scalar, child_hdl : ExtractAxisToScalar, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_extractaxistoscalartoscalar(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_extractaxistoscalartoscalar(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_reducedomaintoscalartoscalar(parent_hdl : Scalar, child_hdl : ReduceDomainToScalar, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_reducedomaintoscalartoscalar(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_reducedomaintoscalartoscalar(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

@typecheck
def add_redistributescalar(parent_hdl : Scalar, child_hdl : RedistributeScalar, child_id : Optional[Union[str, String]] = None):

    if child_id == None:
        lib.cxios_xml_tree_add_redistributescalar(parent_hdl, pointer(child_hdl), String("NONE")._c_value, c_int(-1))
    else:
        child_id = String(child_id)
        child_id_c = child_id._c_value
        len_child_id_c = len(string_at(child_id_c))
        lib.cxios_xml_tree_add_redistributescalar(parent_hdl, pointer(child_hdl), child_id_c, len_child_id_c)

