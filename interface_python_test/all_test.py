import xios
from mpi4py import MPI
import numpy as np

test = 'default'

if test == 'default':
    return_comm  = xios.MPIComm()
    context_id   = xios.String("context")
    context_id2  = xios.String("context2")
    context_hdl  = xios.Context()
    context_hdl2 = xios.Context()
    context_hdl3 = xios.Context()
    axis_id      = "axis" 
    axis_hdl     = xios.Axis()
    domain_id    = "domain"
    domain_hdl   = xios.Domain()
    scalar_id    = "scalar"
    scalar_hdl   = xios.Scalar()
    field_id     = "field"
    field_hdl    = xios.Field()
    file_id     = "file"
    file_hdl    = xios.File()

print('\n\n\n')


xios.initialize("client", return_comm = return_comm)
xios.context_initialize(context_id, return_comm)

############################################################################
#============================  ICONTEXT  ==================================#
############################################################################
print( '                    TEST ICONTEXT                                    ')

if xios.context_is_initialized(context_id): print("context is WELL initialize")
xios.get_context_handle(context_id, context_hdl)
xios.get_current_context_handle(context_hdl2)
if context_hdl.daddr == context_hdl2.daddr : print("check get context handle ok !")
xios.get_current_context_id(context_id)
print("current context id is : ", context_id.value)
print('\n\n\n')
############################################################################
#======================= IAXIS-IDOMAIN-ISCALAR ============================#
############################################################################
print( '                TEST IAXIS-DOMAIN-SCALAR                               ')

xios.get_axis_handle(axis_id, axis_hdl)
if xios.is_valid_axis(axis_id) : print("This is a valid axis :)")

xios.get_domain_handle(domain_id, domain_hdl)
if xios.is_valid_domain(domain_id) : print("This is a valid domain :)")

xios.get_scalar_handle(scalar_id, scalar_hdl)
if xios.is_valid_scalar(scalar_id) : print("This is a valid scalar :)")
print('\n\n\n')

###########################################################################
# ============================  IFIELD  ====================================#
###########################################################################
# still active and get_domain(resp. axis, scalar) from field to test
print( '                    TEST IFIELD                                    ')


xios.get_field_handle(field_id, field_hdl)
if xios.is_valid_field(field_id) : print("This is a valid field :)")
# if xios.field_is_active_id(field_id) : print("This is a active field :)")
print('\n\n\n')

# ##########################################################################
# ============================  IFILE  ====================================#
# ##########################################################################
print( '                    TEST IFILE                                    ')

xios.get_file_handle(file_id, file_hdl)
if xios.is_valid_file(file_id) : print("This is a valid file :)")

print('\n\n\n')
#############################################################################
# ==================== DATE-DURATION_INTERFACE ============================ #
#############################################################################
print( '                        TEST IDURATION-IDATE                      ')

d1 = xios.Duration(year = 2, second = 10, month = 1)
d2 = xios.Duration(day = 14, month = 5)
d3 = xios.Duration(year = 2, second = 10, day = 14, month = 6)
dur_str = xios.String('lalala')
print("Performing operation", d1, "+", d2)


d_sum  = 1*d1 + d2
xios.duration_convert_to_string(d_sum, dur_str)
print("The computed duration is ", dur_str)

if d_sum == d3 : print("the duration computation is correct :)")

date = xios.Date(year = 2, second = 10, month = 1)
date2 = xios.Date(year = 2, second = 10, month = 6, day = 15)

dur_str = xios.String('lalala')
print("Performing operation", date, "+", d2)


d_sum  = date + d2
xios.date_convert_to_string(d_sum, dur_str)
print("The computed date is ", dur_str)

if d_sum == date2 : print("the date computation is correct :)")

######################################################################################
#============================= TRANSFORMATION =======================================#
######################################################################################
print( '                    TEST icompute_connectivity_domain                          ')

g = xios.GenerateRectilinearDomain()
xios.get_generate_rectilinear_domain_handle("gen", g)
if xios.is_valid_generate_rectilinear_domain("gen") : print("This is a valid transformation :)")

print('\n\n\n')


ni = 10
nj = 20
data = np.array([i + 1000*j for i in range(ni) for j in range(nj)])

# xios.close_context_definition()

# xios.send_field_id(field_id, data)

xios.context_finalize()
xios.finalize()