import xios
import numpy as np
from mpi4py import MPI

#Communicator
comm_ = MPI.COMM_WORLD
rank = comm_.Get_rank()
size = comm_.Get_size()

print("Hello, I am the MPI proc number ", rank)

#Variable initialization
current_context_id = xios.String()
comm               = xios.MPIComm(comm_)
calendar_type      = xios.String()
start_date         = xios.Date()
n                  = xios.Int()
ni_glo             = xios.Int()
nj_glo             = xios.Int()

#Initialization phase
xios.initialize('client', return_comm = comm)


# CONTEXT 1 #
xios.context_initialize('context', comm)

#Calling getter and setter
xios.get_current_context(current_context_id)
xios.get_calendar_type(calendar_type)

if not rank: print(f"The type of the calendar's context \'{current_context_id}\' is {calendar_type}")



xios.set_start_date(xios.Date(year = 2000, month = 8))
xios.set_time_origin(xios.Date(year = 2000, month = 8))
xios.set_timestep(xios.Duration(second = 36000))
xios.get_start_date(start_date)

if not rank: print(f"The start date is {start_date}")

xios.get_domain_attr('domain', ni_glo = ni_glo, nj_glo = nj_glo)

if not rank: print(f"The domain size is {ni_glo}*{nj_glo}")


#Domain distribution only along the i coordinate
ni_local = ni_glo // size
ibegin = xios.Int(rank * ni_local)

if rank == size - 1:
    ni_local = ni_glo - ibegin

lonvalue = 360 * (np.arange(ibegin, ibegin + ni_local) / ni_glo) - 180
latvalue = 180 * (np.arange(nj_glo) / nj_glo) - 90

dom = xios.Domain()
plouc = xios.String()
xios.get_handle('domain', dom)
xios.get_domain_attr_hdl(dom, type = plouc)
print(plouc)

xios.set_attr(dom, ni = ni_local, nj = nj_glo, ibegin = ibegin, jbegin = 0, lonvalue_1d = lonvalue, latvalue_1d = latvalue)
ni_dom = xios.Int()
xios.get_attr(dom, ni = ni_dom)
print("Attribute ni is ", ni_dom)
xios.get_axis_attr('axis', n_glo = n)

xios.close_context_definition() 

# CONTEXT 2#
xios.context_initialize('context2', comm)



xios.define_calendar(type = 'd360')

#Calling getter and setter
xios.get_current_context(current_context_id)
xios.get_calendar_type(calendar_type)

if not rank: print(f"The type of the calendar's context \'{current_context_id}\' is {calendar_type}")

xios.set_start_date(xios.Date(year = 2000, month = 8))
xios.set_time_origin(xios.Date(year = 2000, month = 8))
xios.set_timestep(xios.Duration(second = 36000))
xios.get_start_date(start_date)

if not rank: print(f"The start date is {start_date}")

xios.get_domain_attr('domain2', ni_glo = ni_glo, nj_glo = nj_glo)

if not rank: print(f"The domain size is {ni_glo}*{nj_glo}")

xios.set_domain_attr('domain2', ni = ni_local, nj = nj_glo, ibegin = ibegin, jbegin = xios.Int(0),
                     lonvalue_1d = latvalue, latvalue_1d = latvalue)

xios.close_context_definition() 

#Building the data to be send
ni, nj = ni_local, nj_glo.value
x, y, z = np.indices((ni, nj, n.value))
data = rank + 0.1 * x + 0.01 * y + 0.001 * z   #to vizualize the domain distribution

#test getVar
# d = xios.Int()
# my_field_hdl = xios.Field()
# my_field_attr_hdl = xios.Variable()
# xios.get_field_handle("field2", my_field_hdl)
# xios.add_variabletofield(my_field_hdl, my_field_attr_hdl, "new_attr_field_id")
# xios.set_variable_attr("new_attr_field_id", name="field_id", type="int")
# ok = xios.setVar("new_attr_field_id", 0)
# print("setVar ok?", ok)
# ok = xios.getVar("new_attr_field_id", d)
# print("var is", d)

#test getVar
my_field_hdl = xios.Field()
my_field_attr_hdl = xios.Variable()
xios.get_field_handle("field2", my_field_hdl)
xios.add_child(my_field_hdl, my_field_attr_hdl, "new_attr_field_id")
xios.set_variable_attr("new_attr_field_id", name="field_id", type="string")
ok = xios.setVar("new_attr_field_id", "new field attr generated using .. ")
print("setVar ok?", ok)

#Time loop, sending the field
for ts in range(20):
   for s in ['', '2']:
      xios.set_current_context('context'+s)
      xios.update_calendar(ts)
      xios.send_field('field'+s, data)


#context and xios finalization
xios.context_finalize()
xios.set_current_context('context')
xios.context_finalize()


xios.finalize()