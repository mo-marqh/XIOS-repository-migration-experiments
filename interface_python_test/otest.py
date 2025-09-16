import xios
import numpy as np
# from mpi4py import MPI


#Initialization phase
comm = xios.initialize('client')                  # xios.initialize('client', return_comm = comm)

#Communicator
rank = comm.rank
size = comm.size

print("Hello, I am the MPI proc number ", rank)


# CONTEXT 1 #
context = xios.Context('context', comm)

#Calling getter and setter

if not rank: print(f"The type of the calendar's context \'{xios.current_context_id()}\' is {context.calendar.type}")

context.calendar.start_date  = xios.Date(year = 2000, month = 8)
context.calendar.time_origin = xios.Date(year = 2000, month = 8)
context.calendar.timestep    = xios.Duration(second = 36000)

if not rank: print(f"The start date is {context.calendar.start_date}")

domain = xios.Domain('domain')

if not rank: print(f"The domain size is {domain.ni_glo}*{domain.nj_glo}")


#Domain distribution only along the i coordinate
ni_local = domain.ni_glo // size
ibegin = rank * ni_local

if rank == size - 1:
    ni_local = domain.ni_glo - ibegin

lonvalue = 360 * (np.arange(ibegin, ibegin + ni_local) / domain.ni_glo) - 180
latvalue = 180 * (np.arange(domain.nj_glo) / domain.nj_glo) - 90

domain.ni          = ni_local
domain.nj          = domain.nj_glo
domain.ibegin      = ibegin
domain.jbegin      = 0
domain.lonvalue_1d = lonvalue
domain.latvalue_1d = latvalue

name = domain.__class__.__name__
print("TYYYPE", name)

n = context.axis('axis').n_glo

context.close_definition()

# CONTEXT 2#
context2 = xios.Context('context2', comm)


context2.calendar.set_attr(type = 'd360', start_date = xios.Date(year = 2000, month = 8), time_origin = xios.Date(year = 2000, month = 8),
                         timestep = xios.Duration(second = 36000))

context.calendar.comment = 'Hello I\'m a calendar'
print("The calendar is:", context.calendar)

#Calling getter and setter

if not rank: print(f"The type of the calendar's context \'{xios.current_context_id()}\' is {context2.calendar.type}")

if not rank: print(f"The start date is {context2.calendar.start_date}")

domain2 = xios.Domain('domain2', ni = ni_local, nj = domain.nj_glo, ibegin = ibegin, jbegin = 0,
                     lonvalue_1d = latvalue, latvalue_1d = latvalue)

if not rank: print(f"The domain size is {domain2.ni_glo}*{domain2.nj_glo}")

context2.close_definition()

CONTEXT = [context, context2]
FIELD   = [context.field('field'), context2.field('field2')]

#Building the data to be send
ni, nj = ni_local, domain2.nj_glo
x, y, z = np.indices((ni, nj, n))
data = rank + 0.1 * x + 0.01 * y + 0.001 * z   #to vizualize the domain distribution

#Time loop, sending the field
for ts in range(20):
   for (c, f) in zip(CONTEXT, FIELD):
      c.calendar.update(ts)
      f.send(data)

#context and xios finalization
context.finalize()
context2.finalize()


xios.finalize()