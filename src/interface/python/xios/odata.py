from xios.idata import initialize as initialize_, finalize as finalize_, context_is_initialized
from mpi4py import MPI
from xios.config import MPIComm


class XIOS:
    def __init__(self):
        self._communicator = None
        self._initialized = False
        self._finalized = True

    def initialize(self, client_id, local_comm=None, return_comm=None):

        if return_comm is not None:
            self._communicator = return_comm
        else:
            self._communicator = MPIComm(MPI.COMM_WORLD)

        if self._initialized:
            print("xios is already initialized!")
            return
        initialize_(client_id, return_comm=self._communicator)

        self._initialized = True
        self._finalized   = False
        return self._communicator

    def finalize(self):
        if self._finalized:
            print("xios is already finalized !")
            return
        finalize_()
        self._finalized = True
        self._initialized = False
    
    @property
    def initialized(self):
        return self._initialized

    @property
    def finalized(self):
        return self._finalized




_xios_instance = XIOS()
initialize = _xios_instance.initialize
finalize   = _xios_instance.finalize