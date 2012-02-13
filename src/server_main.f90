PROGRAM server_main
  USE xios
  IMPLICIT NONE
  INCLUDE "mpif.h"
  INTEGER :: ierr
  
!    CALL MPI_INIT(ierr)  
    CALL xios_init_server
!    CALL MPI_FINALIZE(ierr)  
  END PROGRAM server_main
