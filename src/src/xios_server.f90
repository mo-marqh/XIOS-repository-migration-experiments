PROGRAM server_main
  USE xios
  IMPLICIT NONE
  INCLUDE "mpif.h"
  INTEGER :: ierr
  INTEGER            :: server_level = 0  
  ! 0 in case of a single server pool 
  ! 1 for primary server in case of two server pools
  ! 2 for secondary server in case of two server pools

  CALL xios_init_server(server_level)

END PROGRAM server_main
