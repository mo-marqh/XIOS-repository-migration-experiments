SUBROUTINE fxios_oasis_init(server_id,str_len) BIND(C,NAME="fxios_oasis_init")
  USE, INTRINSIC :: ISO_C_BINDING






    CHARACTER(kind = C_CHAR),DIMENSION(*) :: server_id
    INTEGER(kind = C_INT),VALUE :: str_len

    INTEGER :: comp_id
    CHARACTER(len=str_len) :: oasis_server_id
    INTEGER :: ierr
    INTEGER :: i

    DO i=1,str_len
      oasis_server_id(i:i)=server_id(i)
    ENDDO




    PRINT *,"---> prism_init",oasis_server_id,ierr

END SUBROUTINE fxios_oasis_init

SUBROUTINE fxios_oasis_enddef() BIND(C,NAME="fxios_oasis_enddef")
  USE, INTRINSIC :: ISO_C_BINDING



  IMPLICIT NONE
  INTEGER :: ierr




END SUBROUTINE fxios_oasis_enddef

SUBROUTINE fxios_oasis_finalize() BIND(C,NAME="fxios_oasis_finalize")
  USE, INTRINSIC :: ISO_C_BINDING






  IMPLICIT NONE
  INTEGER :: ierr





END SUBROUTINE fxios_oasis_finalize


SUBROUTINE fxios_oasis_get_localcomm(f_comm) BIND(C,NAME="fxios_oasis_get_localcomm")
  USE, INTRINSIC :: ISO_C_BINDING






  IMPLICIT NONE
  INTEGER(kind=C_INT) :: f_comm

  INTEGER :: comm
  INTEGER :: ierr




    f_comm=comm

END SUBROUTINE fxios_oasis_get_localcomm




SUBROUTINE fxios_oasis_get_intracomm(f_comm_client_server,client_id,str_len) BIND(C,NAME="fxios_oasis_get_intracomm")
  USE, INTRINSIC :: ISO_C_BINDING






  IMPLICIT NONE
  INTEGER(kind=C_INT) :: f_comm_client_server
  CHARACTER,DIMENSION(*) :: client_id
  INTEGER,VALUE :: str_len

  INTEGER :: comm_client_server
  CHARACTER(len=str_len) :: oasis_client_id
  INTEGER :: ierr
  INTEGER :: i

    DO i=1,str_len
      oasis_client_id(i:i)=client_id(i)
    ENDDO





    f_comm_client_server=comm_client_server

END SUBROUTINE fxios_oasis_get_intracomm

SUBROUTINE fxios_oasis_get_intercomm(f_comm_client_server,client_id,str_len) BIND(C,NAME="fxios_oasis_get_intercomm")
  USE, INTRINSIC :: ISO_C_BINDING






  IMPLICIT NONE
  INTEGER(kind=C_INT) :: f_comm_client_server
  CHARACTER,DIMENSION(*) :: client_id
  INTEGER,VALUE :: str_len

  INTEGER :: comm_client_server
  CHARACTER(len=str_len) :: oasis_client_id
  INTEGER :: ierr
  INTEGER :: i

    DO i=1,str_len
      oasis_client_id(i:i)=client_id(i)
    ENDDO





    f_comm_client_server=comm_client_server

END SUBROUTINE fxios_oasis_get_intercomm
