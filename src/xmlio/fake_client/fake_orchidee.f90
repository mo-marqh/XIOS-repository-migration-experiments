! --------------------------------------------------- !
!        XMLIO SERVER MAIN TEST (ORCHIDEE)            !
! --------------------------------------------------- !

MODULE ORCHIDEE_FAKE

   ! Modules de la bibliothèque xmlioserver
   USE XIOS
   USE ISO_C_BINDING

include 'mpif.h'

   CONTAINS

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE ORCHIDEE_FAKE_ENTRY(comm_client, comm_client_grp, comm_client_server) BIND(C)
      INTEGER(kind = C_INT), INTENT(IN), VALUE :: comm_client,       & ! communicateur des clients
                                                  comm_client_grp,   & ! communicateur du groupe de clients
                                                  comm_client_server   ! communicateur client-serveur
      INTEGER                                  :: rankGrp, error

      CALL MPI_COMM_RANK(comm_client, rankGrp, error)

      IF (rankGrp .EQ. 0) THEN
         PRINT*," Starting ORCHIDEE Client Tests ..."
      END IF

   END SUBROUTINE ORCHIDEE_FAKE_ENTRY

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE ORCHIDEE_FAKE
