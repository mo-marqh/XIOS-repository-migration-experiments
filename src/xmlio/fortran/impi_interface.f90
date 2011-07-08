MODULE IMPI_INTERFACE
   USE ISO_C_BINDING

include "mpif.h"

   INTEGER (kind = C_INT ), BIND(C, NAME = "mpi_success" )      :: xios_mpi_success      = MPI_SUCCESS
   INTEGER (kind = C_INT ), BIND(C, NAME = "mpi_comm_world" )   :: xios_mpi_comm_world   = MPI_COMM_WORLD
   INTEGER (kind = C_INT ), BIND(C, NAME = "mpi_char" )         :: xios_mpi_char         = MPI_CHARACTER
   INTEGER (kind = C_INT ), BIND(C, NAME = "mpi_status_size" )  :: xios_mpi_status_size  = MPI_STATUS_SIZE
   INTEGER (kind = C_INT ), BIND(C, NAME = "mpi_any_tag" )      :: xios_mpi_any_tag      = MPI_ANY_TAG
   INTEGER (kind = C_INT ), BIND(C, NAME = "mpi_request_null" ) :: xios_mpi_request_null = MPI_REQUEST_NULL

   CONTAINS

   ! Initialiser MPI
   SUBROUTINE xios_mpi_init(err) BIND(C, NAME ="mpi_init")
      INTEGER  (kind = C_INT) :: err
      CALL MPI_INIT(err)
   END SUBROUTINE xios_mpi_init

   ! Quitter MPI
   SUBROUTINE xios_mpi_finalize(err) BIND(C, NAME ="mpi_finalize")
      INTEGER  (kind = C_INT) :: err
      CALL MPI_FINALIZE(err)
   END SUBROUTINE xios_mpi_finalize

   ! Quitter brutalement MPI
   SUBROUTINE xios_mpi_abort(comm, errcode, err) BIND(C, NAME ="mpi_abort")
      INTEGER  (kind = C_INT) :: comm, errcode, err
      CALL MPI_ABORT(comm, errcode, err)
   END SUBROUTINE xios_mpi_abort

   ! Savoir si un processus à fait un MPI_INIT
   SUBROUTINE xios_mpi_initialized(flag, err) BIND(C, NAME ="mpi_initialized")
      LOGICAL (kind = C_BOOL) :: flag
      INTEGER (kind = C_INT)  :: err
      LOGICAL                 :: cflag
      cflag = flag
      CALL MPI_INITIALIZED(cflag, err)
   END SUBROUTINE xios_mpi_initialized

   ! Récupérer la chaine de caractères associée au code d'erreur err
   SUBROUTINE xios_mpi_error_string(errcode, chaine, taille_chaine, err) &
                                    BIND(C, NAME ="mpi_error_string")
      INTEGER (kind = C_INT)                 :: errcode, taille_chaine, err
      CHARACTER(kind = C_CHAR), DIMENSION(*) :: chaine
      CHARACTER(len = taille_chaine)         :: cchaine
      CALL MPI_ERROR_STRING(errcode, cchaine, taille_chaine, err)
      chaine(taille_chaine) = cchaine
   END SUBROUTINE xios_mpi_error_string

   ! Envoyer un message à un processus
   SUBROUTINE xios_mpi_send(buf, count, datatype, dest, tag, comm, err) &
                            BIND(C, NAME ="mpi_send")
      CHARACTER(kind = C_CHAR), DIMENSION(*) :: buf
      INTEGER (kind = C_INT)                 :: dest, count, datatype, tag, comm, err
      CALL MPI_SEND(buf, count, datatype, dest, tag, comm, err)
   END SUBROUTINE xios_mpi_send

   ! Recevoir un message d'un processus
   SUBROUTINE xios_mpi_recv(buf, count, datatype, source, &
                            tag, comm, status, err)       &
                            BIND(C, NAME ="mpi_recv")
      CHARACTER(kind = C_CHAR), DIMENSION(*)              :: buf
      INTEGER (kind = C_INT)                              :: count, datatype, source, tag, comm, err
      INTEGER (kind = C_INT) , DIMENSION(mpi_status_size) :: status
      CALL MPI_RECV(buf, count, datatype, source, tag, comm, status, err)
   END SUBROUTINE xios_mpi_recv

   ! Envoyer et recevoir un message
   SUBROUTINE xios_mpi_sendrecv(sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, &
                                recvtype, source, recvtag, comm, status, err) &
                                BIND(C, NAME ="mpi_sendrecv")
      CHARACTER(kind = C_CHAR), DIMENSION(*) :: sendbuf, recvbuf
      INTEGER (kind = C_INT)                 :: sendcount, sendtype, dest, sendtag, recvcount, &
                                                recvtype, source, recvtag, comm, err
      INTEGER (kind = C_INT) , DIMENSION(mpi_status_size) :: status
      CALL MPI_SENDRECV(sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, &
                        recvcount, recvtype, source, recvtag, comm, status, err)
   END SUBROUTINE xios_mpi_sendrecv

   ! Compter le nombre d'éléments reçus
   SUBROUTINE xios_mpi_get_count(status, datatype, count, err) BIND(C, NAME ="mpi_get_count")
      INTEGER (kind = C_INT)                 :: datatype, count, err
      INTEGER (kind = C_INT) , DIMENSION(mpi_status_size) :: status
      
      ! ATTTENTION GROS BUG ICI  SOUS GNU MAIS PAS INTEL ???????? voir datatype
      ! PRINT *, datatype, MPI_CHARACTER, xios_mpi_char
      CALL MPI_GET_COUNT(status, MPI_CHARACTER, count, err)
   END SUBROUTINE xios_mpi_get_count

   ! Tester l'arrivée d'un message
   SUBROUTINE xios_mpi_iprobe(source, tag, comm, flag, status, err) BIND(C, NAME ="mpi_iprobe")
      INTEGER (kind = C_INT) :: source, tag, comm, err
      INTEGER (kind = C_INT) , DIMENSION(mpi_status_size) :: status
      LOGICAL (kind = C_BOOL) :: flag
      CALL MPI_IPROBE(source, tag, comm, flag, status, err)
   END SUBROUTINE xios_mpi_iprobe

   ! Nombre de processus dans un intracommunicateur
   SUBROUTINE xios_mpi_comm_size(comm, nbre, err) BIND(C, NAME ="mpi_comm_size")
      INTEGER (kind = C_INT)  :: comm, nbre, err
      CALL MPI_COMM_SIZE(comm, nbre, err)
   END SUBROUTINE xios_mpi_comm_size

   ! Rang d'un processus dans un intracommunicateur
   SUBROUTINE xios_mpi_comm_rank(comm, rang, err) BIND(C, NAME ="mpi_comm_rank")
      INTEGER (kind = C_INT)  :: comm, rang, err
      CALL MPI_COMM_RANK(comm, rang, err )
   END SUBROUTINE xios_mpi_comm_rank

   ! Partage d'un communicateur
   SUBROUTINE xios_mpi_comm_split(comm, couleur, cle, newcomm, err) BIND(C, NAME ="mpi_comm_split")
      INTEGER (kind = C_INT)  :: comm, couleur, cle, newcomm, err
      CALL MPI_COMM_SPLIT(comm, couleur, cle, newcomm, err)
   END SUBROUTINE xios_mpi_comm_split

   ! Commencer à envoyer un message
   SUBROUTINE xios_mpi_issend(buf, count, datatype, dest, tag, comm, request, err) &
                            BIND(C, NAME ="mpi_issend")
      CHARACTER(kind = C_CHAR), DIMENSION(*) :: buf
      INTEGER (kind = C_INT)                 :: count, datatype, tag, comm, request, err, dest
      CALL MPI_ISSEND(buf, count, datatype, dest, tag, comm, request, err)
   END SUBROUTINE xios_mpi_issend

   ! Commencer à recevoir un message
   SUBROUTINE xios_mpi_irecv(buf, count, datatype, source, &
                            tag, comm, request, err)       &
                            BIND(C, NAME ="mpi_irecv")
      CHARACTER(kind = C_CHAR), DIMENSION(*)              :: buf
      INTEGER (kind = C_INT)                              :: count, datatype, source, tag, &
                                                             comm, request, err
      CALL MPI_IRECV(buf, count, datatype, source, tag, comm, request, err)
   END SUBROUTINE xios_mpi_irecv

   ! Compléter une opération non bloquante
   SUBROUTINE xios_mpi_wait(request, status, err) BIND(C, NAME ="mpi_wait")
      INTEGER (kind = C_INT)                              :: request, err
      INTEGER (kind = C_INT) , DIMENSION(mpi_status_size) :: status
      CALL MPI_WAIT(request, status, err)
   END SUBROUTINE xios_mpi_wait

   ! Tester une opération non bloquante
   SUBROUTINE xios_mpi_test(request, flag, status, err) BIND(C, NAME ="mpi_test")
      INTEGER (kind = C_INT)                              :: request, err
      INTEGER (kind = C_INT) , DIMENSION(mpi_status_size) :: status
      LOGICAL (kind = C_BOOL)                             :: flag
      LOGICAL                                             :: cflag
      cflag = flag
      CALL MPI_TEST(request, cflag, status, err)
   END SUBROUTINE xios_mpi_test

   ! Création d'un communicateur à partir d'un groupe
   SUBROUTINE xios_mpi_comm_create(comm, group, newcomm, err) BIND(C, NAME ="mpi_comm_create")
      INTEGER (kind = C_INT) :: comm, group, newcomm, err
      CALL MPI_COMM_CREATE(comm, group, newcomm, err)
   END SUBROUTINE xios_mpi_comm_create

   ! Obtention d'une groupe à partir d'un communicateur
   SUBROUTINE xios_mpi_comm_group(comm, group, err) BIND(C, NAME ="mpi_comm_group")
      INTEGER (kind = C_INT) :: comm, group, err
      CALL MPI_COMM_GROUP(comm, group, err)
   END SUBROUTINE xios_mpi_comm_group

   ! Création de sous-groupe
   SUBROUTINE xios_mpi_group_incl(group, n, rank, newgroup, err) BIND(C, NAME ="mpi_group_incl")
      INTEGER (kind = C_INT) :: group, n,  newgroup, err
      INTEGER (kind = C_INT) , DIMENSION(*) :: rank
      CALL MPI_GROUP_INCL(group, n, rank, newgroup, err)
   END SUBROUTINE xios_mpi_group_incl

   ! Barrière
   SUBROUTINE xios_mpi_barrier (comm, err) BIND (C, NAME ="mpi_barrier")
      INTEGER (kind = C_INT) :: comm, err
      CALL MPI_BARRIER(comm, err)
   END SUBROUTINE xios_mpi_barrier

   ! Collecte de données et rediffusion
   SUBROUTINE xios_mpi_allgather(sendbuf, sendcount, recvbuf, recvcount, comm, ierror) BIND (C, NAME ="mpi_allgather")
      INTEGER (kind = C_INT), DIMENSION(*) :: sendbuf, recvbuf
      INTEGER (kind = C_INT)               :: sendcount, recvcount, comm, ierror
       CALL MPI_ALLGATHER(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, comm, ierror)
   END SUBROUTINE xios_mpi_allgather

END MODULE IMPI_INTERFACE
