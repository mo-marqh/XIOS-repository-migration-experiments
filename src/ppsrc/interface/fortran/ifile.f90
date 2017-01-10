

MODULE IFILE
   USE, INTRINSIC :: ISO_C_BINDING
   USE FILE_INTERFACE
   USE FILEGROUP_INTERFACE
! USE IFILE_ATTR
! USE IFILEGROUP_ATTR
   USE IDURATION

   TYPE xios_file
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE xios_file

   TYPE xios_filegroup
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE xios_filegroup

   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios_get_file_handle( idt, ret)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: idt
      TYPE(xios_file) , INTENT(OUT):: ret

      CALL cxios_file_handle_create(ret%daddr, idt, len(idt))

   END SUBROUTINE xios_get_file_handle

   SUBROUTINE xios_get_filegroup_handle(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      TYPE(xios_filegroup), INTENT(OUT):: ret

      CALL cxios_filegroup_handle_create(ret%daddr, idt, len(idt))

   END SUBROUTINE xios_get_filegroup_handle

   LOGICAL FUNCTION xios_is_valid_file(idt)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      LOGICAL (kind = 1) :: val

      CALL cxios_file_valid_id(val, idt, len(idt));
      xios_is_valid_file = val

   END FUNCTION xios_is_valid_file

   LOGICAL FUNCTION xios_is_valid_filegroup(idt)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      LOGICAL (kind = 1) :: val

      CALL cxios_filegroup_valid_id(val, idt, len(idt));
      xios_is_valid_filegroup = val

   END FUNCTION xios_is_valid_filegroup


END MODULE IFILE
