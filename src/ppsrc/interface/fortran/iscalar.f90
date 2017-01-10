

MODULE ISCALAR
   USE, INTRINSIC :: ISO_C_BINDING
   USE SCALAR_INTERFACE
   USE SCALARGROUP_INTERFACE

   TYPE xios_scalar
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE xios_scalar

   TYPE xios_scalargroup
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE xios_scalargroup



   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios_get_scalar_handle(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: idt
      TYPE(xios_scalar) , INTENT(OUT):: ret
      CALL cxios_scalar_handle_create(ret%daddr, idt, len(idt))
   END SUBROUTINE xios_get_scalar_handle

   SUBROUTINE xios_get_scalargroup_handle(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      TYPE(xios_scalargroup), INTENT(OUT):: ret

      CALL cxios_scalargroup_handle_create(ret%daddr, idt, len(idt))

   END SUBROUTINE xios_get_scalargroup_handle

   LOGICAL FUNCTION xios_is_valid_scalar(idt)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      LOGICAL (kind = 1) :: val

      CALL cxios_scalar_valid_id(val, idt, len(idt))
      xios_is_valid_scalar = val

   END FUNCTION xios_is_valid_scalar

   LOGICAL FUNCTION xios_is_valid_scalargroup(idt)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      LOGICAL (kind = 1) :: val

      CALL cxios_scalargroup_valid_id(val, idt, len(idt))
      xios_is_valid_scalargroup = val

   END FUNCTION xios_is_valid_scalargroup

END MODULE ISCALAR
