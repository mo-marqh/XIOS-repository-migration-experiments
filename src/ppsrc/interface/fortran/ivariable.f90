

MODULE IVARIABLE
   USE, INTRINSIC :: ISO_C_BINDING
   USE VARIABLE_INTERFACE
   USE VARIABLEGROUP_INTERFACE

   TYPE xios_variable
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE xios_variable

   TYPE xios_variablegroup
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE xios_variablegroup



   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios_get_variable_handle(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: idt
      TYPE(xios_variable) , INTENT(OUT):: ret
      CALL cxios_variable_handle_create(ret%daddr, idt, len(idt))
   END SUBROUTINE xios_get_variable_handle

   SUBROUTINE xios_get_variablegroup_handle(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      TYPE(xios_variablegroup), INTENT(OUT):: ret

      CALL cxios_variablegroup_handle_create(ret%daddr, idt, len(idt))

   END SUBROUTINE xios_get_variablegroup_handle

   LOGICAL FUNCTION xios_is_valid_variable(idt)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      LOGICAL (kind = 1) :: val

      CALL cxios_variable_valid_id(val, idt, len(idt))
      xios_is_valid_variable = val

   END FUNCTION xios_is_valid_variable

   LOGICAL FUNCTION xios_is_valid_variablegroup(idt)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      LOGICAL (kind = 1) :: val

      CALL cxios_variablegroup_valid_id(val, idt, len(idt))
      xios_is_valid_variablegroup = val

   END FUNCTION xios_is_valid_variablegroup

END MODULE IVARIABLE
