

MODULE IINTERPOLATE_AXIS
   USE, INTRINSIC :: ISO_C_BINDING
   USE INTERPOLATE_AXIS_INTERFACE

   TYPE xios_interpolate_axis
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE xios_interpolate_axis

   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios_get_interpolate_axis_handle(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: idt
      TYPE(xios_interpolate_axis) , INTENT(OUT):: ret
      CALL cxios_interpolate_axis_handle_create(ret%daddr, idt, len(idt))
   END SUBROUTINE xios_get_interpolate_axis_handle

   LOGICAL FUNCTION xios_is_valid_interpolate_axis(idt)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      LOGICAL (kind = 1) :: val

      CALL cxios_interpolate_axis_valid_id(val, idt, len(idt))
      xios_is_valid_interpolate_axis = val

   END FUNCTION xios_is_valid_interpolate_axis

END MODULE IINTERPOLATE_AXIS
